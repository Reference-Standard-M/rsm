/*
 * Package: Reference Standard M
 * File:    rsm/compile/parse.c
 * Summary: module compile - parse a line
 *
 * David Wicksell <dlw@linux.com>
 * Copyright © 2020-2024 Fourth Watch Software LC
 * https://gitlab.com/Reference-Standard-M/rsm
 *
 * Based on MUMPS V1 by Raymond Douglas Newman
 * Copyright © 1999-2018
 * https://gitlab.com/Reference-Standard-M/mumpsv1
 *
 * This program is free software: you can redistribute it and/or modify it
 * under the terms of the GNU Affero General Public License (AGPL) as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see https://www.gnu.org/licenses/.
 *
 * SPDX-FileCopyrightText:  © 2020 David Wicksell <dlw@linux.com>
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#include "compile.h"                                                            // compile stuff
#include "error.h"                                                              // and the error defs
#include "opcode.h"                                                             // and the opcodes
#include "proto.h"                                                              // standard prototypes
#include <assert.h>
#include <ctype.h>
#include <string.h>

u_char *jmp_eoc = NULL;                                                         // jump to end of cmd required

static void write_fmt(void)                                                     // called by parse_read/write
{
    int    i;                                                                   // a handy int
    int    args = 0;                                                            // number of args
    u_char *ptr;                                                                // a handy pointer

    source_ptr++;                                                               // increment source
    ptr = comp_ptr;                                                             // save compile pointer
    *comp_ptr++ = CMDOWRT;                                                      // must be one of these
    i = routine(0);                                                             // parse the rouref

    if (i != -1) {                                                              // must be just tag
        comp_ptr = ptr;                                                         // point to begining
        SYNTX;                                                                  // and complain
    }

    if (*source_ptr == '(') {                                                   // any args?
        int savecount = comp_ptr - ptr;                                         // bytes that got compiled
        u_char save[1024];                                                      // a useful save area

        memcpy(save, ptr, savecount);                                           // save that lot
        comp_ptr = ptr;                                                         // back where we started
        source_ptr++;                                                           // skip the (

        while (TRUE) {                                                          // while we have args
            if (args > MAX_NUM_ARGS) SYNTX;                                     // too many
            args++;                                                             // count an argument

            if (*source_ptr == ')') {                                           // trailing bracket ?
                source_ptr++;                                                   // skip the )
                break;                                                          // and exit
            }

            if ((*source_ptr == ',') || (*source_ptr == ')')) {                 // if empty argument
                *comp_ptr++ = VARUNDF;                                          // flag it
            } else if ((*source_ptr == '.') && (isdigit(source_ptr[1]) == 0)) { // by-reference? and not .numeric
                SYNTX;                                                          // that's not allowed
            } else {                                                            // by value
                eval();                                                         // leave the value on the stack
            }

            if (*source_ptr == ')') continue;                                   // trailing bracket ? then do it above

            if (*source_ptr == ',') {                                           // a comma ?
                source_ptr++;                                                   // skip the ,
                continue;                                                       // go for more
            }

            SYNTX;                                                              // all else is an error
        }                                                                       // end of while

        memcpy(comp_ptr, save, savecount);                                      // copy the code back
        comp_ptr += savecount;                                                  // and add to the pointer
    }                                                                           // end of argument decode

    *comp_ptr++ = (u_char) args;                                                // store number of args
    return;                                                                     // and exit
}

static int parse2eq(const u_char *ptr)                                          // scan to = or EOS
{
    int i = 0;                                                                  // a handy int
    int b = 0;                                                                  // in brackets
    int q = 0;                                                                  // in quotes
    int v = 0;                                                                  // in vertical brackets
    int s = 0;                                                                  // in square brackets

    while (TRUE) {                                                              // keep looping
        u_char c = ptr[i++];                                                    // get the current character

        if (c == '\0') break;                                                   // ran out of string

        if (!b && (c == '|')) {                                                 // new style extended reference?
            v = !v;
            continue;                                                           // and go for more
        }

        if (v) continue;                                                        // ignore in vertical brackets

        if (!b && (c == '[')) {                                                 // square bracket extended reference?
            s++;
            continue;                                                           // and go for more
        }

        if (!b && (c == ']')) {                                                 // end square brackets
            s--;
            continue;                                                           // and go for more
        }

        if (s) continue;                                                        // ignore in square brackets

        if (c == '"') {                                                         // a quote?
            q = !q;                                                             // reverse quote switch
            continue;                                                           // and go for more
        }

        if (q) continue;                                                        // continue if in quotes

        if (c == '(') {                                                         // open bracket
            b++;                                                                // increment counter
            continue;                                                           // and go for more
        }

        if (c == ')') {                                                         // close bracket
            b--;                                                                // decrement count
            if (b < 0) break;                                                   // EOS
            continue;                                                           // go for more
        }

        if (b) continue;                                                        // ignore in brackets
        if ((c == '=') || (c == ' ') || (c == ',')) break;
    }

    return i - 1;                                                               // return offset to term
}

static short parse_read_var(int star)                                           // called below
{
    char   c;                                                                   // current character
    short  s;                                                                   // for functions
    int    type;                                                                // a handy flag
    u_char *ptr;                                                                // a handy pointer

    c = *source_ptr;                                                            // get first char

    if (c == '@') {                                                             // indirection ?
        source_ptr++;                                                           // skip the @
        atom();                                                                 // eval the indirect bit

        if ((*source_ptr == '@') || star) {                                     // another one?
            *comp_ptr++ = INDMVAR;                                              // make an mvar out of it
            ptr = comp_ptr;                                                     // save for opcode insert

            if (*source_ptr == '@') {
                s = localvar();                                                 // parse the rest of it

                if (s < 0) {                                                    // if we got an error
                    comperror(s);                                               // compile it
                    return s;                                                   // and exit
                }

                ptr[s] = OPMVAR;                                                // make an mvar of it
            }

            type = (star ? CMREADS : CMREAD);                                   // the default opcode

            if (!star && (*source_ptr == '#')) {                                // read count ?
                source_ptr++;                                                   // skip the #
                eval();                                                         // eval argument
                type = CMREADC;                                                 // now a read with count
            }

            if (*source_ptr == ':') {                                           // timeout ?
                source_ptr++;                                                   // skip the :
                eval();                                                         // eval argument

                if (star) {
                    type = CMREADST;
                } else {
                    type = ((type == CMREAD) ? CMREADT : CMREADCT);             // replace op code
                }
            }

            *comp_ptr++ = (u_char) type;                                        // this opcode
        } else {                                                                // end READ @x@(...)
            *comp_ptr++ = INDREAD;                                              // do a full indirection
        }
    } else {                                                                    // end indirection - must be a variable
        ptr = comp_ptr;                                                         // remember where we are
        s = localvar();                                                         // parse the variable

        if (s < 0) {                                                            // if we got an error
            comperror(s);                                                       // compile it
            return s;                                                           // and exit
        }

        ptr[s] = OPMVAR;                                                        // get an mvar on the addstk[]
        type = (star ? CMREADS : CMREAD);                                       // the default opcode

        if (!star && (*source_ptr == '#')) {                                    // read count ?
            source_ptr++;                                                       // skip the #
            eval();                                                             // eval argument
            type = CMREADC;                                                     // now a read with count
        }

        if (*source_ptr == ':') {                                               // timeout ?
            source_ptr++;                                                       // skip the :
            eval();                                                             // eval argument

            if (star) {
                type = CMREADST;
            } else {
                type = ((type == CMREAD) ? CMREADT : CMREADCT);                 // replace op code
            }
        }

        *comp_ptr++ = (u_char) type;                                            // this opcode
    }

    return 0;
}

void parse_close(void)                                                          // CLOSE
{
    while (TRUE) {                                                              // get the args
        int iflag = (*source_ptr == '@');                                       // check for indirection

        eval();                                                                 // get arg

        if ((*(comp_ptr - 1) == INDEVAL) && iflag) {                            // if it was indirect
            *(comp_ptr - 1) = INDCLOS;                                          // say close indirect
        } else {
            *comp_ptr++ = CMCLOSE;                                              // the op code
        }

        if (*source_ptr != ',') break;                                          // done
        source_ptr++;                                                           // point at next
    }

    return;
}

void parse_do(int runtime)                                                      // DO
{
    short  s;                                                                   // for functions
    u_char *ptr;                                                                // a handy pointer
    u_char *p;                                                                  // a handy pointer
    u_char save[1024];                                                          // a useful save area
    int    savecount;                                                           // number of bytes saved

    while (TRUE) {                                                              // loop thru the arguments
        int i;                                                                  // a handy int

        ptr = comp_ptr;                                                         // save compile pointer
        *comp_ptr++ = CMDOTAG;                                                  // assume a do tag
        i = routine(runtime);                                                   // parse the rouref

        if (i == 0) {                                                           // if it's indirect
            *ptr = OPNOP;                                                       // ignore previous opcode
            *comp_ptr++ = INDDO;                                                // store the opcode

            if (*source_ptr == '(') {                                           // any args?
                comp_ptr = ptr;
                comperror(-(ERRZ70 + ERRMLAST));                                // complain
            }
        } else {
            int args = 0;                                                       // number of args

            if (i == -2) {
                *ptr = CMDORT;                                                  // routine and tag
            } else if (i == -3) {
                *ptr = CMDOROU;                                                 // just a routine
            } else if (i == -4) {
                *ptr = CMDORTO;                                                 // routine, tag, and offset
            }

            if (*source_ptr == '(') {                                           // any args?
                savecount = comp_ptr - ptr;                                     // bytes that got compiled
                memcpy(save, ptr, savecount);                                   // save that lot
                comp_ptr = ptr;                                                 // back where we started
                source_ptr++;                                                   // skip the (

                while (TRUE) {                                                  // while we have args
                    if (args > MAX_NUM_ARGS) SYNTX;                             // too many
                    args++;                                                     // count an argument

                    if (*source_ptr == ')') {                                   // trailing bracket ?
                        source_ptr++;                                           // skip the )
                        break;                                                  // and exit
                    }

                    if ((*source_ptr == ',') || (*source_ptr == ')')) {         // if empty argument
                        *comp_ptr++ = VARUNDF;                                  // flag it
                    } else if ((*source_ptr == '.') && (isdigit(source_ptr[1]) == 0)) { // by-reference? and not .numeric
                        source_ptr++;                                           // skip the dot

                        if (*source_ptr == '@') {                               // if indirection
                            source_ptr++;                                       // skip the @
                            atom();                                             // eval the string
                            *comp_ptr++ = INDMVAR;
                        } else {
                            p = comp_ptr;                                       // save current position
                            s = localvar();                                     // get a variable

                            if (s < 0) {                                        // if we got an error
                                comperror(s);                                   // compile it
                                return;                                         // and exit
                            }

                            p += s;                                             // point here
                            *p = OPMVAR;                                        // get the mvar onto stack
                        }

                        *comp_ptr++ = NEWBREF;                                  // flag 'by-reference'
                    } else {                                                    // by value
                        eval();                                                 // leave the value on the stack
                    }

                    if (*source_ptr == ')') continue;                           // trailing bracket ? then do it above

                    if (*source_ptr == ',') {                                   // a comma ?
                        source_ptr++;                                           // skip the ,
                        continue;                                               // go for more
                    }

                    SYNTX;                                                      // all else is an error
                }                                                               // end of while

                memcpy(comp_ptr, save, savecount);                              // copy the code back
                comp_ptr += savecount;                                          // and add to the pointer
            }                                                                   // end of argument decode

            *comp_ptr++ = (u_char) args;                                        // store number of args
        }

        if (*source_ptr == ':') {                                               // postcond arg?
            savecount = comp_ptr - ptr;                                         // bytes that got compiled
            memcpy(save, ptr, savecount);                                       // save that lot
            comp_ptr = ptr;                                                     // back where we started
            source_ptr++;                                                       // skip the :
            eval();                                                             // evel postcond
            *comp_ptr++ = JMP0;                                                 // jump if false
            s = (short) savecount;
            assert(sizeof(s) == sizeof(short));
            memcpy(comp_ptr, &s, sizeof(short));
            comp_ptr += sizeof(short);
            memcpy(comp_ptr, save, savecount);                                  // copy the code back
            comp_ptr += savecount;                                              // and add to the pointer
        }

        if (*source_ptr != ',') break;                                          // done
        source_ptr++;                                                           // point at next
    }                                                                           // end do argument loop

    return;
}

void parse_goto(int runtime)                                                    // GOTO
{
    u_char *ptr;                                                                // a handy pointer
    u_char save[1024];                                                          // a useful save area
    short  savecount;                                                           // number of bytes saved

    while (TRUE) {                                                              // loop thru the arguments
        int i;                                                                  // a handy int

        ptr = comp_ptr;                                                         // save compile pointer
        *comp_ptr++ = CMGOTAG;                                                  // assume a goto tag
        i = routine(runtime);                                                   // parse the rouref

        if (i == 0) {                                                           // if it's indirect
            *ptr = OPNOP;                                                       // ignore previous opcode
            *comp_ptr++ = INDGO;                                                // store the opcode
        } else {
            if (i == -2) {
                *ptr = CMGORT;                                                  // routine and tag
            } else if (i == -3) {
                *ptr = CMGOROU;                                                 // just a routine
            } else if (i == -4) {
                *ptr = CMGORTO;                                                 // routine, tag, and offset
            }
        }

        if (*source_ptr == '(') {                                               // any args?
            comp_ptr = ptr;
            comperror(-(ERRM45));                                               // complain
        }

        if (*source_ptr == ':') {                                               // postcond arg ?
            savecount = comp_ptr - ptr;                                         // bytes that got compiled
            memcpy(save, ptr, savecount);                                       // save that lot
            comp_ptr = ptr;                                                     // back where we started
            source_ptr++;                                                       // skip the :
            eval();                                                             // evel postcond
            *comp_ptr++ = JMP0;                                                 // jump if false
            assert(sizeof(savecount) == sizeof(short));
            memcpy(comp_ptr, &savecount, sizeof(short));
            comp_ptr += sizeof(short);
            memcpy(comp_ptr, save, savecount);                                  // copy the code back
            comp_ptr += savecount;                                              // and add to the pointer
        }

        if (*source_ptr != ',') break;                                          // done
        source_ptr++;                                                           // point at next
    }                                                                           // end goto argument loop

    return;
}

void parse_hang(void)                                                           // HANG
{
    while (TRUE) {                                                              // scan the line
        int iflag = (*source_ptr == '@');                                       // check for indirection

        eval();                                                                 // eval it
        if (iflag && (*(comp_ptr - 1) != INDEVAL)) iflag = 0;                   // confirm it was indirect

        if (iflag) {                                                            // if it was indirect
            *(comp_ptr - 1) = INDHANG;                                          // say hang indirect
        } else {
            *comp_ptr++ = OPHANG;                                               // write it
        }

        if (*source_ptr == ',') {
            source_ptr++;                                                       // increment past a comma
        } else {
            break;                                                              // else we are done
        }
    }                                                                           // end of while (TRUE) hang

    return;
}

void parse_if(long i)                                                           // IF
{
    while (TRUE) {                                                              // get the args
        int iflag = (*source_ptr == '@');                                       // check for indirection

        eval();                                                                 // get arg
        if (iflag && (*(comp_ptr - 1) != INDEVAL)) iflag = 0;                   // confirm it was indirect

        if (iflag) {                                                            // if it was indirect
            *(comp_ptr - 1) = INDIF;                                            // say if indirect
        } else {
            if (i == -1) {                                                      // non-indirect
                *comp_ptr++ = OPIFA;                                            // the op code
            } else {                                                            // indirect
                *comp_ptr++ = OPIFI;                                            // the op code
                assert(sizeof(i) == sizeof(long));
                memcpy(comp_ptr, &i, sizeof(long));                             // the isp to restore
                comp_ptr += sizeof(long);
            }
        }

        if (*source_ptr != ',') break;                                          // done
        source_ptr++;                                                           // point at next
    }

    return;
}

void parse_job(int runtime)                                                     // JOB
{
    int    args = 0;                                                            // number of args
    u_char *ptr;                                                                // a handy pointer
    u_char save[1024];                                                          // a useful save area
    int    savecount;                                                           // number of bytes saved

    while (TRUE) {                                                              // loop thru the arguments
        int i;                                                                  // a handy int

        ptr = comp_ptr;                                                         // save compile pointer
        *comp_ptr++ = CMJOBTAG;                                                 // assume a do tag
        i = routine(runtime);                                                   // parse the rouref

        if (i == 0) {                                                           // if it's indirect
            *ptr = OPNOP;                                                       // ignore previous opcode
            *comp_ptr++ = INDJOB;                                               // store the opcode

            if (*source_ptr == '(') {                                           // any args?
                comp_ptr = ptr;
                comperror(-(ERRZ70 + ERRMLAST));                                // complain
            }
        } else {
            args = 0;                                                           // number of args

            if (i == -2) {
                *ptr = CMJOBRT;                                                 // routine and tag
            } else if (i == -3) {
                *ptr = CMJOBROU;                                                // just a routine
            } else if (i == -4) {
                *ptr = CMJOBRTO;                                                // routine, tag, and offset
            }

            if (*source_ptr == '(') {                                           // any args?
                savecount = comp_ptr - ptr;                                     // bytes that got compiled
                memcpy(save, ptr, savecount);                                   // save that lot
                comp_ptr = ptr;                                                 // back where we started
                source_ptr++;                                                   // skip the (

                while (TRUE) {                                                  // while we have args
                    if (args > MAX_NUM_ARGS) SYNTX;                             // too many
                    args++;                                                     // count an argument

                    if (*source_ptr == ')') {                                   // trailing bracket ?
                        source_ptr++;                                           // skip the )
                        break;                                                  // and exit
                    }

                    if (*source_ptr == '.') {                                   // by-reference?
                        comperror(-ERRM40);                                     // complain
                        return;                                                 // not permitted
                    }

                    eval();                                                     // leave the value on the stack
                    if (*source_ptr == ')') continue;                           // trailing bracket? then do it above

                    if (*source_ptr == ',') {                                   // a comma ?
                        source_ptr++;                                           // skip the ,
                        continue;                                               // go for more
                    }

                    SYNTX;                                                      // all else is an error
                }                                                               // end of while

                memcpy(comp_ptr, save, savecount);                              // copy the code back
                ptr = comp_ptr;                                                 // move save pointer for timeout
                comp_ptr += savecount;                                          // and add to the pointer
            }                                                                   // end of argument decode
        }

        if (*source_ptr == ':') {                                               // funny timeout
            source_ptr++;                                                       // skip first colon

            if (*source_ptr != ':') {                                           // must be two of them
                *comp_ptr++ = (u_char) args;                                    // store the arg count
                SYNTX;                                                          // and error
            }

            source_ptr++;                                                       // skip second one
            savecount = comp_ptr - ptr;                                         // bytes that got compiled
            memcpy(save, ptr, savecount);                                       // save that lot
            comp_ptr = ptr;                                                     // back where we started
            eval();                                                             // eval timeout
            memcpy(comp_ptr, save, savecount);                                  // copy the code back
            comp_ptr += savecount;                                              // and add to the pointer
            args |= 128;                                                        // flag the timeout
        }

        *comp_ptr++ = (u_char) args;                                            // store number of args
        if (*source_ptr != ',') break;                                          // done
        source_ptr++;                                                           // point at next
    }                                                                           // end job argument loop

    return;
}

void parse_kill(void)                                                           // KILL
{
    short  s;                                                                   // for functions
    u_char *ptr;                                                                // a handy pointer

    if (*source_ptr == '(') {                                                   // exclusive kill
        int args = 0;                                                           // argument count

        source_ptr++;                                                           // skip the (

        while (TRUE) {                                                          // now, get one or more args
            ptr = comp_ptr;                                                     // save for ron
            s = localvar();                                                     // get var

            if (s < 0) {                                                        // if we got an error
                comperror(s);                                                   // compile it
                return;                                                         // and exit
            }

            ptr[s++] = OPMVAR;                                                  // build an mvar, point at type
            if ((ptr[s] != TYPVARNAM) && (ptr[s] != TYPVARIDX)) SYNTX;          // must be local unsubscripted
            args++;                                                             // count the arg

            if (*source_ptr == ')') {                                           // closing bracket?
                source_ptr++;                                                   // skip it
                break;                                                          // and exit
            }

            if (*source_ptr != ',') break;                                      // do it elsewhere
            source_ptr++;                                                       // skip the comma
        }                                                                       // end 'get one or more args'

        *comp_ptr++ = CMKILLB;                                                  // opcode
        *comp_ptr++ = args;                                                     // number of args
    } else {
        while (TRUE) {                                                          // loop thru normal kill
            if (*source_ptr == '@') {                                           // indirection ?
                atom();                                                         // eval the string

                if (*(comp_ptr - 1) == INDEVAL) {                               // if it was indirect
                    *(comp_ptr - 1) = INDKILL;                                  // say kill indirect
                } else {                                                        // experimental for $ORDER(@.@())
                    if (*(comp_ptr - 3) == OPVAR) *(comp_ptr - 3) = OPMVAR;     // change to OPMVAR
                }
            } else {
                ptr = comp_ptr;                                                 // save position
                s = localvar();                                                 // we need a var

                if (s < 0) {
                    comperror(s);                                               // compile the error
                    return;                                                     // and exit
                }

                ptr = &ptr[s];                                                  // point at the OPVAR
                *ptr = OPMVAR;                                                  // change to a OPMVAR
            }

            if (*(comp_ptr - 1) != INDKILL) *comp_ptr++ = CMKILL;               // and the opcode
            if (*source_ptr != ',') break;                                      // done
            source_ptr++;                                                       // point at next
        }
    }                                                                           // end while

    if (*source_ptr == ',') {                                                   // stupid A,A),...
        source_ptr++;                                                           // point past comma
        parse_kill();                                                           // and re-enter
    }

    return;
}

void parse_lock(void)                                                           // LOCK
{
    short   s;                                                                  // for functions
    u_short us;                                                                 // for cstring count
    u_char  *ptr;                                                               // a handy pointer

    while (TRUE) {
        char c = *source_ptr++;                                                 // get next char
        int  type = 0;                                                          // assume 'normal' lock
        int  args = 0;                                                          // init arg count
        int  i = 0;                                                             // flag no bracket

        if (c == '+') {
            type = 1;                                                           // a LOCK +
        } else if (c == '-') {
            type = -1;                                                          // a LOCK -
        }

        if (type) c = *source_ptr++;                                            // skip the + or -

        if (c == '(') {                                                         // a bracket?
            i++;                                                                // flag it
        } else {
            source_ptr--;                                                       // else backup to start of var
        }

        while (TRUE) {                                                          // do all args in brackets
            ptr = comp_ptr;                                                     // save for ron

            if (*source_ptr == '@') {                                           // indirection ?
                atom();                                                         // eval the string

                if (*(comp_ptr - 1) == INDEVAL) {                               // if it was indirect
                    if (!type && !i && (*source_ptr != ':')) {                  // normal lock not in a lock list, with no timeout
                        *(comp_ptr - 1) = INDLOCK;                              // say lock indirect
                        return;                                                 // and return
                    }

                    *(comp_ptr - 1) = INDMVAR;                                  // make an mvar of it
                } else if (*(comp_ptr - 3) == OPVAR) {
                    *(comp_ptr - 3) = OPMVAR;                                   // change to OPMVAR
                }
            } else {
                s = localvar();                                                 // get var

                if (s < 0) {                                                    // if we got an error
                    comperror(s);                                               // compile it
                    return;                                                     // and exit
                }

                if (*(comp_ptr - 2) == TYPVARNAKED) {                           // naked is not allowed in an nref
                    comp_ptr = ptr;                                             // reset location for error
                    EXPRE;
                }

                ptr[s] = OPMVAR;                                                // build an mvar
            }

            args++;                                                             // count the arg
            if (!i) break;                                                      // not expecting more

            if (*source_ptr == ')') {                                           // closing bracket?
                source_ptr++;                                                   // skip it
                break;                                                          // and exit
            }

            if (*source_ptr != ',') break;                                      // do it elsewhere
            source_ptr++;                                                       // skip the comma
        }

        if (*source_ptr == ':') {                                               // timeout ?
            source_ptr++;                                                       // skip the colon
            eval();                                                             // get it on the stack
        } else {                                                                // fake a -1
            *comp_ptr++ = OPSTR;                                                // string follows
            us = 2;                                                             // this many bytes
            assert(sizeof(us) == sizeof(u_short));
            memcpy(comp_ptr, &us, sizeof(u_short));
            comp_ptr += sizeof(u_short);
            *comp_ptr++ = '-';                                                  // the minus
            *comp_ptr++ = '1';                                                  // and the one
            *comp_ptr++ = '\0';                                                 // null terminate
        }

        if (type == -1) {
            *comp_ptr++ = CMLCKM;                                               // LOCK -
        } else if (type == 0) {
            *comp_ptr++ = CMLCK;                                                // LOCK
        } else if (type == 1) {
            *comp_ptr++ = CMLCKP;                                               // LOCK +
        }

        *comp_ptr++ = args;                                                     // and the number of them
        if (*source_ptr != ',') break;                                          // all done if not comma
        source_ptr++;                                                           // skip the comma
    }                                                                           // end lock arg loop

    return;
}

void parse_merge(void)                                                          // MERGE
{
    short s;                                                                    // for functions

    while (TRUE) {
        u_char *ptr1 = NULL;                                                    // a handy pointer
        u_char *ptr2 = NULL;                                                    // and another
        u_char *ptr3 = NULL;                                                    // and another
        int    i;

        i = parse2eq(source_ptr);                                               // look for an equals

        if (source_ptr[i] == '=') {                                             // did we find one?
            ptr1 = source_ptr;                                                  // save for ron
            ptr2 = &source_ptr[i];                                              // and the address of the '='
            source_ptr = ptr2 + 1;                                              // where to start from

            if (*source_ptr == '@') {                                           // indirection ?
                atom();                                                         // eval it
                ptr3 = comp_ptr - 1;                                            // remember where this goes

                if (*ptr3 == INDEVAL) {                                         // if it's going to eval it
                    *ptr3 = INDMVARF;                                           // make an mvar from it
                } else {                                                        // experimental for $ORDER(@.@())
                    ptr3 -= 2;                                                  // back up over subs to type
                    if (*ptr3 == OPVAR) *ptr3 = OPMVARF;                        // change to OPMVARF
                }
            } else {
                ptr3 = comp_ptr;                                                // save position
                s = localvar();                                                 // we need a var

                if (s < 0) {
                    comperror(s);                                               // compile the error
                    return;                                                     // and exit
                }

                ptr3 = &ptr3[s];                                                // point at the OPVAR
                *ptr3 = OPMVARF;                                                // change to a OPMVARF
            }

            *comp_ptr++ = OPNAKED;                                              // reset naked indicator
            ptr3 = source_ptr;                                                  // save for later
            source_ptr = ptr1;                                                  // start from the start
        } else {                                                                // end source processing - we had no '='
            if (*source_ptr != '@') SYNTX;                                      // must be an '@'
            atom();                                                             // eval the string

            if (*(comp_ptr - 1) == INDEVAL) {                                   // if it was indirect
                *(comp_ptr - 1) = INDMERG;                                      // say merge indirect
                if (*source_ptr != ',') break;                                  // no comma? then quit the loop
                source_ptr++;                                                   // increment past comma
                continue;                                                       // and go for more
            }

            SYNTX;                                                              // shouldn't get here
        }                                                                       // end indirect processing

        // Parse the destination
        ptr1 = comp_ptr;                                                        // save position

        if (*source_ptr == '@') {                                               // indirection ?
            atom();                                                             // eval the string

            if (*(comp_ptr - 1) == INDEVAL) {                                   // if it was indirect
                if (*source_ptr != '=') SYNTX;                                  // stuffed up
                *(comp_ptr - 1) = INDMVARF;                                     // else indirect full size mvar
            } else if (*(comp_ptr - 3) == OPVAR) {
                *(comp_ptr - 3) = OPMVARF;                                      // change to OPMVARF
            }
        } else {
            s = localvar();                                                     // we need a var

            if (s < 0) {
                comperror(s);                                                   // compile the error
                return;                                                         // and exit
            }

            ptr1 = &ptr1[s];                                                    // point at the OPVAR
            *ptr1 = OPMVARF;                                                    // change to a OPMVARF
        }

        *comp_ptr++ = OPNAKED;                                                  // reset naked indicator
        if (source_ptr != ptr2) SYNTX;                                          // junk characters
        source_ptr = ptr3;                                                      // restore source pointer
        *comp_ptr++ = CMMERGE;                                                  // do that
        if (*source_ptr != ',') break;                                          // quit if done
        source_ptr++;                                                           // increment past comma
    }

    return;                                                                     // all done
}                                                                               // end of MERGE code

void parse_new(void)                                                            // NEW
{
    char   c;                                                                   // current character
    short  s;                                                                   // for functions
    int    args = 0;                                                            // number of args
    u_char *ptr;                                                                // a handy pointer

    c = *source_ptr++;                                                          // get first char

    if (c == '(') {                                                             // exclusive new
        args = 0;                                                               // argument count

        while (TRUE) {                                                          // now, get one or more args
            ptr = comp_ptr;                                                     // save for ron

            if (*source_ptr == '@') {                                           // indirection ?
                if (args) {
                    *comp_ptr++ = CMNEW;                                        // opcode
                    *comp_ptr++ = args;                                         // number of args
                    args = 0;
                }

                atom();                                                         // eval the string

                if (*(comp_ptr - 1) == INDEVAL) {                               // if it was indirect
                    *(comp_ptr - 1) = INDNEW;                                   // say NEW indirect
                } else {
                    SYNTX;                                                      // can't have NEW @...@()
                }
            } else {                                                            // not indirect
                s = localvar();                                                 // get var

                if (s < 0) {                                                    // if we got an error
                    comperror(s);                                               // compile it
                    return;                                                     // and exit
                }

                ptr[s++] = OPMVAR;                                              // build an mvar, point at type
                if ((ptr[s] != TYPVARNAM) && (ptr[s] != TYPVARIDX)) SYNTX;      // must be local unsubscripted
                args++;                                                         // count the arg
            }

            if (*source_ptr == ')') {                                           // closing bracket?
                source_ptr++;                                                   // skip it
                break;                                                          // and exit
            }

            if (*source_ptr != ',') break;                                      // do it elsewhere
            source_ptr++;                                                       // skip the comma
        }                                                                       // end 'get one or more args'

        *comp_ptr++ = CMNEWB;                                                   // opcode
        *comp_ptr++ = args;                                                     // number of args
    } else {
        source_ptr--;                                                           // backup the source

        while (TRUE) {                                                          // loop thru normal NEW
            ptr = comp_ptr;                                                     // save for ron

            if (*source_ptr == '@') {                                           // indirection ?
                if (args) {
                    *comp_ptr++ = CMNEW;                                        // opcode
                    *comp_ptr++ = args;                                         // number of args
                    args = 0;
                }

                atom();                                                         // eval the string

                if (*(comp_ptr - 1) == INDEVAL) {                               // if it was indirect
                    *(comp_ptr - 1) = INDNEW;                                   // say NEW indirect
                } else {
                    SYNTX;                                                      // can't have NEW @...@()
                }
            } else {                                                            // not indirect
                s = localvar();                                                 // get var

                if (s < 0) {                                                    // if we got an error
                    comperror(s);                                               // compile it
                    return;                                                     // and exit
                }

                ptr[s++] = OPMVAR;                                              // build an mvar, point at type
                if ((ptr[s] != TYPVARNAM) && (ptr[s] != TYPVARIDX)) SYNTX;      // must be local unsubscripted
                args++;                                                         // count the arg
            }

            if (*source_ptr != ',') break;                                      // do it elsewhere
            source_ptr++;                                                       // skip the comma
            if (*source_ptr == '(') break;
        }                                                                       // end 'get one or more args'

        if (args) {
            *comp_ptr++ = CMNEW;                                                // opcode
            *comp_ptr++ = args;                                                 // number of args
        }
    }                                                                           // end while

    if (*source_ptr == ',') {                                                   // stupid A,A),...
        source_ptr++;                                                           // point past comma
        parse_new();                                                            // and re-enter
    }

    if (*source_ptr == '(') parse_new();                                        // stupid A,(A),... - and re-enter
    return;
}

void parse_open(void)                                                           // OPEN
{
    u_short us;                                                                 // a handy unsigned short

    while (TRUE) {                                                              // loop
        // Indirect Open code
        int iflag = (*source_ptr == '@');                                       // check for indirection

        eval();                                                                 // get the channel

        if ((*(comp_ptr - 1) == INDEVAL) && iflag) {                            // if it was indirect
            *(comp_ptr - 1) = INDOPEN;                                          // say open indirect
        } else {
            // Regular Open code
            if ((*source_ptr == ':') && (*(source_ptr + 1) == '(')) {           // colon followed by (
                source_ptr += 2;                                                // move past both for eval
                eval();                                                         // param 1
                if (*source_ptr++ != ':') SYNTX;                                // must be a colon
                eval();                                                         // param 2
                if (*source_ptr++ != ')') SYNTX;                                // must be a )
            } else {
                // We have no parens, valid only for opening $PRINCIPAL - populate the device and mode with empty strings
                if (*source_ptr == ':') {
                    source_ptr++;                                               // move past colon
                    if (*source_ptr != ':') SYNTX;                              // must have another for new syntax
                }

                *comp_ptr++ = OPSTR;                                            // empty string
                *comp_ptr++ = 0;                                                // cstring u_short length
                *comp_ptr++ = 0;                                                // ditto - endian doesn't matter here
                *comp_ptr++ = '\0';                                             // null terminatred cstring text
                *comp_ptr++ = OPSTR;                                            // empty string
                *comp_ptr++ = 0;                                                // cstring u_short length
                *comp_ptr++ = 0;                                                // ditto - endian doesn't matter here
                *comp_ptr++ = '\0';                                             // null terminated cstring text
            }

            if (*source_ptr == ':') {                                           // do we have a timeout
                source_ptr++;                                                   // skip the colon

                if (*source_ptr == ':') {                                       // if another colon
                    *comp_ptr++ = OPSTR;                                        // make up our own
                    us = 2;                                                     // the length
                    assert(sizeof(us) == sizeof(u_short));
                    memcpy(comp_ptr, &us, sizeof(u_short));
                    comp_ptr += sizeof(u_short);
                    *comp_ptr++ = '-';                                          // minus
                    *comp_ptr++ = '1';                                          // 1
                    *comp_ptr++ = '\0';                                         // null terminated
                } else {
                    eval();                                                     // get the timeout
                }
            } else {                                                            // no timeout
                *comp_ptr++ = OPSTR;                                            // make up our own
                us = 2;                                                         // the length
                assert(sizeof(us) == sizeof(u_short));
                memcpy(comp_ptr, &us, sizeof(u_short));
                comp_ptr += sizeof(u_short);
                *comp_ptr++ = '-';                                              // minus
                *comp_ptr++ = '1';                                              // 1
                *comp_ptr++ = '\0';                                             // null terminated
            }

            if (*source_ptr == ':') {                                           // if another colon
                source_ptr++;                                                   // advance past it
                *comp_ptr++ = OPSTR;                                            // push a string
                us = 10;                                                        // the length
                assert(sizeof(us) == sizeof(u_short));
                memcpy(comp_ptr, &us, sizeof(u_short));
                comp_ptr += sizeof(u_short);
                memcpy(comp_ptr, "namespace=\0", 11);                           // copy the param name
                comp_ptr += 11;                                                 // add to comp_ptr
                eval();                                                         // eval the arg
                *comp_ptr++ = OPCAT;                                            // concatenate them
            } else {
                *comp_ptr++ = OPSTR;                                            // push a string
                *comp_ptr++ = 0;                                                // endian doesn't matter here
                *comp_ptr++ = 0;                                                // endian doesn't matter here
                *comp_ptr++ = '\0';                                             // null terminated
            }

            *comp_ptr++ = CMOPEN;                                               // the opcode
        }

        if (*source_ptr != ',') break;                                          // not a comma - all done
        source_ptr++;;                                                          // point past the comma
    }

    return;
}

void parse_read(void)                                                           // READ
{
    int args = 0;                                                               // number of args

    while (TRUE) {                                                              // loop
        char c = *source_ptr;                                                   // get next character

        if (c == '!') {                                                         // check for a new line
            *comp_ptr++ = CMWRTNL;                                              // do a new line
            source_ptr++;                                                       // increment source ptr
            args++;                                                             // count literal
        } else if (c == '#') {                                                  // check for form feed
            *comp_ptr++ = CMWRTFF;                                              // do a ff
            source_ptr++;                                                       // increment source ptr
            args++;                                                             // count literal
        } else if (c == '/') {                                                  // silly device control stuff
            write_fmt();                                                        // do it elsewhere

            if ((*source_ptr != ' ') && (*source_ptr != ',') && (*source_ptr != '\0')) { // a space, comma, or EOL is required
                source_ptr++;                                                   // mark right spot
                comperror(-(ERRZ12 + ERRMLAST));                                // compile the error
                return;                                                         // and exit
            }
        } else if (c == '?') {                                                  // check for a tab
            source_ptr++;                                                       // increment source
            eval();                                                             // eval the expression
            *comp_ptr++ = CMWRTAB;                                              // do a tab expr
            args++;                                                             // count literal
        } else if (c == '*') {                                                  // check for read*
            // a space, comma, or EOL is required
            if ((*(source_ptr - 1) != ' ') && (*(source_ptr - 1) != ',') && (*(source_ptr - 1) != '\0')) {
                source_ptr++;                                                   // mark right spot
                comperror(-(ERRZ12 + ERRMLAST));                                // compile the error
                return;                                                         // and exit
            }

            source_ptr++;                                                       // skip the *

            if (args) {
                *comp_ptr++ = CMFLUSH;                                          // flush input
                args = 0;                                                       // clear the count
            }

            if (parse_read_var(1)) return;                                      // get a variable and quit on error
        } else if (c == '"') {                                                  // a literal ?
            // a space, comma, or EOL is required
            if ((*(source_ptr - 1) != ' ') && (*(source_ptr - 1) != ',') && (*(source_ptr - 1) != '\0')) {
                source_ptr++;                                                   // mark right spot
                comperror(-(ERRZ12 + ERRMLAST));                                // compile the error
                return;                                                         // and exit
            }

            atom();                                                             // get it

            if ((*source_ptr != ' ') && (*source_ptr != ',') && (*source_ptr != '\0')) { // a space, comma, or EOL is required
                source_ptr++;                                                   // mark right spot
                comperror(-(ERRZ12 + ERRMLAST));                                // compile the error
                return;                                                         // and exit
            }

            *comp_ptr++ = CMWRTEX;                                              // write it
            args++;                                                             // count it
        } else {
            // a space, comma, or EOL is required
            if ((*(source_ptr - 1) != ' ') && (*(source_ptr - 1) != ',') && (*(source_ptr - 1) != '\0')) {
                source_ptr++;                                                   // mark right spot
                comperror(-(ERRZ12 + ERRMLAST));                                // compile the error
                return;                                                         // and exit
            }

            if (args) {
                *comp_ptr++ = CMFLUSH;                                          // flush input
                args = 0;                                                       // clear the count
            }

            if (parse_read_var(0)) return;                                      // get a variable and quit on error
        }

        if ((*source_ptr == ' ') || (*source_ptr == '\0')) break;               // a space or end of line - all done
        if (*source_ptr == ',') source_ptr++;                                   // check for comma and skip it
    }                                                                           // end of while (TRUE) write

    return;
}

void parse_set(void)                                                            // SET
{
    short   s;                                                                  // for functions
    u_short us;                                                                 // for functions
    int     bracket;                                                            // bracket flag
    int     args = 0;                                                           // number of args
    u_char  *p;                                                                 // a handy pointer

    while (TRUE) {
        u_char *ptr1 = NULL;                                                    // a handy pointer
        u_char *ptr2 = NULL;                                                    // and another
        u_char *ptr3 = NULL;                                                    // and another
        int    i;

        i = parse2eq(source_ptr);                                               // look for an equals

        if (source_ptr[i] == '=') {                                             // did we find one?
            ptr1 = source_ptr;                                                  // save for ron
            ptr2 = &source_ptr[i];                                              // and the address of the '='
            source_ptr = ptr2 + 1;                                              // where to start from
            eval();                                                             // eval the source
            ptr3 = source_ptr;                                                  // save for later
            source_ptr = ptr1;                                                  // start from the start
        } else {                                                                // end source processing - we had no '='
            if (*source_ptr != '@') SYNTX;                                      // must be an '@'
            atom();                                                             // eval the string

            if (*(comp_ptr - 1) == INDEVAL) {                                   // if it was indirect
                *(comp_ptr - 1) = INDSET;                                       // make an indirect SET
                if (*source_ptr != ',') break;                                  // no comma? then quit the loop
                source_ptr++;                                                   // increment past comma
                continue;                                                       // and go for more
            }

            SYNTX;                                                              // shouldn't get here
        }                                                                       // end indirect processing

        // Parse the destination(s)
        bracket = 0;                                                            // assume no brackets

        if (*source_ptr == '(') {                                               // a bracket?
            bracket = 1;                                                        // flag it
            source_ptr++;                                                       // and skip it
        }

        while (TRUE) {                                                          // in case of brackets
            if ((strncasecmp((char *) source_ptr, "$e(", 3) == 0) || (strncasecmp((char *) source_ptr, "$extract(", 9) == 0) ||
              (strncasecmp((char *) source_ptr, "$p(", 3) == 0) || (strncasecmp((char *) source_ptr, "$piece(", 7) == 0)) {
                args = (toupper(source_ptr[1]) == 'P');                         // $PIECE = 1, $EXTRACT = 0
                while ((*source_ptr != '(') && *source_ptr) source_ptr++;       // skip to bracket
                source_ptr++;                                                   // skip opening bracket
                p = comp_ptr;                                                   // save position

                if (*source_ptr == '@') {                                       // indirection ?
                    atom();                                                     // eval the string

                    if (*(comp_ptr - 1) == INDEVAL) {                           // if it was indirect
                        *(comp_ptr - 1) = INDMVAR;                              // say mvar required
                    } else {                                                    // for $ORDER(@.@())
                        if (*(comp_ptr - 3) == OPVAR) *(comp_ptr - 3) = OPMVAR; // change to OPMVAR
                    }
                } else {
                    s = localvar();                                             // we need a var

                    if (s < 0) {
                        comperror(s);                                           // compile the error
                        return;                                                 // and exit
                    }

                    p = &p[s];                                                  // point at the OPVAR
                    *p = OPMVAR;                                                // change to a OPMVAR
                }

                if (args) {                                                     // $PIECE ?
                    if (*source_ptr != ',') SYNTX;                              // need a comma
                    source_ptr++;                                               // skip comma
                    eval();                                                     // eval the delimiter
                }

                if (*source_ptr == ')') {                                       // end of function?
                    *comp_ptr++ = OPSTR;                                        // say string following
                    us = 1;                                                     // the length
                    assert(sizeof(us) == sizeof(u_short));
                    memcpy(comp_ptr, &us, sizeof(u_short));
                    comp_ptr += sizeof(u_short);
                    *comp_ptr++ = '1';                                          // value 1
                    *comp_ptr++ = '\0';                                         // null terminated
                } else {
                    if (*source_ptr != ',') SYNTX;                              // need a comma
                    source_ptr++;                                               // skip comma
                    eval();                                                     // eval the first numeric arg
                }

                if (*source_ptr == ')') {                                       // end of function?
                    *comp_ptr++ = OPDUPASP;                                     // same as last
                } else {
                    if (*source_ptr != ',') SYNTX;                              // need a comma
                    source_ptr++;                                               // skip comma
                    eval();                                                     // eval the second numeric arg
                }

                if (*source_ptr++ != ')') SYNTX;                                // ensure there is a )
                *comp_ptr++ = (args ? CMSETP : CMSETE);                         // set the opcode
            } else if (*source_ptr == '@') {                                    // end SET $EXTRACT/$PIECE - indirection ?
                source_ptr++;                                                   // skip the @
                atom();                                                         // eval the indirect bit

                if (*source_ptr == '@') {                                       // another one?
                    *comp_ptr++ = INDMVAR;                                      // make an mvar out of it
                    p = comp_ptr;                                               // save for opcode insert
                    s = localvar();                                             // parse the rest of it

                    if (s < 0) {                                                // if we got an error
                      comperror(s);                                             // compile it
                      return;                                                   // and exit
                    }

                    p[s] = OPMVAR;                                              // build an mvar for dest
                    *comp_ptr++ = CMSET;                                        // add opcode
                } else {                                                        // end SET @x@(...) - must be SET @a=
                    *comp_ptr++ = INDMVAR;                                      // make an mvar out of it
                    *comp_ptr++ = CMSET;                                        // add opcode
                }
            } else {                                                            // end indirection - not $PIECE/$EXTRACT/indirection
                p = comp_ptr;                                                   // save for opcode insert
                s = localvar();                                                 // parse the variable

                if (s < 0) {                                                    // if we got an error
                    comperror(s);                                               // compile it
                    return;                                                     // and exit
                }

                p[s] = OPMVAR;                                                  // build an mvar for dest
                *comp_ptr++ = CMSET;                                            // add opcode
            }

            if (!bracket) break;                                                // no brackets

            if (*source_ptr == ')') {                                           // end of list?
                source_ptr++;                                                   // increment past )
                break;                                                          // and exit
            }

            if (*source_ptr++ != ',') SYNTX;                                    // must be a comma
        }                                                                       // end destination while loop

        if (source_ptr != ptr2) SYNTX;                                          // something nasty
        source_ptr = ptr3;                                                      // restore pointer
        if (*source_ptr != ',') break;                                          // check for more
        source_ptr++;                                                           // skip comma
    }                                                                           // end of set loop

    return;
}

void parse_use(void)                                                            // USE
{
    char    c;                                                                  // current character
    u_short us;                                                                 // a handy unsigned short
    int     i;                                                                  // a handy int
    int     args;                                                               // number of args

    while (TRUE) {                                                              // scan the line
        int iflag = (*source_ptr == '@');                                       // check for indirection

        eval();                                                                 // get the channel

        if ((*(comp_ptr - 1) == INDEVAL) && iflag) {                            // if it was indirect
            *(comp_ptr - 1) = INDUSE;                                           // say use indirect
        } else {
            args = 0;                                                           // number of arguments

            if (*source_ptr == ':') {                                           // any args ?
                source_ptr++;                                                   // skip the colon

                if (*source_ptr != ':') {                                       // if not another colon
                    i = (*source_ptr == '(');                                   // check leading bracket
                    if (i) source_ptr++;                                        // skip it if there
                    eval();                                                     // get arg 1
                    args++;                                                     // count it

                    while (i) {                                                 // if we have more args
                        c = *source_ptr++;                                      // get next character
                        if (c == ')') break;                                    // bracket means done
                        if (c != ':') SYNTX;                                    // must be a colon
                        eval();                                                 // eval arg
                        args++;                                                 // count it
                    }
                }
            }

            if (*source_ptr == ':') {                                           // if another colon
                source_ptr++;                                                   // advance past it
                args++;                                                         // count it
                *comp_ptr++ = OPSTR;                                            // push a string
                us = 10;                                                        // the length
                assert(sizeof(us) == sizeof(u_short));
                memcpy(comp_ptr, &us, sizeof(u_short));
                comp_ptr += sizeof(u_short);
                memcpy(comp_ptr, "namespace=\0", 11);                           // copy the param name
                comp_ptr += 11;                                                 // add to comp_ptr
                eval();                                                         // eval the arg
                *comp_ptr++ = OPCAT;                                            // concatenate them
            }

            *comp_ptr++ = CMUSE;                                                // the op code
            *comp_ptr++ = (u_char) args;                                        // number of parameters
        }

        if (*source_ptr != ',') break;                                          // not a comma - all done
        source_ptr++;;                                                          // point past the comma
    }

    return;
}

void parse_write(void)                                                          // WRITE
{
    int iflag;

    while (TRUE) {                                                              // scan the line
        char c = *source_ptr;                                                   // get next character

        if (c == '!') {                                                         // check for a new line
            *comp_ptr++ = CMWRTNL;                                              // do a new line
            source_ptr++;                                                       // increment source ptr
        } else if (c == '#') {                                                  // check for form feed
            *comp_ptr++ = CMWRTFF;                                              // do a ff
            source_ptr++;                                                       // increment source ptr
        } else if (c == '/') {                                                  // silly device control stuff
            write_fmt();                                                        // do it elsewhere

            if ((*source_ptr != ' ') && (*source_ptr != ',') && (*source_ptr != '\0')) { // a space, comma, or EOL is required
                source_ptr++;                                                   // mark right spot
                comperror(-(ERRZ12 + ERRMLAST));                                // compile the error
                return;                                                         // and exit
            }
        } else if (c == '?') {                                                  // check for a tab
            source_ptr++;                                                       // increment source
            eval();                                                             // eval the expression
            *comp_ptr++ = CMWRTAB;                                              // do a tab expr
        } else if (c == '*') {                                                  // check for a write star
            // a space, comma, or EOL is required
            if ((*(source_ptr - 1) != ' ') && (*(source_ptr - 1) != ',') && (*(source_ptr - 1) != '\0')) {
                source_ptr++;                                                   // mark right spot
                comperror(-(ERRZ12 + ERRMLAST));                                // compile the error
                return;                                                         // and exit
            }

            source_ptr++;                                                       // increment source
            eval();                                                             // eval the expression
            *comp_ptr++ = CMWRTST;                                              // do a write star
        } else {                                                                // must be an expression
            // a space, comma, or EOL is required
            if ((*(source_ptr - 1) != ' ') && (*(source_ptr - 1) != ',') && (*(source_ptr - 1) != '\0')) {
                source_ptr++;                                                   // mark right spot
                comperror(-(ERRZ12 + ERRMLAST));                                // compile the error
                return;                                                         // and exit
            }

            iflag = (*source_ptr == '@');                                       // check for indirection
            eval();                                                             // eval it

            if ((*(comp_ptr - 1) == INDEVAL) && iflag) {
                *(comp_ptr - 1) = INDWRIT;                                      // if it was indirect then say write indirect
            } else {
                *comp_ptr++ = CMWRTEX;                                          // write it
            }
        }

        if ((*source_ptr == ' ') || (*source_ptr == '\0')) break;               // a space or end of line and all done
        if (*source_ptr == ',') source_ptr++;                                   // increment past a comma
    }                                                                           // end of while (TRUE) write

    return;
}

void parse_xecute(void)                                                         // XECUTE
{
    u_char *ptr;                                                                // a handy pointer
    u_char save[1024];                                                          // a useful save area
    short  savecount;                                                           // number of bytes saved

    while (TRUE) {                                                              // loop thru the arguments
        int iflag = (*source_ptr == '@');                                       // check for indirection

        ptr = comp_ptr;                                                         // save compile pointer
        eval();                                                                 // parse the string

        if ((*(comp_ptr - 1) == INDEVAL) && iflag) {                            // if it was indirect
            *(comp_ptr - 1) = INDXEC;                                           // say xecute indirect
        } else {
            *comp_ptr++ = CMXECUT;                                              // the opcode

            if (*source_ptr == ':') {                                           // postcond arg ?
                savecount = comp_ptr - ptr;                                     // bytes that got compiled
                mcopy(ptr, save, savecount);                                    // save that lot
                comp_ptr = ptr;                                                 // back where we started
                source_ptr++;                                                   // skip the :
                eval();                                                         // eval postcond
                *comp_ptr++ = JMP0;                                             // jump if false
                assert(sizeof(savecount) == sizeof(short));
                memcpy(comp_ptr, &savecount, sizeof(short));
                comp_ptr += sizeof(short);
                mcopy(save, comp_ptr, savecount);                               // copy the code back
                comp_ptr += savecount;                                          // and add to the pointer
            }
        }

        if (*source_ptr != ',') break;                                          // done
        source_ptr++;                                                           // point at next
    }                                                                           // end do argument loop

    return;
}

/*
 * Parse string pointed to by source_ptr.
 * Compile to location pointed to by comp_ptr.
 */
void parse(void)                                                                // MAIN PARSE LOOP
{
    short  s;                                                                   // for functions
    int    i;                                                                   // a handy int
    int    args = 0;                                                            // number of args
    u_char *ptr;                                                                // a handy pointer

    while (TRUE) {                                                              // loop
        char c;

        c = toupper(*source_ptr++);                                             // get next char in upper case
        jmp_eoc = NULL;                                                         // clear post conditional

        switch (c) {
        case '\0':                                                              // NULL
        case ';':                                                               // comment
            *comp_ptr++ = ENDLIN;                                               // add an end line op
            return;                                                             // all done

        case ' ':                                                               // space
            break;                                                              // go for more

        case 'B':                                                               // BREAK - no indirection
            if (isalpha(*source_ptr) != 0) {                                    // if the next is alpha
                if (strncasecmp((char *) source_ptr, "reak", 4) != 0) SYNTX;
                source_ptr += 4;                                                // point past the "reak"
            }

            c = *source_ptr++;                                                  // get next char

            if (c == ':') {                                                     // postcondition ?
                eval();                                                         // evaluate the TVE
                *comp_ptr++ = JMP0;                                             // store the opcode
                jmp_eoc = comp_ptr;                                             // save the location
                comp_ptr += sizeof(short);                                      // leave space for offset
                c = *source_ptr++;                                              // get next char
            }

            if (c != '\0') {                                                    // if it's not null
                if (c != ' ') SYNTX;                                            // must be a space

                if (*source_ptr == ' ') {                                       // argless form
                    source_ptr++;                                               // point past it
                    *comp_ptr++ = OPBRK0;                                       // save opcode
                } else while (TRUE) {                                           // get the args
                    eval();                                                     // get arg
                    *comp_ptr++ = OPBRKN;                                       // the op code
                    if (*source_ptr != ',') break;                              // done
                    source_ptr++;                                               // point at next
                }
            } else {                                                            // at end of line
                source_ptr--;                                                   // point back at null
                *comp_ptr++ = OPBRK0;                                           // save opcode
            }

            break;                                                              // end of close

        case 'C':                                                               // CLOSE
            if (isalpha(*source_ptr) != 0) {                                    // if the next is alpha
                if (strncasecmp((char *) source_ptr, "lose", 4) != 0) SYNTX;
                source_ptr += 4;                                                // point past the "lose"
            }

            c = *source_ptr++;                                                  // get next char

            if (c == ':') {                                                     // postcondition ?
                eval();                                                         // evaluate the TVE
                *comp_ptr++ = JMP0;                                             // store the opcode
                jmp_eoc = comp_ptr;                                             // save the location
                comp_ptr += sizeof(short);                                      // leave space for offset
                c = *source_ptr++;                                              // get next char
            }

            if (c != ' ') SYNTX;                                                // must be a space
            parse_close();
            break;                                                              // end of close

        case 'D':                                                               // DO
            if (isalpha(*source_ptr) != 0) {                                    // if the next is alpha
                if (strncasecmp((char *) source_ptr, "o", 1) != 0) SYNTX;
                source_ptr++;                                                   // point past the "o"
            }

            c = *source_ptr++;                                                  // get next char

            if (c == ':') {                                                     // postcondition
                eval();                                                         // evaluate the TVE
                *comp_ptr++ = JMP0;                                             // store the opcode
                jmp_eoc = comp_ptr;                                             // save the location
                comp_ptr += sizeof(short);                                      // leave space for offset
                c = *source_ptr++;                                              // get next char
            }

            if ((c != ' ') && (c != '\0')) SYNTX;                               // space or eol required
            if (c == '\0') source_ptr--;                                        // point back at eol

            if ((*source_ptr == ' ') || (*source_ptr == '\0')) {                // argumentless form?
                *comp_ptr++ = CMDON;                                            // add the opcode
            } else {
                parse_do(0);
            }

            break;                                                              // end of do

        case 'E':                                                               // ELSE - no indirection
            if (isalpha(*source_ptr) != 0) {                                    // if the next is alpha
                if (strncasecmp((char *) source_ptr, "lse", 3) != 0) SYNTX;
                source_ptr += 3;                                                // point past the "lse"
            }

            c = *source_ptr++;                                                  // get next char
            if (c != ' ') SYNTX;                                                // must be a space
            if (*source_ptr++ != ' ') SYNTX;                                    // must be a space
            *comp_ptr++ = OPELSE;                                               // save opcode
            break;                                                              // end else

        case 'F':                                                               // FOR - no indirection
            if (isalpha(*source_ptr) != 0) {                                    // if the next is alpha
                if (strncasecmp((char *) source_ptr, "or", 2) != 0) SYNTX;
                source_ptr += 2;                                                // point past the "or"
            }

            c = *source_ptr++;                                                  // get next char
            if (c != ' ') SYNTX;                                                // must be a space
            c = *source_ptr++;                                                  // get next char

            if (c == ' ') {                                                     // space means 'while'
                *comp_ptr++ = CMFOR0;                                           // say argless FOR
                ptr = comp_ptr;                                                 // remember where we are
                comp_ptr += sizeof(short);                                      // space for the QUIT offset
                parse();                                                        // parse the code
                source_ptr--;                                                   // backup to null (I hope)
                comp_ptr--;                                                     // backup over ENDLIN
                *comp_ptr++ = OPENDC;                                           // say end cmd (restore asp)
                *comp_ptr++ = JMP;                                              // add a jump
                s = ptr - comp_ptr;
                assert(sizeof(s) == sizeof(short));
                memcpy(comp_ptr, &s, sizeof(short));
                comp_ptr += sizeof(short);
                *comp_ptr++ = OPNOP;                                            // add the NOP
                s = (short) (comp_ptr - ptr - sizeof(short) - 1);
                assert(sizeof(s) == sizeof(short));
                memcpy(ptr, &s, sizeof(short));
                break;                                                          // and give up
            }

            source_ptr--;                                                       // back up the source
            ptr = comp_ptr;                                                     // save current position
            s = localvar();                                                     // get a variable

            if (s < 0) {                                                        // if we got an error
                comperror(s);                                                   // compile it
                return;                                                         // and exit
            }

            ptr += s;                                                           // point here
            if (*source_ptr++ != '=') SYNTX;                                    // check for equals
            *ptr = CMFORSET;                                                    // setup a for
            ptr = comp_ptr;                                                     // for code and quit offsets
            comp_ptr += (sizeof(short) * 2);                                    // leave space

            while (TRUE) {                                                      // now get the arguments
                eval();                                                         // get the arg
                c = *source_ptr++;                                              // and the delimiter

                if (c != ':') {                                                 // if it's not a colon
                    *comp_ptr++ = CMFOR1;                                       // one arg
                    if (c != ',') break;                                        // quit if no more
                } else {                                                        // it's a colon
                    eval();                                                     // eval arg 2
                    c = *source_ptr++;                                          // and the delimiter

                    if (c != ':') {                                             // if it's not a colon
                        *comp_ptr++ = CMFOR2;                                   // two args
                        if (c != ',') break;                                    // quit if no more
                    } else {                                                    // it's another colon
                        eval();                                                 // eval arg 3
                        c = *source_ptr++;                                      // and the delimiter
                        *comp_ptr++ = CMFOR3;                                   // three args
                        if (c != ',') break;                                    // quit if no more
                    }
                }
            }

            source_ptr--;                                                       // backup the source ptr
            s = comp_ptr - ptr - sizeof(short);                                 // offset to code
            assert(sizeof(s) == sizeof(short));
            memcpy(ptr, &s, sizeof(short));
            ptr += sizeof(short);
            parse();                                                            // parse the code
            source_ptr--;                                                       // backup to null (I hope)
            comp_ptr--;                                                         // backup over ENDLIN
            *comp_ptr++ = CMFOREND;                                             // do end of for processing
            *comp_ptr++ = OPNOP;                                                // add the NOP
            s = (short) (comp_ptr - ptr - sizeof(short) - 1);
            assert(sizeof(s) == sizeof(short));
            memcpy(ptr, &s, sizeof(short));
            break;                                                              // end of FOR

        case 'G':                                                               // GOTO
            if (isalpha(*source_ptr) != 0) {                                    // if the next is alpha
                if (strncasecmp((char *) source_ptr, "oto", 3) != 0) SYNTX;
                source_ptr += 3;                                                // point past the "oto"
            }

            c = *source_ptr++;                                                  // get next char

            if (c == ':') {                                                     // postcondition ?
                eval();                                                         // evaluate the TVE
                *comp_ptr++ = JMP0;                                             // store the opcode
                jmp_eoc = comp_ptr;                                             // save the location
                comp_ptr += sizeof(short);                                      // leave space for offset
                c = *source_ptr++;                                              // get next char
            }

            if (c != ' ') SYNTX;                                                // must be a space
            parse_goto(0);
            break;                                                              // end of goto

        case 'H':                                                               // HALT/HANG
            i = 0;                                                              // halt = 1, hang = 2

            if (isalpha(*source_ptr) != 0) {                                    // if the next is alpha
                if (strncasecmp((char *) source_ptr, "alt", 3) == 0) {
                    i = 1;
                } else if (strncasecmp((char *) source_ptr, "ang", 3) == 0) {
                    i = 2;
                } else {
                    SYNTX;                                                      // neither of these
                }

                source_ptr += 3;                                                // point past the "alt/ang"
            }

            c = ' ';                                                            // assume space
            if (*source_ptr != '\0') c = *source_ptr++;                         // get next char (if any)

            if (c == ':') {                                                     // postcondition ?
                eval();                                                         // evaluate the TVE
                *comp_ptr++ = JMP0;                                             // store the opcode
                jmp_eoc = comp_ptr;                                             // save the location
                comp_ptr += sizeof(short);                                      // leave space for offset
                c = ' ';                                                        // assume space
                if (*source_ptr != '\0') c = *source_ptr++;                     // get next char (if any)
            }

            if (c != ' ') SYNTX;                                                // must be a space
            c = ' ';                                                            // assume space
            if (*source_ptr != '\0') c = *source_ptr++;                         // get next char (if any)

            if (c == ' ') {                                                     // if it's a space
                if (i == 2) SYNTX;                                              // if it is a hang
                *comp_ptr++ = OPHALT;                                           // the opcode
                break;                                                          // end of halt
            }

            if (i == 1) SYNTX;                                                  // if it is a halt
            source_ptr--;                                                       // backup the source ptr
            parse_hang();
            break;                                                              // end of hang

        case 'I':                                                               // IF
            if (isalpha(*source_ptr) != 0) {                                    // if the next is alpha
                if (strncasecmp((char *) source_ptr, "f", 1) != 0) SYNTX;
                source_ptr++;                                                   // point past the "f"
            }

            c = *source_ptr++;                                                  // get next char
            if (c != ' ') SYNTX;                                                // must be a space

            if (*source_ptr == ' ') {                                           // argless form
                source_ptr++;                                                   // point past it
                *comp_ptr++ = OPIFN;                                            // save opcode
                break;                                                          // end argless if
            }

            parse_if(-1);                                                       // say not indirect
            break;                                                              // end of if

        case 'J':                                                               // JOB
            if (isalpha(*source_ptr) != 0) {                                    // if the next is alpha
                if (strncasecmp((char *) source_ptr, "ob", 2) != 0) SYNTX;
                source_ptr += 2;                                                // point past the "ob"
            }

            c = *source_ptr++;                                                  // get next char

            if (c == ':') {                                                     // postcondition
                eval();                                                         // evaluate the TVE
                *comp_ptr++ = JMP0;                                             // store the opcode
                jmp_eoc = comp_ptr;                                             // save the location
                comp_ptr += sizeof(short);                                      // leave space for offset
                c = *source_ptr++;                                              // get next char
            }

            if (c != ' ') SYNTX;                                                // must be a space
            parse_job(0);
            break;                                                              // end of job

        case 'K':                                                               // KILL
            if (isalpha(*source_ptr) != 0) {                                    // if the next is alpha
                if (strncasecmp((char *) source_ptr, "ill", 3) != 0) SYNTX;
                source_ptr += 3;                                                // point past the "ill"
            }

            c = ' ';                                                            // assume a space
            if (*source_ptr != '\0') c = *source_ptr++;                         // get next char (if any)

            if (c == ':') {                                                     // postcondition ?
                eval();                                                         // evaluate the TVE
                *comp_ptr++ = JMP0;                                             // store the opcode
                jmp_eoc = comp_ptr;                                             // save the location
                comp_ptr += sizeof(short);                                      // leave space for offset
                c = ' ';                                                        // assume a space
                if (*source_ptr != '\0') c = *source_ptr++;                     // get next char (if any)
            }

            if (c != ' ') SYNTX;                                                // must be a space
            if (*source_ptr != '\0') c = *source_ptr++;                         // get next char (if any)

            if (c == ' ') {                                                     // argless kill
                *comp_ptr++ = CMKILLB;                                          // opcode
                *comp_ptr++ = 0;                                                // number of args
            } else {
                source_ptr--;                                                   // backup pointer
                parse_kill();
            }

            break;                                                              // end of KILL code

        case 'L':                                                               // LOCK
            if (isalpha(*source_ptr) != 0) {                                    // if the next is alpha
                if (strncasecmp((char *) source_ptr, "ock", 3) != 0) SYNTX;
                source_ptr += 3;                                                // point past the "ock"
            }

            c = *source_ptr++;                                                  // get next char

            if (c == ':') {                                                     // postcondition ?
                eval();                                                         // evaluate the TVE
                *comp_ptr++ = JMP0;                                             // store the opcode
                jmp_eoc = comp_ptr;                                             // save the location
                comp_ptr += sizeof(short);                                      // leave space for offset
                c = *source_ptr++;                                              // get next char
            }

            if ((c != ' ') && (c != '\0')) SYNTX;                               // space or eol required

            if (c == '\0') {                                                    // if at eol
                source_ptr--;                                                   // point back at eol
                c = ' ';                                                        // pretend we have a space
            } else {
                c = *source_ptr++;                                              // get next char
            }

            if (c == ' ') {                                                     // argless form
                *comp_ptr++ = CMLCKU;                                           // save op code
            } else {
                source_ptr--;                                                   // backup pointer
                parse_lock();
            }

            break;                                                              // end of MERGE code

        case 'M':                                                               // MERGE
            if (isalpha(*source_ptr) != 0) {                                    // if the next is alpha
                if (strncasecmp((char *) source_ptr, "erge", 4) != 0) SYNTX;
                source_ptr += 4;                                                // point past the "erge"
            }

            c = *source_ptr++;                                                  // get next char

            if (c == ':') {                                                     // postcondition ?
                eval();                                                         // evaluate the TVE
                *comp_ptr++ = JMP0;                                             // store the opcode
                jmp_eoc = comp_ptr;                                             // save the location
                comp_ptr += sizeof(short);                                      // leave space for offset
                c = *source_ptr++;                                              // get next char
            }

            if (c != ' ') SYNTX;                                                // must be a space
            parse_merge();
            break;                                                              // end of MERGE code

        case 'N':                                                               // NEW
            if (isalpha(*source_ptr) != 0) {                                    // if the next is alpha
                if (strncasecmp((char *) source_ptr, "ew", 2) != 0) SYNTX;
                source_ptr += 2;                                                // point past the "ew"
            }

            args = 0;                                                           // clear arg count
            c = ' ';                                                            // assume a space
            if (*source_ptr != '\0') c = *source_ptr++;                         // get next char (if any)

            if (c == ':') {                                                     // postcondition ?
                eval();                                                         // evaluate the TVE
                *comp_ptr++ = JMP0;                                             // store the opcode
                jmp_eoc = comp_ptr;                                             // save the location
                comp_ptr += sizeof(short);                                      // leave space for offset
                c = ' ';                                                        // assume a space
                if (*source_ptr != '\0') c = *source_ptr++;                     // get next char (if any)
            }

            if (c != ' ') SYNTX;                                                // must be a space
            if (*source_ptr != '\0') c = *source_ptr++;                         // get next char (if any)

            if (c == ' ') {                                                     // argless new
                *comp_ptr++ = CMNEWB;                                           // opcode
                *comp_ptr++ = 0;                                                // number of args
            } else {
                source_ptr--;                                                   // backup pointer
                parse_new();
            }

            break;                                                              // end of NEW code

        case 'O':                                                               // OPEN
            if (isalpha(*source_ptr) != 0) {                                    // if the next is alpha
                if (strncasecmp((char *) source_ptr, "pen", 3) != 0) SYNTX;
                source_ptr += 3;                                                // point past the "pen"
            }

            c = *source_ptr++;                                                  // get next char

            if (c == ':') {                                                     // postcondition ?
                eval();                                                         // evaluate the TVE
                *comp_ptr++ = JMP0;                                             // store the opcode
                jmp_eoc = comp_ptr;                                             // save the location
                comp_ptr += sizeof(short);                                      // leave space for offset
                c = *source_ptr++;                                              // get next char
            }

            if (c != ' ') SYNTX;                                                // must be a space
            parse_open();
            break;                                                              // end of open code

        case 'Q':                                                               // QUIT - no indirection
            if (isalpha(*source_ptr) != 0) {                                    // if the next is alpha
                if (strncasecmp((char *) source_ptr, "uit", 3) != 0) SYNTX;
                source_ptr += 3;                                                // point past the "uit"
            }

            c = ' ';                                                            // assume a space
            if (*source_ptr != '\0') c = *source_ptr++;                         // get next char (if any)

            if (c == ':') {                                                     // postcondition ?
                eval();                                                         // evaluate the TVE
                *comp_ptr++ = JMP0;                                             // store the opcode
                jmp_eoc = comp_ptr;                                             // save the location
                comp_ptr += sizeof(short);                                      // leave space for offset
                c = ' ';                                                        // assume a space
                if (*source_ptr != '\0') c = *source_ptr++;                     // get next char (if any)
            }

            if (c != ' ') SYNTX;                                                // must be a space
            if (*source_ptr != '\0') c = *source_ptr++;                         // get next char (if any)

            if (c == ' ') {                                                     // normal (argless) type
                *comp_ptr++ = CMQUIT;                                           // just store the opcode
            } else {                                                            // must be the type with arg
                source_ptr--;                                                   // back up source
                eval();                                                         // get the value
                *comp_ptr++ = CMQUITA;                                          // and add the opcode
            }

            break;                                                              // end of QUIT code

        case 'R':                                                               // READ
            if (isalpha(*source_ptr) != 0) {                                    // if the next is alpha
                if (strncasecmp((char *) source_ptr, "ead", 3) != 0) SYNTX;
                source_ptr += 3;                                                // point past the "ead"
            }

            c = *source_ptr++;                                                  // get next char

            if (c == ':') {                                                     // postcondition ?
                eval();                                                         // evaluate the TVE
                *comp_ptr++ = JMP0;                                             // store the opcode
                jmp_eoc = comp_ptr;                                             // save the location
                comp_ptr += sizeof(short);                                      // leave space for offset
                c = *source_ptr++;                                              // get next char
            }

            if (c != ' ') SYNTX;                                                // must be a space
            parse_read();
            break;                                                              // end of read

        case 'S':                                                               // SET
            if (isalpha(*source_ptr) != 0) {                                    // if the next is alpha
                if (strncasecmp((char *) source_ptr, "et", 2) != 0) SYNTX;
                source_ptr += 2;                                                // point past the "et"
            }

            c = *source_ptr++;                                                  // get next char

            if (c == ':') {                                                     // postcondition ?
                eval();                                                         // evaluate the TVE
                *comp_ptr++ = JMP0;                                             // store the opcode
                jmp_eoc = comp_ptr;                                             // save the location
                comp_ptr += sizeof(short);                                      // leave space for offset
                c = *source_ptr++;                                              // get next char
            }

            if (c != ' ') SYNTX;                                                // must be a space
            parse_set();                                                        // parse the set stuff
            break;                                                              // end of SET code

        case 'U':                                                               // USE
            if (isalpha(*source_ptr) != 0) {                                    // if the next is alpha
                if (strncasecmp((char *) source_ptr, "se", 2) != 0) SYNTX;
                source_ptr += 2;                                                // point past the "se"
            }

            c = *source_ptr++;                                                  // get next char

            if (c == ':') {                                                     // postcondition ?
                eval();                                                         // evaluate the TVE
                *comp_ptr++ = JMP0;                                             // store the opcode
                jmp_eoc = comp_ptr;                                             // save the location
                comp_ptr += sizeof(short);                                      // leave space for offset
                c = *source_ptr++;                                              // get next char
            }

            if (c != ' ') SYNTX;                                                // must be a space
            parse_use();
            break;                                                              // end of USE

        case 'V':                                                               // VIEW - no indirection
            if (isalpha(*source_ptr) != 0) {                                    // if the next is alpha
                if (strncasecmp((char *) source_ptr, "iew", 3) != 0) SYNTX;
                source_ptr += 3;                                                // point past the "iew"
            }

            c = *source_ptr++;                                                  // get next char

            if (c == ':') {                                                     // postcondition ?
                eval();                                                         // evaluate the TVE
                *comp_ptr++ = JMP0;                                             // store the opcode
                jmp_eoc = comp_ptr;                                             // save the location
                comp_ptr += sizeof(short);                                      // leave space for offset
                c = *source_ptr++;                                              // get next char
            }

            if (c != ' ') SYNTX;                                                // must be a space
            eval();                                                             // get arg 1

            for (args = 1; args < 5; args++) {                                  // up to 4 args
                if (*source_ptr != ':') break;                                  // quit if not a colon
                source_ptr++;                                                   // incr source ptr
                eval();                                                         // eval next
            }

            if (args < 2) SYNTX;                                                // must be at least 2 args
            *comp_ptr++ = CMVIEW;                                               // the opcode
            *comp_ptr++ = (u_char) args;                                        // the arg count
            break;                                                              // end of VIEW

        case 'W':                                                               // WRITE
            if (isalpha(*source_ptr) != 0) {                                    // if the next is alpha
                if (strncasecmp((char *) source_ptr, "rite", 4) != 0) SYNTX;
                source_ptr += 4;                                                // point past the "rite"
            }

            c = *source_ptr++;                                                  // get next char

            if (c == ':') {                                                     // postcondition ?
                eval();                                                         // evaluate the TVE
                *comp_ptr++ = JMP0;                                             // store the opcode
                jmp_eoc = comp_ptr;                                             // save the location
                comp_ptr += sizeof(short);                                      // leave space for offset
                c = *source_ptr++;                                              // get next char
            }

            if (c != ' ') SYNTX;                                                // must be a space
            parse_write();
            break;                                                              // end of WRITE

        case 'X':                                                               // XECUTE
            if (isalpha(*source_ptr) != 0) {                                    // if the next is alpha
                if (strncasecmp((char *) source_ptr, "ecute", 5) != 0) SYNTX;
                source_ptr += 5;                                                // point past the "ecute"
            }

            c = *source_ptr++;                                                  // get next char

            if (c == ':') {                                                     // postcondition ?
                eval();                                                         // evaluate the TVE
                *comp_ptr++ = JMP0;                                             // store the opcode
                jmp_eoc = comp_ptr;                                             // save the location
                comp_ptr += sizeof(short);                                      // leave space for offset
                c = *source_ptr++;                                              // get next char
            }

            if (c != ' ') SYNTX;                                                // must be a space
            parse_xecute();
            break;                                                              // end of XECUTE

        default:                                                                // we didn't understand that
            comperror(-(ERRZ13 + ERRMLAST));                                    // give an error
            return;                                                             // and return
        }                                                                       // end of switch

        if (jmp_eoc != NULL) {                                                  // was there a postcond
            s = (short) (comp_ptr - jmp_eoc - sizeof(short));
            assert(sizeof(s) == sizeof(short));
            memcpy(jmp_eoc, &s, sizeof(short));                                 // save the jump offset
        }

        *comp_ptr++ = OPENDC;                                                   // flag end of command
    }                                                                           // end of while (TRUE) parse
}                                                                               // end of parse()
