/*
 * Package: Reference Standard M
 * File:    rsm/compile/dollar.c
 * Summary: module compile - evaluate functions, vars etc.
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

#include <stdio.h>                                                              // always include
#include <stdlib.h>                                                             // these two
#include <sys/types.h>                                                          // for u_char def
#include <string.h>
#include <ctype.h>
#include <errno.h>                                                              // error stuff
#include <limits.h>                                                             // for LONG_MAX etc.
#include <math.h>
#include <assert.h>
#include "rsm.h"                                                                // standard includes
#include "proto.h"                                                              // standard prototypes
#include "error.h"                                                              // and the error defs
#include "opcode.h"                                                             // and the opcodes
#include "compile.h"                                                            // compiler stuff

void dodollar(void)                                                             // parse var, func etc.
{
    int     len;                                                                // length of name
    short   s;                                                                  // a handy short
    u_short us;                                                                 // a handy unsigned short
    int     i = 0;                                                              // a handy int
    int     sel;                                                                // and another
    int     args = 0;                                                           // function args
    u_char  *ptr;                                                               // a handy pointer
    u_char  *p;                                                                 // a handy pointer
    u_char  *selj[256];                                                         // a heap of them for $SELECT()
    char    name[20];                                                           // where to put the name
    char    c;                                                                  // current character
    u_char  save[1024];                                                         // a useful save area
    int     savecount;                                                          // number of bytes saved
    short   errm4 = -ERRM4;                                                     // useful error number

    c = toupper(*source_ptr++);                                                 // get the character in upper

    if (c == '$') {                                                             // extrinsic
        ptr = comp_ptr;                                                         // save compile pointer
        *comp_ptr++ = CMDOTAG;                                                  // assume a do tag
        s = routine(-1);                                                        // parse the rouref

        if ((s > -1) || (s == -4)) {                                            // indirect etc. not on here
            comp_ptr = ptr;                                                     // back where we started for error
            SYNTX;
        }

        if (s < -4) {                                                           // check for error
            comperror(s);                                                       // complain
            return;                                                             // and exit
        }

        args = 129;                                                             // number of args (128=$$)

        if (s == -2) {
            *ptr = CMDORT;                                                      // routine and tag
        } else if (s == -3) {
            *ptr = CMDOROU;                                                     // just a routine
        }

        if (*source_ptr == '(') {                                               // any args?
            args--;                                                             // back to 128
            savecount = comp_ptr - ptr;                                         // bytes that got compiled
            memcpy(save, ptr, savecount);                                       // save that lot
            comp_ptr = ptr;                                                     // back where we started
            source_ptr++;                                                       // skip the (

            while (TRUE) {                                                      // while we have args
                if (args > (MAX_NUM_ARGS | 128)) SYNTX;                         // too many (128=$$)
                args++;                                                         // count an argument

                if (*source_ptr == ')') {                                       // trailing bracket ?
                    source_ptr++;                                               // skip the )
                    break;                                                      // and exit
                }

                if ((*source_ptr == ',') || (*source_ptr == ')')) {             // if empty argument
                    *comp_ptr++ = VARUNDF;                                      // flag it
                } else if ((*source_ptr == '.') && (isdigit(source_ptr[1]) == 0)) { // by-reference and not .numeric?
                    source_ptr++;                                               // skip the dot

                    if (*source_ptr == '@') {                                   // if indirection
                        source_ptr++;                                           // skip the @
                        atom();                                                 // eval the string
                        *comp_ptr++ = INDMVAR;
                    } else {
                        p = comp_ptr;                                           // save current position
                        s = localvar();                                         // get a variable

                        if (s < 0) {                                            // if we got an error
                            comperror(s);                                       // compile it
                            return;                                             // and exit
                        }

                        p += s;                                                 // point here
                        *p = OPMVAR;                                            // get the mvar onto stack
                    }

                    *comp_ptr++ = NEWBREF;                                      // flag 'by-reference'
                } else {                                                        // by value
                    eval();                                                     // leave the value on the stack
                }

                if (*source_ptr == ')') continue;                               // trailing bracket? - do it above

                if (*source_ptr == ',') {                                       // a comma ?
                    source_ptr++;                                               // skip the ,
                    continue;                                                   // go for more
                }

                SYNTX;                                                          // all else is an error
            }                                                                   // end of while

            memcpy(comp_ptr, save, savecount);                                  // copy the code back
            comp_ptr += savecount;                                              // and add to the pointer
        }                                                                       // end of argument decode

        *comp_ptr++ = (u_char) args;                                            // store number of args
        return;                                                                 // and exit
    }

    if (c == '&') {                                                             // xcall
        c = toupper(*source_ptr++);                                             // get next

        if (c == '%') {                                                         // if it's a percent
            name[i++] = c;                                                      // save it
            c = toupper(*source_ptr++);                                         // get next
        }

        while (isalpha((int) c) != 0) {                                         // while we have alphas
            name[i++] = c;                                                      // save it
            c = toupper(*source_ptr++);                                         // get next
        }

        name[i] = '\0';                                                         // null terminate

        if (c == '(') {                                                         // if it has args
            while (TRUE) {                                                      // loop
                eval();                                                         // get next argument
                args++;                                                         // count an argument
                c = *source_ptr++;                                              // get term char
                if (c == ')') break;                                            // all done if closing )
                if (c != ',') EXPRE;                                            // if it's not a comma
            }                                                                   // end of args loop
        } else {
            source_ptr--;                                                       // else backup the source ptr
        }

        if (args > 2) {                                                         // all XCalls take two args
            comperror(-(ERRZ18 + ERRMLAST));                                    // junk
            return;
        }

        for (i = args; i < 2; i++) {                                            // force two arguments
            *comp_ptr++ = OPSTR;                                                // say string follows
            *comp_ptr++ = 0;                                                    // endian doesn't matter here
            *comp_ptr++ = 0;                                                    // endian doesn't matter here
            *comp_ptr++ = '\0';                                                 // null terminated
        }

        if (strcmp(name, "%DIRECTORY") == 0) {                                  // $&%DIRECTORY()
            *comp_ptr++ = XCDIR;                                                // save the opcode
        } else if (strcmp(name, "%HOST") == 0) {                                // $&%HOST()
            *comp_ptr++ = XCHOST;                                               // save the opcode
        } else if (strcmp(name, "%FILE") == 0) {                                // $&%FILE()
            *comp_ptr++ = XCFILE;                                               // save the opcode
        } else if (strcmp(name, "%ERRMSG") == 0) {                              // $&%ERRMSG()
            *comp_ptr++ = XCERR;                                                // save the opcode
        } else if (strcmp(name, "%OPCOM") == 0) {                               // $&%OPCOM()
            *comp_ptr++ = XCOPC;                                                // save the opcode
        } else if (strcmp(name, "%SIGNAL") == 0) {                              // $&%SIGNAL()
            *comp_ptr++ = XCSIG;                                                // save the opcode
        } else if (strcmp(name, "%SPAWN") == 0) {                               // $&%SPAWN()
            *comp_ptr++ = XCSPA;                                                // save the opcode
        } else if (strcmp(name, "%VERSION") == 0) {                             // $&%VERSION()
            *comp_ptr++ = XCVER;                                                // save the opcode
        } else if (strcmp(name, "%ZWRITE") == 0) {                              // $&%ZWRITE()
            *comp_ptr++ = XCZWR;                                                // save the opcode
        } else if (strcmp(name, "E") == 0) {                                    // $&E()
            *comp_ptr++ = XCE;                                                  // save the opcode
        } else if (strcmp(name, "PASCHK") == 0) {                               // $&PASCHK()
            *comp_ptr++ = XCPAS;                                                // save the opcode
        } else if (strcmp(name, "V") == 0) {                                    // $&V()
            *comp_ptr++ = XCV;                                                  // save the opcode
        } else if (strcmp(name, "X") == 0) {                                    // $&X()
            *comp_ptr++ = XCX;                                                  // save the opcode
        } else if (strcmp(name, "XRSM") == 0) {                                 // $&XRSM()
            *comp_ptr++ = XCXRSM;                                               // save the opcode
        } else if (strcmp(name, "%SETENV") == 0) {                              // $&%SETENV()
            *comp_ptr++ = XCSETENV;                                             // save the opcode
        } else if (strcmp(name, "%GETENV") == 0) {                              // $&%GETENV()
            *comp_ptr++ = XCGETENV;                                             // save the opcode
        } else if (strcmp(name, "%ROUCHK") == 0) {                              // $&%ROUCHK()
            *comp_ptr++ = XCROUCHK;                                             // save the opcode
        } else if (strcmp(name, "%FORK") == 0) {                                // $&%FORK()
            *comp_ptr++ = XCFORK;                                               // save the opcode
        } else if (strcmp(name, "%IC") == 0) {                                  // $&%IC()
            *comp_ptr++ = XCIC;                                                 // save the opcode
        } else if (strcmp(name, "%WAIT") == 0) {                                // $&%WAIT()
            *comp_ptr++ = XCWAIT;                                               // save the opcode
        } else if (strcmp(name, "DEBUG") == 0) {                                // $&DEBUG()
            *comp_ptr++ = XCDEBUG;                                              // save the opcode
        } else if (strcmp(name, "%COMPRESS") == 0) {                            // $&%COMPRESS()
            *comp_ptr++ = XCCOMP;                                               // save the opcode
        } else {
            comperror(-(ERRZ18 + ERRMLAST));                                    // junk
        }

        return;                                                                 // end of xcalls
    }

    name[0] = c;                                                                // save first char

    for (len = 0; isalpha(source_ptr[len]) != 0; len++) {                       // scan string
        name[len + 1] = source_ptr[len];                                        // copy alphas
    }

    source_ptr += len;                                                          // move source along
    len++;                                                                      // add in first character
    name[len] = '\0';                                                           // null terminate name

    if ((*source_ptr == '(') && (strncmp(name, "ZBP\0", 4) != 0)) {             // check for a function
        goto function;                                                          // $ZBP is an M array, not a function
    }

    switch (name[0]) {                                                          // dispatch on initial
    case 'D':                                                                   // $D[EVICE]
        if (len > 1) {                                                          // check for extended name
            if (strncasecmp(name, "device\0", 7) != 0) UNVAR;
        }

        *comp_ptr++ = VARD;                                                     // add the opcode
        return;                                                                 // and exit

    case 'E':                                                                   // $EC[ODE], $ES[TACK], and $ET[RAP]
        if (len < 2) UNVAR;                                                     // must be 2 for this one

        switch (toupper((int) name[1])) {                                       // switch on second char
        case 'C':                                                               // $EC[ODE]
            if (len > 2) {                                                      // check for extended name
                if (strncasecmp(name, "ecode\0", 6) != 0) UNVAR;
            }

            *comp_ptr++ = VAREC;                                                // add the opcode
            return;                                                             // and exit

        case 'S':                                                               // $ES[TACK]
            if (len > 2) {                                                      // check for extended name
                if (strncasecmp(name, "estack\0", 7) != 0) UNVAR;
            }

            *comp_ptr++ = VARES;                                                // add the opcode
            return;                                                             // and exit

        case 'T':                                                               // $ET[RAP]
            if (len > 2) {                                                      // check for extended name
                if (strncasecmp(name, "etrap\0", 6) != 0) UNVAR;
            }

            *comp_ptr++ = VARET;                                                // add the opcode
            return;                                                             // and exit

        default:                                                                // junk
            UNVAR;
        }                                                                       // end of $E... switch

    case 'H':                                                                   // $H[OROLOG]
        if (len > 1) {                                                          // check for extended name
            if (strncasecmp(name, "horolog\0", 8) != 0) UNVAR;
        }

        *comp_ptr++ = VARH;                                                     // add the opcode
        return;                                                                 // and exit

    case 'I':                                                                   // $I[O]
        if (len > 1) {                                                          // check for extended name
            if (strncasecmp(name, "io\0", 3) != 0) UNVAR;
        }

        *comp_ptr++ = VARI;                                                     // add the opcode
        return;                                                                 // and exit

    case 'J':                                                                   // $J[OB]
        if (len > 1) {                                                          // check for extended name
            if (strncasecmp(name, "job\0", 4) != 0) UNVAR;
        }

        *comp_ptr++ = VARJ;                                                     // add the opcode
        return;                                                                 // and exit

    case 'K':                                                                   // $K[EY]
        if (len > 1) {                                                          // check for extended name
            if (strncasecmp(name, "key\0", 4) != 0) UNVAR;
        }

        *comp_ptr++ = VARK;                                                     // add the opcode
        return;                                                                 // and exit

    case 'P':                                                                   // $P[RINCIPAL]
        if (len > 1) {                                                          // check for extended name
            if (strncasecmp(name, "principal\0", 10) != 0) UNVAR;
        }

        *comp_ptr++ = VARP;                                                     // add the opcode
        return;                                                                 // and exit

    case 'Q':                                                                   // $Q[UIT]
        if (len > 1) {                                                          // check for extended name
            if (strncasecmp(name, "quit\0", 5) != 0) UNVAR;
        }

        *comp_ptr++ = VARQ;                                                     // add the opcode
        return;                                                                 // and exit

    case 'R':                                                                   // $R[EFERENCE]
        if (len > 1) {                                                          // check for extended name
            if (strncasecmp(name, "reference\0", 10) != 0) UNVAR;
        }

        *comp_ptr++ = VARR;                                                     // add the opcode
        return;                                                                 // and exit

    case 'S':                                                                   // $ST[ACK], $S[TORAGE], and $SY[STEM]
        if ((len == 1) || (strncasecmp(name, "storage\0", 8) == 0)) {           // $S[TORAGE]
            *comp_ptr++ = VARS;                                                 // add the opcode
            return;                                                             // and exit
        }

        switch (toupper((int) name[1])) {                                       // switch on second char
        case 'T':                                                               // $ST[ACK]
            if (len > 2) {                                                      // check for extended name
                if (strncasecmp(name, "stack\0", 6) != 0) UNVAR;
            }

            *comp_ptr++ = VARST;                                                // add the opcode
            return;                                                             // and exit

        case 'Y':                                                               // $SY[STEM]
            if (len > 2) {                                                      // check for extended name
                if (strncasecmp(name, "system\0", 7) != 0) UNVAR;
            }

            *comp_ptr++ = VARSY;                                                // add the opcode
            return;                                                             // and exit

        default:                                                                // junk
            UNVAR;
        }                                                                       // end of $S... switch

    case 'T':                                                                   // $T[EST]
        if (len > 1) {                                                          // check for extended name
            if (strncasecmp(name, "test\0", 5) != 0) UNVAR;
        }

        *comp_ptr++ = VART;                                                     // add the opcode
        return;                                                                 // and exit

    case 'X':                                                                   // $X
        if (len > 1) UNVAR;                                                     // check for extended name
        *comp_ptr++ = VARX;                                                     // add the opcode
        return;                                                                 // and exit

    case 'Y':                                                                   // $Y
        if (len > 1) UNVAR;                                                     // check for extended name
        *comp_ptr++ = VARY;                                                     // add the opcode
        return;                                                                 // and exit

    case 'Z':                                                                   // $ZBP and $ZUT
        if (strncmp(name, "ZBP\0", 4) == 0) {                                   // $ZBP (M array, not a function)
            source_ptr -= len + 1;                                              // backup to first character
            s = localvar();                                                     // parse the variable
            if (s < 0) comperror(s);                                            // if we got an error, compile it
        } else if (strncasecmp(name, "zut\0", 4) == 0) {                        // $ZUT
            *comp_ptr++ = VARZUT;                                               // add the opcode
        } else {
            UNVAR;                                                              // anything else
        }

        return;                                                                 // and exit

    default:                                                                    // an error
        UNVAR;
    }                                                                           // end of vars switch

function:                                                                       // function code starts here
    source_ptr++;                                                               // incr past the bracket
    ptr = comp_ptr;                                                             // remember where this goes
    sel = ((name[0] == 'S') && (toupper((int) name[1]) != 'T'));                // is $SELECT

    // $DATA, $GET, $INCREMENT, $NAME/$NEXT, $ORDER
    if ((name[0] == 'D') || (name[0] == 'G') || (name[0] == 'I') || (name[0] == 'N') || (name[0] == 'O') ||
      // $QUERY, but not $QSUBSCRIPT, and not $QLENGTH
      ((name[0] == 'Q') && (toupper((int) name[1]) != 'S') && (toupper((int) name[1]) != 'L'))) {
        if (*source_ptr == '@') {                                               // indirection ?
            atom();                                                             // eval it
            ptr = comp_ptr - 1;                                                 // remember where this goes

            if (*ptr == INDEVAL) {                                              // if it's going to eval it
                if ((name[0] == 'N') || (name[0] == 'O') || (name[0] == 'Q')) { // $NAME/$NEXT, $ORDER or $QUERY
                    *ptr = INDMVARN;                                            // allow null subs
                } else {
                    *ptr = INDMVAR;                                             // make an mvar from it
                }
            } else {                                                            // experimental for $ORDER(@.@())
                ptr -= 2;                                                       // back up over subs to type

                if (*ptr == OPVAR) {
                    if ((name[0] == 'N') || (name[0] == 'O') || (name[0] == 'Q')) { // $NAME/$NEXT, $ORDER or $QUERY
                        *ptr = OPMVARN;                                         // allow null subs
                    } else {
                        *ptr = OPMVAR;                                          // change to OPMVAR
                    }
                }
            }
        } else {
            s = localvar();                                                     // we need a var

            if (s < 0) {
                comperror(s);                                                   // compile the error
                return;                                                         // and exit
            }

            ptr = &ptr[s];                                                      // point at the OPVAR

            if ((name[0] == 'N') || (name[0] == 'O') || (name[0] == 'Q')) {     // $NAME/$NEXT, $ORDER or $QUERY
                *ptr = OPMVARN;                                                 // allow null subs
            } else {
                *ptr = OPMVAR;                                                  // change to a OPMVAR
            }
        }
    } else if ((name[0] == 'T') && (toupper((int) name[1]) != 'R')) {           // $TEXT
        s = routine(-2);                                                        // parse to strstk

        if (s < -4) {                                                           // check for error
            comperror(s);                                                       // complain
            return;                                                             // and exit
        }
    } else {
        eval();                                                                 // for other functions
    }

    while (TRUE) {
        args++;                                                                 // count an argument
        if (args > 255) EXPRE;                                                  // too many args (255 for intrinsics)
        c = *source_ptr++;                                                      // get term char
        if (c == ')') break;                                                    // all done if closing )

        if (sel) {                                                              // if in a $SELECT()
            if (c != ((args & 1) ? ':' : ',')) EXPRE;                           // must be colon or comma
            *comp_ptr++ = ((args & 1) ? JMP0 : JMP);                            // the opcode
            selj[args] = comp_ptr;                                              // remember for offset
            comp_ptr += sizeof(short);                                          // leave space for it
        } else if (c != ',') {                                                  // else must be a comma
            EXPRE;
        }                                                                       // end special $SELECT() stuff

        eval();                                                                 // get next argument
    }                                                                           // end of args loop

    switch (name[0]) {                                                          // dispatch on initial
    case 'A':                                                                   // $A[SCII]
        if (len > 1) {                                                          // check for extended name
            if (strncasecmp(name, "ascii\0", 6) != 0) EXPRE;
        }

        if (args == 1) {
            *comp_ptr++ = FUNA1;                                                // one arg form
            return;                                                             // and exit
        }

        if (args == 2) {
            *comp_ptr++ = FUNA2;                                                // two arg form
            return;                                                             // and exit
        }

        EXPRE;

    case 'C':                                                                   // $C[HAR]
        if (len > 1) {                                                          // check for extended name
            if (strncasecmp(name, "char\0", 5) != 0) EXPRE;
        }

        if (args > 255) EXPRE;                                                  // check number of args
        *comp_ptr++ = FUNC;                                                     // push the opcode
        *comp_ptr++ = (u_char) args;                                            // number of arguments
        return;                                                                 // and give up

    case 'D':                                                                   // $D[ATA]
        if (len > 1) {                                                          // check for extended name
            if (strncasecmp(name, "data\0", 5) != 0) EXPRE;
        }

        if (args > 1) EXPRE;                                                    // check number of args
        *comp_ptr++ = FUND;                                                     // set the opcode
        return;                                                                 // and give up

    case 'E':                                                                   // $E[XTRACT]
        if (len > 1) {                                                          // check for extended name
            if (strncasecmp(name, "extract\0", 8) != 0) EXPRE;
        }

        if (args == 1) {
            *comp_ptr++ = FUNE1;                                                // one arg form
            return;                                                             // and exit
        }

        if (args == 2) {
            *comp_ptr++ = FUNE2;                                                // two arg form
            return;                                                             // and exit
        }

        if (args == 3) {
            *comp_ptr++ = FUNE3;                                                // two arg form
            return;                                                             // and exit
        }

        EXPRE;

    case 'F':                                                                   // $F[IND] and $FN[UMBER]
        if ((len == 1) || (strncasecmp(name, "find\0", 5) == 0)) {              // $F[IND]
            if (args == 2) {
                *comp_ptr++ = FUNF2;                                            // two arg form
                return;                                                         // and exit
            }

            if (args == 3) {
                *comp_ptr++ = FUNF3;                                            // three arg form
                return;                                                         // and exit
            }

            EXPRE;
        }                                                                       // end $FIND

        if (((len == 2) && (toupper((int) name[1]) == 'N')) || (strncasecmp(name, "fnumber\0", 8) == 0)) { // $FNUMBER
            if (args == 2) {
                *comp_ptr++ = FUNFN2;                                           // two arg form
                return;                                                         // and exit
            }

            if (args == 3) {
                *comp_ptr++ = FUNFN3;                                           // two arg form
                return;                                                         // and exit
            }

            EXPRE;
        }                                                                       // end $FIND

        EXPRE;

    case 'G':                                                                   // $G[ET]
        if (len > 1) {                                                          // check for extended name
            if (strncasecmp(name, "get\0", 4) != 0) EXPRE;
        }

        if (args == 1) {
            *comp_ptr++ = FUNG1;                                                // one arg form
        } else if (args == 2) {
            *comp_ptr++ = FUNG2;                                                // the two arg opcode
        } else {
            EXPRE;                                                              // all others junk
        }

        return;                                                                 // done

    case 'I':                                                                   // $I[NCREMENT]
        if (len > 1) {                                                          // check for extended name
            if (strncasecmp(name, "increment\0", 10) != 0) EXPRE;
        }

        if (args == 1) {
            *comp_ptr++ = FUNI1;                                                // one arg form
        } else if (args == 2) {
            *comp_ptr++ = FUNI2;                                                // the two arg opcode
        } else {
            EXPRE;                                                              // all others junk
        }

        return;                                                                 // done

    case 'J':                                                                   // $J[USTIFY]
        if (len > 1) {                                                          // check for extended name
            if (strncasecmp(name, "justify\0", 8) != 0) EXPRE;
        }

        if (args == 2) {
            *comp_ptr++ = FUNJ2;                                                // two arg form
        } else if (args == 3) {
            *comp_ptr++ = FUNJ3;                                                // three arg form
        } else {                                                                // all else is junk
            EXPRE;
        }

        return;                                                                 // and exit

    case 'L':                                                                   // $L[ENGTH]
        if (len > 1) {                                                          // check for extended name
            if (strncasecmp(name, "length\0", 7) != 0) EXPRE;
        }

        if (args == 1) {
            *comp_ptr++ = FUNL1;                                                // one arg form
        } else if (args == 2) {
            *comp_ptr++ = FUNL2;                                                // two arg form
        } else {
            EXPRE;
        }

        return;

    case 'N':                                                                   // $NA[ME] or $N[EXT]
        if (toupper((int) name[1]) != 'A') {                                    // check second letter
            if (len > 1) {
                if (strncasecmp(name, "next\0", 5) != 0) EXPRE;
            }

            if (!(systab->historic & HISTORIC_DNOK)) EXPRE;
            if (args != 1) EXPRE;
            *comp_ptr++ = OPSTR;
            us = 1;                                                             // the string length
            assert(sizeof(us) == sizeof(u_short));
            memcpy(comp_ptr, &us, sizeof(u_short));
            comp_ptr += sizeof(u_short);
            *comp_ptr++ = '2';                                                  // $NEXT kludge
            *comp_ptr++ = '\0';                                                 // null terminated
            *comp_ptr++ = FUNO2;                                                // two arg form of $ORDER()
            return;
        }

        if (len > 2) {                                                          // check for extended name
            if (strncasecmp(name, "name\0", 5) != 0) EXPRE;
        }

        if (args == 1) {
            *comp_ptr++ = FUNNA1;                                               // one arg form
        } else if (args == 2) {
            *comp_ptr++ = FUNNA2;                                               // two arg opcode
        } else {                                                                // all else is junk
            EXPRE;
        }

        return;                                                                 // and exit

    case 'O':                                                                   // $O[RDER]
        if (len > 1) {                                                          // check for extended name
            if (strncasecmp(name, "order\0", 6) != 0) EXPRE;
        }

        if (args == 1) {
            *comp_ptr++ = FUNO1;                                                // one arg form
        } else if (args == 2) {
            *comp_ptr++ = FUNO2;                                                // two arg form
        } else {
            EXPRE;
        }

        return;

    case 'P':                                                                   // $P[IECE]
        if (len > 1) {                                                          // check for extended name
            if (strncasecmp(name, "piece\0", 6) != 0) {
                comperror(-(ERRZ12 + ERRMLAST));                                // compile an error
                return;                                                         // and give up
            }
        }

        if (args == 2) {
            *comp_ptr++ = FUNP2;                                                // two arg form
        } else if (args == 3) {
            *comp_ptr++ = FUNP3;                                                // three arg form
        } else if (args == 4) {
            *comp_ptr++ = FUNP4;                                                // four arg form
        } else {
            EXPRE;
        }

        return;

    case 'Q':                                                                   // $Q[UERY], $QS[UBSCRIPT], and $QL[ENGTH]
        if ((len == 1) || (strncasecmp(name, "query\0", 6) == 0)) {             // $Q[UERY]
            if (args == 1) {
                *comp_ptr++ = FUNQ1;                                            // one arg form
            } else if (args == 2) {
                *comp_ptr++ = FUNQ2;                                            // two arg form
            } else {
                EXPRE;
            }

            return;                                                             // and exit
        }                                                                       // end $Q[UERY]

        if (((len == 2) && (toupper((int) name[1]) == 'L')) || (strncasecmp(name, "qlength\0", 8) == 0)) { // $QLENGTH
            if (args == 1) {
                *comp_ptr++ = FUNQL;
                return;                                                         // and exit
            }

            EXPRE;
        }                                                                       // end $FIND

        if (((len == 2) && (toupper((int) name[1]) == 'S')) || (strncasecmp(name, "qsubscript\0", 11) == 0)) { // $QSUBSCRIPT
            if (args == 2) {
                *comp_ptr++ = FUNQS;
                return;                                                         // and exit
            }

            EXPRE;
        }                                                                       // end $FIND

        EXPRE;

    case 'R':                                                                   // $R[ANDOM], $RE[VERSE]
        if ((len == 1) || (strncasecmp(name, "random\0", 7) == 0)) {            // $R[ANDOM]
            if (args == 1) {
                *comp_ptr++ = FUNR;                                             // one arg form
                return;                                                         // and exit
            }

            EXPRE;
        }

        if (((len == 2) && (toupper((int) name[1]) == 'E')) || (strncasecmp(name, "reverse\0", 8) == 0)) { // $REVERSE
            if (args == 1) {
                *comp_ptr++ = FUNRE;
                return;                                                         // and exit
            }

            EXPRE;
        }

        EXPRE;

    case 'S':                                                                   // $S[ELECT], $ST[ACK]
        if ((len == 1) || (strncasecmp(name, "select\0", 7) == 0)) {            // $S[ELECT]
            if (args & 1) {                                                     // must be even number
                comp_ptr = ptr;                                                 // start of this
                EXPRE;                                                          // and error it
            }

            *comp_ptr++ = JMP;                                                  // for the last expr
            selj[args] = comp_ptr;                                              // remember for offset
            comp_ptr += sizeof(short);                                          // leave space for it
            selj[args + 1] = comp_ptr;                                          // for the last JMP0
            *comp_ptr++ = OPERROR;                                              // no TVE is an error
            assert(sizeof(errm4) == sizeof(short));
            memcpy(comp_ptr, &errm4, sizeof(short));
            comp_ptr += sizeof(short);

            for (i = 1; i <= args; i++) {                                       // scan the addr array
                if (i & 1) {
                    s = (short) (selj[i + 1] - selj[i]);
                } else {
                    s = (short) (comp_ptr - selj[i] - sizeof(short));
                }

                assert(sizeof(s) == sizeof(short));
                memcpy(selj[i], &s, sizeof(short));
            }

            return;                                                             // end of $SELECT()
        }

        if (((len == 2) && (toupper((int) name[1]) == 'T')) || (strncasecmp(name, "stack\0", 6) == 0)) { // $ST[ACK]
            if (args == 1) {
                *comp_ptr++ = FUNST1;
                return;                                                         // and exit
            }

            if (args == 2) {
                *comp_ptr++ = FUNST2;
                return;                                                         // and exit
            }

            EXPRE;
        }

        EXPRE;

    case 'T':                                                                   // $T[EXT], $TR[ANSLATE]
        if ((len == 1) || (strncasecmp(name, "text\0", 5) == 0)) {              // $T[EXT]
            if (args == 1) {
                *comp_ptr++ = FUNT;                                             // one arg form
                return;                                                         // and exit
            }

            EXPRE;
        }

        if (((len == 2) && (toupper((int) name[1]) == 'R')) || (strncasecmp(name, "translate\0", 10) == 0)) { // $TR[ANSLATE]
            if (args == 2) {
                *comp_ptr++ = FUNTR2;
                return;                                                         // and exit
            }

            if (args == 3) {
                *comp_ptr++ = FUNTR3;
                return;                                                         // and exit
            }

            EXPRE;
        }

        EXPRE;

    case 'V':                                                                   // $VIEW
        if (len > 1) {                                                          // check for extended name
            if (strncasecmp(name, "view\0", 5) != 0) EXPRE;
        }

        if (args == 2) {
            *comp_ptr++ = FUNV2;                                                // two arg form
            return;                                                             // and exit
        }

        if (args == 3) {
            *comp_ptr++ = FUNV3;                                                // three arg form
            return;                                                             // and exit
        }

        if (args == 4) {
            *comp_ptr++ = FUNV4;                                                // four arg form
            return;                                                             // and exit
        }

        EXPRE;

    default:
        EXPRE;
    }                                                                           // end of switch
}
