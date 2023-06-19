/*
 * Package:  Reference Standard M
 * File:     rsm/compile/eval.c
 * Summary:  module compile - evaluate
 *
 * David Wicksell <dlw@linux.com>
 * Copyright © 2020-2023 Fourth Watch Software LC
 * https://gitlab.com/Reference-Standard-M/rsm
 *
 * Based on MUMPS V1 by Raymond Douglas Newman
 * Copyright (c) 1999-2018
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
 * along with this program. If not, see http://www.gnu.org/licenses/.
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

u_char *source_ptr;                                                             // pointer to source code
u_char *comp_ptr;                                                               // pointer to compiled code

void comperror(short err)                                                       // compile error
{
    int     s;                                                                  // for functions
    u_short us;                                                                 // for functions
    cstring *line;                                                              // line of code
    u_char  *src;                                                               // current src ptr
    int     i;                                                                  // a handy int
    u_char  tmp[128];                                                           // some space

    *comp_ptr++ = OPERROR;                                                      // say it's an error
    assert(sizeof(err) == sizeof(short));
    memcpy(comp_ptr, &err, sizeof(short));
    comp_ptr += sizeof(short);
    *comp_ptr++ = OPNOP;                                                        // in case of IF etc.
    *comp_ptr++ = OPNOP;                                                        // in case of IF etc.
    if (!partab.checkonly) goto scan;                                           // done
    if (partab.checkonly == *partab.ln) return;                                 // done this one once
    partab.checkonly = *partab.ln;                                              // record done
    line = *partab.lp;                                                          // get the line address
    src = *partab.sp;                                                           // and the current source
    s = SQ_Write(line);                                                         // write the line
    if (s < 0) goto scan;                                                       // exit on error
    s = SQ_WriteFormat(SQ_LF);                                                  // return
    if (s < 0) goto scan;                                                       // exit on error
    i = src - line->buf - 1;                                                    // get the offset

    if (i > 0) {
        s = SQ_WriteFormat(i);                                                  // tab
        if (s < 0) goto scan;                                                   // exit on error
    }

    line = (cstring *) tmp;                                                     // some space
DISABLE_WARN(-Warray-bounds)
    line->buf[0] = '^';                                                         // point
    line->buf[1] = ' ';                                                         // and a space
    us = UTIL_strerror(err, &line->buf[2]);                                     // get the error
    line->len = us + 2;                                                         // the length
    memcpy(&line->buf[line->len], " - At line ", 11);                           // front bit
    us = itocstring(&line->buf[line->len + 11], *partab.ln);                    // format line number
    line->len += us + 11;                                                       // the length
ENABLE_WARN
    s = SQ_Write(line);                                                         // write the line
    if (s >= 0) SQ_WriteFormat(SQ_LF);                                          // if no error return
    if (partab.checkonly) partab.errors++;                                      // syntax check so increment error count

scan:
    while (*source_ptr) source_ptr++;                                           // skip rest of line
    return;                                                                     // and done
}

/*
 * Function atom entered with source_ptr pointing at the source
 * atom to evaluate and comp_ptr pointing at where to put the code.
 */
void atom(void)                                                                 // evaluate source
{
    char   c;                                                                   // current character
    short  s;                                                                   // for function returns

    c = *source_ptr++;                                                          // get a character

    if (c == '@') {                                                             // indirection?
        atom();                                                                 // eval what follows

        if (*source_ptr != '@') {                                               // another one?
            *comp_ptr++ = INDEVAL;                                              // no, eval what follows
            return;                                                             // and just exit
        }

        *comp_ptr++ = INDMVAR;                                                  // make an mvar out of it
        s = localvar();                                                         // parse the rest of it

        if (s < 0) {                                                            // if we got an error
            comperror(s);                                                       // compile it
            return;                                                             // and exit
        }

        return;                                                                 // and exit
    }

    if ((isalpha((int) c) != 0) || (c == '%') || (c == '^')) {                  // check for local variable or a global var
        source_ptr--;                                                           // backup to first character
        s = localvar();                                                         // parse the variable

        if (s < 0) {                                                            // if we got an error
            comperror(s);                                                       // compile it
            return;                                                             // and exit
        }

        return;                                                                 // and exit
    }                                                                           // end variable parse

    if (c == '$') {                                                             // check for a function
        dodollar();                                                             // eval it
        return;                                                                 // and exit
    }                                                                           // end function parse

    if ((isdigit((int) c) != 0) || (c == '.')) {                                // check for number or dot
        source_ptr--;                                                           // back up the source ptr
        *comp_ptr++ = OPSTR;                                                    // say string following
        s = ncopy(&source_ptr, comp_ptr + sizeof(u_short));                     // copy as number

        if (s < 0) {                                                            // if we got an error
          comp_ptr--;                                                           // remove the OPSTR
          comperror(s);                                                         // compile it
          return;                                                               // and exit
        }

        *((u_short *) comp_ptr) = s;                                            // store string count
        comp_ptr += sizeof(u_short) + s + 1;                                    // allow for null byte
        return;
    }                                                                           // end numeric parse

    if (c == '"') {                                                             // rabbit ear
        int    j = sizeof(u_short);                                             // point at p->buf[0]
        u_char *p;                                                              // a pointer

        *comp_ptr++ = OPSTR;                                                    // say string following
        p = comp_ptr;                                                           // possible destination

        while (TRUE) {                                                          // scan the string
            if (*source_ptr == '\0') {                                          // check for end of string
                comp_ptr--;                                                     // remove the OPSTR
                comperror(-(ERRZ12 + ERRMLAST));                                // compile an error
                return;                                                         // and exit
            }                                                                   // end of error bit

            if ((*source_ptr == '"') && (source_ptr[1] != '"')) {               // check end of literal
                p[j] = '\0';                                                    // null terminate it
                source_ptr++;                                                   // point past it
                break;                                                          // and exit
            }                                                                   // end 'end of str' code

            p[j++] = *source_ptr++;                                             // copy the character
            if ((*(source_ptr - 1) == '"') && (*source_ptr == '"')) source_ptr++; // got rabbit ears? then point past the second one
        }                                                                       // end of copy loop

        *((u_short *) p) = (u_short) (j - sizeof(u_short));                     // store cstring count
        comp_ptr += j + 1;                                                      // point past str and null
        return;
    }                                                                           // end string literal

    if (c == '\'') {                                                            // check for single quote
        atom();                                                                 // get the following
        *comp_ptr++ = OPNOT;                                                    // do the NOT
        return;
    }                                                                           // end NOT parsing

    if (c == '+') {                                                             // check for plus
        atom();                                                                 // get the following
        *comp_ptr++ = OPPLUS;                                                   // do the plus
        return;
    }                                                                           // end NOT parsing

    if (c == '-') {                                                             // check for unary minus
        atom();                                                                 // get the following
        *comp_ptr++ = OPMINUS;                                                  // do the minus
        return;
    }                                                                           // end NOT parsing

    if (c == '(') {                                                             // open bracket
        eval();                                                                 // eval content of ()

        if (*source_ptr++ != ')') {                                             // error if no trailing ) found
            comperror(-(ERRZ12 + ERRMLAST));                                    // compile an error
            return;                                                             // and exit
        }                                                                       // end error

        return;
    }                                                                           // end open bracket parse

    comperror(-(ERRZ12 + ERRMLAST));                                            // compile an error
    return;                                                                     // and exit
}

int operator(void)                                                              // extract an operator
{
    char c;                                                                     // the character
    int  not = 0;                                                               // not flag

    c = *source_ptr++;                                                          // get next char

    if (c == '\'') {                                                            // a NOT?
        not = 1;                                                                // set the not
        c = *source_ptr++;                                                      // get next char
    }

    switch (c) {
    case '+':                                                                   // add
        if (not) return 0;                                                      // a not here is junk
        return OPADD;                                                           // save opcode

    case '-':                                                                   // subtract
        if (not) return 0;                                                      // a not here is junk
        return OPSUB;                                                           // save opcode

    case '*':                                                                   // multiply (or power)
        if (not) return 0;                                                      // a not here is junk

        if (*source_ptr == '*') {                                               // if there is another
            source_ptr++;                                                       // advance the pointer
            return OPPOW;                                                       // it's a power
        }

        return OPMUL;                                                           // set as a multiply

    case '/':                                                                   // divide
        if (not) return 0;                                                      // a not here is junk
        return OPDIV;                                                           // set the op code

    case '\\':                                                                  // back-slash
        if (not) return 0;                                                      // a not here is junk
        return OPINT;                                                           // integer divide

    case '#':                                                                   // hash
        if (not) return 0;                                                      // a not here is junk
        return OPMOD;                                                           // modulus

    case '_':                                                                   // underscore
        if (not) return 0;                                                      // a not here is junk
        return OPCAT;                                                           // concatenate

    case '=':                                                                   // equal sign
        return (not ? OPNEQL : OPEQL);                                          // equal or not

    case '<':                                                                   // less than
        if (*source_ptr == '=') {                                               // if there is another
            source_ptr++;                                                       // advance the pointer
            if (not) return 0;                                                  // a not here is junk
            return OPNGTR;                                                      // less than or equal
        }

        return (not ? OPNLES : OPLES);                                          // less than or not

    case '>':                                                                   // greater than
        if (*source_ptr == '=') {                                               // if there is another
            source_ptr++;                                                       // advance the pointer
            if (not) return 0;                                                  // a not here is junk
            return OPNLES;                                                      // greater than or equal
        }

        return (not ? OPNGTR : OPGTR);                                          // greater than or not

    case '&':                                                                   // and
        return (not ? OPNAND : OPAND);                                          // and or nand

    case '!':                                                                   // exclam
        if (*source_ptr == '!') {                                               // if there is another
            source_ptr++;                                                       // advance the pointer
            return not ? OPNXOR : OPXOR;                                        // xor or xnor
        }

        return (not ? OPNIOR : OPIOR);                                          // or or nor

    case '[':                                                                   // left square bracket
        return (not ? OPNCON : OPCON);                                          // contains or not

    case ']':                                                                   // right square bracket
        if (*source_ptr == ']') {                                               // if there is another
            source_ptr++;                                                       // advance the pointer

            if (*source_ptr == '=') {                                           // if there is another
                source_ptr++;                                                   // advance the pointer
                if (not) return 0;                                              // a not here is junk
                return OPSAFEQL;                                                // sorts after or equal
            }

            return (not ? OPNSAF : OPSAF);                                      // sorts after or not
        }

        if (*source_ptr == '=') {                                               // if there is another
            source_ptr++;                                                       // advance the pointer
            if (not) return 0;                                                  // a not here is junk
            return OPFOLEQL;                                                    // follows or equal
        }

        return (not ? OPNFOL : OPFOL);                                          // follows or not

    case '?':                                                                   // question
        return (not ? OPNPAT : OPPAT);                                          // matches or not

    default:                                                                    // stuffed up
        return 0;                                                               // clear op
    }                                                                           // end of switch for operators
}

/*
 * Function eval entered with source_ptr pointing at the source
 * expression to evaluate and comp_ptr pointing at where to put the code.
 */
void eval(void)                                                                 // evaluate source
{
    int     q;                                                                  // in quotes indicator
    cstring *ptr;                                                               // spare pointer
    u_char  c;

    atom();                                                                     // get first operand

    // do it at a higher level
    if ((*source_ptr == ')') || (*source_ptr == ',') || (*source_ptr == ':') ||
      // end of string or start of routine ref or end of name indirection or end of command
      (*source_ptr == '\0') || (*source_ptr == '^') || (*source_ptr == '@') || (*source_ptr == ' ')) {
        return;                                                                 // exit
    }

    while (TRUE) {                                                              // until the end
        int op = operator();                                                    // get the operator
        int pattern = 0;                                                        // for pattern match funny

        if (op == 0) {                                                          // an error??
          comperror(-(ERRZ12 + ERRMLAST));                                      // compile the error
          return;                                                               // and exit
        }

        pattern = ((op == OPPAT) || (op == OPNPAT));                            // bloody pattern match

        if (pattern && (*source_ptr == '@')) {                                  // indirect pattern
            source_ptr++;                                                       // skip the @
            pattern = 0;                                                        // clear funny pattern match
        }

        if (pattern) {                                                          // normal (not @) pattern match
            q = 0;                                                              // not in quotes
            *comp_ptr++ = OPSTR;                                                // pretend it's a string
            ptr = (cstring *) comp_ptr;                                         // remember for ron
            comp_ptr += sizeof(u_short);                                        // skip the count

            while (TRUE) {                                                      // loop
                c = *source_ptr++;                                              // get next char

                if (q) {                                                        // if in quotes
                    *comp_ptr++ = c;                                            // copy char
                    if (c == '"') q = 0;                                        // check for a quote
                    continue;                                                   // go for more
                }

                if (c == '"') {                                                 // if it's a quote
                    q = 1;                                                      // turn on
                    *comp_ptr++ = c;                                            // copy char
                    continue;                                                   // go for more
                }

                // alpha numeric or a dot
                if ((isalnum(c) != 0) || (c == '.')) {
                    *comp_ptr++ = c;                                            // copy char
                    continue;                                                   // go for more
                }

                if (c == '(') {                                                 // open bracket
                    pattern++;                                                  // count it
                    *comp_ptr++ = c;                                            // copy char
                    continue;                                                   // go for more
                }

                if ((c == ')') && (pattern > 1)) {                              // close bracket
                    pattern--;                                                  // count it
                    *comp_ptr++ = c;                                            // copy char
                    continue;                                                   // go for more
                }

                if ((pattern > 1) && (c == ',')) {                              // comma inside ()
                    *comp_ptr++ = c;                                            // copy char
                    continue;                                                   // go for more
                }

                source_ptr--;                                                   // backup
                break;                                                          // and exit
            }                                                                   // end while

            ptr->len = comp_ptr - ptr->buf;                                     // get the length
            *comp_ptr++ = '\0';                                                 // null terminate it
        } else {
            atom();                                                             // else get next operand
        }

        *comp_ptr++ = (u_char) op;                                              // store the operator

        // do it at a higher level
        if ((*source_ptr == ')') || (*source_ptr == ',') || (*source_ptr == ':') ||
          // end of string or start of routine ref or end of command
          (*source_ptr == '\0') || (*source_ptr == '^') || (*source_ptr == ' ')) {
            return;                                                             // exit
        }
    }
}
