/*
 * Package:  Reference Standard M
 * File:     rsm/compile/localvar.c
 * Summary:  module compile - parse a local variable
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
#include "rsm.h"                                                                // standard includes
#include "proto.h"                                                              // standard prototypes
#include "error.h"                                                              // and the error defs
#include "opcode.h"                                                             // and the opcodes
#include "compile.h"                                                            // compile stuff

/*
 * Function localvar entered with source_ptr pointing at the source
 * variable to evaluate and comp_ptr pointing at where to put the code.
 *
 * Return    Means
 * -ERR      Nothing compiled, error returned
 * off       Offset from starting point of comp_ptr for OPVAR
 *
 * Following the OPVAR is a byte indicating type of
 * variable as per the following:
 *     TYPMAXSUB           63                                                   // max subscripts
 *     TYPVARNAM           0                                                    // name only (VAR_LEN bytes)
 *     TYPVARLOCMAX        TYPVARNAM+TYPMAXSUB                                  // local is 1->63 subs
 *     TYPVARIDX           64                                                   // 1 byte index (+ #subs)
 *     TYPVARGBL           128                                                  // first global
 *     TYPVARGBLMAX        TYPVARGBL+TYPMAXSUB                                  // global 128->191 subs
 *     TYPVARNAKED         252                                                  // global naked reference
 *     TYPVARGBLUCI        253                                                  // global with UCI
 *     TYPVARGBLUCIENV     254                                                  // global with UCI and env
 *     TYPVARIND           255                                                  // indirect
 *
 * For TYPVARNAM:          OPVAR TYPVARNAM (var_u) name
 *     TYPVARLOC:          subscripts OPVAR TYPVARNAM+#subs (var_u) name
 *     TYPVARIDX:          subscripts OPVAR TYPVARIDX+#subs (u_char) idx
 *     TYPVARGBL:          subscripts OPVAR TYPVARGBL+#subs (var_u) name
 *     TYPVARNAKED:        subscripts OPVAR TYPVARNAKED #subs
 *     TYPVARGBLUCI:       subscripts UCI OPVAR TYPVARGBLUCI #subs (var_u) name
 *     TYPVARGBLUCIENV:    subscripts UCI env OPVAR TYPVARGBLUCIENV #subs (var_u) name
 *     TYPVARIND:          (str on addstk[]) subscripts OPVAR TYPEVARIND #subs
*/
short localvar(void)                                                            // evaluate local variable
{
    char   c;                                                                   // current character
    u_char idx = 0;                                                             // index
    int    i;                                                                   // a useful int
    int    v;                                                                   // for vertical bar
    int    count = 0;                                                           // count subscripts
    var_u  var;                                                                 // to hold variable names
    u_char *sptr;                                                               // to save source_ptr
    u_char *ptr;                                                                // to save comp_ptr
    short  type = TYPVARNAM;                                                    // the type code
    short  ret;                                                                 // the return

    ptr = comp_ptr;                                                             // save comp_ptr
    c = *source_ptr++;                                                          // get a character

    if (c == '@') {                                                             // indirect ?
        if (*source_ptr == '(') {
            type = TYPVARIND;                                                   // yes @...@ ... on addstk[]
            goto subs;                                                          // go do subscripts
        }

        sptr = source_ptr;                                                      // save source_ptr
        atom();

        if ((*source_ptr == '@') && (*(source_ptr + 1) == '(')) {
            *comp_ptr++ = INDMVAR;                                              // make an mvar out of it
            type = TYPVARIND;                                                   // yes @...@ ... on addstk[]
            source_ptr++;
            goto subs;                                                          // go do subscripts
        } else if ((*source_ptr == '(') || (*source_ptr == '\0')) {
            source_ptr = sptr;                                                  // reset source_ptr
            comp_ptr = ptr;                                                     // reset comp_ptr
            c = *source_ptr++;                                                  // get a character
        } else {
            return -(ERRZ12 + ERRMLAST);                                        // else it's junk
        }
    }

    if (c == '^') {                                                             // if it's global
        type = TYPVARGBL;                                                       // say it's global
        c = *source_ptr++;                                                      // point past it
        v = (c == '|');                                                         // UCI/env specified

        if (v || (c == '[')) {                                                  // UCI/env specified
            type = TYPVARGBLUCI;                                                // we have a UCI specified
            atom();                                                             // eval the argument
            c = *source_ptr++;                                                  // get next

            if (!v && (c == ',')) {                                             // if it's a comma with square bracket syntax
                type = TYPVARGBLUCIENV;                                         // say we have vol as well
                atom();                                                         // eval the argument
                c = *source_ptr++;                                              // get next
            }

            if ((v && (c != '|')) || (!v && (c != ']'))) return -(ERRZ12 + ERRMLAST); // that's junk
            c = *source_ptr++;                                                  // get next
        } else if (c == '(') {                                                  // naked reference ?
            type = TYPVARNAKED;                                                 // set type
            source_ptr--;                                                       // back up source
            goto subs;
        }
    }

    // check for a variable and if none return the error
    if ((isalpha((int) c) == 0) && (c != '%') && (c != '$')) return -(ERRZ12 + ERRMLAST);

    if ((c == '$') && (type == TYPVARNAM)) {                                    // check $...
        if (isalpha(*source_ptr) == 0) return -(ERRZ12 + ERRMLAST);             // next must be alpha or return the error
        i = toupper(*source_ptr);                                               // get the next character

        /*
         * TODO: Add check for real intrinsic variables, not just their first letter
         *       $DEVICE, $ECODE, $ESTACK, $ETRAP, $HOROLOG, $IO, $JOB, $KEY, $PRINCIPAL,
         *       $QUIT, $REFERENCE, $STORAGE, $STACK, $SYSTEM, $TEST, $X, $Y, $ZBP (an array, not a function)
         *       cf. dodollar() in rsm/compile/dollar.c
        */
        if (strchr("DEHIJKPQRSTXYZ", i) == NULL) return -ERRM8;                 // if letter is invalid complain
    }

    VAR_CLEAR(var);                                                             // clear the variable name
    var.var_cu[0] = c;                                                          // save first char

    for (i = 1; i < VAR_LEN; i++) {                                             // scan for rest of name
        c = *source_ptr++;                                                      // get next char

        if (isalnum((int) c) == 0) {                                            // if not alpha numeric
            source_ptr--;                                                       // point back at it
            break;                                                              // and exit
        }

        var.var_cu[i] = c;                                                      // save in the variable
    }

    if (isalnum(*source_ptr) != 0) return -ERRM56;                              // complain about name length

subs:
    if (*source_ptr == '(') {                                                   // see if it's subscripted
        source_ptr++;                                                           // skip the bracket

        while (TRUE) {                                                          // loop
            eval();                                                             // get a subscript
            count++;                                                            // count it
            c = *source_ptr++;                                                  // get next character
            if (c == ')') break;                                                // quit when done
            if (c != ',') return -(ERRZ12 + ERRMLAST);                          // return the error
        }
    }

    if (count > TYPMAXSUB) return -(ERRZ15 + ERRMLAST);                         // too many then error
    ret = comp_ptr - ptr;                                                       // remember here
    *comp_ptr++ = OPVAR;                                                        // opcode

    // candidate for index? and in a routine compile and it's not $...
    if ((type < TYPVARGBL) && (partab.varlst != NULL) && (var.var_cu[0] != '$')) {
        for (i = 0; ; i++) {                                                    // scan list
            if (i == (MAX_KEY_SIZE + 1)) break;                                 // too many
            if (var_equal(partab.varlst[i], var)) break;                        // found it

            if (var_empty(partab.varlst[i])) {
                VAR_COPY(partab.varlst[i], var);                                // set it
                break;
            }
        }

        if (i != (MAX_KEY_SIZE + 1)) {
            type |= TYPVARIDX;                                                  // change the type
            idx = i;                                                            // save index
        }
    }

    if (type < TYPVARNAKED) {                                                   // normal local or global var
        type += count;                                                          // add the count
        *comp_ptr++ = (u_char) type;                                            // store it
        if (type & TYPVARIDX) *comp_ptr++ = idx;                                // index type? then save the index
    } else {                                                                    // funny type
        *comp_ptr++ = (u_char) type;                                            // store the type
        *comp_ptr++ = count;                                                    // then the subscripts
    }

    // if simple local (not idx) or a 'normal' global and not naked or indirect
    if (((type < TYPVARIDX) || (type >= TYPVARGBL)) && (type != TYPVARNAKED) && (type != TYPVARIND)) {
        for (i = 0; i < VAR_LEN; i++) *comp_ptr++ = var.var_cu[i];              // scan the name and copy into compiled code
    }

    return ret;                                                                 // say what we did
}                                                                               // end variable parse
