/*
 * Package: Reference Standard M
 * File:    rsm/util/memory.c
 * Summary: module util - memory subroutines
 *
 * David Wicksell <dlw@linux.com>
 * Copyright © 2020-2024 Fourth Watch Software LC
 * https://gitlab.com/Reference-Standard-M/rsm
 *
 * Based on MUMPS V1 by Raymond Douglas Newman
 * Copyright © 1999-2016
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

#include "compile.h"                                                            // for rbd def
#include "error.h"                                                              // standard errors
#include "proto.h"                                                              // standard prototypes
#include "symbol.h"                                                             // for NEW stuff
#include <ctype.h>                                                              // for isdigit
#include <limits.h>                                                             // for INT_MAX
#include <string.h>                                                             // for memmove

// This function is used in place of memmove() to trap strstk overflows
int mcopy(u_char *src, u_char *dst, int bytes)                                  // copy bytes
{
    // if dst is at or after strstk and before the end of strstk and this will overflow strstk
    if ((dst >= partab.strstk_start) && (dst < partab.strstk_last) && (&dst[bytes] > partab.strstk_last)) {
        return -(ERRZ8 + ERRMLAST);                                             // complain
    }

    if (bytes > MAX_STR_LEN) return -ERRM75;
    memmove(dst, src, bytes);                                                   // if OK - copy it
    dst[bytes] = '\0';                                                          // ensure null terminated
    return bytes;                                                               // and return bytes copied
}

/*
 * Convert string (**src) to canonic number at (*dst) returning length
 * The source pointer is left pointing at the terminating character
 */
short ncopy(u_char **src, u_char *dst)                                          // copy as number
{
    u_char c;                                                                   // the character
    u_char *p;                                                                  // a pointer
    int    i = 0;                                                               // a useful int
    int    k = 0;                                                               // and another
    int    dp = 0;                                                              // decimal place flag
    int    minus = FALSE;                                                       // minus flag
    long   exp = 0;                                                             // exponent
    int    expsgn = 1;                                                          // exponent sign

    // if dst is at or after strstk and before the end of strstk and this will overflow strstk
    if ((dst >= partab.strstk_start) && (dst < partab.strstk_last) && (&dst[MAX_NUM_BYTES] > partab.strstk_last)) {
        return -(ERRZ8 + ERRMLAST);                                             // complain
    }

    p = *src;                                                                   // get pointer

    while (TRUE) {                                                              // scan the string
        if (i > MAX_NUM_BYTES) return -ERRM92;                                  // if too big error
        c = *p++;                                                               // get the character

        if ((i == 0) && (k == 0)) {                                             // still on first char (no '0')
            if (c == '+') continue;                                             // check for a plus then go for more

            if (c == '-') {                                                     // check for a minus
                minus = !minus;                                                 // negate minus flag
                continue;                                                       // go for more
            }

            if (minus) dst[i++] = '-';                                          // store minus if required
        }

        if ((i == minus) && (c == '0')) {                                       // if '0' and nothing saved
            k = 1;                                                              // flag '0' seen
            continue;                                                           // and keep going
        }

        if (c == '.') {                                                         // check for a dot
            if (dp != 0) break;                                                 // quit if already have one
            dp = 1;                                                             // indicate 'got one'
            dst[i++] = c;                                                       // copy the character
            continue;
        }

        if ((systab->historic & HISTORIC_EOK) && (c == 'E')) {                  // bloody E stuff
            c = *p++;                                                           // get next

            if (c == '-') {                                                     // check minus
                expsgn = -1;                                                    // change sign
            } else if (isdigit(c)) {                                            // if digit
                exp = c - '0';                                                  // store value
            } else if (c != '+') {                                              // if not a plus
                break;                                                          // done
            }

            while (TRUE) {                                                      // scan exponent
                c = *p++;                                                       // get next
                if (isdigit(c) == 0) break;                                     // if not a digit break
                exp = (exp * 10) + (c - '0');                                   // add to exponent
                if (exp > INT_MAX) return -ERRM92;                              // if too big then error
            }

            break;                                                              // done
        }

        if (isdigit(c) == 0) break;                                             // if not a digit break
        dst[i++] = c;                                                           // copy the character
    }                                                                           // string now copied

    if (dp) {                                                                   // if there was a dot
        if ((i == 1) && !k) return -(ERRZ12 + ERRMLAST);                        // if just a dp, complain
        for (k = 0; dst[i - k - 1] == '0'; k++) {}                              // check for trailing zeroes
        i -= k;                                                                 // remove them (if any)
        if (dst[i - 1] == '.') i--;                                             // ensure last is not dot
    }

    if (i && (dst[i - 1] == '-')) i--;                                          // ensure last is not minus
    if (i == 0) dst[i++] = '0';                                                 // make sure we have something
    dst[i] = '\0';                                                              // null terminate it
    *src = --p;                                                                 // back up the source pointer and store it
    if (!exp) return (short) i;                                                 // if no exponent then return the count
    dst[i] = '0';                                                               // jic
    dp = 0;                                                                     // clear DP flag

    for (k = 0; k < i; k++) {                                                   // scan string again
        if (dst[k] == '.') {                                                    // if we found a dot
            dp = 1;                                                             // flag it
            break;                                                              // and exit
        }
    }

    if (expsgn > 0) {                                                           // if positive
        if (dp) {                                                               // if found a dot
            for (; k < i; k++) {                                                // scan to eos
                dst[k] = dst[k + 1];                                            // copy one char
                dst[k + 1] = '.';                                               // replace the dot
                exp--;                                                          // count one
                if (!exp) goto exit;                                            // if all done just exit
            }
        }

        if ((exp + i) > MAX_NUM_BYTES) return -ERRM92;                          // if too big then error

        while (exp > 0) {                                                       // while still need zeroes
            dst[i++] = '0';                                                     // copy a zero
            exp--;                                                              // count it
        }

        dst[i] = '\0';                                                          // null terminate
        goto exit;                                                              // and exit
    }                                                                           // end positive exponent

    if (!dp) {                                                                  // check for assumed dp
        k = i;                                                                  // put it here
        i++;                                                                    // and incr length
    }

    if (k > 0) {                                                                // some to the left of dp
        for (dp = k; dp > minus; dp--) {                                        // scan back
            dst[dp] = dst[dp - 1];                                              // copy the number
            dst[dp - 1] = '.';                                                  // replace the dot
            exp--;                                                              // count one
            if (!exp) goto exit;                                                // if all done exit
        }
    }

    if ((exp + i) > MAX_NUM_BYTES) return -ERRM92;                              // if too big error
    memmove(&dst[minus + exp + 1], &dst[minus + 1], i);                         // move right exp places
    for (k = minus + 1; k <= (minus + exp); dst[k++] = '0') {}                  // zero fill
    i += exp;                                                                   // add to the length

exit:
    dp = 0;                                                                     // assume no dp

    for (k = 0; k < i; k++) {                                                   // scan string
        if (dst[k] == '.') {                                                    // if a dp
            dp = 1;                                                             // flag it
            break;                                                              // and quit
        }
    }

    if (dp) {                                                                   // if there is a dp
        while (dst[i - 1] == '0') i--;                                          // for all trailing 0 then ignore them
        if (dst[i - 1] == '.') i--;                                             // if a trailing dp then ignore them
    }

    if (!i) dst[i++] = '0';                                                     // if no char then put a zero back
    dst[i] = '\0';                                                              // ensure null terminated
    dp = (dst[0] == '-');                                                       // start point

    if (dst[dp] == '0') {                                                       // if leading zeroes
        for (k = dp; (k < i) && (dst[k] == '0'); k++) {}                        // find first non-zero
        memmove(&dst[dp], &dst[k], i - k);                                      // copy down
        i -= (k - dp);                                                          // adjust size

        if (i == dp) {                                                          // if nothing
            if (dp) i--;                                                        // if a minus then ignore it
            dst[i++] = '0';                                                     // store a zero
        }

        dst[i] = '\0';                                                          // null terminate
    }

    return (short) i;                                                           // exit
}

/*
 * CleanJob is called to release all locks and unwind any routine attaches
 * It is called with zero (current job) or the job# (i.e., internal job+1)
 * If not the current job, also free jobtab entry.
 */
void CleanJob(int job)                                                          // tidy up a job
{
    int j;                                                                      // the job number
    int i;                                                                      // a handy int

    j = job - 1;                                                                // copy argument to int job form
    if (!job) j = partab.jobtab - partab.job_table;                             // or get current int job#
    LCK_Remove(j + 1);                                                          // remove locks
    i = partab.job_table[j].cur_do;                                             // get current do

    while (i) {                                                                 // for each i
        if (!job) {                                                             // called by ourselves ?
            if (partab.job_table[j].dostk[i].newtab != NULL) {
                ST_Restore((ST_newtab *) partab.job_table[j].dostk[i].newtab);
            }

            // detach symbols
            if ((partab.job_table[j].dostk[i].flags & DO_FLAG_ATT) && (partab.job_table[j].dostk[i].symbol != NULL)) {
                ST_SymDet(((rbd *) SOA(partab.job_table[j].dostk[i].routine))->num_vars, partab.job_table[j].dostk[i].symbol);
            }
        }

        if (partab.job_table[j].dostk[i].flags & DO_FLAG_ATT) {                 // if we attached
            Routine_Detach((rbd *) SOA(partab.job_table[j].dostk[i].routine));  // detach routine
        }

        i--;                                                                    // decrement do ptr
    }                                                                           // end routine detach while

    if (!job) {                                                                 // called by ourselves ?
        ST_KillAll(0, NULL);                                                    // kill all vars
        partab.src_var.volset = 0;                                              // clear vol
        partab.src_var.slen = 0;                                                // and slen
        partab.src_var.uci = UCI_IS_LOCALVAR;                                   // say - local variable
        VAR_CLEAR(partab.src_var.name);
        memcpy(partab.src_var.name.var_cu, "$ETRAP", 6);
        ST_Kill(&partab.src_var);                                               // Kill $ETRAP
        VAR_CLEAR(partab.src_var.name);
        memcpy(partab.src_var.name.var_cu, "$ECODE", 6);
        ST_Kill(&partab.src_var);                                               // Kill $ECODE
    }

    for (i = 0; i < MAX_VOL; i++) {                                             // scan view table
        if (partab.job_table[j].view[i] != NULL) {                              // if buffer locked
            DB_ViewRel(i + 1, SOA(partab.job_table[j].view[i]));                // release it
            partab.job_table[j].view[i] = NULL;                                 // clear entry
        }
    }

    partab.job_table[j].cur_do = 0;                                             // in case we get back here

    if (!job) {                                                                 // if current job
        for (i = 1; i < MAX_SEQ_IO; SQ_Close(i++)) {}                           // close all io
        partab.jobtab = NULL;                                                   // clear jobtab
    }

    memset(&partab.job_table[j], 0, sizeof(jobtab));                            // zot all
    return;                                                                     // and exit
}
