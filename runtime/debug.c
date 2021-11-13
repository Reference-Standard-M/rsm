/*
 * Package:  Reference Standard M
 * File:     rsm/runtime/debug.c
 * Summary:  module runtime - debug
 *
 * David Wicksell <dlw@linux.com>
 * Copyright © 2020-2021 Fourth Watch Software LC
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
#include <strings.h>
#include <ctype.h>
#include <errno.h>                                                              // error stuff
#include "rsm.h"                                                                // standard includes
#include "proto.h"                                                              // standard prototypes
#include "error.h"                                                              // standard errors
#include "opcode.h"                                                             // the op codes
#include "compile.h"                                                            // for XECUTE

mvar   dvar;                                                                    // an mvar for debugging
u_char src[1024];                                                               // some space for entered
u_char cmp[1024];                                                               // ditto compiled
u_char *debug;                                                                  // a pointer to compiled code

extern char    history[MAX_HISTORY][MAX_STR_LEN];                               // history buffer
extern u_short hist_next;                                                       // next history pointer
extern u_short hist_curr;                                                       // history entry pointer
extern short   in_hist;                                                         // are we in the history buffer

void Debug_off(void)                                                            // turn off debugging
{
    VAR_CLEAR(dvar.name);
    bcopy("$ZBP", &dvar.name.var_cu[0], 4);
    dvar.volset = 0;                                                            // clear volume
    dvar.uci = UCI_IS_LOCALVAR;                                                 // local variable
    dvar.slen = 0;                                                              // no subscripts
    (void) ST_Kill(&dvar);                                                      // dong it
    partab.debug = 0;                                                           // turn off flag
    return;                                                                     // done
}

/*
 * Turn on (or modify) debug stuff
 * We are passed one cstring containing:
 *     Debug_ref         Remove breakpoint
 *     Debug_ref:        Add simple breakpoint
 *     Debug_ref:code    Add breakpoint with code to Xecute
 *     :code             Code to execute at QUIT n breakpoint
 */
short Debug_on(cstring *param)                                                  // turn on/modify debug
{
    int     i = 0;                                                              // a handy int
    int     j = 0;                                                              // and another
    int     s;
    int     off = 1;                                                            // line offset
    cstring *ptr;                                                               // string pointer

    debug = NULL;                                                               // clear auto debug
    VAR_CLEAR(dvar.name);
    bcopy("$ZBP", &dvar.name.var_cu[0], 4);
    dvar.volset = 0;                                                            // clear volume
    dvar.uci = UCI_IS_LOCALVAR;                                                 // local variable
    dvar.slen = 0;                                                              // assume no key - aka :code
    dvar.key[0] = 128;                                                          // setup for string key

    if (param->buf[0] != ':') {                                                 // If not a : first
        if (param->buf[i] == '+') {                                             // offset?
            i++;                                                                // point past it
            off = 0;                                                            // clear offset

            while (isdigit(param->buf[i])) {                                    // for the digits
                off = (off * 10) + (param->buf[i++] - '0');                     // convert
            }
        }

        if (param->buf[i++] != '^') return -(ERRZ9 + ERRMLAST);                 // we don't like it

        for (j = 0; j < VAR_LEN; j++) {                                         // copy the routine name
            if ((isalnum(param->buf[j + i]) == 0) && ((param->buf[j + i] != '%') || (j != 0))) {
                break;                                                          // done
            }

            dvar.key[j + 1] = param->buf[j + i];                                // copy it
        }

        dvar.key[j + 1] = '\0';                                                 // null terminate it
        dvar.slen = j + 2;                                                      // save the length
        j = j + i;                                                              // point to next char
        if (isalnum(param->buf[j])) return -ERRM56;                             // complain about long names

        if ((param->buf[j] != ':') && (param->buf[j] != '\0')) {                // not : AND not eol
            return -(ERRZ9 + ERRMLAST);                                         // we don't like it
        }

        dvar.key[dvar.slen++] = 64;                                             // new key
        s = itocstring(&dvar.key[dvar.slen], off);                              // copy offset
        dvar.key[dvar.slen - 1] |= s;                                           // fixup type byte
        dvar.slen += s;                                                         // and count
        dvar.slen++;                                                            // count the null
    }                                                                           // end of +off^rou code

    if (param->buf[j++] == '\0') return ST_Kill(&dvar);                         // end of string? then dong and exit
    partab.debug = -1;                                                          // turn on debug

    if (param->buf[j] == '\0') {                                                // end of string?
        ptr = (cstring *) src;                                                  // make a cstring
DISABLE_WARN(-Warray-bounds)
        ptr->len = 0;                                                           // length
        ptr->buf[0] = '\0';                                                     // null terminated
        return ST_Set(&dvar, ptr);                                              // set and return
    }

    if ((param->len - j) > 255) return -(ERRZ9 + ERRMLAST);                     // too bloody long
    source_ptr = &param->buf[j];                                                // point at the source
    ptr = (cstring *) cmp;                                                      // where it goes
    comp_ptr = ptr->buf;                                                        // for parse
    parse();                                                                    // compile it
    *comp_ptr++ = ENDLIN;                                                       // eol
    *comp_ptr++ = ENDLIN;                                                       // eor
    ptr->len = (comp_ptr - ptr->buf);                                           // save the length
ENABLE_WARN
    return ST_Set(&dvar, ptr);                                                  // set and return
}

/*
 * dot = -1 for the return from a QUIT n
 *        0 to check to see if we need to break
 *        1 from a BREAK sp sp
 */
short Debug(int savasp, int savssp, int dot)                                    // drop into debug
{
    int      i;                                                                 // a handy int
    int      io;                                                                // save current $IO
    int      s = 0;                                                             // for calls
    short    ts;                                                                // for temp index
    do_frame *curframe;                                                         // a do frame pointer
    cstring  *ptr;                                                              // a string pointer
    mvar     *var;                                                              // and an mvar ptr

    if (!partab.debug) partab.debug = -1;                                       // ensure it's on
    VAR_CLEAR(dvar.name);
    bcopy("$ZBP", &dvar.name.var_cu[0], 4);
    dvar.volset = 0;                                                            // clear volume
    dvar.uci = UCI_IS_LOCALVAR;                                                 // local variable
    dvar.slen = 0;                                                              // no key
    curframe = &partab.jobtab->dostk[partab.jobtab->cur_do];                    // point at it

    if (dot == 0) {                                                             // a check type, setup mvar
        if ((curframe->type != TYPE_DO) && (curframe->type != TYPE_EXTRINSIC)) {
            return 0;                                                           // ensure we have a routine
        }

        dvar.key[0] = 128;                                                      // setup for string key

        for (i = 0; i < VAR_LEN; i++) {
            if (!(dvar.key[i + 1] = curframe->rounam.var_cu[i])) break;
        }

        dvar.slen = i + 1;                                                      // the length so far
        dvar.key[dvar.slen++] = '\0';                                           // null terminate it
        dvar.key[dvar.slen++] = 64;                                             // next key
        s = itocstring(&dvar.key[dvar.slen], curframe->line_num);               // setup second key
        dvar.key[dvar.slen - 1] |= s;                                           // fix type
        dvar.slen += s;                                                         // and length
        dvar.slen++;                                                            // count the null
        s = ST_Get(&dvar, &cmp[sizeof(short)]);                                 // get whatever
        if (s < 0) return 0;                                                    // just return if nothing
        ts = (short) s;                                                         // endian agnostic
        memcpy(cmp, &ts, sizeof(short));                                        // save the length
    }

    if (dot == -1) {                                                            // from a QUIT n
        s = ST_Get(&dvar, &cmp[sizeof(short)]);                                 // get whatever
        if (s < 0) s = 0;                                                       // ignore errors
        ts = (short) s;                                                         // endian agnostic
        memcpy(cmp, &ts, sizeof(short));                                        // save the length
    }

    if (partab.jobtab->cur_do >= MAX_DO_FRAMES) return -(ERRZ8 + ERRMLAST);     // too many (perhaps ??????)
    partab.jobtab->dostk[partab.jobtab->cur_do].pc = rsmpc;                     // save current

    if (s > 0) {                                                                // code to execute
        partab.jobtab->cur_do++;                                                // increment do frame
        rsmpc = &cmp[sizeof(short)];                                            // where it is
        src[0] = '\0';                                                          // a spare null
        partab.jobtab->dostk[partab.jobtab->cur_do].routine = src;
        partab.jobtab->dostk[partab.jobtab->cur_do].pc = rsmpc;
        partab.jobtab->dostk[partab.jobtab->cur_do].symbol = NULL;
        partab.jobtab->dostk[partab.jobtab->cur_do].newtab = NULL;
        memcpy(&ts, &cmp, sizeof(short));
        partab.jobtab->dostk[partab.jobtab->cur_do].endlin = &cmp[ts - 3 + sizeof(short)];
        VAR_CLEAR(partab.jobtab->dostk[partab.jobtab->cur_do].rounam);
        partab.jobtab->dostk[partab.jobtab->cur_do].vol = partab.jobtab->vol;
        partab.jobtab->dostk[partab.jobtab->cur_do].uci = partab.jobtab->uci;
        partab.jobtab->dostk[partab.jobtab->cur_do].line_num = 0;
        partab.jobtab->dostk[partab.jobtab->cur_do].type = TYPE_RUN;
        partab.jobtab->dostk[partab.jobtab->cur_do].level = 0;
        partab.jobtab->dostk[partab.jobtab->cur_do].flags = 0;
        partab.jobtab->dostk[partab.jobtab->cur_do].isp = isp;
        partab.jobtab->dostk[partab.jobtab->cur_do].asp = savasp;
        partab.jobtab->dostk[partab.jobtab->cur_do].ssp = savssp;
        partab.jobtab->dostk[partab.jobtab->cur_do].savasp = savasp;
        partab.jobtab->dostk[partab.jobtab->cur_do].savssp = savssp;
        s = run(savasp, savssp);                                                // do it
        if (s == OPHALT) return (short) s;                                      // just halt if reqd
        --partab.jobtab->cur_do;                                                // restore do frame
        rsmpc = partab.jobtab->dostk[partab.jobtab->cur_do].pc;                 // restore pc

        if (s & BREAK_QN) {
            s = s & ~BREAK_QN;                                                  // clear the bit

            if (s > 0) {
                partab.debug = s + partab.jobtab->commands;                     // when to stop
                partab.jobtab->attention = 1;                                   // say to check this thing
                s = 0;                                                          // don't confuse the return
            }
        }

        return (short) s;                                                       // return whatever
    }

    io = partab.jobtab->io;                                                     // save current $IO
    debug = NULL;                                                               // clear code ptr
    partab.jobtab->io = 0;                                                      // ensure 0
    partab.jobtab->seqio[0].options |= 8;                                       // ensure echo on
    ptr = (cstring *) src;                                                      // some space

    while (TRUE) {                                                              // see what they want
        if (in_hist == FALSE) {
            if (partab.jobtab->seqio[0].dx) s = SQ_WriteFormat(SQ_LF);          // need a CRLF then do it

            if (var_empty(curframe->rounam)) {
                bcopy("Debug", ptr->buf, 5);
                ptr->len = 5;
                partab.debug = -1;                                              // reset debug state
            } else {
                ptr->len = 0;                                                   // clear ptr
                ptr->buf[ptr->len++] = '+';                                     // lead off
                ptr->len = (u_short) itocstring(&ptr->buf[ptr->len], curframe->line_num) + ptr->len; // setup line number
                ptr->buf[ptr->len++] = '^';                                     // lead off routine

                for (i = 0; i < VAR_LEN; i++) {
                    if (!(ptr->buf[i + ptr->len] = curframe->rounam.var_cu[i])) {
                        break;                                                  // copy rou name
                    }
                }

                ptr->len += i;                                                  // save length
            }

            ptr->buf[ptr->len++] = '>';                                         // and that bit
            ptr->buf[ptr->len++] = ' ';                                         // and that bit
            ptr->buf[ptr->len] = '\0';                                          // null terminate
            s = SQ_Write(ptr);                                                  // write it
        }

        s = SQ_Read(ptr->buf, -1, 256);                                         // read something
        if (s < 1) continue;                                                    // ignore nulls and errors
        strcpy(history[hist_next], (char *) ptr->buf);

        if (hist_next == (MAX_HISTORY - 1)) {
            hist_next = 0;
        } else {
            hist_next++;
        }

        hist_curr = hist_next;
        s = SQ_WriteFormat(SQ_LF);                                              // return
        source_ptr = ptr->buf;                                                  // point at source
        comp_ptr = cmp;                                                         // and where it goes
        parse();                                                                // compile it
        *comp_ptr++ = ENDLIN;                                                   // JIC
        *comp_ptr++ = ENDLIN;                                                   // JIC
        partab.jobtab->cur_do++;                                                // increment do frame
        rsmpc = cmp;                                                            // where it is
        src[0] = '\0';                                                          // a spare null
        partab.jobtab->dostk[partab.jobtab->cur_do].routine = src;
        partab.jobtab->dostk[partab.jobtab->cur_do].pc = rsmpc;
        partab.jobtab->dostk[partab.jobtab->cur_do].symbol = NULL;
        partab.jobtab->dostk[partab.jobtab->cur_do].newtab = NULL;
        partab.jobtab->dostk[partab.jobtab->cur_do].endlin = comp_ptr - 3;
        VAR_CLEAR(partab.jobtab->dostk[partab.jobtab->cur_do].rounam);
        partab.jobtab->dostk[partab.jobtab->cur_do].vol = partab.jobtab->vol;
        partab.jobtab->dostk[partab.jobtab->cur_do].uci = partab.jobtab->uci;
        partab.jobtab->dostk[partab.jobtab->cur_do].line_num = 0;
        partab.jobtab->dostk[partab.jobtab->cur_do].type = TYPE_RUN;
        partab.jobtab->dostk[partab.jobtab->cur_do].level = 0;
        partab.jobtab->dostk[partab.jobtab->cur_do].flags = 0;
        partab.jobtab->dostk[partab.jobtab->cur_do].savasp = savasp;
        partab.jobtab->dostk[partab.jobtab->cur_do].savssp = savssp;
        partab.jobtab->dostk[partab.jobtab->cur_do].asp = savasp;
        partab.jobtab->dostk[partab.jobtab->cur_do].ssp = savssp;
        partab.jobtab->dostk[partab.jobtab->cur_do].isp = isp;
        s = run(savasp, savssp);                                                // do it
        if (s == OPHALT) return (short) s;                                      // just halt if reqd
        --partab.jobtab->cur_do;                                                // restore do frame
        if (!partab.debug) break;                                               // go away if debug now off

        if (s & BREAK_QN) {
            s = s - BREAK_QN;                                                   // clear the bit

            if (s > 0) {
                partab.debug = s + partab.jobtab->commands;                     // when to stop
                partab.jobtab->attention = 1;                                   // say to check this thing
                s = 0;
                break;                                                          // exit
            }
        }

        if (s == CMQUIT) break;                                                 // exit on QUIT

        if (s == OPHALT) {
            partab.jobtab->io = io;                                             // restore io
            rsmpc = partab.jobtab->dostk[partab.jobtab->cur_do].pc;             // restore pc
            return (short) s;
        }

        var = (mvar *) &strstk[savssp];                                         // space to setup a var
        VAR_CLEAR(var->name);
        bcopy("$ECODE", &var->name.var_cu[0], 6);
        var->volset = 0;
        var->uci = UCI_IS_LOCALVAR;
        var->slen = 0;                                                          // setup for $EC
        ptr = (cstring *) (&strstk[savssp] + sizeof(mvar));                     // for result
        bcopy("$ECODE=", ptr->buf, 7);
        s = ST_Get(var, &ptr->buf[7]);
        if (s < 1) continue;                                                    // ignore if nothing there
        ptr->len = s + 7;
        if (partab.jobtab->seqio[0].dx) s = SQ_WriteFormat(SQ_LF);              // need a CRLF then do it
        s = SQ_Write(ptr);                                                      // write the prompt
        s = SQ_WriteFormat(SQ_LF);                                              // new line
    }

    partab.jobtab->io = io;                                                     // restore io
    rsmpc = partab.jobtab->dostk[partab.jobtab->cur_do].pc;                     // restore pc
    return 0;
}
