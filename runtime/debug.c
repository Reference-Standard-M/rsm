/*
 * Package: Reference Standard M
 * File:    rsm/runtime/debug.c
 * Summary: module runtime - debug
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

#include "compile.h"                                                            // for XECUTE
#include "error.h"                                                              // standard errors
#include "opcode.h"                                                             // the op codes
#include "proto.h"                                                              // standard prototypes
#include <ctype.h>
#include <string.h>

mvar   dvar;                                                                    // an mvar for debugging
u_char src[256];                                                                // some space for entered
u_char cmp[2048];                                                               // ditto compiled

extern char    history[MAX_HISTORY][MAX_STR_LEN];                               // history buffer
extern u_short hist_next;                                                       // next history pointer
extern u_short hist_curr;                                                       // history entry pointer
extern u_short prompt_len;                                                      // length of the current direct mode prompt

void Debug_off(void)                                                            // turn off debugging
{
    VAR_CLEAR(dvar.name);
    memcpy(&dvar.name.var_cu[0], "$ZBP", 4);
    dvar.volset = 0;                                                            // clear volume
    dvar.uci = UCI_IS_LOCALVAR;                                                 // local variable
    dvar.slen = 0;                                                              // no subscripts
    ST_Kill(&dvar);                                                             // dong it
    partab.debug = BREAK_OFF;                                                   // turn off flag
    return;                                                                     // done
}

/*
 * Turn on (or modify) debug stuff
 * We are passed one cstring containing:
 *     Debug_ref:        Add simple breakpoint
 *     Debug_ref         Remove breakpoint
 *     Debug_ref:code    Add breakpoint with code to Xecute
 *     :code             Code to execute at QUIT n breakpoint
 *     :                 Remove QUIT n breakpoint
 *     ""                Remove all breakpoints and turn off debugging
 */
short Debug_on(cstring *param)                                                  // turn on/modify debug
{
    int     j = 0;                                                              // a handy int
    cstring *ptr;                                                               // string pointer
    u_char  temp_src[256];                                                      // some space for entered

    VAR_CLEAR(dvar.name);
    memcpy(&dvar.name.var_cu[0], "$ZBP", 4);
    dvar.volset = 0;                                                            // clear volume
    dvar.uci = UCI_IS_LOCALVAR;                                                 // local variable
    dvar.slen = 0;                                                              // assume no key - aka :code
    dvar.key[0] = 128;                                                          // setup for string key

    if (param->buf[0] != ':') {                                                 // If not a : first
        int     i = 0;                                                          // a handy int
        int     t;
        int     off = 1;                                                        // line offset
        u_char  tag[VAR_LEN + 4] = {0};                                         // the tag
        u_char  rou[VAR_LEN + 4] = {0};                                         // the routine
        cstring *ct;                                                            // and the tag
        cstring *cr;                                                            // and the routine

        ct = (cstring *) &tag[0];                                               // use it this way
        cr = (cstring *) &rou[0];                                               // ditto
DISABLE_WARN(-Warray-bounds)
        ct->len = 0;                                                            // assume no tag
        cr->len = 0;                                                            // no routine for now
ENABLE_WARN

        if ((param->buf[i] != '+') && (param->buf[i] != '^')) {                 // is there a tag?
            while (j < VAR_LEN) {
                if ((i == 0) && (param->buf[i] == '%')) {                       // leading %
DISABLE_WARN(-Warray-bounds)
                    ct->buf[j++] = param->buf[i++];                             // copy it
                    continue;                                                   // and go for more
                }

                if (isalnum(param->buf[i]) == 0) break;                         // done
                ct->buf[j++] = param->buf[i++];                                 // copy it
            }

            ct->buf[j] = '\0';                                                  // null terminate tag
            ct->len = j;                                                        // save the length
ENABLE_WARN
            off = 0;                                                            // change offset to zero
        }

        if ((param->buf[i] != '+') && (param->buf[i] != '^')) return -(ERRZ9 + ERRMLAST); // we don't like it

        if (param->buf[i] == '+') {                                             // offset?
            off = 0;                                                            // clear offset
            i++;                                                                // point past it
            while (isdigit(param->buf[i])) off = (off * 10) + (param->buf[i++] - '0'); // convert the digits
        }

        if (param->buf[i++] != '^') return -(ERRZ9 + ERRMLAST);                 // we don't like it

        for (j = 0; j < VAR_LEN; j++) {                                         // copy the routine name
            if ((isalnum(param->buf[j + i]) == 0) && ((param->buf[j + i] != '%') || (j != 0))) {
                break;                                                          // done
            }

DISABLE_WARN(-Warray-bounds)
            cr->buf[j] = param->buf[j + i];                                     // copy it for tag resolution
            dvar.key[j + 1] = param->buf[j + i];                                // copy it
        }

        cr->buf[j] = '\0';                                                      // null terminate it
        cr->len = j;                                                            // save the length
ENABLE_WARN
        dvar.key[j + 1] = '\0';                                                 // null terminate it
        dvar.slen = j + 2;                                                      // save the length
        j += i;                                                                 // point to next char
        if (isalnum(param->buf[j])) return -ERRM56;                             // complain about long names

        if ((param->buf[j] != ':') && (param->buf[j] != '\0')) {                // not : AND not eol
            return -(ERRZ9 + ERRMLAST);                                         // we don't like it
        }

DISABLE_WARN(-Warray-bounds)
        if (ct->len) {
ENABLE_WARN
            var_u   lbl = {0};
            tags    *ttbl;
            u_char  *pc;
            u_short us = 0;
            u_short len;
            u_short src_ttbl;
            u_short src_num;
            u_short src_code;

DISABLE_WARN(-Warray-bounds)
            if (cr->len == 0) return -(ERRZ9 + ERRMLAST);                       // have to have a routine
ENABLE_WARN
            VAR_CLEAR(partab.src_var.name);
            memcpy(&partab.src_var.name.var_cu[0], "$ROUTINE", 8);              // setup for DB_Get
            partab.src_var.volset = partab.jobtab->rvol;                        // volume
            partab.src_var.uci = partab.jobtab->ruci;                           // UCI
DISABLE_WARN(-Warray-bounds)
            if (cr->buf[0] == '%') partab.src_var.uci = 1;                      // manager routine? then point there
ENABLE_WARN
            partab.src_var.slen = 0;                                            // init key size
            t = UTIL_Key_Build(cr, &partab.src_var.key[0]);                     // first key
            if (t < 0) return t;                                                // die on error
            len = t;
DISABLE_WARN(-Warray-bounds)
            cr->len = ltocstring(cr->buf, 0);
ENABLE_WARN
            t = UTIL_Key_Build(cr, &partab.src_var.key[len]);                   // next key
            if (t < 0) return t;                                                // die on error
            partab.src_var.slen = t + len;                                      // save key size
            t = DB_Get(&partab.src_var, partab.src_ln);                         // get it
            if (t < 0) return -(ERRZ9 + ERRMLAST);                              // we don't like it
DISABLE_WARN(-Warray-bounds)
            memcpy(&lbl.var_cu[0], &ct->buf[0], VAR_LEN);
ENABLE_WARN
            memcpy(&src_ttbl, &partab.src_ln[12], sizeof(u_short));             // partab.src_ln[12] == rbd->tag_tbl
            memcpy(&src_num, &partab.src_ln[14], sizeof(u_short));              // partab.src_ln[14] == rbd->num_tags
            memcpy(&src_code, &partab.src_ln[20], sizeof(u_short));             // partab.src_ln[20] == rbd->code
            ttbl = (tags *) &partab.src_ln[src_ttbl];

            for (int k = 0; k < src_num; k++) {
                if (var_equal(lbl, ttbl[k].name)) {
                    pc = &partab.src_ln[src_code] + ttbl[k].code;
                    while (*pc != LINENUM) pc++;
                    memcpy(&us, ++pc, sizeof(u_short));                         // get the linenum
                    off += us;
                    break;
                }
            }
        }

        if ((off < 1) || (off > MAXROULINE)) return -(ERRZ9 + ERRMLAST);        // we don't like it
        dvar.key[dvar.slen++] = 64;                                             // new key
        t = ltocstring(&dvar.key[dvar.slen], off);                              // copy offset
        dvar.key[dvar.slen - 1] |= t;                                           // fixup type byte
        dvar.slen += t;                                                         // and count
        dvar.slen++;                                                            // count the null
    }                                                                           // end of +off^rou code

    if (param->buf[j++] == '\0') return ST_Kill(&dvar);                         // end of string? then dong and exit
    if (partab.debug == BREAK_OFF) partab.debug = BREAK_ON;                     // ensure it's on
    ptr = (cstring *) temp_src;                                                 // make a cstring

    if (param->buf[j] == '\0') {                                                // end of string?
DISABLE_WARN(-Warray-bounds)
        ptr->len = 0;                                                           // length
        ptr->buf[0] = '\0';                                                     // null terminated
ENABLE_WARN
        return ST_Set(&dvar, ptr);                                              // set and return
    }

    if ((param->len - j) > 255) return -(ERRZ9 + ERRMLAST);                     // too bloody long
    source_ptr = &param->buf[j];                                                // point at the source
DISABLE_WARN(-Warray-bounds)
    ptr->len = param->len - j;                                                  // save the length
ENABLE_WARN
    memcpy(&ptr->buf[0], source_ptr, ptr->len);                                 // store source for handler
    return ST_Set(&dvar, ptr);                                                  // set and return
}

/*
 * dot = -1 for the return from a QUIT n
 *        0 to check to see if we need to break
 *        1 from a BREAK sp sp
 */
short Debug(int savasp, int savssp, int dot)                                    // drop into debug
{
    int         i;                                                              // a handy int
    int         io;                                                             // save current $IO
    u_char      options;                                                        // save current $IO options
    int         t = 0;                                                          // for calls
    short       s;                                                              // for temp index
    do_frame    *curframe;                                                      // a do frame pointer
    cstring     *ptr;                                                           // a string pointer
    mvar        *var;                                                           // and an mvar ptr
    static char debug = FALSE;                                                  // debug frame flag

    if (debug) return 0;                                                        // prevent recursive BREAK frames
    if (partab.debug == BREAK_OFF) partab.debug = BREAK_ON;                     // ensure it's on
    VAR_CLEAR(dvar.name);
    memcpy(&dvar.name.var_cu[0], "$ZBP", 4);
    dvar.volset = 0;                                                            // clear volume
    dvar.uci = UCI_IS_LOCALVAR;                                                 // local variable
    dvar.slen = 0;                                                              // no key
    curframe = &partab.jobtab->dostk[partab.jobtab->cur_do];                    // point at it

    if (dot == 0) {                                                             // a check type, setup mvar
        if (var_empty(curframe->rounam)) return 0;                              // ensure we have a routine
        dvar.key[0] = 128;                                                      // setup for string key

        for (i = 0; i < VAR_LEN; i++) {
            if (!(dvar.key[i + 1] = curframe->rounam.var_cu[i])) break;
        }

        dvar.slen = i + 1;                                                      // the length so far
        dvar.key[dvar.slen++] = '\0';                                           // null terminate it
        dvar.key[dvar.slen++] = 64;                                             // next key
        t = ultocstring(&dvar.key[dvar.slen], curframe->line_num);              // setup second key
        dvar.key[dvar.slen - 1] |= t;                                           // fix type
        dvar.slen += t;                                                         // and length
        dvar.slen++;                                                            // count the null
        t = ST_Get(&dvar, &src[sizeof(u_short)]);                               // get whatever
        if (t < 0) return 0;                                                    // just return if nothing
        s = (short) t;                                                          // endian agnostic
        memcpy(src, &s, sizeof(short));                                         // save the length
    } else if (dot == -1) {                                                     // from a QUIT n
        t = ST_Get(&dvar, &src[sizeof(u_short)]);                               // get whatever
        if (t < 0) t = 0;                                                       // ignore errors
        s = (short) t;                                                          // endian agnostic
        memcpy(src, &s, sizeof(short));                                         // save the length
    }

    if ((partab.jobtab->cur_do + 1) == MAX_DO_FRAMES) return -(ERRZ7 + ERRMLAST); // too many
    partab.jobtab->dostk[partab.jobtab->cur_do].pc = rsmpc;                     // save current

    if (t > 0) {                                                                // code to execute
        source_ptr = &src[sizeof(u_short)];                                     // point at the source
        ptr = (cstring *) cmp;                                                  // where it goes
        comp_ptr = ptr->buf;                                                    // for parse
        parse();                                                                // compile it
        *comp_ptr++ = ENDLIN;                                                   // eol
        *comp_ptr++ = ENDLIN;                                                   // eor
        rsmpc = &cmp[sizeof(u_short)];                                          // where it is
        partab.jobtab->cur_do++;                                                // increment do frame
        partab.jobtab->dostk[partab.jobtab->cur_do].routine = SBA(&src[sizeof(u_short)]);
        partab.jobtab->dostk[partab.jobtab->cur_do].pc = rsmpc;
        partab.jobtab->dostk[partab.jobtab->cur_do].symbol = NULL;
        partab.jobtab->dostk[partab.jobtab->cur_do].newtab = NULL;
        memcpy(&s, &cmp, sizeof(short));
        partab.jobtab->dostk[partab.jobtab->cur_do].endlin = &cmp[s - 3 + sizeof(short)];
        VAR_CLEAR(partab.jobtab->dostk[partab.jobtab->cur_do].rounam);
        partab.jobtab->dostk[partab.jobtab->cur_do].vol = partab.jobtab->vol;
        partab.jobtab->dostk[partab.jobtab->cur_do].uci = partab.jobtab->uci;
        partab.jobtab->dostk[partab.jobtab->cur_do].line_num = 0;
        partab.jobtab->dostk[partab.jobtab->cur_do].type = TYPE_RUN;
        partab.jobtab->dostk[partab.jobtab->cur_do].level = 0;
        partab.jobtab->dostk[partab.jobtab->cur_do].flags = 0;
        partab.jobtab->dostk[partab.jobtab->cur_do].asp = savasp;
        partab.jobtab->dostk[partab.jobtab->cur_do].ssp = savssp;
        partab.jobtab->dostk[partab.jobtab->cur_do].isp = isp;
        partab.jobtab->dostk[partab.jobtab->cur_do].savasp = savasp;
        partab.jobtab->dostk[partab.jobtab->cur_do].savssp = savssp;
        debug = TRUE;                                                           // prevent recursive BREAK frames
        t = run(savasp, savssp);                                                // do it
        debug = FALSE;
        if (t == OPHALT) return (short) t;                                      // just halt if required
        partab.jobtab->cur_do--;                                                // restore do frame
        rsmpc = partab.jobtab->dostk[partab.jobtab->cur_do].pc;                 // restore pc
        if ((partab.debug == BREAK_OFF) || (partab.debug == BREAK_DISABLE)) return 0; // return if debug now off

        if (t & BREAK_QN) {
            t &= ~BREAK_QN;                                                     // clear the bit

            if (t > 0) {
                partab.debug = t + partab.jobtab->commands;                     // when to stop
                partab.jobtab->attention = 1;                                   // say to check this thing
                t = 0;                                                          // don't confuse the CMQUIT test
            }
        }

        if (t == CMQUIT) {
            if (dot == 1) {                                                     // reset debug state
                partab.debug = BREAK_OFF;
            } else {
                partab.debug = BREAK_ON;
            }

            return 0;                                                           // return from breakpoint handler
        }
    }

    io = partab.jobtab->io;                                                     // save current $IO
    options = partab.jobtab->seqio[0].options;                                  // save channel 0 options
    partab.jobtab->io = 0;                                                      // ensure 0
    partab.jobtab->seqio[0].options |= 8;                                       // ensure echo on
    ptr = (cstring *) src;                                                      // some space

    while (TRUE) {                                                              // see what they want
        if (partab.jobtab->seqio[0].dx) {
            t = SQ_WriteFormat(SQ_LF);                                          // need a CRLF then do it
            if (t < 0) return (short) t;                                        // if error, return it
        }

        if (var_empty(curframe->rounam)) {
            memcpy(ptr->buf, "Debug", 5);
            ptr->len = 5;
        } else {
            ptr->len = 0;                                                       // clear ptr
            ptr->buf[ptr->len++] = '+';                                         // lead off
            ptr->len += ultocstring(&ptr->buf[ptr->len], curframe->line_num);   // setup line number
            ptr->buf[ptr->len++] = '^';                                         // lead off routine

            for (i = 0; i < VAR_LEN; i++) {
                if (!(ptr->buf[i + ptr->len] = curframe->rounam.var_cu[i])) {
                    break;                                                      // copy rou name
                }
            }

            ptr->len += i;                                                      // save length
        }

        ptr->buf[ptr->len++] = '>';                                             // and that bit
        ptr->buf[ptr->len++] = ' ';                                             // and that bit
        ptr->buf[ptr->len] = '\0';                                              // null terminate
        prompt_len = ptr->len;                                                  // update the prompt length for direct mode editing
        t = SQ_Write(ptr);                                                      // write it
        if (t < 0) return (short) t;                                            // if error, return it
        t = SQ_Read(ptr->buf, UNLIMITED, 256);                                  // read something
        if (t < 1) continue;                                                    // ignore nulls and errors

        if (!hist_next || strcmp(history[hist_next - 1], (char *) ptr->buf)) {
            strcpy(history[hist_next], (char *) ptr->buf);

            if (hist_next == (MAX_HISTORY - 1)) {
                hist_next = 0;
            } else {
                hist_next++;
            }
        }

        hist_curr = hist_next;
        t = SQ_WriteFormat(SQ_LF);                                              // return
        if (t < 0) return (short) t;                                            // if error, return it
        source_ptr = ptr->buf;                                                  // point at source
        comp_ptr = cmp;                                                         // and where it goes
        parse();                                                                // compile it
        *comp_ptr++ = ENDLIN;                                                   // JIC
        *comp_ptr++ = ENDLIN;                                                   // JIC
        rsmpc = cmp;                                                            // where it is
        partab.jobtab->cur_do++;                                                // increment do frame

        partab.jobtab->dostk[partab.jobtab->cur_do].routine =
          (u_char *) SBM(&history[(hist_curr) ? hist_curr - 1 : MAX_HISTORY - 1]);

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
        partab.jobtab->dostk[partab.jobtab->cur_do].asp = savasp;
        partab.jobtab->dostk[partab.jobtab->cur_do].ssp = savssp;
        partab.jobtab->dostk[partab.jobtab->cur_do].isp = isp;
        partab.jobtab->dostk[partab.jobtab->cur_do].savasp = savasp;
        partab.jobtab->dostk[partab.jobtab->cur_do].savssp = savssp;
        debug = TRUE;                                                           // prevent recursive BREAK frames
        t = run(savasp, savssp);                                                // do it
        debug = FALSE;
        if (t == OPHALT) return (short) t;                                      // just halt if required
        partab.jobtab->cur_do--;                                                // restore do frame
        if ((partab.debug == BREAK_OFF) || (partab.debug == BREAK_DISABLE)) break; // go away if debug now off

        if (t & BREAK_QN) {
            t &= ~BREAK_QN;                                                     // clear the bit

            if (t > 0) {
                partab.debug = t + partab.jobtab->commands;                     // when to stop
                partab.jobtab->attention = 1;                                   // say to check this thing
                break;                                                          // exit
            }
        }

        if (t == CMQUIT) {
            if (dot == 1) {                                                     // reset debug state
                partab.debug = BREAK_OFF;
            } else {
                partab.debug = BREAK_ON;
            }

            break;                                                              // exit on QUIT
        }

        var = (mvar *) &strstk[savssp];                                         // space to setup a var
        VAR_CLEAR(var->name);
        memcpy(&var->name.var_cu[0], "$ECODE", 6);
        var->volset = 0;
        var->uci = UCI_IS_LOCALVAR;
        var->slen = 0;                                                          // setup for $ECODE
        ptr = (cstring *) (&strstk[savssp] + sizeof(mvar));                     // for result
        memcpy(ptr->buf, "$ECODE=", 7);
        t = ST_Get(var, &ptr->buf[7]);
        if (t < 1) continue;                                                    // ignore if nothing there
        ptr->len = t + 7;

        if (partab.jobtab->seqio[0].dx) {
            t = SQ_WriteFormat(SQ_LF);                                          // need a CRLF then do it
            if (t < 0) return (short) t;                                        // if error, return it
        }

        t = SQ_Write(ptr);                                                      // write the prompt
        if (t < 0) return (short) t;                                            // if error, return it
        t = SQ_WriteFormat(SQ_LF);                                              // new line
        if (t < 0) return (short) t;                                            // if error, return it
    }

    partab.jobtab->io = io;                                                     // restore io
    partab.jobtab->seqio[0].options = options;                                  // restore channel 0 options
    rsmpc = partab.jobtab->dostk[partab.jobtab->cur_do].pc;                     // restore pc
    return 0;
}
