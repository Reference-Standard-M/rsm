/*
 * Package: Reference Standard M
 * File:    rsm/compile/routine.c
 * Summary: module compile - parse a routine ref
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
#include "compile.h"                                                            // compile stuff

/*
 * Function routine entered with source_ptr pointing at the source
 * to evaluate and comp_ptr pointing at where to put the code.
 *
 * Runtime is: 0 for compile
 *             -1 for compile extrinsic
 *             1 for runtime
 *             -2 for $TEXT() compile
 *             2 for $TEXT() runtime
 *
 * Return    Means
 * >0        Reserved for byte offset (not currently applicable)
 * 0         Compiled indirect to end up at addstk[asp]
 * -1        Compiled just a tag (VAR_LEN bytes)
 * -2        Compiled routine and tag (VAR_LEN * 2 bytes)
 * -3        Compiled routine (no tag) (VAR_LEN bytes)
 * -4        Runtime only, routine, tag and offset (VAR_LEN * 2 + 2 bytes)
 */
short routine(int runtime)                                                      // parse routine ref
{
    char    c;                                                                  // current character
    int     i = 0;                                                              // a useful int
    u_short us;                                                                 // for tag offsets
    u_char *ptr;                                                                // a handy pointer
    int     j;                                                                  // and another
    int     dp = 0;                                                             // dp in offset - blah
    var_u   tag;                                                                // to hold the tag
    var_u   rou;                                                                // to hold the rou
    int     ntag;                                                               // numeric tag flag
    int     isinder = 0;                                                        // indirect flag
    int     offset = 0;                                                         // tag offset
    int     gotplus = 0;                                                        // Flag for offset
    int     p1indirect = 0;                                                     // Piece one is indirected

    VAR_CLEAR(tag);                                                             // clear the tag
    VAR_CLEAR(rou);                                                             // and the routine
    c = *source_ptr++;                                                          // get first character

    // If initial atom is an indirect string, evaluate it and put on strstk as a variable
    if (c == '@') {                                                             // if it's indirect
        isinder = 1;                                                            // say indirect
        if (runtime == -2) ptr = comp_ptr;                                      // save compile pointer if $TEXT compile
        atom();                                                                 // stack it

        if ((runtime == -2) && (*ptr == OPSTR) && (*((u_short *) (ptr + 1)) == 0)) { // check for empty indirection string
            return -(ERRZ12 + ERRMLAST);                                        // not allowed, so pass compiler error
        }

        p1indirect = 1;                                                         // piece 1 is indirect... make sure to concat later
        c = *source_ptr++;                                                      // get the next

        if ((c == ')') || (c == ',') || (c == ' ') || (c == '\0')) {            // check for end of it all
            source_ptr--;
            return 0;
        }
    } else {                                                                    // If initial atom is a tag, extract first
        ntag = isdigit((int) c);                                                // check for a numeric tag

        for (i = 0; i < VAR_LEN; i++) {
            if (ntag && (!isdigit((int) c))) break;                             // numeric tag is all digits
            if ((i != 0) && (c == '%')) break;                                  // % only permitted as first
            if ((!isalnum((int) c)) && (c != '%')) break;                       // allow alphanumeric + %
            tag.var_cu[i] = c;                                                  // save that one
            c = *source_ptr++;                                                  // get the next
        }

        if (isalnum((int) c)) comperror(-ERRM56);                               // tag is longer than VAR_LEN, then complain
    }

    // The following ONLY executes if we have a tag && $TEXT --> append tag to stack as a string.
    if (runtime == -2) {                                                        // if $TEXT() compile
        if (!isinder && !var_empty(tag)) {                                      // if not indirected & tag is defined
            *comp_ptr++ = OPSTR;                                                // string follows
            us = (u_short) i;
            assert(sizeof(us) == sizeof(u_short));
            memcpy(comp_ptr, &us, sizeof(u_short));
            comp_ptr += sizeof(u_short);
            for (j = 0; j < i; *comp_ptr++ = tag.var_cu[j++]) continue;         // copy tag
            *comp_ptr++ = '\0';                                                 // and null terminate
        }

        runtime = 0;                                                            // pretend a compile
        isinder = 1;                                                            // and now indirect
    }
    // <-- end execute only if tag

    if ((c == ')') || (c == ',') || (c == ' ') || (c == '\0')) {                // check for end of it all
        source_ptr--;
        goto exit;
    }

    // The following is for a numeric offset -->
    if ((c == '+') && (runtime != -1)) {                                        // bloody offset
        gotplus = 1;                                                            // flag it

        if (!runtime) {                                                         // if just compiling
            if (!isinder && !var_empty(tag)) {
                *comp_ptr++ = OPSTR;                                            // string follows
                us = (u_short) i;
                assert(sizeof(us) == sizeof(u_short));
                memcpy(comp_ptr, &us, sizeof(u_short));
                comp_ptr += sizeof(u_short);
                for (j = 0; j < i; *comp_ptr++ = tag.var_cu[j++]) continue;     // copy tag
                *comp_ptr++ = '\0';                                             // and null terminate
            }

            isinder = 1;                                                        // and now indirect
            *comp_ptr++ = OPSTR;                                                // string follows
            us = 1;                                                             // the length
            assert(sizeof(us) == sizeof(u_short));
            memcpy(comp_ptr, &us, sizeof(u_short));
            comp_ptr += sizeof(u_short);
            *comp_ptr++ = '+';                                                  // add the plus
            *comp_ptr++ = '\0';                                                 // and null terminate
            if (!var_empty(tag) || p1indirect) *comp_ptr++ = OPCAT;             // tag or 1st piece is indirect? then concatenate
            eval();                                                             // get the value
            *comp_ptr++ = OPPLUS;                                               // force evaluation
            *comp_ptr++ = OPCAT;                                                // concatenate
        } else {                                                                // it is runtime
            if (!isdigit(*source_ptr)) {                                        // next must be a digit
                comperror(-ERRM5);                                              // complain
                return -1;                                                      // so caller won't change it
            }

            while (TRUE) {                                                      // while we have digits
                if (*source_ptr == '.') {                                       // if a bloody dot
                    if (dp) break;                                              // if already got one then just exit
                    dp = 1;                                                     // flag it
                    source_ptr++;                                               // ignore it
                    continue;                                                   // and continue
                }

                if (!isdigit(*source_ptr)) break;

                if (!dp) {                                                      // if part of int
                    offset = (offset * 10) + *source_ptr++ - '0';               // convert it
                } else {
                    source_ptr++;                                               // just ignore it
                }
            }

            if (runtime == 1) {                                                 // if not $TEXT
                if (!(systab->historic & HISTORIC_OFFOK)) {                     // if offset not OK
                    comperror(-(ERRZ70 + ERRMLAST));                            // complain
                    return -1;                                                  // so caller won't change it
                }
            }                                                                   // end runtime code
        }                                                                       // end offset code

        c = *source_ptr++;                                                      // get the next
    }                                                                           // end offset junk
    // <-- end numeric offset

    if ((c == ')') || (c == ',') || (c == ' ') || (c == '\0')) {                // check for end of it all
        source_ptr--;
        goto exit;
    }

    // The following resolves a routine name ^XYZ or ^@XYZ -->
    if (c == '^') {                                                             // if it's a routine
        c = *source_ptr++;                                                      // get next character

        if (c != '@') {                                                         // if not indirect
            for (i = 0; i < VAR_LEN; i++) {                                     // increment i to be routine name length as follows
                if ((i == 0) && (isdigit((int) c) != 0)) break;                 // can't start with a number
                if ((i != 0) && (c == '%')) break;                              // % only permitted as first
                if ((isalnum((int) c) == 0) && (c != '%')) break;               // allow alphanumeric + %
                rou.var_cu[i] = c;                                              // save that one
                c = *source_ptr++;                                              // get the next
            }

            if (isalnum((int) c) != 0) comperror(-ERRM56);                      // routine name is longer than VAR_LEN then complain

            if (isinder) {                                                      // is indirect already
                *comp_ptr++ = OPSTR;                                            // string follows
                us = (u_short) (i + 1);                                         // routine name length plus ^
                assert(sizeof(us) == sizeof(u_short));
                memcpy(comp_ptr, &us, sizeof(u_short));                         // store short int
                comp_ptr += sizeof(u_short);                                    // move past it
                *comp_ptr++ = '^';                                              // store the caret
                for (j = 0; j < i; *comp_ptr++ = rou.var_cu[j++]) continue;     // copy rou
                *comp_ptr++ = '\0';                                             // and null terminate
                if (!var_empty(tag) || gotplus || p1indirect) *comp_ptr++ = OPCAT; // concatenate
            }

            source_ptr--;
        } else {                                                                // indirect (^@XYZ)
            // if a $TEXT compile, this never executes because $TEXT sets isinder to 1, therefore, it's only for DO TAG^@XYZ
            if (!isinder && !var_empty(tag)) {
                *comp_ptr++ = OPSTR;                                            // string follows
                us = (u_short) i;
                assert(sizeof(us) == sizeof(u_short));
                memcpy(comp_ptr, &us, sizeof(u_short));
                comp_ptr += sizeof(u_short);
                for (j = 0; j < i; *comp_ptr++ = tag.var_cu[j++]) continue;     // copy tag
                *comp_ptr++ = '\0';                                             // and null terminate
            }

            isinder = 1;                                                        // and now indirect

            if (offset) {
                *comp_ptr++ = OPSTR;                                            // string follows
                us = 1;
                assert(sizeof(us) == sizeof(u_short));
                memcpy(comp_ptr, &us, sizeof(u_short));
                comp_ptr += sizeof(u_short);
                *comp_ptr++ = '+';                                              // add the plus
                *comp_ptr++ = '\0';                                             // and null terminate
                if (!var_empty(tag)) *comp_ptr++ = OPCAT;                       // if we have a tag then concatenate
                eval();                                                         // get the value
                *comp_ptr++ = OPPLUS;                                           // force evaluation
                *comp_ptr++ = OPCAT;                                            // concatenate
            }

            *comp_ptr++ = OPSTR;                                                // string follows
            us = 1;
            assert(sizeof(us) == sizeof(u_short));
            memcpy(comp_ptr, &us, sizeof(u_short));
            comp_ptr += sizeof(u_short);
            *comp_ptr++ = '^';                                                  // the caret
            *comp_ptr++ = '\0';                                                 // and null terminate
            if ((!var_empty(tag)) || (gotplus) || p1indirect) *comp_ptr++ = OPCAT; // concatenate it
            atom();                                                             // get routine name
            *comp_ptr++ = OPCAT;                                                // concatenate it
        }
    } else {                                                                    // end routineref - no ^ has been found.
        source_ptr--;
    }

exit:
    if (isinder) return 0;                                                      // if there was indirection then exit flagging that

    // Below if we are doing a DO sans indirection... we must copy tag and rou to comp_ptr
    if (!var_empty(tag) || offset) {                                            // if we have a tag
        ntag = 1;                                                               // assume just a tag

        if ((!var_empty(rou)) || offset) {                                      // if we have a routine
            ntag = 2;                                                           // say both
            assert(sizeof(rou.var_qu) == VAR_LEN);
            memcpy(comp_ptr, &rou.var_qu, VAR_LEN);                             // save the routine
            comp_ptr += VAR_LEN;
        }

        assert(sizeof(tag.var_qu) == VAR_LEN);
        memcpy(comp_ptr, &tag.var_qu, VAR_LEN);                                 // save the tag
        comp_ptr += VAR_LEN;

        if (offset) {                                                           // if we have an offset
            if (offset > (MAXROULINE - 1)) {
                comperror(-(ERRZ70 + ERRMLAST));                                // complain
                return -4;                                                      // and exit
            }

            us = (u_short) offset;
            assert(sizeof(us) == sizeof(u_short));
            memcpy(comp_ptr, &us, sizeof(u_short));                             // save the offset
            comp_ptr += sizeof(u_short);
            return -4;                                                          // and exit
        }

        return (short) -ntag;                                                   // return saying 1 or 2
    }

    assert(sizeof(rou.var_qu) == VAR_LEN);
    memcpy(comp_ptr, &rou.var_qu, VAR_LEN);                                     // save the routine
    comp_ptr += VAR_LEN;
    return -3;                                                                  // say just the routine
}

/*
 * The following function compiles a routine, the result of code like:
 *   MERGE ^$ROUTINE(routine_name) = local or global ref
 *
 * This always succeeds even if the routine is junk
 * The name is checked first though...
 *
 * If rou == NULL, check the routine->src
 */
int Compile_Routine(mvar *rou, mvar *src, u_char *stack)
{
    cstring *line;                                                              // the source line
    u_char  *code;                                                              // the code
    int     t;                                                                  // for returns
    u_short us;                                                                 // for routine header
    u_short uss;                                                                // for routine header
    int     cnt;                                                                // count things
    u_char  src_slen;                                                           // key in source
    u_char  rou_slen = 0;                                                       // key in routine
    u_char  temp[100];                                                          // temp space
    tags    tag_tbl[MAX_NUM_TAGS];                                              // space for the tags
    var_u   var_tbl[MAX_NUM_VARS];                                              // and the variables
    var_u   var;                                                                // for one var
    int     num_tags = 0;                                                       // count tags
    int     num_vars = 0;                                                       // and variables
    int     last_dots = 0;                                                      // remember last line dots
    int     i;                                                                  // a handy int
    int     j;                                                                  // and another
    cstring *cptr;                                                              // a cstring pointer
    u_char  *p;                                                                 // and a char ptr
    var_u   rounam;                                                             // the routine name
    int     same = 0;                                                           // same routine flag

    partab.checkonly = 0;                                                       // a real compile
    partab.errors = 0;                                                          // total number of syntax errors - checkonly
    partab.ln = 0;                                                              // save for $&%ROUCHK()
    line = (cstring *) stack;                                                   // for source lines
    code = stack + sizeof(cstring);                                             // where the code goes
    cptr = (cstring *) temp;                                                    // point at temp space
    for (i = 0; i < MAX_NUM_VARS; i++) VAR_CLEAR(var_tbl[i]);                   // clear var table
    if ((code + MAXROUSIZE) > partab.strstk_last) return -(ERRZ8 + ERRMLAST);   // too big? then complain

    if (rou != NULL) {
        t = SS_Norm(rou);                                                       // normalize mvar
        if (t < 0) return t;                                                    // quit on error
    }

    i = 0;                                                                      // clear i

    if (rou != NULL) {                                                          // if it's real
        int nsubs = 0;                                                          // count subscripts

        while (i < rou->slen) {                                                 // for all subs
            cnt = 0;                                                            // flag no rabbit ears
            if (nsubs > 0) return -ERRM38;                                      // junk
            if (rou->slen > (VAR_LEN + 2)) return -ERRM38;                      // ditto
            t = UTIL_Key_Extract(&rou->key[i], temp, &cnt);                     // key to extract
            if (t < 0) return t;                                                // die on error
            if ((t > VAR_LEN) || (t < 1)) return -ERRM38;                       // outside routine length (1 to VAR_LEN) then junk
            i += cnt;                                                           // count used bytes
            nsubs++;                                                            // count it
        }

        for (i = 0; i < VAR_LEN; i++) {                                         // scan the routine name
            if ((i > 0) && !temp[i]) {                                          // done
                for (j = i; j < VAR_LEN; j++) rounam.var_cu[j] = '\0';          // copy 0
                break;                                                          // and exit
            }

            if (isalpha(temp[i])) {                                             // alpha ok anywhere
                rounam.var_cu[i] = temp[i];                                     // copy it
                continue;                                                       // and continue
            }

            if (isdigit(temp[i]) && i) {                                        // digit ok after first
                rounam.var_cu[i] = temp[i];                                     // copy it
                continue;                                                       // and continue
            }

            if ((temp[i] == '%') && !i) {                                       // % ok as first
                rounam.var_cu[i] = temp[i];                                     // copy it
                continue;                                                       // and continue
            }

            return -ERRM38;                                                     // junk
        }                                                                       // destination now validated
    } else {
        partab.checkonly = 1;                                                   // just a check
        same = 1;                                                               // stop writing
        partab.sp = &source_ptr;                                                // where source ptr is
        partab.lp = &line;                                                      // and where line is
    }

    if (src->name.var_cu[0] == '$') {                                           // source an SSVN?
        t = SS_Norm(src);                                                       // normalize mvar

        if (t < 0) {                                                            // quit on error
            partab.checkonly = 0;                                               // reset to avoid buffer bugs in comperror
            return t;
        }

#if RSM_DBVER != 1
        if (memcmp(src->name.var_cu, "$ROUTINE\0", 9)) {                        // a routine? then junk
#else
        if (memcmp(src->name.var_cu, "$ROUTINE", 8)) {                          // a routine? then junk
#endif
            partab.checkonly = 0;                                               // reset to avoid buffer bugs in comperror
            return -ERRM38;
        }

        if (!partab.checkonly) {                                                // if it's real
            if (src->volset == rou->volset) {                                   // same volset
                if (src->uci == rou->uci) {                                     // same UCI
                    if (src->slen == rou->slen) {                               // same key size
                        if (memcmp(src->key, rou->key, rou->slen) == 0) {       // same key
                            same = 1;                                           // same rou and source
                        }
                    }
                }
            }
        }
    }                                                                           // end source SSVN check

    if (!partab.checkonly) {                                                    // if it's a real compile
        t = SemOp(SEM_ROU, SEM_WRITE);                                          // grab the routine semaphore
        if (t < 0) return t;                                                    // if we got an error, quit
        rou_slen = rou->slen;                                                   // save routine key size
    }

    if (!same) {                                                                // if not the same
        t = DB_Kill(rou);                                                       // dong it

        if (t < 0) {                                                            // complain on error
            SemOp(SEM_ROU, -SEM_WRITE);                                         // release sem
            partab.checkonly = 0;                                               // reset to avoid buffer bugs in comperror
            return t;                                                           // exit
        }

        Routine_Delete(rounam, rou->volset, rou->uci);                          // delete the routine
    }

    src_slen = src->slen;                                                       // save source key size
    line->buf[0] = '0';                                                         // seed the $ORDER()
    line->buf[1] = '\0';                                                        // null terminated
    line->len = 1;                                                              // this long
    t = UTIL_Key_Build(line, &src->key[src_slen]);                              // build the key
    src->slen = src_slen + t;                                                   // store the new length
    comp_ptr = code;                                                            // setup the compiler ptr
    partab.varlst = var_tbl;                                                    // for localvar()

    while (TRUE) {                                                              // for all lines
        t = Dorder1(line->buf, src);                                            // get next in source
        if (!t) break;                                                          // all done

        if (t < 0) {                                                            // if we got an error, quit
            partab.checkonly = 0;                                               // reset to avoid buffer bugs in comperror
            return t;
        }

        line->len = t;                                                          // save length
        t = UTIL_Key_Build(line, &src->key[src_slen]);                          // build the key
        src->slen = src_slen + t;                                               // store the new length
        t = Dget1(line->buf, src);                                              // get the data
        if (t < 1) continue;                                                    // ignore empty/undefined lines
        line->len = t;                                                          // save the length
        source_ptr = line->buf;                                                 // where the source is

        if (isalnum(*source_ptr) || (*source_ptr == '%')) {                     // check for a tag
            j = isdigit(*source_ptr);                                           // remember if digit

            if (num_tags == MAX_NUM_TAGS) {                                     // check number of tags
                comperror(-(ERRZ53 + ERRMLAST));                                // complain
                continue;                                                       // ignore remainder of line
            }

            tag_tbl[num_tags].code = comp_ptr - code;                           // save code offset
            VAR_CLEAR(tag_tbl[num_tags].name);                                  // zot the name
            tag_tbl[num_tags].name.var_cu[0] = *source_ptr++;                   // copy first char
            i = 1;                                                              // init name index

            while (TRUE) {
                if (!isalnum(*source_ptr)) break;                               // give up if wrong
                if (j && !isdigit(*source_ptr)) break;                          // must be all digits
                if (i < VAR_LEN) tag_tbl[num_tags].name.var_cu[i++] = *source_ptr; // still copying? then copy one
                source_ptr++;                                                   // increment source pointer
            }

            for (i = 0; i < num_tags; i++) {                                    // check for duplicate
                if (var_equal(tag_tbl[num_tags].name, tag_tbl[i].name)) {       // the same ?
                    comperror(-(ERRZ65 + ERRMLAST));                            // complain
                    p = comp_ptr;                                               // save
                    comp_ptr = code + tag_tbl[i].code;                          // point at other one
                    comperror(-(ERRZ65 + ERRMLAST));                            // complain there too
                    comp_ptr = p;                                               // restore comp pointer
                    continue;                                                   // ignore remainder of line
                }
            }

            num_tags++;                                                         // increment

            if (*source_ptr == '(') {                                           // any argument list ?
                cnt = 0;                                                        // to count formal list
                source_ptr++;                                                   // skip the (
                *comp_ptr++ = LOADARG;                                          // add the opcode
                p = comp_ptr;                                                   // remember where the count is
                comp_ptr++;                                                     // skip the count

                while (TRUE) {                                                  // scan the list
                    if (*source_ptr == ')') {                                   // found end yet?
                        source_ptr++;                                           // skip )
                        break;                                                  // and exit
                    }

                    VAR_CLEAR(var);                                             // clear var name

                    for (i = 0; i < VAR_LEN; i++) {                             // scan possible var name
                        if (isalpha(*source_ptr) || ((*source_ptr == '%') && !i)) { // first char alpha or %
                            var.var_cu[i] = *source_ptr++;                      // copy it
                            continue;                                           // and go for more
                        }

                        if ((isalnum(*source_ptr)) && i) {                      // the rest alpha/numeric
                            var.var_cu[i] = *source_ptr++;                      // copy it
                            continue;                                           // and go for more
                        }

                        if ((*source_ptr == ',') && i) break;                   // end of name then exit
                        if (*source_ptr == ')') break;                          // end of name then exit
                        cnt = -(ERRZ13 + ERRMLAST);                             // else error
                        break;                                                  // and exit
                    }

                    if (cnt < 0) break;                                         // quit on error

                    if (isalnum(*source_ptr)) {
                        cnt = -ERRM56;                                          // complain about name length
                        break;
                    }

                    if (*source_ptr == ',') {
                        source_ptr++;                                           // skip the comma
                    } else if (*source_ptr != ')') {                            // check for )
                        cnt = -(ERRZ13 + ERRMLAST);                             // error
                        break;                                                  // exit
                    }

                    for (i = 0; i < MAX_NUM_VARS; i++) {                        // scan var list
                        if (var_equal(var_tbl[i], var)) break;                  // quit on match

                        if (var_empty(var_tbl[i])) {
                            VAR_COPY(var_tbl[i], var);                          // save it
                            break;                                              // and exit
                        }
                    }

                    if (i == MAX_NUM_VARS) {                                    // too many?
                        cnt = -(ERRZ74 + ERRMLAST);                             // too many
                        break;                                                  // exit
                    }

                    *comp_ptr++ = (u_char) i;                                   // save index
                    cnt++;                                                      // count it

                    for (j = 1; j < cnt; j++) {                                 // scan what's already there
                        if (p[j] == i) {                                        // if already got that one
                            cnt = -ERRM21;                                      // complain
                            break;                                              // exit
                        }
                    }
                }

                if (cnt > MAX_NUM_ARGS) cnt = -(ERRZ75 + ERRMLAST);             // too many

                if (cnt < 0) {                                                  // got an error?
                    comp_ptr = --p;                                             // backup
                    comperror(cnt);                                             // compile error
                    continue;                                                   // ignore rest of line
                }

                *p = (u_char) cnt;                                              // save the count
            }
        }                                                                       // end tag processing

        partab.ln++;                                                            // count a line

        if (!same) {                                                            // write if required
            for (i = 0; source_ptr[i] == '\t'; source_ptr[i++] = ' ') continue; // convert leading tab to space
DISABLE_WARN(-Warray-bounds)
            cptr->len = ltocstring(cptr->buf, partab.ln);                       // convert to a cstring
ENABLE_WARN
            t = UTIL_Key_Build(cptr, &rou->key[rou_slen]);                      // build the key
            rou->slen = rou_slen + t;                                           // store the new length
            t = DB_Set(rou, line);                                              // write out the source line

            if (t < 0) {
                if (!partab.checkonly) SemOp(SEM_ROU, -SEM_WRITE);              // unlock
                partab.varlst = NULL;                                           // for localvar()
                partab.checkonly = 0;                                           // reset to avoid buffer bugs in comperror
                return t;                                                       // exit with error
            }
        }

        if (partab.ln > MAXROULINE) {
            comperror(-(ERRZ54 + ERRMLAST));                                    // complain
            continue;                                                           // ignore the rest
        }

        if (*source_ptr == ';') continue;                                       // ignore comment lines

        if (*source_ptr++ != ' ') {
            comperror(-(ERRZ13 + ERRMLAST));                                    // must be a space
            continue;                                                           // ignore the rest
        }

        while (*source_ptr == ' ') source_ptr++;                                // skip spare spaces
        *comp_ptr++ = LINENUM;                                                  // mark new line
        us = (u_short) partab.ln;
        assert(sizeof(us) == sizeof(u_short));
        memcpy(comp_ptr, &us, sizeof(u_short));
        comp_ptr += sizeof(u_short);
        p = comp_ptr;                                                           // where the offset will go
        *comp_ptr++ = 0;                                                        // endian doesn't matter here
        *comp_ptr++ = 0;                                                        // endian doesn't matter here
        j = 0;                                                                  // a dot counter

        if (*source_ptr == '.') {                                               // any dots ?
            for (i = 0; ; i++) {                                                // scan them
                if (*source_ptr == '.') {                                       // found a dot
                    j++;                                                        // count it
                    source_ptr++;                                               // go past it
                    continue;                                                   // go for more
                }

                if (*source_ptr == ' ') {                                       // a space?
                    source_ptr++;                                               // go past it
                    continue;                                                   // go for more
                }

                break;                                                          // exit loop
            }
        }

        if (j > 255) {                                                          // too many dots
            comperror(-(ERRZ13 + ERRMLAST));                                    // complain
            continue;                                                           // go for more
        }

        if (j || last_dots) {                                                   // any dots (or on last line)
            *comp_ptr++ = CHKDOTS;                                              // the op code
            *comp_ptr++ = (u_char) j;                                           // the count
            last_dots = j;                                                      // remember for ron
        }

        if ((*source_ptr != ';') && (*source_ptr != '\0')) {                    // ignore comment and empty lines
            parse();                                                            // parse the line
        } else {
            *comp_ptr++ = ENDLIN;                                               // end of null line
        }

        *((u_short *) p) = (u_short) (comp_ptr - p - 1);                        // offset to ENDLIN
    }                                                                           // end main compile loop

    *comp_ptr++ = CMQUIT;                                                       // mark end of routine
    *comp_ptr++ = ENDLIN;                                                       // mark end of routine
    *comp_ptr++ = ENDLIN;                                                       // mark end of routine
    partab.varlst = NULL;                                                       // for localvar()
    for (num_vars = 0; !var_empty(var_tbl[num_vars]); num_vars++) continue;     // count them
    p = line->buf;                                                              // where we put it now
DISABLE_WARN(-Warray-bounds)
    cptr->len = Vhorolog(cptr->buf);                                            // get current date/time
ENABLE_WARN
    us = COMP_VER;
    memcpy(p, &us, sizeof(u_short));                                            // compiler version
    p += sizeof(u_short);
    us = partab.jobtab->user;                                                   // this is an int in jobtab
    memcpy(p, &us, sizeof(u_short));                                            // but a u_short here for now - user who compiled it
    p += sizeof(u_short);
    i = cstringtoi(cptr);
    memcpy(p, &i, sizeof(int));                                                 // get the date
    p += sizeof(int);
    i = atoi((char *) &cptr->buf[6]);
    memcpy(p, &i, sizeof(int));                                                 // and the time
    p += sizeof(int);
    i = sizeof(tags) * num_tags;                                                // space for tags
    j = sizeof(var_u) * num_vars;                                               // space for vars
    uss = (p - line->buf) + (6 * sizeof(u_short));                              // offset for tags
    memcpy(&line->buf[uss], tag_tbl, i);                                        // copy tag table
    memcpy(p, &uss, sizeof(u_short));                                           // where it went
    p += sizeof(u_short);
    us = (u_short) num_tags;
    memcpy(p, &us, sizeof(u_short));                                            // copy the count
    p += sizeof(u_short);
    uss += i;                                                                   // wher vars go
    memcpy(&line->buf[uss], var_tbl, j);                                        // copy var table
    memcpy(p, &uss, sizeof(u_short));                                           // where it went
    p += sizeof(u_short);
    us = (u_short) num_vars;
    memcpy(p, &us, sizeof(u_short));                                            // copy the count
    p += sizeof(u_short);
    uss += j;                                                                   // where the code goes
    memmove(&line->buf[uss], code, comp_ptr - code);                            // copy the code
    memcpy(p, &uss, sizeof(u_short));                                           // where it went
    p += sizeof(u_short);
    us = (u_short) (comp_ptr - code);
    memcpy(p, &us, sizeof(u_short));                                            // and the size
    p += sizeof(u_short);
    i = p - line->buf + (comp_ptr - code) + i + j;                              // total size

    if (i > MAX_STR_LEN) {
        comperror(-(ERRZ54 + ERRMLAST));                                        // complain
        if (!partab.checkonly) SemOp(SEM_ROU, -SEM_WRITE);                      // unlock
        partab.checkonly = 0;                                                   // reset to avoid buffer bugs in comperror
        return -ERRM75;                                                         // ignore the rest
    }

    line->len = i;                                                              // save the length
DISABLE_WARN(-Warray-bounds)
    cptr->buf[0] = '0';                                                         // where it goes
    cptr->buf[1] = '\0';                                                        // null terminated
    cptr->len = 1;                                                              // the size
ENABLE_WARN

    if (partab.checkonly) {                                                     // just a check so exit with error count
        partab.checkonly = 0;                                                   // reset to avoid buffer bugs in comperror
        return partab.errors;
    }

    t = UTIL_Key_Build(cptr, &rou->key[rou_slen]);                              // build the key
    rou->slen = rou_slen + t;                                                   // store the new length
    t = DB_Set(rou, line);                                                      // set it
    if (same) Routine_Delete(rounam, rou->volset, rou->uci);                    // delete the routine
    i = SemOp(SEM_ROU, -SEM_WRITE);                                             // release sem
    partab.checkonly = 0;                                                       // reset to avoid buffer bugs in comperror
    return t;                                                                   // NEED MORE HERE
}
