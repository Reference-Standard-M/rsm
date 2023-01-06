/*
 * Package:  Reference Standard M
 * File:     rsm/runtime/run.c
 * Summary:  module runtime - run it
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
#include <unistd.h>                                                             // for sleep
#include <errno.h>                                                              // error stuff
#include <math.h>                                                               // maths functions
#include <assert.h>
#include "rsm.h"                                                                // standard includes
#include "proto.h"                                                              // standard prototypes
#include "error.h"                                                              // standard errors
#include "opcode.h"                                                             // the op codes
#include "compile.h"                                                            // for XECUTE
#include "symbol.h"                                                             // for fast symbol stuff
#include "database.h"                                                           // for GBD def

extern short in_hist;                                                           // are we in the history buffer

/*
 * This module is the main run-time dispatch. It starts interpreting
 * at the opcode pointed to by rsmpc updating it as it goes.
 */
short run(int savasp, int savssp)                                               // run compiled code
{
    int       opc;                                                              // current opcode
    int       infor = 0;                                                        // for flag
    int       offset;                                                           // for DO, GO, JOB offset
    short     s = 0;                                                            // for function returns
    u_short   us = 0;                                                           // for offsets
    int       t = 0;                                                            // for function returns
    short     hist;                                                             // for history buffer
    int       i;                                                                // a handy int
    int       j;                                                                // and another
    int       args;                                                             // num arguments
    int       flag;                                                             // a random flag
    cstring   *cptr;                                                            // a cstring ptr
    cstring   *ptr1 = NULL;                                                     // a cstring ptr
    cstring   *ptr2;                                                            // a cstring ptr
    cstring   *tmp;                                                             // and another
    mvar      *var;                                                             // an mvar pointer
    mvar      *var2;                                                            // an mvar pointer
    u_char    *p;                                                               // useful ptr
    u_short   var_undefined = VAR_UNDEFINED;                                    // for CMDO undefined vars
    var_u     rou;                                                              // a routine name
    var_u     tag;                                                              // a tag name
    var_u     *list;                                                            // pointer to var_u things
    tags      *ttbl;                                                            // a structure of tags
    rbd       *rouadd;                                                          // routine pointer
    do_frame  *curframe;                                                        // a do frame pointer
    int       asp;                                                              // copy of asp
    int       ssp;                                                              // and ssp
    u_char    test;                                                             // handy temp $TEST
    u_char    temp[MAX_NUM_BYTES];                                              // some temp storage for arithmetic
    for_stack *forx;                                                            // point at a for stack
    var_u     *vt;                                                              // pointer for var tab
    ST_data   *data;                                                            // for direct symbol access

    asp = savasp;
    ssp = savssp;

    while (TRUE) {                                                              // keep going till done
        if (ssp >= MAX_SSTK) panic("String Stack overflow in runtime!!");       // check ssp, if too much die

        if (partab.jobtab->attention) {                                         // any attention thingys
            s = attention();                                                    // do it

            if (s == BREAK_NOW) {                                               // funny debug stuff
                if (Debug(asp, ssp, -1) == OPHALT) return OPHALT;               // go away if reqd
                continue;                                                       // or start again
            }

            if (s > 0) return s;                                                // return other than error

            if ((s < 0) || (partab.jobtab->error_frame > partab.jobtab->cur_do)) { // if we have an error
                savasp = partab.jobtab->dostk[partab.jobtab->cur_do].asp;
                savssp = partab.jobtab->dostk[partab.jobtab->cur_do].ssp;
                ssp = savssp;                                                   // restore ssp
                asp = savasp;                                                   // and asp
                infor = 0;                                                      // cancel any for loops

                if (!partab.jobtab->error_frame) {
                    partab.jobtab->error_frame = partab.jobtab->cur_do;         // set $STACK(-1)

                    // save for ron
                    memcpy(&partab.jobtab->dostk[STM1_FRAME], &partab.jobtab->dostk[partab.jobtab->cur_do], sizeof(do_frame));
                }

                if (s == -(ERRZ51 + ERRMLAST)) partab.jobtab->io = 0;           // if it's a Control-C then $IO = 0
                partab.jobtab->dostk[partab.jobtab->cur_do].pc = rsmpc;         // save pc
                cptr = (cstring *) &strstk[ssp];                                // where we will put it
                var = &partab.src_var;                                          // a spare mvar
                var->volset = 0;                                                // local var
                var->uci = UCI_IS_LOCALVAR;                                     // ditto
                flag = 0;                                                       // say no error here
                if (s) flag = Set_Error(s, ptr1, cptr);                         // set $ECODE if reqd
                cptr->len = 0;                                                  // clear the cstring
                VAR_CLEAR(var->name);
                memcpy(&var->name.var_cu[0], "$ETRAP", 6);                      // $ETRAP
                var->slen = 0;                                                  // setup for $ETRAP
                t = ST_Get(var, cptr->buf);                                     // get it
                if (t < 0) t = 0;                                               // ignore undefined
                cptr->len = t;                                                  // save the length
                ssp += cptr->len + sizeof(u_short) + 1;                         // point past it
                p = &strstk[ssp];                                               // some space
                comp_ptr = p;                                                   // call it a compile ptr

                if (cptr->len && !flag) {                                       // if something there and no prev err at this lev
                    source_ptr = cptr->buf;                                     // where the data is
                    parse();                                                    // compile it
                    partab.jobtab->etrap_at = partab.jobtab->cur_do;            // remember current
                }

                partab.jobtab->dostk[partab.jobtab->cur_do].endlin = comp_ptr;

                if ((partab.jobtab->dostk[partab.jobtab->cur_do].type) == TYPE_EXTRINSIC) {
                    *comp_ptr++ = OPSTR;                                        // string follows
                    *comp_ptr++ = 0;                                            // endian doesn't matter here
                    *comp_ptr++ = 0;                                            // endian doesn't matter here
                    *comp_ptr++ = '\0';                                         // null terminated
                    *comp_ptr++ = CMQUITA;                                      // quit with arg
                } else if ((partab.jobtab->dostk[partab.jobtab->cur_do].type) != TYPE_RUN) {
                    *comp_ptr++ = CMQUIT;                                       // quit without arg
                }

                *comp_ptr++ = ENDLIN;                                           // JIC
                *comp_ptr++ = ENDLIN;                                           // JIC
                i = comp_ptr - p;                                               // get the length
                ssp += i;                                                       // point past it
                rsmpc = p;                                                      // new pc
                savasp = asp;
                savssp = ssp;
            }
        }

        // *** WHAT FOLLOWS IS THE MAIN INTERPRETER LOOP ***
        opc = *rsmpc++;                                                         // get an opcode

        switch (opc) {                                                          // dispatch on it
        case ENDLIN:                                                            // END OF LINE
            if (*rsmpc == ENDLIN) return ENDLIN;                                // two in a row is end of rou
            break;                                                              // go for more

        case OPHALT:                                                            // HALT
            CleanJob(0);                                                        // remove all locks etc.
            return OPHALT;                                                      // all done

        case OPERROR:                                                           // ERROR
            assert(sizeof(s) == sizeof(short));
            memcpy(&s, rsmpc, sizeof(short));
            rsmpc += sizeof(short);
            ERROR(s);                                                           // return the error

        case OPNOT:                                                             // UNARY NOT
            i = cstringtob((cstring *) addstk[--asp]);                          // get the arg
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            cptr->len = 1;                                                      // the count
            cptr->buf[0] = '0';                                                 // assume false
            cptr->buf[1] = '\0';                                                // null terminate
            if (i == 0) cptr->buf[0] = '1';                                     // or maybe true
            ssp += sizeof(u_short) + 2;                                         // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case OPENDC:                                                            // END OF COMMAND
            asp = savasp;                                                       // restore asp
            ssp = savssp;                                                       // and ssp
            isp = partab.jobtab->dostk[partab.jobtab->cur_do].isp;              // and isp
            break;

        case JMP0:                                                              // JUMP IF FALSE
            assert(sizeof(s) == sizeof(short));
            memcpy(&s, rsmpc, sizeof(short));                                   // get the offset
            rsmpc += sizeof(short);
            if (cstringtob((cstring *) addstk[--asp]) == 0) rsmpc = rsmpc + s;  // jump if reqd
            break;

        case OPIFN:                                                             // IF no args
            partab.jobtab->commands++;                                          // count a command

            test = ((partab.jobtab->dostk[partab.jobtab->cur_do].test != -1) ?
                   partab.jobtab->dostk[partab.jobtab->cur_do].test : partab.jobtab->test);

            if (test == 0) {                                                    // check $TEST
                if (infor) {
                    i = 1;                                                      // offset for standard FORs

                    if ((((for_stack *) addstk[savasp - 1])->type & 7) == FOR_TYP_0) {
                        i = 3;                                                  // FOR0 is a bit different
                        ssp = savssp;
                        asp = savasp;
                    }

                    rsmpc = ((for_stack *) addstk[savasp - 1])->quit - i;
                } else {
                    rsmpc = partab.jobtab->dostk[partab.jobtab->cur_do].endlin;
                }
            }

            break;

        case OPIFA:                                                             // IF with args
        case OPIFI:                                                             // IF indirect
            partab.jobtab->commands++;                                          // count a command

            if (partab.jobtab->dostk[partab.jobtab->cur_do].test != -1) {
                partab.jobtab->dostk[partab.jobtab->cur_do].test = 1;           // set $TEST
            } else {
                partab.jobtab->test = 1;                                        // set $TEST
            }

            if (cstringtob((cstring *) addstk[--asp]) == 0) {
                if (partab.jobtab->dostk[partab.jobtab->cur_do].test != -1) {
                    partab.jobtab->dostk[partab.jobtab->cur_do].test = 0;       // clear $TEST
                } else {
                    partab.jobtab->test = 0;                                    // clear $TEST
                }

                if (opc == OPIFI) {                                             // indirect
                    assert(sizeof(isp) == sizeof(long));
                    memcpy(&isp, rsmpc, sizeof(long));                          // restore the isp
                    rsmpc += sizeof(long);
                }

                if (infor) {
                    i = 1;                                                      // offset for standard FORs

                    if ((((for_stack *) addstk[savasp - 1])->type & 7) == FOR_TYP_0) {
                        i = 3;                                                  // FOR0 is a bit different
                        ssp = savssp;
                        asp = savasp;
                    }

                    rsmpc = ((for_stack *) addstk[savasp - 1])->quit - i;
                } else {
                    rsmpc = partab.jobtab->dostk[partab.jobtab->cur_do].endlin;
                }
            } else if (opc == OPIFI) {                                          // indirect
                rsmpc += sizeof(isp);                                           // skip the stored isp
            }

            break;

        case OPELSE:                                                            // ELSE
            partab.jobtab->commands++;                                          // count a command

            test = ((partab.jobtab->dostk[partab.jobtab->cur_do].test != -1) ?
                   partab.jobtab->dostk[partab.jobtab->cur_do].test : partab.jobtab->test);

            if (test != 0) {                                                    // check $TEST
                if (infor) {
                    i = 1;                                                      // offset for standard FORs

                    if ((((for_stack *) addstk[savasp - 1])->type & 7) == FOR_TYP_0) {
                        i = 3;                                                  // FOR0 is a bit different
                        ssp = savssp;
                        asp = savasp;
                    }

                    rsmpc = ((for_stack *) addstk[savasp - 1])->quit - i;
                } else {
                    rsmpc = partab.jobtab->dostk[partab.jobtab->cur_do].endlin;
                }
            }

            break;

        case OPADD:                                                             // ADD
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            ptr1 = (cstring *) addstk[--asp];
            ptr2 = (cstring *) addstk[--asp];
            p = ptr2->buf;
            s = ncopy(&p, temp);
            if (s < 0) ERROR(s);
            p = ptr1->buf;
            s = ncopy(&p, cptr->buf);
            if (s < 0) ERROR(s);
            s = runtime_add((char *) cptr->buf, (char *) temp);
            if (s < 0) ERROR(s);
            cptr->len = s;                                                      // save length
            ssp += s + sizeof(u_short) + 1;                                     // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;                                                              // done

        case OPSUB:                                                             // SUBTRACT
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            ptr2 = (cstring *) addstk[--asp];
            ptr1 = (cstring *) addstk[--asp];
            p = ptr2->buf;
            s = ncopy(&p, &temp[1]);
            if (s < 0) ERROR(s);
            p = ptr1->buf;
            s = ncopy(&p, cptr->buf);
            if (s < 0) ERROR(s);
            temp[0] = '-';
            s = 0;

            if ((temp[1] == '0') && (temp[2] == '\0')) {
                s = 1;
            } else if (temp[1] == '-') {
                s = 2;
            }

            s = runtime_add((char *) cptr->buf, (char *) &temp[s]);
            if (s < 0) ERROR(s);
            cptr->len = s;                                                      // save length
            ssp += s + sizeof(u_short) + 1;                                     // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;                                                              // done

        case OPMUL:                                                             // MULTIPLY
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            ptr1 = (cstring *) addstk[--asp];
            ptr2 = (cstring *) addstk[--asp];
            p = ptr2->buf;
            s = ncopy(&p, temp);
            if (s < 0) ERROR(s);
            p = ptr1->buf;
            s = ncopy(&p, cptr->buf);
            if (s < 0) ERROR(s);
            s = runtime_mul((char *) cptr->buf, (char *) temp);
            if (s < 0) ERROR(s);
            cptr->len = s;                                                      // save length
            ssp += s + sizeof(u_short) + 1;                                     // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;                                                              // done

        case OPDIV:                                                             // DIVIDE
        case OPINT:                                                             // INTEGER DIVIDE
        case OPMOD:                                                             // MODULUS
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            ptr2 = (cstring *) addstk[--asp];
            ptr1 = (cstring *) addstk[--asp];
            p = ptr2->buf;
            s = ncopy(&p, temp);
            if (s < 0) ERROR(s);
            p = ptr1->buf;
            s = ncopy(&p, cptr->buf);
            if (s < 0) ERROR(s);
            s = runtime_div((char *) cptr->buf, (char *) temp, opc);
            if (s < 0) ERROR(s);
            cptr->len = s;                                                      // save length
            ssp += s + sizeof(u_short) + 1;                                     // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;                                                              // done

        case OPPOW:                                                             // TO THE POWER
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            ptr2 = (cstring *) addstk[--asp];
            ptr1 = (cstring *) addstk[--asp];
            p = ptr2->buf;
            s = ncopy(&p, temp);
            if (s < 0) ERROR(s);
            p = ptr1->buf;
            s = ncopy(&p, cptr->buf);
            if (s < 0) ERROR(s);
            s = runtime_power((char *) cptr->buf, (char *) temp);
            if (s < 0) ERROR(s);
            cptr->len = s;                                                      // save length
            ssp += s + sizeof(u_short) + 1;                                     // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;                                                              // done

        case OPCAT:                                                             // CONCATENATE
            ptr2 = (cstring *) addstk[--asp];                                   // get second string ptr
            ptr1 = (cstring *) addstk[--asp];                                   // get first string ptr
            if ((ptr1->len + ptr2->len) > MAX_STR_LEN) ERROR(-ERRM75);          // string too long

            if (((u_char *) ptr1 >= partab.strstk_start) &&                     // if at or after strstk
              ((u_char *) ptr1 < partab.strstk_last) &&                         // and before end of strstk
              ((rsmpc < partab.strstk_start) || (rsmpc > partab.strstk_last))) { // ensure the rsmpc isn't also on the string stack
                cptr = ptr1;                                                    // use it in place
            } else {
                cptr = (cstring *) &strstk[ssp];                                // where we will put it
                t = mcopy(ptr1->buf, cptr->buf, ptr1->len);                     // copy first string
                if (t < 0) ERROR(t);                                            // check for error
                cptr->len = t;                                                  // save the length
            }

            t = mcopy(ptr2->buf, &cptr->buf[cptr->len], ptr2->len);             // and the 2nd
            if (t < 0) ERROR(t);                                                // check for error
            cptr->len = cptr->len + t;                                          // save the length
            ssp += cptr->len + sizeof(u_short) + 1;                             // point past it (sort of)
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case OPPLUS:                                                            // UNARY PLUS
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            ptr1 = (cstring *) addstk[--asp];                                   // get source string ptr
            p = ptr1->buf;                                                      // ptr for ncopy to play with
            s = ncopy(&p, cptr->buf);                                           // make a number
            if (s < 0) ERROR(s);
            cptr->len = s;                                                      // save the length
            ssp += s + sizeof(u_short) + 1;                                     // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case OPMINUS:                                                           // UNARY MINUS
            cptr = (cstring *) &strstk[ssp + 1];                                // where we will put it
            ptr1 = (cstring *) addstk[--asp];                                   // get source string ptr
            p = ptr1->buf;                                                      // ptr for ncopy to play with
            s = ncopy(&p, cptr->buf);                                           // make a number

            if ((s > 1) || (cptr->buf[0] != '0')) {                             // if it's not zero
                if (cptr->buf[0] == '-') {                                      // if there is a minus there
                    s--;                                                        // decrement count
                    cptr = (cstring *) ((u_char *) cptr + 1);                   // move up
                } else {                                                        // we need one
                    s++;                                                        // increment count
                    cptr = (cstring *) ((u_char *) cptr - 1);                   // move down
                    cptr->buf[0] = '-';                                         // stick in the minus
                }
            }

            cptr->len = s;                                                      // save the length
            ssp += s + sizeof(u_short) + 2;                                     // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case OPEQL:                                                             // EQUALS
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            cptr->len = 1;                                                      // the count
            cptr->buf[0] = '0';                                                 // assume false
            cptr->buf[1] = '\0';                                                // null terminate
            ptr2 = (cstring *) addstk[--asp];                                   // get second string ptr
            ptr1 = (cstring *) addstk[--asp];                                   // get first string ptr

            if (ptr1->len == ptr2->len) {                                       // if same length
                if (memcmp(ptr1->buf, ptr2->buf, ptr1->len) == 0) {
                    cptr->buf[0] = '1';                                         // they are the same
                }
            }

            ssp += sizeof(u_short) + 2;                                         // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case OPLES:                                                             // LESS THAN
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            ptr2 = (cstring *) addstk[--asp];
            ptr1 = (cstring *) addstk[--asp];
            p = ptr2->buf;
            s = ncopy(&p, temp);
            if (s < 0) ERROR(s);
            p = ptr1->buf;
            s = ncopy(&p, cptr->buf);
            if (s < 0) ERROR(s);
            cptr->len = 1;                                                      // the count
            cptr->buf[0] = (runtime_comp((char *) cptr->buf, (char *) temp) ? '1' : '0');
            cptr->buf[1] = '\0';                                                // null terminate
            ssp += sizeof(u_short) + 2;                                         // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case OPGTR:                                                             // GREATER THAN
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            ptr2 = (cstring *) addstk[--asp];
            ptr1 = (cstring *) addstk[--asp];
            p = ptr2->buf;
            s = ncopy(&p, temp);
            if (s < 0) ERROR(s);
            p = ptr1->buf;
            s = ncopy(&p, cptr->buf);
            if (s < 0) ERROR(s);
            cptr->len = 1;                                                      // the count
            cptr->buf[0] = (runtime_comp((char *) temp, (char *) cptr->buf) ? '1' : '0');
            cptr->buf[1] = '\0';                                                // null terminate
            ssp += sizeof(u_short) + 2;                                         // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case OPAND:                                                             // AND
            i = cstringtob((cstring *) addstk[--asp]);                          // get second arg
            j = cstringtob((cstring *) addstk[--asp]);                          // get first arg
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            cptr->len = 1;                                                      // the count
            cptr->buf[0] = '0';                                                 // assume false
            cptr->buf[1] = '\0';                                                // null terminate
            if ((i != 0) && (j != 0)) cptr->buf[0] = '1';                       // result true
            ssp += sizeof(u_short) + 2;                                         // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case OPIOR:                                                             // OR
            i = cstringtob((cstring *) addstk[--asp]);                          // get second arg
            j = cstringtob((cstring *) addstk[--asp]);                          // get first arg
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            cptr->len = 1;                                                      // the count
            cptr->buf[0] = '0';                                                 // assume false
            cptr->buf[1] = '\0';                                                // null terminate
            if ((i != 0) || (j != 0)) cptr->buf[0] = '1';                       // result true
            ssp += sizeof(u_short) + 2;                                         // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case OPXOR:                                                             // XOR
            ptr2 = (cstring *) addstk[--asp];                                   // get second string ptr
            ptr1 = (cstring *) addstk[--asp];                                   // get first string ptr
            i = cstringtob(ptr2);                                               // get second arg
            j = cstringtob(ptr1);                                               // get first arg
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            cptr->len = 1;                                                      // the count
            cptr->buf[0] = '0';                                                 // assume false
            cptr->buf[1] = '\0';                                                // null terminate

            if ((i != 0) || (j != 0)) {
                if (ptr1->len == ptr2->len) {                                   // if same length
                    if (bcmp(ptr1->buf, ptr2->buf, ptr1->len) == 0) {
                        cptr->buf[0] = '0';                                     // they are the same
                    } else {
                        cptr->buf[0] = '1';                                     // result true
                    }
                } else {
                    cptr->buf[0] = '1';                                         // result true
                }
            }

            ssp += sizeof(u_short) + 2;                                         // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case OPCON:                                                             // CONTAINS
            ptr2 = (cstring *) addstk[--asp];                                   // get second string ptr
            ptr1 = (cstring *) addstk[--asp];                                   // get first string ptr
            t = Dfind3x(ptr1, ptr2, 1);                                         // check it
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            cptr->len = 1;                                                      // the count
            cptr->buf[0] = ((t == 0) ? '0' : '1');                              // store the result
            cptr->buf[1] = '\0';                                                // null terminate
            ssp += sizeof(u_short) + 2;                                         // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case OPFOL:                                                             // FOLLOWS
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            cptr->len = 1;                                                      // the count
            cptr->buf[0] = '0';                                                 // assume false
            cptr->buf[1] = '\0';                                                // null terminate
            ptr2 = (cstring *) addstk[--asp];                                   // get second string ptr
            ptr1 = (cstring *) addstk[--asp];                                   // get first string ptr
            t = ptr1->len;                                                      // length of first string
            if (t > ptr2->len) t = ptr2->len;                                   // get the smallest
            i = memcmp(ptr1->buf, ptr2->buf, t);                                // compare them
            if (i > 0) cptr->buf[0] = '1';                                      // true
            if ((i == 0) && (ptr2->len < ptr1->len)) cptr->buf[0] = '1';        // also true
            ssp += sizeof(u_short) + 2;                                         // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case OPFOLEQL:                                                          // FOLLOWS or EQUALS
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            cptr->len = 1;                                                      // the count
            cptr->buf[0] = '0';                                                 // assume false
            cptr->buf[1] = '\0';                                                // null terminate
            ptr2 = (cstring *) addstk[--asp];                                   // get second string ptr
            ptr1 = (cstring *) addstk[--asp];                                   // get first string ptr
            t = ptr1->len;                                                      // length of first string
            if (t > ptr2->len) t = ptr2->len;                                   // get the smallest
            i = memcmp(ptr1->buf, ptr2->buf, t);                                // compare them
            if (i > 0) cptr->buf[0] = '1';                                      // true
            if ((i == 0) && (ptr2->len < ptr1->len)) cptr->buf[0] = '1';        // also true

            if (ptr1->len == ptr2->len) {                                       // if same length
                if (bcmp(ptr1->buf, ptr2->buf, ptr1->len) == 0) cptr->buf[0] = '1'; // they are the same
            }

            ssp += sizeof(u_short) + 2;                                         // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case OPSAF:                                                             // SORTS AFTER
            ptr2 = (cstring *) &strstk[ssp];                                    // where we put the second arg
            s = UTIL_Key_Build((cstring *) addstk[--asp], ptr2->buf);           // make a key out of it
            if (s < 0) ERROR(s);                                                // check for error
            ptr2->len = s;                                                      // save the length
            ssp += s + sizeof(u_short) + 1;                                     // move ssp along
            ptr1 = (cstring *) &strstk[ssp];                                    // where we put the first arg
            s = UTIL_Key_Build((cstring *) addstk[--asp], ptr1->buf);           // make a key out of it
            if (s < 0) ERROR(s)                                                 // check for error
            ptr1->len = s;                                                      // save the length
            ssp += s + sizeof(u_short) + 1;                                     // move ssp along
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            cptr->len = 1;                                                      // the count
            cptr->buf[0] = '1';                                                 // assume true
            cptr->buf[1] = '\0';                                                // null terminate
            t = ptr1->len;                                                      // length of first string
            if (t < ptr2->len) t = ptr2->len;                                   // get the smallest
            i = memcmp(ptr1->buf, ptr2->buf, t);                                // compare them
            if (i <= 0) cptr->buf[0] = '0';                                     // false
            if ((i == 0) && (ptr2->buf > ptr1->buf)) cptr->buf[0] = '0';        // also false
            ssp += sizeof(u_short) + 2;                                         // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case OPSAFEQL:                                                          // SORTS AFTER or EQUALS
            ptr2 = (cstring *) &strstk[ssp];                                    // where we put the second arg
            s = UTIL_Key_Build((cstring *) addstk[--asp], ptr2->buf);           // make a key out of it
            if (s < 0) ERROR(s)                                                 // check for error
            ptr2->len = s;                                                      // save the length
            ssp += s + sizeof(u_short) + 1;                                     // move ssp along
            ptr1 = (cstring *) &strstk[ssp];                                    // where we put the first arg
            s = UTIL_Key_Build((cstring *) addstk[--asp], ptr1->buf);           // make a key out of it
            if (s < 0) ERROR(s);                                                // check for error
            ptr1->len = s;                                                      // save the length
            ssp += s + sizeof(u_short) + 1;                                     // move ssp along
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            cptr->len = 1;                                                      // the count
            cptr->buf[0] = '1';                                                 // assume true
            cptr->buf[1] = '\0';                                                // null terminate
            t = ptr1->len;                                                      // length of first string
            if (t < ptr2->len) t = ptr2->len;                                   // get the smallest
            i = memcmp(ptr1->buf, ptr2->buf, t);                                // compare them
            if (i <= 0) cptr->buf[0] = '0';                                     // false
            if ((i == 0) && (ptr2->buf > ptr1->buf)) cptr->buf[0] = '0';        // also false

            if (ptr1->len == ptr2->len) {                                       // if same length
                if (bcmp(ptr1->buf, ptr2->buf, ptr1->len) == 0) cptr->buf[0] = '1'; // they are the same
            }

            ssp += sizeof(u_short) + 2;                                         // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case OPPAT:                                                             // PATTERN MATCHES
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            ptr2 = (cstring *) addstk[--asp];                                   // get second string ptr
            ptr1 = (cstring *) addstk[--asp];                                   // get first string ptr
            s = patmat(ptr1, ptr2);                                             // do it
            if (s < 0) ERROR(s);                                                // check for error
            cptr->len = 1;                                                      // the count
            cptr->buf[0] = '0' + s;                                             // s is either 1 or 0
            cptr->buf[1] = '\0';                                                // null terminate
            ssp += sizeof(u_short) + 2;                                         // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case OPHANG:                                                            // HANG
            partab.jobtab->commands++;                                          // count a command
            i = cstringtoi((cstring *) addstk[--asp]);                          // get the arg

            if (i < 1) {                                                        // zero value
                SchedYield();                                                   // give up slice
                break;                                                          // done
            }

            i = sleep(i);                                                       // sleep for i secs
            break;                                                              // done

        case OPNEQL:                                                            // NOT EQUALS
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            cptr->len = 1;                                                      // the count
            cptr->buf[0] = '1';                                                 // assume true
            cptr->buf[1] = '\0';                                                // null terminate
            ptr2 = (cstring *) addstk[--asp];                                   // get second string ptr
            ptr1 = (cstring *) addstk[--asp];                                   // get first string ptr

            if (ptr1->len == ptr2->len) {                                       // if same length
                if (memcmp(ptr1->buf, ptr2->buf, ptr1->len) == 0) {
                    cptr->buf[0] = '0';                                         // they are not the same
                }
            }

            ssp += sizeof(u_short) + 2;                                         // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case OPNLES:                                                            // NOT LESS THAN
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            ptr2 = (cstring *) addstk[--asp];
            ptr1 = (cstring *) addstk[--asp];
            p = ptr2->buf;
            s = ncopy(&p, temp);
            if (s < 0) ERROR(s);
            p = ptr1->buf;
            s = ncopy(&p, cptr->buf);
            if (s < 0) ERROR(s);
            cptr->len = 1;                                                      // the count
            cptr->buf[0] = (!runtime_comp((char *) cptr->buf, (char *) temp) ? '1' : '0');
            cptr->buf[1] = '\0';                                                // null terminate
            ssp += sizeof(u_short) + 2;                                         // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case OPNGTR:                                                            // NOT GREATER THAN
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            ptr2 = (cstring *) addstk[--asp];
            ptr1 = (cstring *) addstk[--asp];
            p = ptr2->buf;
            s = ncopy(&p, temp);
            if (s < 0) ERROR(s);
            p = ptr1->buf;
            s = ncopy(&p, cptr->buf);
            if (s < 0) ERROR(s);
            cptr->len = 1;                                                      // the count
            cptr->buf[0] = (!runtime_comp((char *) temp, (char *) cptr->buf) ? '1' : '0');
            cptr->buf[1] = '\0';                                                // null terminate
            ssp += sizeof(u_short) + 2;                                         // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case OPNAND:                                                            // NAND
            i = cstringtob((cstring *) addstk[--asp]);                          // get second arg
            j = cstringtob((cstring *) addstk[--asp]);                          // get first arg
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            cptr->len = 1;                                                      // the count
            cptr->buf[0] = '1';                                                 // assume true
            cptr->buf[1] = '\0';                                                // null terminate
            if ((i != 0) && (j != 0)) cptr->buf[0] = '0';                       // result false
            ssp += sizeof(u_short) + 2;                                         // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case OPNIOR:                                                            // NOR
            i = cstringtob((cstring *) addstk[--asp]);                          // get second arg
            j = cstringtob((cstring *) addstk[--asp]);                          // get first arg
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            cptr->len = 1;                                                      // the count
            cptr->buf[0] = '1';                                                 // assume true
            cptr->buf[1] = '\0';                                                // null terminate
            if ((i != 0) || (j != 0)) cptr->buf[0] = '0';                       // result false
            ssp += sizeof(u_short) + 2;                                         // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case OPNXOR:                                                            // XNOR
            ptr2 = (cstring *) addstk[--asp];                                   // get second string ptr
            ptr1 = (cstring *) addstk[--asp];                                   // get first string ptr
            i = cstringtob(ptr2);                                               // get second arg
            j = cstringtob(ptr1);                                               // get first arg
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            cptr->len = 1;                                                      // the count
            cptr->buf[0] = '1';                                                 // assume true
            cptr->buf[1] = '\0';                                                // null terminate

            if ((i != 0) || (j != 0)) {
                if (ptr1->len == ptr2->len) {                                   // if same length
                    if (bcmp(ptr1->buf, ptr2->buf, ptr1->len) == 0) {
                        cptr->buf[0] = '1';                                     // they are the same
                    } else {
                        cptr->buf[0] = '0';                                     // result true
                    }
                } else {
                    cptr->buf[0] = '0';                                         // result false
                }
            }

            ssp += sizeof(u_short) + 2;                                         // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case OPNCON:                                                            // NOT CONTAINS
            ptr2 = (cstring *) addstk[--asp];                                   // get second string ptr
            ptr1 = (cstring *) addstk[--asp];                                   // get first string ptr
            i = Dfind3x(ptr1, ptr2, 1);                                         // check it
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            cptr->len = 1;                                                      // the count
            cptr->buf[0] = ((i == 0) ? '1' : '0');                              // store the result
            cptr->buf[1] = '\0';                                                // null terminate
            ssp += sizeof(u_short) + 2;                                         // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case OPNFOL:                                                            // NOT FOLLOWS
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            cptr->len = 1;                                                      // the count
            cptr->buf[0] = '1';                                                 // assume true
            cptr->buf[1] = '\0';                                                // null terminate
            ptr2 = (cstring *) addstk[--asp];                                   // get second string ptr
            ptr1 = (cstring *) addstk[--asp];                                   // get first string ptr
            t = ptr1->len;                                                      // length of first string
            if (t > ptr2->len) t = ptr2->len;                                   // get the smallest
            i = memcmp(ptr1->buf, ptr2->buf, t);                                // compare them
            if (i > 0) cptr->buf[0] = '0';                                      // false
            if ((i == 0) && (ptr2->len < ptr1->len)) cptr->buf[0] = '0';        // also false
            ssp += sizeof(u_short) + 2;                                         // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case OPNSAF:                                                            // NOT SORTS AFTER
            ptr2 = (cstring *) &strstk[ssp];                                    // where we put the second arg
            s = UTIL_Key_Build((cstring *) addstk[--asp], ptr2->buf);           // make a key out of it
            if (s < 0) ERROR(s);                                                // check for error
            ptr2->len = s;                                                      // save the length
            ssp += s + sizeof(u_short) + 1;                                     // move ssp along
            ptr1 = (cstring *) &strstk[ssp];                                    // where we put the first arg
            s = UTIL_Key_Build((cstring *) addstk[--asp], ptr1->buf);           // make a key out of it
            if (s < 0) ERROR(s);                                                // check for error
            ptr1->len = s;                                                      // save the length
            ssp += s + sizeof(u_short) + 1;                                     // move ssp along
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            cptr->len = 1;                                                      // the count
            cptr->buf[0] = '0';                                                 // assume false
            cptr->buf[1] = '\0';                                                // null terminate
            t = ptr1->len;                                                      // length of first string
            if (t < ptr2->len) t = ptr2->len;                                   // get the smallest
            i = memcmp(ptr1->buf, ptr2->buf, t);                                // compare them
            if (i <= 0) cptr->buf[0] = '1';                                     // true
            if ((i == 0) && (ptr2->buf > ptr1->buf)) cptr->buf[0] = '1';        // also true
            ssp += sizeof(u_short) + 2;                                         // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case OPNPAT:                                                            // NOT PATTERN MATCHES
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            ptr2 = (cstring *) addstk[--asp];                                   // get second string ptr
            ptr1 = (cstring *) addstk[--asp];                                   // get first string ptr
            s = patmat(ptr1, ptr2);                                             // do it
            if (s < 0) ERROR(s);                                                // check for error
            cptr->len = 1;                                                      // the count
            cptr->buf[0] = '0' + !s;                                            // s is either 1 or 0
            cptr->buf[1] = '\0';                                                // null terminate
            ssp += sizeof(u_short) + 2;                                         // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case CMSET:                                                             // SET from within ()
            partab.jobtab->commands++;                                          // count a command
            var = (mvar *) addstk[--asp];                                       // the destination
            ptr1 = (cstring *) addstk[asp - 1];                                 // source - leave asp alone

            if (var->uci == UCI_IS_LOCALVAR) {                                  // if it's local
                if (var->name.var_cu[0] == '$') {
                    t = Vset(var, ptr1);                                        // do a special variable
                } else {
                    t = ST_Set(var, ptr1);                                      // do it - local
                }
            } else if (var->name.var_cu[0] == '$') {                            // SSVN?
                t = SS_Set(var, ptr1);                                          // do it - SSVN
            } else {
                memcpy(&partab.jobtab->last_ref, var, sizeof(var_u) + 5 + var->slen); // update naked
                t = DB_Set(var, ptr1);                                          // do it - global
            }

            if (t < 0) ERROR(t);                                                // complain on error
            break;

        case CMSETE:                                                            // SET $EXTRACT() - 3 args
        case CMSETP:                                                            // SET $PIECE() - 4 args
        case CMSETQS:                                                           // set $QSUBSCRIPT() - 2 args
            partab.jobtab->commands++;                                          // count a command
            p = &strstk[ssp];                                                   // some workspace

            if (opc != CMSETQS) {
                j = cstringtoi((cstring *) addstk[--asp]);                      // second numeric arg
                if (j > (MAX_STR_LEN + 1)) ERROR(-ERRM75)                       // check for too long
            }

            i = cstringtoi((cstring *) addstk[--asp]);                          // first numeric arg
            if (i > (MAX_STR_LEN + 1)) ERROR(-ERRM75)                           // check for too long
            ptr1 = NULL;                                                        // SET $EXTRACT or $QSUBSCRIPT

            if (opc == CMSETP) {
                ptr1 = (cstring *) addstk[--asp];                               // $PIECE delimiter

                if (((ptr1->len * i) > (MAX_STR_LEN + 1)) || ((ptr1->len * j) > (MAX_STR_LEN + 1))) {
                    ERROR(-ERRM75);
                }
            }

            var = (mvar *) addstk[--asp];                                       // the variable
            cptr = (cstring *) addstk[asp - 1];                                 // source - leave asp alone
            if (var->name.var_cu[0] == '$') ERROR(-ERRM8);                      // can't do that so complain

            if (opc == CMSETQS) {                                               // set $QSUBSCRIPT()
                if (i < -1) ERROR(-(ERRZ12 + ERRMLAST))                         // can't do that
                t = DSetqsubscript(p, cptr, var, i);                            // do a SET $QSUBSCRIPT()
            } else if (opc == CMSETP) {                                         // set $PIECE()
                t = DSetpiece(p, cptr, var, ptr1, i, j);                        // do a SET $PIECE()
            } else {                                                            // must be set $EXTRACT()
                t = DSetextract(p, cptr, var, i, j);                            // do a SET $EXTRACT()
            }

            if (t < 0) ERROR(t);                                                // complain on error
            break;

        case OPNAKED:                                                           // RESET NAKED pointer
            var = (mvar *) addstk[asp - 1];                                     // get the variable

            if ((var->uci != UCI_IS_LOCALVAR) && (var->name.var_cu[0] != '$')) { // a real global
                memcpy(&partab.jobtab->last_ref, var, sizeof(var_u) + 5 + var->slen); // update naked
            }

            break;                                                              // done

        case CMFLUSH:                                                           // FLUSH typeahead
            SQ_Flush();                                                         // do it
            break;                                                              // and quit

        case CMREADS:                                                           // READ star
            partab.jobtab->commands++;                                          // count a command
            var = (mvar *) addstk[--asp];                                       // get the variable
            hist = in_hist;
            in_hist = -1;
            s = SQ_ReadStar(&i, -1);                                            // read it in
            in_hist = hist;
            if (s < 0) ERROR(s);                                                // complain on error
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            cptr->len = itocstring(cptr->buf, i);                               // convert to string

            if (var->uci == UCI_IS_LOCALVAR) {                                  // if it's local
                t = ST_Set(var, cptr);                                          // do it - local
            } else if (var->name.var_cu[0] == '$') {                            // SSVN?
                t = SS_Set(var, cptr);                                          // do it - SSVN
            } else {
                memcpy(&partab.jobtab->last_ref, var, sizeof(var_u) + 5 + var->slen); // update naked
                t = DB_Set(var, cptr);                                          // do it - global
            }

            if (t < 0) ERROR(t);                                                // complain on error
            break;

        case CMREADST:                                                          // READ star with timeout
            partab.jobtab->commands++;                                          // count a command
            j = cstringtoi((cstring *) addstk[--asp]);                          // get timeout
            var = (mvar *) addstk[--asp];                                       // get the variable
            if (j < 0) j = 0;                                                   // check for negative and set to zero
            hist = in_hist;
            in_hist = -1;
            s = SQ_ReadStar(&i, j);                                             // read it in
            in_hist = hist;
            if (s < 0) ERROR(s);                                                // complain on error
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            cptr->len = itocstring(cptr->buf, i);                               // convert to string

            if (var->uci == UCI_IS_LOCALVAR) {                                  // if it's local
                t = ST_Set(var, cptr);                                          // do it - local
            } else if (var->name.var_cu[0] == '$') {                            // SSVN?
                t = SS_Set(var, cptr);                                          // do it - SSVN
            } else {
                memcpy(&partab.jobtab->last_ref, var, sizeof(var_u) + 5 + var->slen); // update naked
                t = DB_Set(var, cptr);                                          // do it - global
            }

            if (t < 0) ERROR(t);                                                // complain on error
            break;

        case CMREAD:                                                            // READ
            partab.jobtab->commands++;                                          // count a command
            var = (mvar *) addstk[--asp];                                       // get the variable
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            hist = in_hist;
            in_hist = -1;
            t = SQ_Read(cptr->buf, -1, -1);                                     // read it in
            in_hist = hist;
            if (t < 0) ERROR(t);                                                // complain on error
            cptr->len = t;                                                      // save the length

            if (var->uci == UCI_IS_LOCALVAR) {                                  // if it's local
                t = ST_Set(var, cptr);                                          // do it - local
            } else if (var->name.var_cu[0] == '$') {                            // SSVN?
                t = SS_Set(var, cptr);                                          // do it - SSVN
            } else {
                memcpy(&partab.jobtab->last_ref, var, sizeof(var_u) + 5 + var->slen); // update naked
                t = DB_Set(var, cptr);                                          // do it - global
            }

            if (t < 0) ERROR(t);                                                // complain on error
            break;

        case CMREADT:                                                           // READ with timeout
            partab.jobtab->commands++;                                          // count a command
            j = cstringtoi((cstring *) addstk[--asp]);                          // get timeout
            if (j < 0) j = 0;                                                   // check for negative so set to zero
            var = (mvar *) addstk[--asp];                                       // get the variable
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            hist = in_hist;
            in_hist = -1;
            t = SQ_Read(cptr->buf, j, -1);                                      // read it in
            in_hist = hist;
            if (t < 0) ERROR(t);                                                // complain on error
            cptr->len = t;                                                      // save the length

            if (var->uci == UCI_IS_LOCALVAR) {                                  // if it's local
                t = ST_Set(var, cptr);                                          // do it - local
            } else if (var->name.var_cu[0] == '$') {                            // SSVN?
                t = SS_Set(var, cptr);                                          // do it - SSVN
            } else {
                memcpy(&partab.jobtab->last_ref, var, sizeof(var_u) + 5 + var->slen); // update naked
                t = DB_Set(var, cptr);                                          // do it - global
            }

            if (t < 0) ERROR(t);                                                // complain on error
            break;

        case CMREADC:                                                           // READ with count
            partab.jobtab->commands++;                                          // count a command
            i = cstringtoi((cstring *) addstk[--asp]);                          // get count
            var = (mvar *) addstk[--asp];                                       // get the variable
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            hist = in_hist;
            in_hist = -1;
            t = SQ_Read(cptr->buf, -1, i);                                      // read it in
            in_hist = hist;
            if (t < 0) ERROR(t);                                                // complain on error
            cptr->len = t;                                                      // save the length

            if (var->uci == UCI_IS_LOCALVAR) {                                  // if it's local
                t = ST_Set(var, cptr);                                          // do it - local
            } else if (var->name.var_cu[0] == '$') {                            // SSVN?
                t = SS_Set(var, cptr);                                          // do it - SSVN
            } else {
                memcpy(&partab.jobtab->last_ref, var, sizeof(var_u) + 5 + var->slen); // update naked
                t = DB_Set(var, cptr);                                          // do it - global
            }

            if (t < 0) ERROR(t);                                                // complain on error
            break;

        case CMREADCT:                                                          // READ with count & timeout
            partab.jobtab->commands++;                                          // count a command
            j = cstringtoi((cstring *) addstk[--asp]);                          // get timeout
            i = cstringtoi((cstring *) addstk[--asp]);                          // get count
            var = (mvar *) addstk[--asp];                                       // get the variable
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            hist = in_hist;
            in_hist = -1;
            t = SQ_Read(cptr->buf, j, i);                                       // read it in
            in_hist = hist;
            if (t < 0) ERROR(t);                                                // complain on error
            cptr->len = t;                                                      // save the length

            if (var->uci == UCI_IS_LOCALVAR) {                                  // if it's local
                t = ST_Set(var, cptr);                                          // do it - local
            } else if (var->name.var_cu[0] == '$') {                            // SSVN?
                t = SS_Set(var, cptr);                                          // do it - SSVN
            } else {
                memcpy(&partab.jobtab->last_ref, var, sizeof(var_u) + 5 + var->slen); // update naked
                t = DB_Set(var, cptr);                                          // do it - global
            }

            if (t < 0) ERROR(t);                                                // complain on error
            break;

        case CMWRTST:                                                           // WRITE star
            partab.jobtab->commands++;                                          // count a command
            i = cstringtoi((cstring *) addstk[--asp]);                          // get the argument
            s = SQ_WriteStar((u_char) i);                                       // do it
            if (s < 0) ERROR(s);                                                // complain on error
            break;

        case CMWRTNL:                                                           // WRITE !
            partab.jobtab->commands++;                                          // count a command
            s = SQ_WriteFormat(SQ_LF);                                          // do it
            if (s < 0) ERROR(s);                                                // complain on error
            break;

        case CMWRTFF:                                                           // WRITE #
            partab.jobtab->commands++;                                          // count a command
            s = SQ_WriteFormat(SQ_FF);                                          // do it
            if (s < 0) ERROR(s);                                                // complain on error
            break;

        case CMWRTAB:                                                           // WRITE ?expr
            partab.jobtab->commands++;                                          // count a command
            i = cstringtoi((cstring *) addstk[--asp]);                          // get the value
            if (i < 1) break;                                                   // ingore junk
            s = SQ_WriteFormat(i);                                              // do it
            if (s < 0) ERROR(s);                                                // complain on error
            break;

        case CMWRTEX:                                                           // WRITE expr
            partab.jobtab->commands++;                                          // count a command
            t = SQ_Write((cstring *) addstk[--asp]);                            // do it
            if (t < 0) ERROR(t);                                                // complain on error
            break;

        case CMUSE:                                                             // USE (args) ch, a1, a2, ...
            partab.jobtab->commands++;                                          // count a command
            VAR_CLEAR(rou);                                                     // clear this
            args = (int) *rsmpc++;                                              // number of args
            ptr1 = (cstring *) NULL;                                            // default to nothing
            ptr2 = (cstring *) NULL;                                            // default to nothing
            i = 0;                                                              // other parameters

            while (args) {                                                      // for each argument
                tmp = (cstring *) addstk[--asp];                                // get an argument
                args--;                                                         // count it

                if (strncasecmp((const char *) tmp->buf, "terminator=", 11) == 0) {
                    ptr1 = (cstring *) &strstk[ssp];                            // where we will put it
                    t = mcopy(&tmp->buf[11], ptr1->buf, tmp->len - 11);
                    if (t < 0) ERROR(t);                                        // die on error
                    ptr1->len = t;
                } else if (strncasecmp((const char *) tmp->buf, "output=", 7) == 0) {
                    ptr2 = (cstring *) &strstk[ssp + 256];                      // where we will put it
                    t = mcopy(&tmp->buf[7], ptr2->buf, tmp->len - 7);
                    if (t < 0) ERROR(t);                                        // die on error
                    ptr2->len = t;
                } else if (strncasecmp((const char *) tmp->buf, "escape", 6) == 0) {
                    i |= SQ_USE_ESCAPE;
                } else if (strncasecmp((const char *) tmp->buf, "noescape", 8) == 0) {
                    i |= SQ_USE_NOESCAPE;
                } else if (strncasecmp((const char *) tmp->buf, "echo", 4) == 0) {
                    i |= SQ_USE_ECHO;
                } else if (strncasecmp((const char *) tmp->buf, "noecho", 6) == 0) {
                    i |= SQ_USE_NOECHO;
                } else if (strncasecmp((const char *) tmp->buf, "disconnect", 10) == 0) {
                    i |= SQ_USE_DISCON;
                } else if (strncasecmp((const char *) tmp->buf, "delete=none", 11) == 0) {
                    i |= SQ_USE_DELNONE;
                } else if (strncasecmp((const char *) tmp->buf, "delete=back", 11) == 0) {
                    i |= SQ_USE_DEL8;
                } else if (strncasecmp((const char *) tmp->buf, "delete=delete", 13) == 0) {
                    i |= SQ_USE_DEL127;
                } else if (strncasecmp((const char *) tmp->buf, "delete=both", 11) == 0) {
                    i |= SQ_USE_DELBOTH;
                } else if (strncasecmp((const char *) tmp->buf, "controlc", 8) == 0) {
                    i |= SQ_CONTROLC;
                } else if (strncasecmp((const char *) tmp->buf, "nocontrolc", 10) == 0) {
                    i |= SQ_NOCONTROLC;
                } else if (strncasecmp((const char *) tmp->buf, "controlt", 8) == 0) {
                    i |= SQ_CONTROLT;
                } else if (strncasecmp((const char *) tmp->buf, "nocontrolt", 10) == 0) {
                    i |= SQ_NOCONTROLT;
                } else if (strncasecmp((const char *) tmp->buf, "namespace=", 10) == 0) {
                    p = &tmp->buf[10];                                          // point past the =

                    for (j = 0; j < VAR_LEN; j++) {                             // scan the remainder
                        if (!p[j]) break;                                       // quit when done
                        if (!j && (p[j] != '%') && !isalpha(p[j])) ERROR(-ERRM36); // complain if invalid
                        if (j && !isalnum(p[j])) ERROR(-ERRM36);                // complain if invalid
                        ((u_char *) &rou)[j] = p[j];                            // copy one char
                    }
                } else {
                    ERROR(-(ERRZ13 + ERRMLAST));                                // else error
                }
            }

            j = cstringtoi((cstring *) addstk[--asp]);                          // finally get chan#
            s = SQ_Use(j, ptr1, ptr2, i);                                       // do it
            if (s < 0) ERROR(s);                                                // complain on error

            if (!var_empty(rou)) {                                              // if a routine was specified
                VAR_COPY(partab.jobtab->seqio[partab.jobtab->io].namespace, rou);
            }

            break;

        case CMOPEN:                                                            // OPEN, ch, p1, p2, to, mnspc
            partab.jobtab->commands++;                                          // count a command
            VAR_CLEAR(rou);                                                     // clear this
            cptr = (cstring *) addstk[--asp];                                   // get the namespace
            i = cstringtoi((cstring *) addstk[--asp]);                          // get the timeout
            ptr2 = (cstring *) addstk[--asp];                                   // get second string ptr
            ptr1 = (cstring *) addstk[--asp];                                   // get first string ptr
            j = cstringtoi((cstring *) addstk[--asp]);                          // get the ch#
            s = SQ_Open(j, ptr1, ptr2, i);                                      // do it
            if (s < 0) ERROR(s);                                                // complain on error

            if (cptr->len && (strncasecmp((const char *) cptr->buf, "namespace=", 10) == 0)) {
                p = &cptr->buf[10];                                             // point past the =

                for (i = 0; i < VAR_LEN; i++) {                                 // scan the remainder
                    if (!p[i]) break;                                           // quit when done
                    if (!i && (p[i] != '%') && !isalpha(p[i])) ERROR(-ERRM36);  // complain if invalid
                    if (i && !isalnum(p[i])) ERROR(-ERRM36);                    // complain if invalid
                    ((u_char *) &rou)[i] = p[i];                                // copy one char
                }
            }

            VAR_COPY(partab.jobtab->seqio[j].namespace, rou);
            break;                                                              // done

        case CMCLOSE:                                                           // CLOSE channel
            partab.jobtab->commands++;                                          // count a command
            j = cstringtoi((cstring *) addstk[--asp]);                          // get the ch#
            s = SQ_Close(j);                                                    // do it
            if (s < 0) ERROR(s);                                                // complain on error
            break;                                                              // done

        case OPSTR:                                                             // STRING FOLLOWS in line
            addstk[asp++] = rsmpc;                                              // store the address
            rsmpc = rsmpc + ((cstring *) rsmpc)->len + sizeof(u_short) + 1;     // point past the string
            break;

        case OPVAR:                                                             // VAR name
            s = buildmvar(&partab.src_var, 0, asp);                             // build mvar from the code
            if (s < 0) ERROR(s);                                                // complain on error
            asp = s;                                                            // restore returned asp
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it

            if (partab.src_var.uci == UCI_IS_LOCALVAR) {
                t = ST_Get(&partab.src_var, cptr->buf);                         // do it - local
            } else if (partab.src_var.name.var_cu[0] == '$') {                  // SSVN?
                t = SS_Get(&partab.src_var, cptr->buf);                         // do it - SSVN
            } else {
                memcpy(&partab.jobtab->last_ref, &partab.src_var, sizeof(var_u) + 5 + partab.src_var.slen); // update naked
                t = DB_Get(&partab.src_var, cptr->buf);                         // do it - global
            }

            if (t < 0) ERROR(t);                                                // complain on error
            cptr->len = t;                                                      // save the length
            ssp += t + sizeof(u_short) + 1;                                     // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case OPMVAR:                                                            // BUILD mvar on the strstk
        case OPMVARN:                                                           // BUILD mvar (null subs OK)
        case OPMVARF:                                                           // BUILD mvar on the strstk
            var = (mvar *) &strstk[ssp];                                        // where we will put it
            s = buildmvar(var, (opc == OPMVARN), asp);                          // build (chk null OK)
            if (s < 0) ERROR(s);                                                // complain on error
            asp = s;                                                            // restore returned asp
            addstk[asp++] = (u_char *) var;                                     // stack it

            if (opc == OPMVARF) {                                               // if reqd
                ssp += sizeof(mvar);                                            // allow maximum size
            } else {                                                            // else 'normal' mvar
                ssp += var->slen + sizeof(var_u) + (5 * sizeof(u_char));
            }

            break;

        case INDEVAL:                                                           // EVAL NAME INDIRECTION - comp as: expr INDEVAL
            cptr = (cstring *) addstk[--asp];                                   // get string to eval
            if (INDSNOK(cptr->len)) ERROR(-(ERRZ58 + ERRMLAST));                // too much indirection
            source_ptr = cptr->buf;                                             // what to compile
            comp_ptr = &indstk[isp];                                            // where it goes
            for (i = 0; *source_ptr == '@'; i++) source_ptr++;                  // count INDEVALs

            if (i) {
                source_ptr--;                                                   // let localvar handle the last @
                s = localvar();                                                 // attempt to compile it
                if (s < 0) ERROR(s);                                            // complain on error
                while (i--) *comp_ptr++ = INDEVAL;                              // even more indirection!
            } else {
                eval();                                                         // attempt to compile it
            }

            if (*source_ptr != '\0') ERROR(-(ERRZ57 + ERRMLAST));               // must point at end of var or complain
            if (INDANOK(comp_ptr)) ERROR(-(ERRZ58 + ERRMLAST));                 // too much indirection
            *comp_ptr++ = INDREST;                                              // restore things
            assert(sizeof(isp) == sizeof(long));
            memcpy(comp_ptr, &isp, sizeof(long));                               // the isp to restore
            comp_ptr += sizeof(long);
            assert(sizeof(rsmpc) == sizeof(u_char *));
            memcpy(comp_ptr, &rsmpc, sizeof(u_char *));                         // and the rsmpc
            comp_ptr += sizeof(u_char *);
            rsmpc = &indstk[isp];                                               // what we are going to do
            isp = comp_ptr - &indstk[isp] + isp;                                // adjust isp
            break;                                                              // go do it

        case INDMVAR:                                                           // BUILD mvar from @...@(...)
        case INDMVARN:                                                          // ditto, null OK
        case INDMVARF:                                                          // ditto, full size
            cptr = (cstring *) addstk[--asp];                                   // get string to eval
            if (INDSNOK(cptr->len)) ERROR(-(ERRZ58 + ERRMLAST));                // too much indirection
            source_ptr = cptr->buf;                                             // what to compile
            comp_ptr = &indstk[isp];                                            // where it goes
            for (i = 0; *source_ptr == '@'; i++) source_ptr++;                  // count indirection levels
            if (i) source_ptr--;                                                // let localvar handle the last @
            s = localvar();                                                     // compile it
            if (s < 0) ERROR(s);                                                // complain on error

            if (i && (*(comp_ptr - 2) != TYPVARIND)) {
                while (i--) *comp_ptr++ = INDEVAL;                              // even more indirection!
                *comp_ptr++ = INDMVAR;
            } else {
                indstk[isp + s] = OPMVAR;                                       // change to OPMVAR
                if (opc == INDMVARN) indstk[isp + s] = OPMVARN;                 // if null ok then change to OPMVARN
                if (opc == INDMVARF) indstk[isp + s] = OPMVARF;                 // full size? then change to OPMVARF
            }

            if (*source_ptr != '\0') ERROR(-(ERRZ57 + ERRMLAST));               // must point at end of var then complain
            if (INDANOK(comp_ptr)) ERROR(-(ERRZ58 + ERRMLAST));                 // too much indirection
            *comp_ptr++ = INDREST;                                              // restore things
            assert(sizeof(isp) == sizeof(long));
            memcpy(comp_ptr, &isp, sizeof(long));                               // the isp to restore
            comp_ptr += sizeof(long);
            assert(sizeof(rsmpc) == sizeof(u_char *));
            memcpy(comp_ptr, &rsmpc, sizeof(u_char *));                         // and the rsmpc
            comp_ptr += sizeof(u_char *);
            rsmpc = &indstk[isp];                                               // what we are going to do
            isp = comp_ptr - &indstk[isp] + isp;                                // adjust isp
            break;

        case OPDUPASP:                                                          // DUPLICATE top of addstk[]
            addstk[asp] = addstk[asp - 1];                                      // duplicate it
            asp++;                                                              // point past
            break;                                                              // and continue

        case OPBRK0:                                                            // BREAK now
            partab.jobtab->commands++;                                          // count a command
            s = Debug(savasp, savssp, 1);                                       // go for it
            if (s < 0) ERROR(s);                                                // complain on error
            if (s == OPHALT) return OPHALT;
            break;                                                              // and exit

        case OPBRKN:                                                            // Modify BREAKPOINTS
            partab.jobtab->commands++;                                          // count a command
            ptr1 = (cstring *) addstk[--asp];                                   // get the string

            if (ptr1->len == 0) {
                Debug_off();                                                    // turn it off
                break;                                                          // continue
            }

            s = Debug_on(ptr1);                                                 // turn it on
            if (s < 0) ERROR(s);                                                // complain on error
            break;                                                              // continue

        // ***** Start of Special Variables *****
        case VARD:                                                              // $D[EVICE]
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            t = SQ_Device(cptr->buf);                                           // do it in seqio
            if (t < 0) ERROR(t);                                                // complain on error
            cptr->len = t;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case VAREC:                                                             // $EC[ODE]
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            t = Vecode(cptr->buf);                                              // get the info
            if (t < 0) ERROR(t);                                                // complain on error
            cptr->len = t;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case VARES:                                                             // $ES[TACK]
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            cptr->len = itocstring(cptr->buf, partab.jobtab->cur_do - partab.jobtab->dostk[partab.jobtab->cur_do].estack);
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case VARET:                                                             // $ET[RAP]
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            t = Vetrap(cptr->buf);                                              // get the info
            if (t < 0) ERROR(t);                                                // complain on error
            cptr->len = t;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case VARH:                                                              // $H[OROLOG]
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Vhorolog(cptr->buf);                                            // get the info
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case VARI:                                                              // $I[O]
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            cptr->len = uitocstring(cptr->buf, partab.jobtab->io);              // return count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case VARJ:                                                              // $J[OB]
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            cptr->len = itocstring(cptr->buf, partab.jobtab - systab->jobtab + 1); // return it
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case VARK:                                                              // $K[EY]
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Vkey(cptr->buf);                                                // get the info
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case VARP:                                                              // $P[RINCIPAL]
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            cptr->len = 1;                                                      // the count
            cptr->buf[0] = '0';                                                 // always zero
            cptr->buf[1] = '\0';                                                // null terminate
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case VARQ:                                                              // $Q[UIT]
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            cptr->buf[0] = '0';

            if (partab.jobtab->dostk[partab.jobtab->cur_do].type == TYPE_EXTRINSIC) {
                cptr->buf[0] = '1';
            }

            cptr->buf[1] = '\0';
            cptr->len = 1;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case VARR:                                                              // $R[EFERENCE]
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Vreference(cptr->buf);                                          // get the info
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case VARS:                                                              // $S[TORAGE]
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = 0;

            for (i = 0; i < STORAGE; i++) {
                if (symtab[i].data == NULL) s++;
            }

            cptr->len = itocstring(cptr->buf, s);
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case VARST:                                                             // $ST[ACK]
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            cptr->len = itocstring(cptr->buf, partab.jobtab->cur_do);           // return size
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case VARSY:                                                             // $SY[STEM]
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Vsystem(cptr->buf);                                             // get the info
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case VART:                                                              // $T[EST]
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            cptr->buf[0] = '0';                                                 // assume zero

            test = ((partab.jobtab->dostk[partab.jobtab->cur_do].test != -1) ?
                   partab.jobtab->dostk[partab.jobtab->cur_do].test : partab.jobtab->test);

            if (test != 0) cptr->buf[0] = '1';                                  // but, if true then it's a one
            cptr->buf[1] = '\0';
            cptr->len = 1;
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case VARX:                                                              // $X
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Vx(cptr->buf);                                                  // get the info
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case VARY:                                                              // $Y
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Vy(cptr->buf);                                                  // get the info
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        // ***** Start of Functions *****
        case FUNA1:                                                             // $A[SCII] 1 arg
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Dascii1(cptr->buf, (cstring *) addstk[--asp]);                  // do it
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNA2:                                                             // $A[SCII] 2 arg
            i = cstringtoi((cstring *) addstk[--asp]);                          // get second arg
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Dascii2(cptr->buf, (cstring *) addstk[--asp], i);               // do it
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNC:                                                              // $C[HARACTER]
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            cptr->len = 0;                                                      // clear the count
            args = *rsmpc++;                                                    // get arg count

            for (i = 0; i < args; i++) {
                s = Dchar(&cptr->buf[cptr->len], cstringtoi((cstring *) addstk[asp - args + i])); // do it
                cptr->len += s;                                                 // add the count
            }

            asp -= args;                                                        // remove the args
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUND:                                                              // $D[ATA]
            var = (mvar *) addstk[--asp];                                       // get the variable pointer
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Ddata(cptr->buf, var);                                          // do it
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNE1:                                                             // $E[XTRACT] 1 arg
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            t = Dextract(cptr->buf, (cstring *) addstk[--asp], 1, 1);           // do it
            if (t < 0) ERROR(t);                                                // complain on error
            cptr->len = t;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNE2:                                                             // $E[XTRACT] 2 arg
            i = cstringtoi((cstring *) addstk[--asp]);                          // get second arg
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            t = Dextract(cptr->buf, (cstring *) addstk[--asp], i, i);           // do it
            if (t < 0) ERROR(t);                                                // complain on error
            cptr->len = t;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNE3:                                                             // $E[XTRACT] 3 arg
            j = cstringtoi((cstring *) addstk[--asp]);                          // get third arg
            i = cstringtoi((cstring *) addstk[--asp]);                          // get second arg
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            t = Dextract(cptr->buf, (cstring *) addstk[--asp], i, j);           // do it
            if (t < 0) ERROR(t);                                                // complain on error
            cptr->len = t;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNF2:                                                             // $F[IND] 2 arg
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            ptr1 = (cstring *) addstk[--asp];                                   // get arg 2
            t = Dfind2(cptr->buf, (cstring *) addstk[--asp], ptr1);             // do it
            if (t < 0) ERROR(t);                                                // complain on error
            cptr->len = t;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNF3:                                                             // $F[IND] 3 arg
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            i = cstringtoi((cstring *) addstk[--asp]);                          // get third arg
            ptr1 = (cstring *) addstk[--asp];                                   // get arg 2
            t = Dfind3(cptr->buf, (cstring *) addstk[--asp], ptr1, i);          // do it
            if (t < 0) ERROR(t);                                                // complain on error
            cptr->len = t;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNFN2:                                                            // $FN[UMBER] 2 arg
            ptr2 = (cstring *) addstk[--asp];                                   // get arg 2
            cptr = (cstring *) addstk[--asp];
            p = cptr->buf;
            ptr1 = (cstring *) &strstk[ssp];
            s = ncopy(&p, ptr1->buf);                                           // convert to canonic number
            if (s < 0) ERROR(s);                                                // complain on error
            ptr1->len = s;
            ssp += sizeof(u_short) + ptr1->len + 1;                             // point past it
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            t = Dfnumber2(cptr->buf, ptr1, ptr2);                               // do it
            if (t < 0) ERROR(t);                                                // complain on error
            cptr->len = t;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNFN3:                                                            // $FN[UMBER] 3 arg
            i = cstringtoi((cstring *) addstk[--asp]);                          // get third arg
            ptr2 = (cstring *) addstk[--asp];                                   // get arg 2
            cptr = (cstring *) addstk[--asp];
            p = cptr->buf;
            ptr1 = (cstring *) &strstk[ssp];
            s = ncopy(&p, ptr1->buf);                                           // convert to canonic number
            if (s < 0) ERROR(s);                                                // complain on error
            ptr1->len = s;
            ssp += sizeof(u_short) + ptr1->len + 1;                             // point past it
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            t = Dfnumber3(cptr->buf, ptr1, ptr2, i);                            // do it
            if (t < 0) ERROR(t);                                                // complain on error
            cptr->len = t;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNG1:                                                             // $G[ET] 1 arg
            var = (mvar *) addstk[--asp];                                       // get the variable pointer
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            t = Dget1(cptr->buf, var);                                          // do it
            if (t < 0) ERROR(t);                                                // complain on error
            cptr->len = t;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNG2:                                                             // $G[ET] 2 arg
            ptr1 = (cstring *) addstk[--asp];                                   // get arg 2
            var = (mvar *) addstk[--asp];                                       // get first arg
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            t = Dget2(cptr->buf, var, ptr1);                                    // do it
            if (t < 0) ERROR(t);                                                // complain on error
            cptr->len = t;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNI1:                                                             // $I[NCREMENT] 1 arg
            var = (mvar *) addstk[--asp];                                       // get the variable pointer
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Dincrement1(cptr->buf, var);                                    // do it
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNI2:                                                             // $I[NCREMENT] 2 arg
            cptr = (cstring *) addstk[--asp];
            p = cptr->buf;
            ptr1 = (cstring *) &strstk[ssp];
            s = ncopy(&p, ptr1->buf);                                           // convert to canonic number
            if (s < 0) ERROR(s);                                                // complain on error
            ptr1->len = s;
            ssp += sizeof(u_short) + ptr1->len + 1;                             // point past it
            var = (mvar *) addstk[--asp];                                       // get first arg
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Dincrement2(cptr->buf, var, ptr1);                              // do it
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNJ2:                                                             // $J[USTIFY] 2 arg
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            i = cstringtoi((cstring *) addstk[--asp]);                          // get second arg
            t = Djustify2(cptr->buf, (cstring *) addstk[--asp], i);             // do it
            if (t < 0) ERROR(t);                                                // complain on error
            cptr->len = t;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNJ3:                                                             // $J[USTIFY] 3 arg
            j = cstringtoi((cstring *) addstk[--asp]);                          // get third arg
            i = cstringtoi((cstring *) addstk[--asp]);                          // get second arg
            cptr = (cstring *) addstk[--asp];
            p = cptr->buf;
            ptr1 = (cstring *) &strstk[ssp];
            s = ncopy(&p, ptr1->buf);                                           // convert to canonic number
            if (s < 0) ERROR(s);                                                // complain on error
            ptr1->len = s;
            ssp += sizeof(u_short) + ptr1->len + 1;                             // point past it
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            t = Djustify3(cptr->buf, ptr1, i, j);                               // do it
            if (t < 0) ERROR(t);                                                // complain on error
            cptr->len = t;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNL1:                                                             // $L[ENGTH] 1 arg
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Dlength1(cptr->buf, (cstring *) addstk[--asp]);                 // do it
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNL2:                                                             // $L[ENGTH] 2 arg
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            ptr1 = (cstring *) addstk[--asp];                                   // get arg 2
            s = Dlength2(cptr->buf, (cstring *) addstk[--asp], ptr1);           // do it
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNNA1:                                                            // $NA[ME] 1 arg
            var = (mvar *) addstk[--asp];                                       // get the variable pointer
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Dname1(cptr->buf, var);                                         // do it
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNNA2:                                                            // $NA[ME] 2 arg
            i = cstringtoi((cstring *) addstk[--asp]);                          // get second arg
            var = (mvar *) addstk[--asp];                                       // get first arg
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Dname2(cptr->buf, var, i);                                      // do it
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNO1:                                                             // $O[RDER] 1 arg
            var = (mvar *) addstk[--asp];                                       // get first arg
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Dorder1(cptr->buf, var);                                        // do it
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNO2:                                                             // $O[RDER] 2 arg
            i = cstringtoi((cstring *) addstk[--asp]);                          // get second arg
            var = (mvar *) addstk[--asp];                                       // get first arg
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Dorder2(cptr->buf, var, i);                                     // do it
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNP2:                                                             // $P[IECE] 2 arg
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            ptr1 = (cstring *) addstk[--asp];                                   // get arg 2
            t = Dpiece2(cptr->buf, (cstring *) addstk[--asp], ptr1);            // do it
            if (t < 0) ERROR(t);                                                // complain on error
            cptr->len = t;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNP3:                                                             // $P[IECE] 3 arg
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            i = cstringtoi((cstring *) addstk[--asp]);                          // get third arg
            ptr1 = (cstring *) addstk[--asp];                                   // get arg 2
            t = Dpiece3(cptr->buf, (cstring *) addstk[--asp], ptr1, i);         // do it
            if (t < 0) ERROR(t);                                                // complain on error
            cptr->len = t;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNP4:                                                             // $P[IECE] 4 arg
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            j = cstringtoi((cstring *) addstk[--asp]);                          // get fourth arg
            i = cstringtoi((cstring *) addstk[--asp]);                          // get third arg
            ptr1 = (cstring *) addstk[--asp];                                   // get arg 2
            t = Dpiece4(cptr->buf, (cstring *) addstk[--asp], ptr1, i, j);      // do it
            if (t < 0) ERROR(t);                                                // complain on error
            cptr->len = t;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNQL:                                                             // $QL[ENGTH]
            cptr = (cstring *) addstk[--asp];                                   // the argument
            var = (mvar *) &strstk[ssp];                                        // some space
            s = UTIL_MvarFromCStr(cptr, var);                                   // convert to an mvar
            if (s < 0) ERROR(s);                                                // complain on error
            cptr = (cstring *) var;                                             // where the answer goes
            cptr->len = itocstring(cptr->buf, s);                               // the subs count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNQS:                                                             // $QS[UBSCRIPT]
            j = cstringtoi((cstring *) addstk[--asp]);                          // get second arg
            if (j < -1) ERROR(-(ERRZ12 + ERRMLAST));                            // can't do that
            ptr1 = (cstring *) addstk[--asp];                                   // the first argument
            var = (mvar *) &strstk[ssp];                                        // some space
            s = UTIL_MvarFromCStr(ptr1, var);                                   // convert to an mvar
            if (s < 0) ERROR(s);                                                // complain on error
            ssp += sizeof(var_u) + 4 + var->slen;                               // protect the mvar
            cptr = (cstring *) &strstk[ssp];                                    // where the answer goes
            addstk[asp++] = (u_char *) cptr;                                    // stack it

            if (j == -1) {                                                      // "environment" reqd
                if ((var->uci == UCI_IS_LOCALVAR) || (var->uci == 0)) {         // if it's local or no UCI there
                    cptr->len = 0;                                              // length 0
                    cptr->buf[0] = '\0';                                        // null terminated
                    ssp += sizeof(u_short) + cptr->len + 1;                     // point past it
                    break;                                                      // done
                }

                i = 0;                                                          // clear the counter

                if (var->volset) {                                              // vol specified ?
                    if (systab->vol[var->volset - 1] == NULL) ERROR(-ERRM26);   // not there
                    list = &systab->vol[var->volset - 1]->vollab->uci[var->uci - 1].name; // UCI table

                    for (args = 0; args < VAR_LEN; args++) {                    // use a random int
                        if (list->var_cu[args] == 0) break;                     // quit on null
                        cptr->buf[i++] = list->var_cu[args];                    // copy a character
                    }

                    cptr->buf[i++] = ',';                                       // add the comma
                    list = &systab->vol[var->volset - 1]->vollab->volnam;       // point at volume name

                    for (args = 0; args < VAR_LEN; args++) {                    // use a random int
                        if (list->var_cu[args] == 0) break;                     // quit on null
                        cptr->buf[i++] = list->var_cu[args];                    // copy a character
                    }
                }                                                               // end volset processing

                cptr->buf[i] = '\0';                                            // null terminate
                cptr->len = i;                                                  // the length
                ssp += sizeof(u_short) + cptr->len + 1;                         // point past it
                break;
            }                                                                   // end environment stuff

            if (j == 0) {                                                       // the name?
                int k = 0;

                if (var->uci != UCI_IS_LOCALVAR) {                              // if it's a global
                    cptr->buf[0] = '^';                                         // copy the caret
                    k++;                                                        // increment past it
                }

                for (i = 0; i < VAR_LEN; i++) {                                 // max VAR_LEN chars
                    if (var->name.var_cu[i] == '\0') break;                     // done
                    cptr->buf[i + k] = var->name.var_cu[i];                     // copy a character
                }

                cptr->buf[i + k] = '\0';                                        // null terminate
                cptr->len = i + k;                                              // save the length
                ssp += sizeof(u_short) + cptr->len + 1;                         // point past it
                break;                                                          // done
            }

            if (j > s) {                                                        // no such subscript?
                cptr->len = 0;                                                  // length 0
                cptr->buf[0] = '\0';                                            // null terminated
                ssp += sizeof(u_short) + cptr->len + 1;                         // point past it
                break;                                                          // done
            }

            args = 0;                                                           // clear key index

            while (j) {                                                         // look for the subscript
                i = 0;                                                          // don't do the rabbit's ears
                s = UTIL_Key_Extract(&var->key[args], cptr->buf, &i);           // get key from here
                args += i;                                                      // add key bytes used
                --j;                                                            // decrement subscript count
            }

            cptr->len = s;                                                      // store the length
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            break;

        case FUNQ1:                                                             // $Q[UERY] 1 arg
        case FUNQ2:                                                             // $Q[UERY] 2 arg
            i = 1;                                                              // default direction
            if (opc == FUNQ2) i = cstringtoi((cstring *) addstk[--asp]);        // if 2 arg form then get second arg
            var = (mvar *) addstk[--asp];                                       // get first arg
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Dquery2(cptr->buf, var, i);                                     // do it
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNR:                                                              // $R[ANDOM]
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            i = cstringtoi((cstring *) addstk[--asp]);                          // get arg
            s = Drandom(cptr->buf, i);                                          // do it
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNRE:                                                             // $RE[VERSE]
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            t = Dreverse(cptr->buf, (cstring *) addstk[--asp]);                 // do it
            if (t < 0) ERROR(t);                                                // complain on error
            cptr->len = t;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNST1:                                                            // $ST[ACK] - 1 arg
            i = cstringtoi((cstring *) addstk[--asp]);                          // get arg
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Dstack1(cptr->buf, i);                                          // do it
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNST2:                                                            // $ST[ACK] - 2 arg
            ptr1 = (cstring *) addstk[--asp];                                   // get arg 2
            i = cstringtoi((cstring *) addstk[--asp]);                          // get arg
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            t = Dstack2(cptr->buf, i, ptr1);                                    // do it
            if (t < 0) ERROR(t);                                                // complain on error
            cptr->len = t;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNT:                                                              // $T[EXT]
            ptr1 = (cstring *) addstk[--asp];                                   // get the arg
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            t = Dtext(cptr->buf, ptr1);                                         // do it
            if (t < 0) ERROR(t);                                                // complain on error
            cptr->len = t;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNTR2:                                                            // $TR[ANSLATE] 2 arg
            ptr1 = (cstring *) addstk[--asp];                                   // get arg 2
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            t = Dtranslate2(cptr->buf, (cstring *) addstk[--asp], ptr1);        // do it
            if (t < 0) ERROR(t);                                                // complain on error
            cptr->len = t;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNTR3:                                                            // $TR[ANSLATE] 3 arg
            ptr2 = (cstring *) addstk[--asp];                                   // get arg 3
            ptr1 = (cstring *) addstk[--asp];                                   // get arg 2
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            t = Dtranslate3(cptr->buf, (cstring *) addstk[--asp], ptr1, ptr2);  // do it
            if (t < 0) ERROR(t);                                                // complain on error
            cptr->len = t;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case FUNV2:                                                             // $V[IEW] - 2 arg
        case FUNV3:                                                             // $V[IEW] - 3 arg
        case FUNV4:                                                             // $V[IEW] - 4 arg
            ptr1 = NULL;                                                        // default type to write
            j = 1;                                                              // default size to 1 byte
            if (opc == FUNV4) ptr1 = (cstring *) addstk[--asp];                 // get write data

            if ((opc == FUNV3) || (opc == FUNV4)) {
                j = cstringtoi((cstring *) addstk[--asp]);                      // get size
            }

            i = cstringtoi((cstring *) addstk[--asp]);                          // get location
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            t = Dview(cptr->buf, cstringtoi((cstring *) addstk[--asp]), i, j, ptr1); // do it
            if (t < 0) ERROR(t);                                                // complain on error
            cptr->len = t;                                                      // the count
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        // ***** Start of Commands *****
        case CMVIEW:                                                            // VIEW
            partab.jobtab->commands++;                                          // count a command
            args = *rsmpc++;                                                    // get arg count

            if (args == 2) {                                                    // only form currently
                j = cstringtoi((cstring *) addstk[--asp]);                      // get arg 2 (block)
                i = cstringtoi((cstring *) addstk[--asp]);                      // get arg 1 (vol)
                if (i > -1) ERROR(-(ERRZ63 + ERRMLAST));                        // junk
                i = -i;                                                         // negate i
                if (i > MAX_VOL) ERROR(-(ERRZ63 + ERRMLAST));                   // junk

                if (j > -1) {                                                   // a release or get?
                    if (partab.jobtab->view[i - 1] != NULL) {                   // anything there?
                        DB_ViewRel(i, partab.jobtab->view[i - 1]);              // yes - release it
                    }

                    partab.jobtab->view[i - 1] = NULL;                          // say it's released
                    if (j == 0) break;                                          // done if release
                    partab.jobtab->view[i - 1] = DB_ViewGet(i, j);              // get another block

                    if (partab.jobtab->view[i - 1] == NULL) {                   // failed?
                        ERROR(-(ERRZ63 + ERRMLAST));                            // die
                    }

                    break;                                                      // and exit
                }

                j = -j;                                                         // negate block number

                if (partab.jobtab->view[i - 1] == NULL) {                       // do we have a block?
                    ERROR(-(ERRZ63 + ERRMLAST));                                // no - die
                }

                if ((int) partab.jobtab->view[i - 1]->block != j) {
                    ERROR(-(ERRZ63 + ERRMLAST));                                // wrong block - die
                }

                DB_ViewPut(i, partab.jobtab->view[i - 1]);                      // mark for write
                partab.jobtab->view[i - 1] = NULL;                              // say it's released
                break;                                                          // and exit
            }

            ERROR(-(ERRZ63 + ERRMLAST));                                        // general VIEW error

        case CMMERGE:                                                           // MERGE 1 var from next
            partab.jobtab->commands++;                                          // count a command

            // Note: The below two mvars have been pre-expanded to maximum size (i.e., sizeof(mvar)).
            var = (mvar *) addstk[--asp];                                       // get the dest mvar ptr
            var2 = (mvar *) addstk[--asp];                                      // get the source mvar ptr

            if (var->name.var_cu[0] == '$') {                                   // dest is SSVN
                if (toupper(var->name.var_cu[1]) != 'R') ERROR(-ERRM29);        // must be ^$R()
                t = Compile_Routine(var, var2, &strstk[ssp]);                   // compile this routine
                if (t < 0) ERROR(t);                                            // give up on error
                break;
            }

            if (var2->name.var_cu[0] == '$') ERROR(-ERRM29);                    // source is SSVN so can't do that (except above)
            s = Ddata(temp, var2);                                              // see if source exists
            if (s < 0) ERROR(s);                                                // complain on error
            if (temp[0] == '0') break;                                          // quit if no such
            cptr = (cstring *) &strstk[ssp];                                    // somewhere to put this

            if (var2->uci == UCI_IS_LOCALVAR) {
                t = ST_Get(var2, cptr->buf);                                    // get this one local
            } else {
                t = DB_Get(var2, cptr->buf);                                    // get this one global
            }

            if ((t < 0) && (t != -ERRM6) && (t != -ERRM7)) ERROR(t);            // error other than UNDEF
            if (t >= 0) {                                                       // was defined
                cptr->len = t;                                                  // save the length

                if (var->uci == UCI_IS_LOCALVAR) {
                    t = ST_Set(var, cptr);                                      // set local
                } else {
                    t = DB_Set(var, cptr);                                      // set local
                }

                if (t < 0) ERROR(t);                                            // die on error
            }

            i = var->slen;                                                      // dest key size
            j = var2->slen;                                                     // source key size
            memcpy(temp, var2->key, j);                                         // save source key

            while (TRUE) {                                                      // all of them
                if (var2->uci == UCI_IS_LOCALVAR) {
                    t = ST_QueryD(var2, cptr->buf);                             // get next local
                } else {
                    t = DB_QueryD(var2, cptr->buf);                             // get next global
                }

                if (t == -(ERRZ55 + ERRMLAST)) break;                           // done (ran out)
                if (t < 0) ERROR(t);                                            // die on error
                if (memcmp(var2->key, temp, j)) break;                          // all done
                cptr->len = t;                                                  // save the length
                memmove(&var->key[i], &var2->key[j], var2->slen - j);           // from end of src key to end of dest key
                if ((i + var2->slen - j) > MAX_KEY_SIZE) ERROR(-(ERRZ2 + ERRMLAST)); // complain if too big
                var->slen = i + var2->slen - j;                                 // get the length of the new

                if (var->uci == UCI_IS_LOCALVAR) {
                    t = ST_Set(var, cptr);                                      // set local
                } else {
                    t = DB_Set(var, cptr);                                      // set global
                }

                if (t < 0) ERROR(t);                                            // die on error
            }

            break;                                                              // end of merge

        case CMDOWRT:                                                           // from a WRITE /...
        case CMDOTAG:                                                           // DO tag in this rou
        case CMDOROU:                                                           // DO routine (no tag)
        case CMDORT:                                                            // DO routine, tag
        case CMDORTO:                                                           // DO routine, tag, off
        case CMDON:                                                             // DO - no arguments
            partab.jobtab->commands++;                                          // count a command
            offset = 0;                                                         // clear offset
            VAR_CLEAR(tag);                                                     // clear tag

            if (opc == CMDOWRT) {
                VAR_COPY(rou, partab.jobtab->seqio[partab.jobtab->io].namespace);
            } else {
                VAR_COPY(rou, partab.jobtab->dostk[partab.jobtab->cur_do].rounam); // default to current routine

                if ((opc == CMDOROU) || (opc == CMDORT) || (opc == CMDORTO)) {
                    assert(sizeof(rou) == VAR_LEN);

                    if (*rsmpc == OPERROR) {
                        s = *(short *) ++rsmpc;                                 // get the routine name error
                        ERROR(s);
                    }

                    memcpy(&rou, rsmpc, VAR_LEN);                               // get routine name
                    rsmpc += VAR_LEN;
                }

                if ((opc == CMDORTO) && (var_empty(rou))) {                     // could be zero from this op
                    VAR_COPY(rou, partab.jobtab->dostk[partab.jobtab->cur_do].rounam); // reset to current
                }
            }

            if ((opc == CMDOTAG) || (opc == CMDORT) || (opc == CMDORTO) || (opc == CMDOWRT)) {
                assert(sizeof(tag) == VAR_LEN);

                if (*rsmpc == OPERROR) {
                    s = *(short *) ++rsmpc;                                     // get the tag error
                    ERROR(s);
                }

                memcpy(&tag, rsmpc, VAR_LEN);                                   // get tag name
                rsmpc += VAR_LEN;
            }

            if (opc == CMDORTO) {                                               // if there is one
                if (!(systab->historic & HISTORIC_OFFOK)) {
                    ERROR(-(ERRZ70 + ERRMLAST));                                // if not permitted then complain
                }

                assert(sizeof(us) == sizeof(u_short));
                memcpy(&us, rsmpc, sizeof(u_short));                            // get the offset
                rsmpc += sizeof(u_short);
                offset = us;
            }

            args = 0;                                                           // asume no args
            if (opc != CMDON) args = *rsmpc++;                                  // if not argless type then get the arg count
            if ((args || var_empty(tag)) && offset) ERROR(-ERRM20);             // can't do that

            if ((partab.jobtab->cur_do + 1) == MAX_DO_FRAMES) {
                ERROR(-(ERRZ7 + ERRMLAST));                                     // too many
            }

            if (var_empty(rou) && (opc != CMDON)) {                             // check for no such
                for (i = partab.jobtab->cur_do - 1; i > 0; i--) {
                    if (!var_empty(partab.jobtab->dostk[i].rounam)) {
                        VAR_COPY(rou, partab.jobtab->dostk[i].rounam);
                        break;
                    }
                }
            }

            if (var_empty(rou)) ERROR(-ERRM13);                                 // check for no such, if so give up
            partab.jobtab->dostk[partab.jobtab->cur_do].pc = rsmpc;             // save current pc
            partab.jobtab->cur_do++;                                            // increment do level
            curframe = &partab.jobtab->dostk[partab.jobtab->cur_do];            // point at it
            rouadd = NULL;                                                      // clear rouadd

            for (i = partab.jobtab->cur_do - 1; i > 0; i--) {
                if ((var_equal(rou, partab.jobtab->dostk[i].rounam)) &&
                  (partab.jobtab->ruci == partab.jobtab->dostk[i].uci) &&
                  (partab.jobtab->rvol == partab.jobtab->dostk[i].vol)) {
                    memcpy(curframe, &partab.jobtab->dostk[i], sizeof(do_frame));
                    curframe->flags = 0;                                        // flag no attach etc.
                    rouadd = (rbd *) partab.jobtab->dostk[i].routine;           // setup rou addr
                    break;                                                      // copy and exit
                }
            }

            if (rouadd == NULL) {                                               // if the above failed
                rouadd = Routine_Attach(rou);                                   // attach to it

                if (rouadd == NULL) {                                           // check for no such
                    partab.jobtab->cur_do--;                                    // back to original frame
                    ERROR(-ERRM13);                                             // give up
                }

                if (rouadd == (rbd *) -1) {                                     // no space
                    partab.jobtab->cur_do--;                                    // back to original frame
                    ERROR(-(ERRZ52 + ERRMLAST));                                // give up
                }

                if (rouadd == (rbd *) -2) {                                     // wrong version
                    partab.jobtab->cur_do--;                                    // back to original frame
                    ERROR(-(ERRZ59 + ERRMLAST));                                // give up
                }

                curframe->routine = (u_char *) rouadd;                          // save address
                curframe->symbol = NULL;                                        // symbol table
                VAR_COPY(curframe->rounam, rou);                                // routine name
                curframe->vol = partab.jobtab->rvol;                            // current volset
                curframe->uci = partab.jobtab->ruci;                            // current UCI
                curframe->flags = DO_FLAG_ATT;                                  // flag an attach
            }                                                                   // end get new one

            if (infor) {                                                        // if in a for loop
                curframe->flags |= DO_FLAG_FOR;                                 // remember that
                infor = 0;                                                      // and clear the for flag
            }

            curframe->pc = &((u_char *) rouadd)[rouadd->code];                  // save start pc

            if (!var_empty(tag)) {                                              // tag specified ?
                ttbl = (tags *) &((u_char *) rouadd)[rouadd->tag_tbl];
                j = 0;                                                          // setup j as a flag

                for (i = 0; i < rouadd->num_tags; i++) {                        // scan the tags
                    if (var_equal(ttbl[i].name, tag)) {                         // found it
                        curframe->pc += ttbl[i].code;                           // adjust pc
                        j = 1;                                                  // flag ok
                        break;                                                  // and exit
                    }
                }

                if (j == 0) {                                                   // if that didn't work
                    partab.jobtab->cur_do--;                                    // back to original frame
                    ERROR(-ERRM13);                                             // give up
                }

                while (offset) {                                                // if there is an offset
                    i = 0;                                                      // clear this
                    if (*curframe->pc == LOADARG) i = curframe->pc[1] + 2;      // if args then point at LINENUM

                    if (curframe->pc[i] != LINENUM) {                           // check this
                        partab.jobtab->cur_do--;                                // back to original frame
                        ERROR(-ERRM13);                                         // fail
                    }

                    curframe->pc = &curframe->pc[i + 3];                        // point at offset
                    i = *(u_short *) curframe->pc;                              // get it
                    curframe->pc = &curframe->pc[i + 1];                        // point at next line
                    offset--;                                                   // decrement the offset
                }                                                               // end offset junk
            }

            curframe->newtab = NULL;                                            // where news go
            curframe->estack = partab.jobtab->dostk[partab.jobtab->cur_do - 1].estack;
            curframe->line_num = 1;                                             // current routine line#
            curframe->type = ((args & 128) ? TYPE_EXTRINSIC : TYPE_DO);         // how we got here
            curframe->level = 0;                                                // no dots

            if (opc == CMDON) {                                                 // argless do?
                curframe->level = partab.jobtab->dostk[partab.jobtab->cur_do - 1].level + 1;
                curframe->pc = partab.jobtab->dostk[partab.jobtab->cur_do - 1].endlin;
                p = curframe->pc;                                               // the new pc
                if (!*p) p++;                                                   // skip possible eol
                if (*p == LINENUM) p += sizeof(short) * 2 + 1;

                if (*p != CHKDOTS) {
                    partab.jobtab->cur_do--;                                    // back to original frame
                    rsmpc = partab.jobtab->dostk[partab.jobtab->cur_do].pc;
                    break;
                }
            }

            if ((curframe->symbol == NULL) && rouadd->num_vars) {               // need symbol space? and have any vars?
                curframe->symbol = malloc(rouadd->num_vars * sizeof(short));    // symbol index space
                for (i = 0; i < rouadd->num_vars; i++) curframe->symbol[i] = -1; // for each one, mark not setup
            }

            rsmpc = curframe->pc;                                               // get the new pc
            args &= 127;                                                        // clear $$ bit of count

            if (args > 0) {                                                     // check for args
                if (*rsmpc++ != LOADARG) {                                      // any there?
                    if (curframe->symbol != NULL) {
                        free(curframe->symbol);
                        curframe->symbol = NULL;
                    }

                    partab.jobtab->cur_do--;                                    // point back
                    s = -ERRM58;                                                // default error

                    if (*--rsmpc == OPERROR) {                                  // if an error there
                        rsmpc++;                                                // point back at error
                        s = *(short *) rsmpc;                                   // get it
                    }

                    ERROR(s);                                                   // complain
                }

                j = *rsmpc++;                                                   // number of them

                if ((args - 1) > j) {                                           // too many supplied?
                    /*
                    if (curframe->symbol != NULL) {
                        free(curframe->symbol);                                 // DLW - causes segfault in some cases (M errors)
                        curframe->symbol = NULL;
                    }
                    */

                    curframe->symbol = NULL;
                    partab.jobtab->cur_do--;                                    // point back
                    ERROR(-ERRM58);                                             // complain
                }

                list = (var_u *) &strstk[ssp];                                  // where we put this
                VAR_CLEAR((*list));
                vt = (var_u *) (((u_char *) rouadd) + rouadd->var_tbl);
                for (i = 0; i < j; i++) VAR_COPY(list[i], vt[rsmpc[i]]);        // for each arg, get the var name
                s = ST_New(j, list);                                            // new them
                if (s < 0) ERROR(s);                                            // complain on error
                var = (mvar *) &strstk[ssp];                                    // get some space
                VAR_CLEAR(var->name);                                           // clear the name
                var->uci = UCI_IS_LOCALVAR;                                     // all locals
                var->slen = 0;                                                  // no subscripts
                t = 0;                                                          // clear error flag

                for (i = args - 2; i >= 0; --i) {                               // for each supplied arg
                    var->volset = rsmpc[i] + 1;                                 // get the index
                    cptr = (cstring *) addstk[--asp];                           // get data ptr

                    if (cptr != NULL) {                                         // normal data type?
                        if (cptr->len != VAR_UNDEFINED) {
                            t = ST_Set(var, cptr);                              // set it
                            if (t < 0) break;                                   // exit on error
                        }
                    } else {                                                    // must be by reference
                        p = addstk[--asp];                                      // the data pointer
                        cptr = (cstring *) addstk[--asp];                       // get real data ptr
                        var->name = ((mvar *) cptr)->name;                      // copy the name
                        t = ST_ConData(var, p);                                 // connect them
                        if (t < 0) break;                                       // exit on error
                        VAR_CLEAR(var->name);                                   // clear the name for more
                    }
                }

                if (t < 0) ERROR(t);                                            // exit on error
                rsmpc += j;                                                     // skip args
            }

            if ((curframe->type & TYPE_EXTRINSIC) || curframe->level) {         // if it's extrinsic or argless do
                curframe->flags &= ~DO_FLAG_TEST;                               // clear test bit

                if (partab.jobtab->dostk[partab.jobtab->cur_do].test != -1) {
                    curframe->flags |= partab.jobtab->dostk[partab.jobtab->cur_do].test; // set $TEST
                } else {
                    curframe->flags |= partab.jobtab->test;                     // set $TEST
                }
            }

            p = rsmpc;                                                          // the new pc
            if (!*p) p++;                                                       // skip possible eol
            if (*p++ == LINENUM) curframe->line_num = *(u_short *) p;           // store the line number
            curframe->savasp = savasp;                                          // save
            curframe->savssp = savssp;                                          // save
            curframe->asp = asp;                                                // save
            curframe->ssp = ssp;                                                // save
            curframe->isp = isp;                                                // and indirect stack ptr
            savasp = asp;                                                       // use these in
            savssp = ssp;                                                       // the subrou
            break;                                                              // return to interp

        case CMJOBTAG:                                                          // JOB tag in this rou
        case CMJOBROU:                                                          // JOB routine (no tag)
        case CMJOBRT:                                                           // JOB routine, tag
        case CMJOBRTO:                                                          // JOB routine, tag, off
            partab.jobtab->commands++;                                          // count a command
            offset = 0;                                                         // clear offset
            VAR_COPY(rou, partab.jobtab->dostk[partab.jobtab->cur_do].rounam);

            // default to current routine
            VAR_CLEAR(tag);                                                     // clear tag

            if ((opc == CMJOBROU) || (opc == CMJOBRT) || (opc == CMJOBRTO)) {
                assert(sizeof(rou) == VAR_LEN);
                memcpy(&rou, rsmpc, VAR_LEN);                                   // get routine name
                rsmpc += VAR_LEN;
            }

            if ((opc == CMJOBRTO) && var_empty(rou)) {                          // could be zero from this op
                VAR_COPY(rou, partab.jobtab->dostk[partab.jobtab->cur_do].rounam);
            }                                                                   // reset to current

            if ((opc == CMJOBTAG) || (opc == CMJOBRT) || (opc == CMJOBRTO)) {
                assert(sizeof(tag) == VAR_LEN);
                memcpy(&tag, rsmpc, VAR_LEN);                                   // get tag name
                rsmpc += VAR_LEN;
            }

            j = -1;                                                             // timeout (if any)

            if (opc == CMJOBRTO) {                                              // if there is one
                if (!(systab->historic & HISTORIC_OFFOK)) {                     // if not permitted
                    ERROR(-(ERRZ70 + ERRMLAST));                                // complain
                }

                assert(sizeof(us) == sizeof(u_short));
                memcpy(&us, rsmpc, sizeof(u_short));                            // get the offset
                rsmpc += sizeof(u_short);
                offset = us;
            }

            args = *rsmpc++;                                                    // get argument count

            if (args & 128) {                                                   // timeout specified ?
                j = cstringtoi((cstring *) addstk[--asp]);                      // get the timeout
                args &= 127;                                                    // clear timeout flag

                if (partab.jobtab->dostk[partab.jobtab->cur_do].test != -1) {
                    partab.jobtab->dostk[partab.jobtab->cur_do].test = 1;       // ALWAYS WORKS ???
                } else {
                    partab.jobtab->test = 1;                                    // ALWAYS WORKS ???
                }
            }

            i = ForkIt(0);                                                      // fork with no file table
            if (i > 0) break;                                                   // check for ok (parent)

            if (i == 0) {                                                       // die on error
                if (errno) ERROR(-(ERRMLAST + ERRZLAST + errno));
                ERROR(-(ERRZ49 + ERRMLAST));                                    // job table full
            }

            i = 0;                                                              // strstk ptr
            strstk[i++] = 'D';                                                  // pretend it's a DO
            strstk[i++] = ' ';                                                  // and a space
            list = (var_u *) &tag;                                              // point at the tag
            j = 0;                                                              // clear pointer

            while ((j < VAR_LEN) && (list->var_cu[j] != 0)) {
                strstk[i++] = list->var_cu[j++];                                // copy it
            }

            if (offset) {                                                       // if we have an offset
                strstk[i++] = '+';                                              // add the plus
                i += itocstring(&strstk[i], offset);                            // and the number
            }

            strstk[i++] = '^';                                                  // the ^
            list = (var_u *) &rou;                                              // point at the rou
            j = 0;                                                              // clear pointer

            while ((j < VAR_LEN) && (list->var_cu[j] != 0)) {
                strstk[i++] = list->var_cu[j++];                                // copy it
            }

            if (args) {
                strstk[i++] = '(';                                              // an open bracket

                for (j = args - 1; j > 0; --j) {                                // for each arg
                    strstk[i++] = '"';                                          // quote it
                    cptr = (cstring *) addstk[asp - j];                         // get the arg
                    memmove(&strstk[i], cptr->buf, cptr->len);                  // copy the arg
                    i = i + cptr->len;                                          // count it
                    strstk[i++] = '"';                                          // close the quote
                    strstk[i++] = ',';                                          // add a comma
                }

                --i;                                                            // backup over last comma
                strstk[i++] = ')';                                              // the close bracket
            }

            strstk[i] = '\0';                                                   // null terminate it
            return JOBIT;                                                       // get it going

        case CMGOTAG:                                                           // GOTO tag in this rou
        case CMGOROU:                                                           // GOTO routine (no tag)
        case CMGORT:                                                            // GOTO routine, tag
        case CMGORTO:                                                           // GOTO routine, tag, off
            partab.jobtab->commands++;                                          // count a command

            while (infor) {                                                     // if in a for
                forx = (for_stack *) addstk[--asp];                             // get current
                infor = forx->type & FOR_NESTED;                                // any more?
                ssp = ((u_char *) forx - strstk);                               // reset string stack
                savssp = ssp;                                                   // save that
                savasp = asp;                                                   // and that
            }

            offset = 0;                                                         // clear offset
            VAR_COPY(rou, partab.jobtab->dostk[partab.jobtab->cur_do].rounam);

            // default to current routine
            VAR_CLEAR(tag);                                                     // clear tag

            if ((opc == CMGOROU) || (opc == CMGORT) || (opc == CMGORTO)) {
                assert(sizeof(rou) == VAR_LEN);
                memcpy(&rou, rsmpc, VAR_LEN);                                   // get routine name
                rsmpc += VAR_LEN;
            }

            if ((opc == CMGORTO) && (var_empty(rou))) {                         // could be zero from this op
                VAR_COPY(rou, partab.jobtab->dostk[partab.jobtab->cur_do].rounam);
            }                                                                   // reset to current

            if ((opc == CMGOTAG) || (opc == CMGORT) || (opc == CMGORTO)) {
                assert(sizeof(tag) == VAR_LEN);
                memcpy(&tag, rsmpc, VAR_LEN);                                   // get tag name
                rsmpc += VAR_LEN;
            }

            if (opc == CMGORTO) {                                               // if there is one
                if (var_empty(tag)) ERROR(-ERRM20);                             // can't do that

                if (!(systab->historic & HISTORIC_OFFOK)) {                     // if not permitted
                    ERROR(-(ERRZ70 + ERRMLAST));                                // complain
                }

                assert(sizeof(us) == sizeof(u_short));
                memcpy(&us, rsmpc, sizeof(u_short));                            // get the offset
                rsmpc += sizeof(u_short);
                offset = us;
            }

            curframe = &partab.jobtab->dostk[partab.jobtab->cur_do];            // point at it
            rouadd = NULL;                                                      // clear rouadd

            if (var_empty(rou)) {                                               // check for no such (Xecute)
                for (i = partab.jobtab->cur_do - 1; i > 0; i--) {
                    if (!var_empty(partab.jobtab->dostk[i].rounam)) {
                        VAR_COPY(rou, partab.jobtab->dostk[i].rounam);
                        break;
                    }
                }
            }

            if (var_empty(rou)) ERROR(-ERRM13);                                 // check for no such and give up
            if (!var_equal(rou, partab.jobtab->dostk[partab.jobtab->cur_do].rounam) &&
              partab.jobtab->dostk[partab.jobtab->cur_do].level) {
                ERROR(-ERRM14);                                                 // can't GOTO from ....
            }

            for (i = partab.jobtab->cur_do; i > 0; i--) {
                if (var_equal(rou, partab.jobtab->dostk[i].rounam) &&
                  (partab.jobtab->ruci == partab.jobtab->dostk[i].uci) &&
                  (partab.jobtab->rvol == partab.jobtab->dostk[i].vol)) {
                    rouadd = (rbd *) partab.jobtab->dostk[i].routine;           // remember rou

                    if (i != partab.jobtab->cur_do) {
                        if (curframe->flags & DO_FLAG_ATT) {                    // if current was an attach
                            ST_SymDet(((rbd *) curframe->routine)->num_vars, curframe->symbol); // detach symbols
                            Routine_Detach((rbd *) curframe->routine);          // detach it
                        }

                        curframe->routine = (u_char *) rouadd;                  // where the routine is
                        curframe->symbol = partab.jobtab->dostk[i].symbol;      // same symb
                        VAR_COPY(curframe->rounam, rou);                        // the routine name
                        curframe->vol = partab.jobtab->rvol;
                        curframe->uci = partab.jobtab->ruci;
                        curframe->flags &= ~DO_FLAG_ATT;                        // flag no attach not same frame
                    }

                    break;                                                      // setup attach and exit
                }
            }

            if (rouadd == NULL) {                                               // if the above failed
                rouadd = Routine_Attach(rou);                                   // attach to it

                if (rouadd == NULL) ERROR(-ERRM13);                             // check for no such and give up
                if (rouadd == (rbd *) -1) ERROR(-(ERRZ52 + ERRMLAST));          // no space so give up
                if (rouadd == (rbd *) -2) ERROR(-(ERRZ59 + ERRMLAST));          // wrong version so give up

                if (curframe->flags & DO_FLAG_ATT) {                            // if current was an attach
                    ST_SymDet(((rbd *) curframe->routine)->num_vars, curframe->symbol); // detach symbols
                    Routine_Detach((rbd *) curframe->routine);                  // detach it
                }

                curframe->routine = (u_char *) rouadd;                          // save address
                curframe->symbol = NULL;                                        // symbol table
                VAR_COPY(curframe->rounam, rou);                                // routine name
                curframe->vol = partab.jobtab->rvol;                            // current volset
                curframe->uci = partab.jobtab->ruci;                            // current UCI
                curframe->flags |= DO_FLAG_ATT;                                 // flag an attach
            }                                                                   // end get new one

            curframe->pc = &((u_char *) rouadd)[rouadd->code];                  // save start pc

            if (!var_empty(tag)) {                                              // tag specified ?
                ttbl = (tags *) &((u_char *) rouadd)[rouadd->tag_tbl];
                j = 0;                                                          // setup j as a flag

                for (i = 0; i < rouadd->num_tags; i++) {                        // scan the tags
                    if (var_equal(ttbl[i].name, tag)) {                         // found it
                        curframe->pc = curframe->pc + ttbl[i].code;             // adjust pc
                        j = 1;                                                  // flag ok
                        break;                                                  // and exit
                    }
                }

                if (j == 0) ERROR(-ERRM13);                                     // if that didn't work then give up

                while (offset) {                                                // if there is an offset
                    i = 0;                                                      // clear this
                    if (*curframe->pc == LOADARG) i = curframe->pc[1] + 2;      // if args then point at LINENUM
                    if (curframe->pc[i] != LINENUM) ERROR(-ERRM13);             // check this and fail if they don't match
                    curframe->pc = &curframe->pc[i + 3];                        // point at offset
                    i = *(u_short *) curframe->pc;                              // get it
                    curframe->pc = &curframe->pc[i + 1];                        // point at next line
                    offset--;                                                   // decrement the offset
                }                                                               // end offset junk
            }

            curframe->line_num = 1;                                             // current routine line#
            p = curframe->pc;                                                   // the new pc

            if (*p++ == LINENUM) {
                assert(sizeof(us) == sizeof(u_short));
                memcpy(&us, p, sizeof(u_short));                                // get the linenum
                p += sizeof(u_short);
                curframe->line_num = us;                                        // store the line number
                p += sizeof(u_short);                                           // point past the offset
            }

            i = 0;                                                              // assume no dots here
            if (*p++ == CHKDOTS) i = *p;                                        // dots here? then get number of dots
            if (curframe->level != i) ERROR(-ERRM45);                           // different dots? then complain

            /*
             * For dots still need to check dots and new routine
             * and lesser dots between source and destination
             */

            if ((curframe->symbol == NULL) && rouadd->num_vars) {               // need symbol space? and have any vars?
                curframe->symbol = malloc(rouadd->num_vars * sizeof(short));    // symbol index space
                for (i = 0; i < rouadd->num_vars; i++) curframe->symbol[i] = -1; // for each one, mark not setup
            }

            rsmpc = curframe->pc;                                               // get the new pc
            break;                                                              // return to interp

        case CMXECUT:                                                           // XECUTE
            partab.jobtab->commands++;                                          // count a command
            tmp = (cstring *) addstk[--asp];                                    // get the arg
            source_ptr = tmp->buf;                                              // the data part
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            comp_ptr = cptr->buf;                                               // the data part
            parse();                                                            // compile it
            *comp_ptr++ = CMQUIT;                                               // quit from the execute
            *comp_ptr++ = ENDLIN;                                               // JIC
            *comp_ptr++ = ENDLIN;                                               // JIC
            cptr->len = (u_short) (comp_ptr - cptr->buf);                       // get the length
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            if (partab.jobtab->cur_do >= MAX_DO_FRAMES) ERROR(-(ERRZ8 + ERRMLAST)); // too many
            partab.jobtab->dostk[partab.jobtab->cur_do].pc = rsmpc;
            partab.jobtab->cur_do++;                                            // increment do frame
            rsmpc = cptr->buf;                                                  // new pc
            partab.jobtab->dostk[partab.jobtab->cur_do].routine = tmp->buf;
            partab.jobtab->dostk[partab.jobtab->cur_do].pc = rsmpc;
            partab.jobtab->dostk[partab.jobtab->cur_do].symbol = NULL;
            partab.jobtab->dostk[partab.jobtab->cur_do].newtab = NULL;
            partab.jobtab->dostk[partab.jobtab->cur_do].endlin = rsmpc + cptr->len - 4;
            VAR_CLEAR(partab.jobtab->dostk[partab.jobtab->cur_do].rounam);
            partab.jobtab->dostk[partab.jobtab->cur_do].vol = partab.jobtab->vol;
            partab.jobtab->dostk[partab.jobtab->cur_do].uci = partab.jobtab->uci;
            partab.jobtab->dostk[partab.jobtab->cur_do].line_num = 0;
            partab.jobtab->dostk[partab.jobtab->cur_do].type = TYPE_XECUTE;
            partab.jobtab->dostk[partab.jobtab->cur_do].level = 0;
            partab.jobtab->dostk[partab.jobtab->cur_do].estack = partab.jobtab->dostk[partab.jobtab->cur_do - 1].estack;
            partab.jobtab->dostk[partab.jobtab->cur_do].flags = 0;
            partab.jobtab->dostk[partab.jobtab->cur_do].test = -1;
            partab.jobtab->dostk[partab.jobtab->cur_do].savasp = savasp;
            partab.jobtab->dostk[partab.jobtab->cur_do].savssp = savssp;
            partab.jobtab->dostk[partab.jobtab->cur_do].asp = asp;
            partab.jobtab->dostk[partab.jobtab->cur_do].ssp = ssp;
            partab.jobtab->dostk[partab.jobtab->cur_do].isp = isp;
            savasp = asp;
            savssp = ssp;

            if (infor) {
                partab.jobtab->dostk[partab.jobtab->cur_do].flags |= DO_FLAG_FOR;
                infor = 0;
            }

            break;                                                              // return to interp

        case CHKDOTS:                                                           // Check Current LEVEL
            i = *rsmpc++;                                                       // get number of dots
            if (i == partab.jobtab->dostk[partab.jobtab->cur_do].level) break;  // same, just continue

            if (i > partab.jobtab->dostk[partab.jobtab->cur_do].level) {
                rsmpc = partab.jobtab->dostk[partab.jobtab->cur_do].endlin;
                break;                                                          // greater than current, cont
            }

            opc = CMQUIT;                                                       // pretend it was QUIT
            partab.jobtab->commands--;                                          // don't count the command
            // fall through

        case CMQUIT:                                                            // QUIT no args
        case CMQUITA:                                                           // QUIT with args
            partab.jobtab->commands++;                                          // count a command

            if (infor) {                                                        // quit from a for?
                if (opc == CMQUITA) ERROR(-ERRM16);                             // did it have an arg then complain
                asp = savasp;
                forx = (for_stack *) addstk[--asp];                             // get the frame
                infor = forx->type & FOR_NESTED;                                // check nesting
                rsmpc = forx->quit;                                             // new pc
                ssp = (u_char *) forx - strstk;                                 // reset string stack
                savssp = ssp;                                                   // needs to be better
                savasp = asp;                                                   // needs to be better
                break;                                                          // and continue
            }

            curframe = &partab.jobtab->dostk[partab.jobtab->cur_do];            // point at it

            if ((curframe->type == TYPE_RUN) || (curframe->type == TYPE_JOB)) {
                if (opc == CMQUIT) return (short) opc;                          // return the quit
                return (cstringtoi((cstring *) addstk[--asp]) | BREAK_QN);      // tell it how many
            }

            if ((curframe->type == TYPE_EXTRINSIC) && (opc == CMQUIT)) {        // was it a $$? and a normal QUIT
                ERROR(-ERRM17);                                                 // complain
            }

            if ((curframe->type != TYPE_EXTRINSIC) && (opc != CMQUIT)) {        // was it NOT a $$? and a QUIT with arg
                ERROR(-ERRM16);                                                 // complain
            }

            if (curframe->newtab != NULL) {                                     // any news there?
                ST_Restore((ST_newtab *) curframe->newtab);                     // restore them
            }

            infor = curframe->flags & DO_FLAG_FOR;                              // reset for flag if reqd

            if (partab.jobtab->error_frame) {
                partab.jobtab->dostk[partab.jobtab->cur_do].symbol = NULL;      // clear pointer to prevent nasty bugs in error
            }

            if (curframe->flags & DO_FLAG_ATT) {                                // if we attached
                if (curframe->symbol != NULL) {                                 // had some vars?
                    ST_SymDet(((rbd *) curframe->routine)->num_vars, curframe->symbol); // detach symbols
                }

                Routine_Detach((rbd *) curframe->routine);                      // detach routine
            }

            cptr = NULL;                                                        // shut up the C compiler
            if (opc == CMQUITA) cptr = (cstring *) addstk[--asp];               // if there was an arg then pick it up
            savasp = curframe->savasp;
            savssp = curframe->savssp;
            asp = curframe->asp;
            ssp = curframe->ssp;
            isp = curframe->isp;

            if (opc == CMQUITA) {                                               // if there was an arg
                ptr1 = (cstring *) &strstk[ssp];                                // where we will put it
                memmove(ptr1, cptr, cptr->len + sizeof(u_short) + 1);           // copy it
                ssp += ptr1->len + sizeof(u_short) + 1;                         // protect it
                addstk[asp++] = (u_char *) ptr1;                                // save the address
            }

            if ((curframe->type == TYPE_EXTRINSIC) || curframe->level) {        // if it's extrinsic or argless do
                if (partab.jobtab->dostk[partab.jobtab->cur_do].test != -1) {
                    partab.jobtab->dostk[partab.jobtab->cur_do].test = curframe->flags & DO_FLAG_TEST; // set $TEST
                } else {
                    partab.jobtab->test = curframe->flags & DO_FLAG_TEST;       // set $TEST
                }
            }

            rsmpc = partab.jobtab->dostk[--partab.jobtab->cur_do].pc;

            if ((partab.jobtab->error_frame > partab.jobtab->cur_do) && (partab.jobtab->cur_do < partab.jobtab->etrap_at)) {
                ERROR(0);                                                       // drop into error again
            }

            break;

        case CMLCKU:                                                            // UNLOCK all
            partab.jobtab->commands++;                                          // count a command
            LCK_Remove(0);                                                      // do it
            break;                                                              // and exit

        case CMLCK:                                                             // LOCK #args()
        case CMLCKP:                                                            // LOCK + #args()
        case CMLCKM:                                                            // LOCK - #args()
            partab.jobtab->commands++;                                          // count a command
            j = cstringtoi((cstring *) addstk[--asp]);                          // get the timeout
            args = *rsmpc++;                                                    // get arg count
            p = &strstk[ssp];                                                   // where it goes
            if ((long) p & 1) p++;                                              // ensure even
            cptr = (cstring *) p;                                               // for function call

            for (i = 0; i < args; i++) {                                        // for each arg
                s = UTIL_mvartolock((mvar *) addstk[--asp], p + sizeof(short));
                if (s < 0) ERROR(s);                                            // check for error
                *((short *) p) = s;                                             // save the size
                p += s + sizeof(short);                                         // add the length
                if ((long) p & 1) p++;                                          // ensure even
            }

            if (opc == CMLCK) {
                s = LCK_Old(args, cptr, j);                                     // old style lock
            } else if (opc == CMLCKP) {
                s = LCK_Add(args, cptr, j);                                     // lock plus
            } else {
                s = LCK_Sub(args, cptr);                                        // lock minus
            }

            if (s < 0) ERROR(s);                                                // check for error
            break;                                                              // keep trucking

        case CMNEW:                                                             // NEW variables
        case CMNEWB:                                                            // EXCLUSIVE NEW variables
            partab.jobtab->commands++;                                          // count a command
            cptr = NULL;                                                        // flag for $ETRAP
            list = (var_u *) &strstk[ssp];                                      // where we put this
            VAR_CLEAR((*list));
            flag = *rsmpc++;                                                    // get arg count
            args = 0;                                                           // for the calls

            for (i = 0; i < flag; i++) {                                        // for each arg
                var = (mvar *) addstk[--asp];                                   // get next from list
                VAR_COPY(list[args], var->name);                                // get each name
                args++;

                if (var_empty(list[args - 1])) {                                // an index type?
                    rouadd = (rbd *) (partab.jobtab->dostk[partab.jobtab->cur_do].routine);
                    vt = (var_u *) (((u_char *) rouadd) + rouadd->var_tbl);
                    VAR_COPY(list[args - 1], vt[var->volset - 1]);              // get the var name
                }

                if (var->slen) ERROR(-(ERRZ13 + ERRMLAST));                     // any subscripts? that's not permitted
                if (var->uci != UCI_IS_LOCALVAR) ERROR(-(ERRZ13 + ERRMLAST));   // local? also not permitted

                if (var->name.var_cu[0] == '$') {                               // special var?
                    if (opc != CMNEW) ERROR(-ERRM8);                            // can't do that

                    if ((strncasecmp((const char *) &var->name.var_cu[0], "$et\0", 4) == 0) ||
                      (strncasecmp((const char *) &var->name.var_cu[0], "$etrap\0", 7) == 0)) {
                        VAR_CLEAR(var->name);
                        memcpy(&var->name.var_cu[0], "$ETRAP", 6);
                        VAR_COPY(list[args - 1], var->name);                    // ensure list OK
                        t = ST_GetAdd(var, &cptr);                              // get address of current value
                        if (t < 1) cptr = NULL;                                 // ignore junk
                    } else if ((strncasecmp((const char *) &var->name.var_cu[0], "$es\0", 4) == 0) ||
                      (strncasecmp((const char *) &var->name.var_cu[0], "$estack\0", 8) == 0)) {
                        partab.jobtab->dostk[partab.jobtab->cur_do].estack = partab.jobtab->cur_do; // set new estack value
                        --args;                                                 // decrease arg count
                    } else if ((strncasecmp((const char *) &var->name.var_cu[0], "$t\0", 3) == 0) ||
                      (strncasecmp((const char *) &var->name.var_cu[0], "$test\0", 6) == 0)) {
                        partab.jobtab->dostk[partab.jobtab->cur_do].test =
                        (((partab.jobtab->cur_do > 0) && (partab.jobtab->dostk[partab.jobtab->cur_do - 1].test != -1)) ?
                          partab.jobtab->dostk[partab.jobtab->cur_do - 1].test : partab.jobtab->test); // set new test value
                        --args;                                                 // decrease arg count
                    } else {
                        ERROR(-ERRM8);                                          // can't do that
                    }
                }
            }

            if ((args == 0) && flag) break;                                     // in case it was a NEW $ES

            if (opc == CMNEW) {
                s = ST_New(args, list);                                         // a new
            } else {
                s = ST_NewAll(args, list);                                      // or a new except
            }

            if (s < 0) ERROR(s);                                                // complain on error

            if (cptr != NULL) {                                                 // need to restore $ETRAP?
                var = (mvar *) &strstk[ssp];                                    // where to put this
                VAR_CLEAR(var->name);
                memcpy(&var->name.var_cu[0], "$ETRAP", 6);
                var->uci = UCI_IS_LOCALVAR;                                     // local
                var->volset = 0;
                var->slen = 0;                                                  // no subscripts
                t = ST_Set(var, cptr);                                          // set it back
                if (t < 0) ERROR(t);                                            // complain on error
            }

            break;

        case CMKILL:                                                            // KILL 1 var
            partab.jobtab->commands++;                                          // count a command
            var = (mvar *) addstk[--asp];                                       // get the var

            if (var->uci == UCI_IS_LOCALVAR) {                                  // if it's local
                if (var->name.var_cu[0] == '$') ERROR(-ERRM8);                  // can't do that
                s = ST_Kill(var);                                               // do it - local
            } else if (var->name.var_cu[0] == '$') {                            // SSVN?
                s = SS_Kill(var);                                               // do it - SSVN
            } else {
                memcpy(&partab.jobtab->last_ref, var, sizeof(var_u) + 5 + var->slen); // update naked
                s = DB_Kill(var);                                               // do it - global
            }

            if (s < 0) ERROR(s);                                                // complain on error
            break;

        case CMKILLB:                                                           // KILL but()
            partab.jobtab->commands++;                                          // count a command
            list = (var_u *) &strstk[ssp];                                      // where we put this
            VAR_CLEAR((*list));
            args = *rsmpc++;                                                    // get arg count

            for (i = 0; i < args; i++) {                                        // for each arg
                var = (mvar *) addstk[--asp];                                   // point at mvar
                if (var->uci != UCI_IS_LOCALVAR) ERROR(-(ERRZ13 + ERRMLAST));   // local? that's not permitted
                if (var->slen) ERROR(-(ERRZ13 + ERRMLAST));                     // any subscripts? also not permitted

                if (var->volset) {                                              // index type?
                    rouadd = (rbd *) (partab.jobtab->dostk[partab.jobtab->cur_do].routine);
                    vt = (var_u *) (((u_char *) rouadd) + rouadd->var_tbl);

                    // point at var table
                    VAR_COPY(var->name, vt[var->volset - 1]);                   // get the var name
                }

                VAR_COPY(list[i], var->name);                                   // get the name
            }

            s = ST_KillAll(args, list);                                         // do it in symbol
            if (s < 0) ERROR(s);                                                // complain on error
            break;

        case NEWBREF:                                                           // PUSH a NULL on addstk[] etc.
            var = (mvar *) addstk[asp - 1];                                     // point at previous mvar
            if (var->uci != UCI_IS_LOCALVAR) ERROR(-(ERRZ13 + ERRMLAST));       // local? that's not permitted
            if (var->slen) ERROR(-(ERRZ13 + ERRMLAST));                         // any subscripts? also not permitted

            if (var->volset) {                                                  // index type?
                rouadd = (rbd *) (partab.jobtab->dostk[partab.jobtab->cur_do].routine);
                vt = (var_u *) (((u_char *) rouadd) + rouadd->var_tbl);         // point at var table
                VAR_COPY(var->name, vt[var->volset - 1]);                       // get the var name
                var->volset = 0;                                                // clear the index
            }

            s = ST_Create(var->name);                                           // get its index
            if (s < 0) ERROR(s);                                                // die on error

            if (symtab[s].data == ST_DATA_NULL) {                               // if data block undef
                symtab[s].data = malloc(DTMINSIZE);                             // allocate some mem
                if (symtab[s].data == NULL) ERROR(-(ERRZ56 + ERRMLAST));        // no memory
                memset(symtab[s].data, 0, DTMINSIZE);                           // clear it
DISABLE_WARN(-Warray-bounds)
                symtab[s].data->attach = 1;                                     // this one attached
                symtab[s].data->dbc = VAR_UNDEFINED;                            // say undefined
ENABLE_WARN
            }

            addstk[asp++] = (u_char *) symtab[s].data;                          // save the address
            addstk[asp++] = NULL;                                               // store a null
            break;

        case VARUNDF:                                                           // Setup UNDEFINED pointer
            addstk[asp++] = (u_char *) &var_undefined;
            break;

        case LINENUM:                                                           // Set current LINE NUMBER
            assert(sizeof(us) == sizeof(u_short));
            memcpy(&us, rsmpc, sizeof(u_short));                                // get the linenum
            rsmpc += sizeof(u_short);
            partab.jobtab->dostk[partab.jobtab->cur_do].line_num = us;          // store the line number
            memcpy(&us, rsmpc, sizeof(u_short));
            partab.jobtab->dostk[partab.jobtab->cur_do].endlin = rsmpc + us;
            rsmpc += sizeof(u_short);

            if (partab.debug == -1) {                                           // in debug?
                s = Debug(savasp, savssp, 0);                                   // do it
                if (s == OPHALT) return s;                                      // halt if reqd
                if (s < 0) ERROR(s);                                            // complain if reqd
            }

            break;                                                              // and continue

        case LOADARG:                                                           // Illegal in Line
            if (partab.jobtab->dostk[partab.jobtab->cur_do].level) {            // if dots
                i = *rsmpc + 1;                                                 // get args + arg count

                if (rsmpc[i] == LINENUM) {                                      // if a LINENUM
                    i += sizeof(short) * 2 + 1;                                 // skip that

                    if (rsmpc[i] == CHKDOTS) {                                  // if that's a CHKDOTS
                        rsmpc = &rsmpc[i];                                      // point at it
                        break;                                                  // and quit - DLW: was in LINENUM, typo?
                    }
                }
            }

            ERROR(-ERRM11);                                                     // complain

        case JMP:                                                               // JUMP
            assert(sizeof(s) == sizeof(short));
            memcpy(&s, rsmpc, sizeof(short));                                   // get the offset
            rsmpc += sizeof(short);
            rsmpc = rsmpc + s;                                                  // jump
            break;

        case CMFOR0:                                                            // argless FOR
            partab.jobtab->commands++;                                          // count a command
            forx = (for_stack *) &strstk[ssp];                                  // where to put for stuff
            ssp += sizeof(for_stack);                                           // protect it
            addstk[asp++] = (u_char *) forx;                                    // save the address
            savasp = asp;                                                       // protect these
            savssp = ssp;                                                       // needs to be better
            forx->type = FOR_TYP_0;                                             // save type
            if (infor) forx->type |= FOR_NESTED;                                // check for nesting
            assert(sizeof(s) == sizeof(short));
            memcpy(&s, rsmpc, sizeof(short));                                   // get the offset
            rsmpc += sizeof(short);
            forx->quit = rsmpc + s;                                             // save the new address
            infor = 1;                                                          // say in a for loop
            break;                                                              // and keep trucking

        case CMFOR1:                                                            // FOR with 1 arg
            cptr = (cstring *) addstk[--asp];                                   // the value
            forx = (for_stack *) addstk[asp - 1];                               // the for info
            forx->type = (forx->type & ~15) | FOR_TYP_1;                        // set type
            s = forx->svar;                                                     // get syment

            if (s == -1) {                                                      // mvar type
                t = ST_Set(forx->var, cptr);                                    // set it
            } else {
                t = ST_SymSet(s, cptr);                                         // or this way
            }

            if (t < 0) ERROR(t);                                                // check error

            if (rsmpc == forx->startpc) {                                       // out of arguments
                forx->nxtarg = NULL;                                            // flag it
            } else {
                forx->nxtarg = rsmpc;                                           // next one
                rsmpc = forx->startpc;                                          // set the pc
            }

            break;                                                              // and go do it

        case CMFOR2:                                                            // FOR with 2 args
            forx = (for_stack *) addstk[asp - 3];                               // the for info
            forx->type = (forx->type & ~15) | FOR_TYP_2;                        // set type
            forx->increment = (u_char *) &strstk[ssp];                          // where we will put it
            cptr = (cstring *) addstk[--asp];                                   // point at the incr
            p = cptr->buf;
            s = ncopy(&p, forx->increment);                                     // copy numerically
            if (s < 0) ERROR(s);
            ssp += s + 2;                                                       // cover it
            savssp = ssp;
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            ptr1 = (cstring *) addstk[--asp];                                   // point at the start value
            p = ptr1->buf;
            s = ncopy(&p, cptr->buf);                                           // copy numerically
            if (s < 0) ERROR(s);
            cptr->len = s;                                                      // start value now in cptr
            ssp += s + 4;
            s = forx->svar;                                                     // get syment

            if (s == -1) {                                                      // mvar type
                t = ST_Set(forx->var, cptr);                                    // set it
            } else {
                t = ST_SymSet(s, cptr);                                         // or this way
            }

            if (t < 0) ERROR(t);                                                // check error

            if (rsmpc == forx->startpc) {                                       // out of arguments
                forx->nxtarg = NULL;                                            // flag it
            } else {
                forx->nxtarg = rsmpc;                                           // next one
                rsmpc = forx->startpc;                                          // set the pc
            }

            break;                                                              // go do it

        case CMFOR3:                                                            // FOR with 3 args
            forx = (for_stack *) addstk[asp - 4];                               // the for info
            forx->type = (forx->type & ~15) | FOR_TYP_3;                        // set type
            forx->done = (u_char *) &strstk[ssp];                               // where we will put it
            cptr = (cstring *) addstk[--asp];                                   // point at final value
            p = cptr->buf;
            s = ncopy(&p, forx->done);                                          // copy numerically
            if (s < 0) ERROR(s);
            ssp += s + 2;                                                       // cover it
            forx->increment = (u_char *) &strstk[ssp];                          // where we will put it
            cptr = (cstring *) addstk[--asp];                                   // point at the incr
            p = cptr->buf;
            s = ncopy(&p, forx->increment);                                     // copy numerically
            if (s < 0) ERROR(s);
            ssp += s + 2;                                                       // cover it
            savssp = ssp;
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            ptr1 = (cstring *) addstk[--asp];                                   // point at the start value
            p = ptr1->buf;
            s = ncopy(&p, cptr->buf);                                           // copy numerically
            if (s < 0) ERROR(s);
            cptr->len = s;                                                      // start value now in cptr
            s = forx->svar;                                                     // get syment

            if (s == -1) {                                                      // mvar type
                t = ST_Set(forx->var, cptr);                                    // set it
            } else {
                t = ST_SymSet(s, cptr);                                         // or this way
            }

            if (t < 0) ERROR(t);                                                // check error

            if (rsmpc == forx->startpc) {                                       // out of arguments
                forx->nxtarg = NULL;                                            // flag it
            } else {
                forx->nxtarg = rsmpc;                                           // next one
                rsmpc = forx->startpc;                                          // set the pc
            }

            if (forx->increment[0] == '-') {                                    // going backwards?
                i = runtime_comp((char *) cptr->buf, (char *) forx->done);      // past limit
            } else {
                i = runtime_comp((char *) forx->done, (char *) cptr->buf);      // past limit forwards
            }

            if (i) {                                                            // all done?
                rsmpc = forx->nxtarg;                                           // get address of next bit
                if (rsmpc != NULL) break;
                rsmpc = forx->quit;                                             // quit address
                infor = forx->type & FOR_NESTED;                                // reset infor
                --asp;                                                          // decrement address stack
                ssp = ((u_char *) forx) - strstk;                               // and string pointer
                savasp = asp;
                savssp = ssp;
            }

            break;                                                              // go do it

        case CMFORSET:                                                          // setup FOR code
            partab.jobtab->commands++;                                          // count a command
            i = *rsmpc;                                                         // get var type
            var = NULL;                                                         // clear var address

            if (i == TYPVARIDX) {                                               // index type?
                rsmpc++;                                                        // skip type
                i = *rsmpc++;                                                   // get var idx
                s = partab.jobtab->dostk[partab.jobtab->cur_do].symbol[i];

                if (s == -1) {                                                  // if not attached
                    rouadd = (rbd *) partab.jobtab->dostk[partab.jobtab->cur_do].routine;
                    vt = (var_u *) (((u_char *) rouadd) + rouadd->var_tbl);
                    VAR_COPY(tag, vt[i]);                                       // get the var name
                    s = ST_SymAtt(tag);                                         // attach to var
                    if (s < 0) ERROR(s);                                        // die on error
                    partab.jobtab->dostk[partab.jobtab->cur_do].symbol[i] = s;
                }
            } else {
                var = (mvar *) &strstk[ssp];                                    // somewhere to put it
                s = buildmvar(var, 0, asp);                                     // build it
                if (s < 0) ERROR(s);                                            // check it
                asp = s;                                                        // restore returned asp
                if (var->uci != UCI_IS_LOCALVAR) ERROR(-(ERRZ13 + ERRMLAST));   // not a local? well it must be local
                ssp += var->slen + sizeof(var_u) + (sizeof(u_char) * 4);
                s = -1;                                                         // flag var type
            }

            forx = (for_stack *) &strstk[ssp];                                  // where to put for stuff
            ssp += sizeof(for_stack);                                           // protect it
            addstk[asp++] = (u_char *) forx;                                    // save the address
            savasp = asp;                                                       // protect these
            savssp = ssp;                                                       // needs to be better
            forx->type = FOR_TYP_0;                                             // save type (none yet)
            if (infor) forx->type |= FOR_NESTED;                                // check for nesting
            forx->svar = s;                                                     // save syment
            forx->var = var;                                                    // or mvar address
            assert(sizeof(s) == sizeof(short));
            memcpy(&s, rsmpc, sizeof(short));                                   // get the offset
            rsmpc += sizeof(short);
            forx->startpc = rsmpc + s;                                          // save the new address
            assert(sizeof(s) == sizeof(short));
            memcpy(&s, rsmpc, sizeof(short));                                   // get the offset
            rsmpc += sizeof(short);
            forx->quit = rsmpc + s;                                             // save the new address
            infor = 1;                                                          // say in a for loop
            break;                                                              // and keep trucking

        case CMFOREND:                                                          // JUMP
            forx = (for_stack *) addstk[savasp - 1];                            // get for frame addr

            if ((forx->type & 7) == FOR_TYP_1) {                                // single arg?
                rsmpc = forx->nxtarg;                                           // get address of next bit
                if (rsmpc != NULL) break;
                rsmpc = forx->quit;                                             // quit address
                infor = forx->type & FOR_NESTED;                                // reset infor
                --savasp;                                                       // decrement address stack
                savssp = (u_char *) forx - strstk;                              // reset string stack
                ssp = savssp;
                asp = savasp;
                break;                                                          // and exit
            }

            s = forx->svar;                                                     // syment

            if (s == -1) {                                                      // mvar type?
                t = ST_GetAdd(forx->var, &ptr1);                                // get index
                if (t == -ERRM6) ERROR(-ERRM15);                                // complain
                if (t < 0) ERROR(t);                                            // or this way
            } else {
                data = symtab[s].data;                                          // get data block address
                if (data == NULL) ERROR(-ERRM15);                               // complain if missing
                if (data->dbc == VAR_UNDEFINED) ERROR(-ERRM15);                 // complain if missing
                ptr1 = (cstring *) &data->dbc;                                  // point at it
            }

            p = ptr1->buf;                                                      // point at the data
            cptr = (cstring *) &strstk[ssp];                                    // some space
            s = ncopy(&p, cptr->buf);
            if (s < 0) ERROR(s);
            strcpy((char *) temp, (char *) forx->increment);
            s = runtime_add((char *) cptr->buf, (char *) temp);                 // increment the index
            if (s < 0) ERROR(s);
            cptr->len = s;

            if ((forx->type & 7) == FOR_TYP_3) {                                // three arg type
                if (forx->increment[0] == '-') {                                // going backwards?
                    i = runtime_comp((char *) cptr->buf, (char *) forx->done);  // past limit
                } else {
                    i = runtime_comp((char *) forx->done, (char *) cptr->buf);  // past limit forwards
                }

                if (i) {                                                        // all done?
                    rsmpc = forx->nxtarg;                                       // get address of next bit
                    if (rsmpc != NULL) break;
                    rsmpc = forx->quit;                                         // quit address
                    infor = forx->type & FOR_NESTED;                            // reset infor
                    --savasp;                                                   // decrement address stack
                    savssp = (u_char *) forx - strstk;                          // reset string stack
                    ssp = savssp;
                    asp = savasp;
                    break;                                                      // and exit
                }
            }

            s = forx->svar;                                                     // syment

            if (s == -1) {                                                      // mvar type
                t = ST_Set(forx->var, cptr);                                    // set it back
            } else {                                                            // smart type
                t = ST_SymSet(s, cptr);                                         // set it this way
            }

            if (t < 0) ERROR(t);                                                // complain on error
            rsmpc = forx->startpc;                                              // point at code again
            break;                                                              // and go do it

        case OPNOP:                                                             // NOP
            break;                                                              // do nothing

        // ***** Indirection Stuff *****
        case INDREST:                                                           // RESTORE isp and rsmpc
            assert(sizeof(isp) == sizeof(long));
            memcpy(&isp, rsmpc, sizeof(long));                                  // restore the isp
            rsmpc += sizeof(long);
            assert(sizeof(rsmpc) == sizeof(u_char *));
            memcpy(&rsmpc, rsmpc, sizeof(u_char *));                            // and the rsmpc
            break;                                                              // continue

        case INDCLOS:                                                           // CLOSE indirect
        case INDDO:                                                             // DO indirect
        case INDGO:                                                             // GOTO indirect
        case INDHANG:                                                           // HANG indirection
        case INDIF:                                                             // IF indirection
        case INDJOB:                                                            // JOB indirection
        case INDKILL:                                                           // KILL indirection
        case INDKILLB:                                                          // KILL but() indirection
        case INDLOCK:                                                           // LOCK indirection
        case INDMERG:                                                           // MERGE indirection
        case INDNEW:                                                            // NEW indirection
        case INDOPEN:                                                           // OPEN indirection
        case INDREAD:                                                           // READ indirection
        case INDSET:                                                            // SET indirection
        case INDUSE:                                                            // USE indirection
        case INDWRIT:                                                           // WRITE indirection
        case INDXEC:                                                            // XECUTE indirection
            partab.jobtab->commands++;                                          // count a command
            cptr = (cstring *) addstk[--asp];                                   // get string to eval
            if (INDSNOK(cptr->len)) ERROR(-(ERRZ58 + ERRMLAST));                // too much indirection
            source_ptr = cptr->buf;                                             // what to compile
            comp_ptr = &indstk[isp];                                            // where it goes

            switch (opc) {                                                      // dispatch on opc
            case INDCLOS:                                                       // CLOSE
                parse_close();
                break;

            case INDDO:                                                         // DO
                parse_do(1);
                break;

            case INDGO:                                                         // GOTO
                parse_goto(1);
                break;

            case INDHANG:                                                       // HANG
                parse_hang();
                break;

            case INDIF:                                                         // IF
                parse_if(isp);                                                  // pass the isp to restore
                break;

            case INDJOB:                                                        // JOB
                parse_job(1);
                break;

            case INDKILL:                                                       // KILL
                parse_kill(0);
                break;

            case INDKILLB:                                                      // exclusive KILL
                parse_kill(1);
                break;

            case INDLOCK:                                                       // LOCK
                parse_lock();
                break;

            case INDMERG:                                                       // MERGE
                parse_merge();
                break;

            case INDNEW:                                                        // NEW
                parse_new();
                break;

            case INDOPEN:                                                       // OPEN
                parse_open();
                break;

            case INDREAD:                                                       // READ
                parse_read();
                break;

            case INDSET:                                                        // SET
                parse_set();
                break;

            case INDUSE:                                                        // USE
                parse_use();
                break;

            case INDWRIT:                                                       // WRITE
                parse_write();
                break;

            case INDXEC:                                                        // XECUTE
                parse_xecute();
                break;
            }

            if (*source_ptr != '\0') ERROR(-(ERRZ57 + ERRMLAST));               // must point at end of var or complain
            if (INDANOK(comp_ptr)) ERROR(-(ERRZ58 + ERRMLAST));                 // too much indirection
            *comp_ptr++ = INDREST;                                              // restore things
            assert(sizeof(isp) == sizeof(long));
            memcpy(comp_ptr, &isp, sizeof(long));                               // the isp to restore
            comp_ptr += sizeof(long);
            assert(sizeof(rsmpc) == sizeof(u_char *));
            memcpy(comp_ptr, &rsmpc, sizeof(u_char *));                         // and the rsmpc
            comp_ptr += sizeof(u_char *);
            rsmpc = &indstk[isp];                                               // what we are going to do
            isp = comp_ptr - &indstk[isp] + isp;                                // adjust isp
            break;                                                              // go do it

        // ***** Start of Xcalls *****
        case XCCOMP:                                                            // Xcall $&%COMPRESS()
            j = cstringtoi((cstring *) addstk[--asp]);                          // get second arg
            ptr1 = (cstring *) addstk[--asp];                                   // the first argument
            if ((j & 15) < 1) ERROR(-(ERRZ64 + ERRMLAST));                      // range check
            var = &partab.jobtab->last_ref;                                     // use this so all can see it
            s = UTIL_MvarFromCStr(ptr1, var);                                   // convert to an mvar
            if (s < 0) ERROR(s);                                                // complain on error
            s = DB_Compress(var, j);                                            // do it
            if (s < 0) ERROR(s);                                                // complain on error
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            cptr->len = itocstring(cptr->buf, s);                               // convert return to string
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        //case XCSIG:                                                           // Xcall $&%SIGNAL() -  done elsewhere

        case XCHOST:                                                            // Xcall $&%HOST()
            ptr2 = (cstring *) addstk[--asp];                                   // get arg 2
            ptr1 = (cstring *) addstk[--asp];                                   // get arg 1
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Xcall_host((char *) cptr->buf, ptr1, ptr2);                     // do it
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // save the length
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case XCFILE:                                                            // Xcall $&%FILE()
            ptr2 = (cstring *) addstk[--asp];                                   // get arg 2
            ptr1 = (cstring *) addstk[--asp];                                   // get arg 1
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Xcall_file((char *) cptr->buf, ptr1, ptr2);                     // do it
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // save the length
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case XCWAIT:                                                            // Xcall $&%WAIT()
            ptr2 = (cstring *) addstk[--asp];                                   // get arg 2
            ptr1 = (cstring *) addstk[--asp];                                   // get arg 1
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Xcall_wait((char *) cptr->buf, ptr1, ptr2);                     // do it
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // save the length
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case XCDEBUG:                                                           // Xcall $&DEBUG()
            ptr2 = (cstring *) addstk[--asp];                                   // get arg 2
            ptr1 = (cstring *) addstk[--asp];                                   // get arg 1
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Xcall_debug((char *) cptr->buf, ptr1, ptr2);                    // do it
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // save the length
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case XCDIR:                                                             // Xcall $&%DIRECTORY()
            ptr2 = (cstring *) addstk[--asp];                                   // get arg 2
            ptr1 = (cstring *) addstk[--asp];                                   // get arg 1
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Xcall_directory((char *) cptr->buf, ptr1, ptr2);                // do it
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // save the length
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case XCERR:                                                             // Xcall $&%ERRMSG()
            ptr2 = (cstring *) addstk[--asp];                                   // get arg 2
            ptr1 = (cstring *) addstk[--asp];                                   // get arg 1
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Xcall_errmsg((char *) cptr->buf, ptr1, ptr2);                   // do it
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // save the length
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case XCOPC:                                                             // Xcall $&%OPCOM()
            ptr2 = (cstring *) addstk[--asp];                                   // get arg 2
            ptr1 = (cstring *) addstk[--asp];                                   // get arg 1
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Xcall_opcom((char *) cptr->buf, ptr1, ptr2);                    // do it
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // save the length
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case XCSIG:                                                             // Xcall $&%SIGNAL()
            ptr2 = (cstring *) addstk[--asp];                                   // get arg 2
            ptr1 = (cstring *) addstk[--asp];                                   // get arg 1
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Xcall_signal((char *) cptr->buf, ptr1, ptr2);                   // do it
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // save the length
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case XCSPA:                                                             // Xcall $&%SPAWN()
            ptr2 = (cstring *) addstk[--asp];                                   // get arg 2
            ptr1 = (cstring *) addstk[--asp];                                   // get arg 1
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Xcall_spawn((char *) cptr->buf, ptr1, ptr2);                    // do it
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // save the length
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case XCVER:                                                             // Xcall $&%VERSION()
            ptr2 = (cstring *) addstk[--asp];                                   // get arg 2
            ptr1 = (cstring *) addstk[--asp];                                   // get arg 1
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Xcall_version((char *) cptr->buf, ptr1, ptr2);                  // do it
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // save the length
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case XCZWR:                                                             // Xcall $&%ZWRITE()
            ptr2 = (cstring *) addstk[--asp];                                   // get arg 2
            ptr1 = (cstring *) addstk[--asp];                                   // get arg 1
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Xcall_zwrite((char *) cptr->buf, ptr1, ptr2);                   // do it
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // save the length
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case XCE:                                                               // Xcall $&E()
            ptr2 = (cstring *) addstk[--asp];                                   // get arg 2
            ptr1 = (cstring *) addstk[--asp];                                   // get arg 1
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Xcall_e((char *) cptr->buf, ptr1, ptr2);                        // do it
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // save the length
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case XCPAS:                                                             // Xcall $&PASCHK()
            ptr2 = (cstring *) addstk[--asp];                                   // get arg 2
            ptr1 = (cstring *) addstk[--asp];                                   // get arg 1
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Xcall_paschk((char *) cptr->buf, ptr1, ptr2);                   // do it
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // save the length
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case XCV:                                                               // Xcall $&V()
            ptr2 = (cstring *) addstk[--asp];                                   // get arg 2
            ptr1 = (cstring *) addstk[--asp];                                   // get arg 1
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            t = Xcall_v((char *) cptr->buf, ptr1, ptr2);                        // do it
            if (t < 0) ERROR(t);                                                // complain on error
            cptr->len = t;                                                      // save the length
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case XCX:                                                               // Xcall $&X()
            ptr2 = (cstring *) addstk[--asp];                                   // get arg 2
            ptr1 = (cstring *) addstk[--asp];                                   // get arg 1
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            t = Xcall_x((char *) cptr->buf, ptr1, ptr2);                        // do it
            if (t < 0) ERROR(t);                                                // complain on error
            cptr->len = t;                                                      // save the length
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case XCXRSM:                                                            // Xcall $&XRSM()
            ptr2 = (cstring *) addstk[--asp];                                   // get arg 2
            ptr1 = (cstring *) addstk[--asp];                                   // get arg 1
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Xcall_xrsm((char *) cptr->buf, ptr1, ptr2);                     // do it
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // save the length
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case XCSETENV:                                                          // Xcall $&%SETENV()
            ptr2 = (cstring *) addstk[--asp];                                   // get arg 2
            ptr1 = (cstring *) addstk[--asp];                                   // get arg 1
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Xcall_setenv((char *) cptr->buf, ptr1, ptr2);                   // do it
            if (s < 0) ERROR(s);                                                // complain on error
            cptr->len = s;                                                      // save the length
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case XCGETENV:                                                          // Xcall $&%GETENV()
            ptr2 = (cstring *) addstk[--asp];                                   // get arg 2
            ptr1 = (cstring *) addstk[--asp];                                   // get arg 1
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            t = Xcall_getenv((char *) cptr->buf, ptr1, ptr2);                   // do it
            if (t < 0) ERROR(t);                                                // complain on error
            cptr->len = t;                                                      // save the length
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;

        case XCROUCHK:                                                          // Xcall $&%ROUCHK()
            ptr2 = (cstring *) addstk[--asp];                                   // get arg 2 (ignored)
            ptr1 = (cstring *) addstk[--asp];                                   // get arg 1
            var2 = (mvar *) &strstk[ssp];                                       // some space
            ssp += sizeof(mvar);                                                // cover it
            VAR_CLEAR(var2->name);
            memcpy(&var2->name.var_cu, "$ROUTINE", 8);                          // ^$ROUTINE
            var2->volset = partab.jobtab->rvol;                                 // the volume
            var2->uci = partab.jobtab->ruci;                                    // and the UCI
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = UTIL_Key_Build(ptr1, &var2->key[0]);                            // build the key
            if (s < 0) ERROR(s);                                                // give up on error
            var2->slen = s;                                                     // save the length

            t = Compile_Routine((mvar *) NULL,                                  // don't compile a routine
                                var2,                                           // check this one
                                &strstk[ssp]);                                  // use this temp space

            if (t < 0) ERROR(t);                                                // give up on error
            cptr = (cstring *) var2;                                            // reuse the space
            s = itocstring(cptr->buf, t);                                       // copy in the number
            cptr->len = s;                                                      // save length
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;                                                              // and exit

        case XCFORK:                                                            // Xcall $&%FORK()
            ptr2 = (cstring *) addstk[--asp];                                   // get arg 2 (ignored)
            ptr1 = (cstring *) addstk[--asp];                                   // get arg 1
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            s = Xcall_fork((char *) cptr->buf, ptr1, ptr2);                     // do it
            if (s < 0) ERROR(s);                                                // give up on error
            cptr->len = s;                                                      // save length
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;                                                              // done

        case XCIC:                                                              // Xcall $&%IC()
            j = cstringtoi((cstring *) addstk[--asp]);                          // get arg 2 (type)
            i = cstringtoi((cstring *) addstk[--asp]);                          // get arg 1 (vol)
            if ((i > MAX_VOL) || (i < 1)) ERROR(-ERRM26);                       // out of range
            if (j < -2) ERROR(-(ERRZ64 + ERRMLAST));                            // range check
            cptr = (cstring *) &strstk[ssp];                                    // where we will put it
            t = DB_ic(i, j);                                                    // do it
            if (t < 0) ERROR(t);                                                // give up on error
            cptr->len = itocstring(cptr->buf, t);                               // make a string
            ssp += sizeof(u_short) + cptr->len + 1;                             // point past it
            addstk[asp++] = (u_char *) cptr;                                    // stack it
            break;                                                              // done

        default:                                                                // can't happen
            ERROR(-(ERRZ14 + ERRMLAST));                                        // I hope
        }                                                                       // end of switch(opc)
    }                                                                           // end main while loop
}
