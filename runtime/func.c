/*
 * Package: Reference Standard M
 * File:    rsm/runtime/func.c
 * Summary: module runtime - runtime functions
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
#include "rsm.h"                                                                // standard includes
#include "proto.h"                                                              // standard prototypes
#include "error.h"                                                              // standard errors
#include "database.h"                                                           // for GBD def

#ifdef linux
#   include <values.h>
#endif

/*
 * All functions use the following structure
 *
 * short Dfunc(u_char *ret_buf, ...
 *     or
 * int   Dfunc(u_char *ret_buf, ...
 *
 * The first argument is of type u_char* and is the destination
 * for the value returned by the function (max size is MAX_STR_LEN).
 * The subsequent arguments are all read only and are the passed in values.
 * The function returns a count of characters in the return string
 * or a negative error number (M error).
 *
 * The function name is DfuncnameN
 *   where the call is $funcname() and N is the number of arguments.
 */

// $ASCII(expr[,int]) - return ascii value
short Dascii1(u_char *ret_buffer, cstring *expr)
{
    return Dascii2(ret_buffer, expr, 1);
}

short Dascii2(u_char *ret_buffer, cstring *expr, int posn)
{
    int asc = -1;                                                               // default to -1

    if ((posn > 0) && (posn <= (int) expr->len)) asc = (int) expr->buf[posn - 1]; // if within range, get from string
    return ltocstring(ret_buffer, asc);                                         // convert answer to string and return count
}

/*
 * $CHAR(int1[,int2[,...]]) - return a string - we implement 1 char at a time
 *     called for each argument (valid range is 0 to 255)
 */
short Dchar(u_char *ret_buffer, int i)
{
    ret_buffer[0] = '\0';                                                       // assume nothing
    if ((i < 0) || (i > 255)) return 0;                                         // out of range
    ret_buffer[0] = (u_char) i;                                                 // store the char
    ret_buffer[1] = '\0';                                                       // extra for null term
    return 1;                                                                   // count of 1
}

// $DATA(variable)
short Ddata(u_char *ret_buffer, mvar *var, int update)
{
    if (var->uci == UCI_IS_LOCALVAR) return ST_Data(var, ret_buffer);           // for a local var
    if (var->name.var_cu[0] == '$') return SS_Data(var, ret_buffer);            // SSVN? then do it
    if (update) memcpy(&partab.jobtab->last_ref, var, sizeof(var_u) + 5 + var->slen); // update naked
    return DB_Data(var, ret_buffer);                                            // else it's global
}

// $EXTRACT(expr[,start[,stop]])
int Dextract(u_char *ret_buffer, cstring *expr, int start, int stop)
{
    int i;                                                                      // for loops

    /* NOTE: support negative offsets
    if (start < 0) start += expr->len + 1;                                      // support negative arguments
    if (stop < 0) stop += expr->len + 1;                                        // support negative arguments
    */
    if ((start < 1) && (stop > 0)) start = 1;                                   // ensure sensible
    ret_buffer[0] = '\0';                                                       // setup null string
    if (start < 1) return 0;                                                    // that's a null
    if (stop > (int) expr->len) stop = (int) expr->len;                         // if past end of string, point at the end
    if ((stop < start) || (start > (int) expr->len)) return 0;                  // and return it
    i = stop - start + 1;                                                       // bytes to copy
    memmove(ret_buffer, &expr->buf[start - 1], i);                              // copy here
    ret_buffer[i] = '\0';                                                       // null terminate
    return i;                                                                   // return count
}

// $FIND(expr1,expr2[,int])
int Dfind2(u_char *ret_buffer, cstring *expr1, cstring *expr2)
{
    return Dfind3(ret_buffer, expr1, expr2, 1);
}

int Dfind3(u_char *ret_buffer, cstring *expr1, cstring *expr2, int start)
{
    return ltocstring(ret_buffer, Dfind3x(expr1, expr2, start));                // eval into buffer and return length
}

int Dfind3x(cstring *expr1, cstring *expr2, int start)
{
    int ret = 0;                                                                // return value

    if (start < 1) start = 1;                                                   // ensure sensible
    if (expr2->len == 0) return start;                                          // if expr2 is null str then just return start

    for (int i = start - 1; i < (int) expr1->len; i++) {                        // scan expr1
        for (int j = 0; j != (int) expr2->len; j++) {                           // scan expr2
            if (expr1->buf[i + j] != expr2->buf[j]) break;                      // quit if not the same
            if ((j + 1) == (int) expr2->len) ret = i + j + 2;                   // if at end of expr2, remember where we are
        }                                                                       // end compare loop

        if (ret != 0) break;                                                    // quit if found
    }                                                                           // end of expr

    return ret;                                                                 // and return it
}

/*
 * $FNUMBER(numexp,code[,int])
 *
 *   +  Inserts a plus sign (+) on numbers greater than 0.
 *   -  Supresses the negative sign on negative values of string
 *        either by default (on positive numbers), or by design
 *        (using the code string "-").
 *   ,  Inserts delimiters every third position to the left of the
 *        decimal point within a string.
 *   T  Places a plus sign (+) or a minus (-) sign after the
 *        string. (If sign suppression is active, using this code
 *        string results in a trailing space.)
 *   P  Places a negative number in parentheses, or enters spaces
 *        (prefix and suffix) if the number is non-negative.
 *
 * NOTE: numexp MUST be a canonic number!!
 */
int Dfnumber2(u_char *ret_buffer, cstring *numexp, cstring *code)
{
    cstring *tempc;
    cstring *dest;
    int     z;
    int     dlen;
    char    *a1 = NULL;                                                         // flag
    char    *a2 = NULL;                                                         // flag
    char    *b1 = NULL;                                                         // flag
    char    *b2 = NULL;                                                         // flag
    char    *c1 = NULL;                                                         // flag
    char    *c2 = NULL;                                                         // flag
    char    *d1 = NULL;                                                         // flag
    char    *dp = NULL;                                                         // decimal point position
    u_char  cnt = 0;                                                            // count of valid codes

    ret_buffer[0] = '\0';
    if ((a1 = strchr((const char *) &code->buf[0], 'p'))) cnt++;                // in code string ??
    if ((a2 = strchr((const char *) &code->buf[0], 'P'))) cnt++;                // in code string ??
    if ((b1 = strchr((const char *) &code->buf[0], '+'))) cnt++;                // in code string ??
    if ((b2 = strchr((const char *) &code->buf[0], '-'))) cnt++;                // in code string ??
    if ((c1 = strchr((const char *) &code->buf[0], 't'))) cnt++;                // in code string ??
    if ((c2 = strchr((const char *) &code->buf[0], 'T'))) cnt++;                // in code string ??
    if ((d1 = strchr((const char *) &code->buf[0], ','))) cnt++;                // in code string ??
    if ((dp = strchr((const char *) &numexp->buf[0], '.'))) cnt++;              // decimal position in number *|Null

    if (((a1 != NULL) || (a2 != NULL)) && ((b1 != NULL) || (b2 != NULL) || (c1 != NULL) || (c2 != NULL))) { // check for invalid
        return -ERRM2;                                                          // invalid code, error
    }

    if (code->len > cnt) return -ERRM2;                                         // extra invalid characters

    if (numexp->len > 1) {
        for (z = 0; z <= numexp->len; z++) {
            if (numexp->buf[z] != '0') break;
        }
    } else {
        z = 0;
    }

    if ((z == 1) && (numexp->buf[1] == '.')) z = 0;                             // check for '0.bla', leave the leading zero
    tempc = malloc(sizeof(short) + numexp->len + (numexp->len / 3) + 3);
    dest = malloc(sizeof(short) + numexp->len + (numexp->len / 3) + 3);
    dest->len = numexp->len - z;
    memcpy(dest->buf, &numexp->buf[z], numexp->len);

    if (d1 != NULL) {                                                           // add in commas
        int    ndlen;
        int    nlen;
        int    nc;
        int    cd = 0;
        u_char *ptr1;

        if (dp != NULL) {                                                       // contains a decimal point
            int i;

            for (i = 0; i <= dest->len - 1; i++) {
                if (dest->buf[i] == '.') break;
            }

            ndlen = i;                                                          // save this pos
            if (numexp->buf[0] == '-') ndlen -= 1;                              // dont count "-"
            nc = ndlen / 3;                                                     // num commas required
            if (((ndlen % 3) == 0) && (i > 0)) nc -= 1;                         // if *3 need one less,
            nlen = dest->len + nc - 1;                                          // orig+num commas-idx
            tempc->len = nlen + 1;
            ptr1 = &dest->buf[dest->len - 1];                                   // copy all including

            while (*ptr1 != '.') {                                              // the NULL term, up to
                memcpy(&tempc->buf[nlen], ptr1, 1);                             // the decimal point
                nlen -= 1;                                                      // but not including
                ptr1 -= 1;                                                      // the decimal point
            }

            memcpy(&tempc->buf[nlen], ptr1, 1);                                 // now copy over
            nlen -= 1;                                                          // the decimal
            ptr1 -= 1;                                                          // point only

            while (nlen >= 0) {                                                 // copy the rest
                cd += 1;                                                        // of the string
                memcpy(&tempc->buf[nlen], ptr1, 1);                             // to the destination
                nlen -= 1;                                                      // and every
                ptr1 -= 1;                                                      // third position

                if (((cd % 3) == 0) && (nlen > 0)) {                            // copy over
                    tempc->buf[nlen] = ',';                                     // a comma
                    nlen -= 1;
                }
            }
        } else {                                                                // no decimal point
            ndlen = numexp->len;                                                // save this start pos
            if (numexp->buf[0] == '-') ndlen -= 1;                              // dont count "-"
            nc = ndlen / 3;                                                     // num commas required
            if ((ndlen % 3) == 0) nc -= 1;                                      // if *3 need one less,
            nlen = numexp->len + nc - 1;                                        // orig+num commas-idx
            tempc->len = nlen + 1;
            ptr1 = &dest->buf[dest->len - 1];

            while (nlen >= 0) {                                                 // copy the rest
                cd += 1;                                                        // of the string
                memcpy(&tempc->buf[nlen], ptr1, 1);                             // to the destination
                nlen -= 1;                                                      // and every
                ptr1 -= 1;                                                      // third position

                if (((cd % 3) == 0) && (nlen > 0)) {                            // copy over
                    tempc->buf[nlen] = ',';                                     // a comma
                    nlen -= 1;
                }
            }
        }

        dest->len = tempc->len;
        memcpy(dest->buf, tempc->buf, tempc->len);
    }

    if ((a1 != NULL) || (a2 != NULL)) {
        if (numexp->buf[0] != '-') {
            tempc->buf[0] = ' ';                                                // space pad for '('
            memcpy(&tempc->buf[1], dest->buf, dest->len);                       // copy original data
            tempc->buf[1 + dest->len] = ' ';                                    // space pad for ')'
            tempc->len = dest->len + 2;
        }                                                                       // no further action required

        if (numexp->buf[0] == '-') {
            tempc->buf[0] = '(';                                                // prefix a '('
            memcpy(&tempc->buf[1], &dest->buf[1], dest->len - 1);               // copy original data
            tempc->buf[dest->len] = ')';                                        // suffix a ')'
            tempc->len = dest->len + 1;
        }                                                                       // no further action required

        dest->len = tempc->len;
        memcpy(dest->buf, tempc->buf, tempc->len);
    }

    if ((c1 != NULL) || (c2 != NULL)) {                                         // trailing signs
        if (numexp->buf[0] != '-') {
            if (b1 != NULL) {                                                   // force + sign at end
                if (dest->buf[0] == '+') {                                      // if + sign already at front
                    memcpy(&tempc->buf[0], &dest->buf[1], dest->len - 1);
                    tempc->buf[dest->len - 1] = '+';
                    tempc->len = dest->len;
                } else {                                                        // no + sign at front
                    memcpy(&tempc->buf[0], &dest->buf[0], dest->len);
                    tempc->buf[dest->len] = '+';
                    tempc->len = dest->len + 1;
                }
            } else {
                memcpy(tempc->buf, &dest->buf[0], dest->len);
                tempc->buf[dest->len] = ' ';
                tempc->len = dest->len + 1;
            }
        } else {                                                                // negative number
            if (b2 != NULL) {                                                   // - sign supress, tack a space
                memcpy(&tempc->buf[0], &dest->buf[1], dest->len - 1);
                tempc->buf[dest->len - 1] = ' ';
                tempc->len = dest->len;
            } else {                                                            // force - sign at end
                memcpy(&tempc->buf[0], &dest->buf[1], dest->len - 1);
                tempc->buf[dest->len - 1] = '-';
                tempc->len = dest->len;
            }
        }

        dest->len = tempc->len;
        memcpy(dest->buf, tempc->buf, tempc->len);
    } else {                                                                    // non trailing signs
        if (numexp->buf[0] != '-') {
            if ((numexp->buf[0] == '0') && (numexp->len == 1)) b1 = NULL;       // Turn off + for 0 case

            if (b1 != NULL) {                                                   // force + sign at front
                if (dest->buf[0] != '+') {
                    tempc->buf[0] = '+';
                    memcpy(&tempc->buf[1], dest->buf, dest->len);
                    tempc->len = dest->len + 1;
                }
            } else {
                memcpy(tempc->buf, &dest->buf[0], dest->len);
                tempc->len = dest->len;
            }
        } else {                                                                // negative number
            if (b2 != NULL) {                                                   // - sign supressed
                memcpy(tempc->buf, &dest->buf[1], dest->len - 1);
                tempc->len = dest->len - 1;
            } else {
                memcpy(tempc->buf, &dest->buf[0], dest->len);
                tempc->len = dest->len;
            }
        }

        dest->len = tempc->len;
        memcpy(dest->buf, tempc->buf, tempc->len);
    }

    dest->len = tempc->len;
    memcpy(ret_buffer, dest->buf, dest->len);
    ret_buffer[dest->len] = '\0';
    dlen = dest->len;
    free(tempc);
    free(dest);
    return dlen;
}                                                                               // end function $FNUMBER

int Dfnumber3(u_char *ret_buffer, cstring *numexp, cstring *code, int rnd)
{
    cstring *change;
    int     t;

    t = Djustify3(ret_buffer, numexp, 0, rnd);
    if (t < 0) return t;
    change = malloc(sizeof(short) + t + 1);
    change->len = t;
    memcpy(change->buf, ret_buffer, t + 1);
    return Dfnumber2(ret_buffer, change, code);
}

// $GET(variable[,expr])
int Dget1(u_char *ret_buffer, mvar *var)
{
    u_char  tmp[8];                                                             // some space
    cstring *cptr;                                                              // for the call

    cptr = (cstring *) tmp;                                                     // point at the space
DISABLE_WARN(-Warray-bounds)
    cptr->len = 0;                                                              // zero length
    cptr->buf[0] = '\0';                                                        // null terminated
ENABLE_WARN
    return Dget2(ret_buffer, var, cptr);                                        // do it below
}

int Dget2(u_char *ret_buffer, mvar *var, cstring *expr)
{
    int t;                                                                      // for return values

    if (var->uci == UCI_IS_LOCALVAR) {                                          // for a local var
        t = ST_Get(var, ret_buffer);                                            // attempt to get the data
        if (t >= 0) return t;                                                   // if we got data, return it
        if (t == -ERRM6) t = 0;                                                 // flag undefined local var
    } else if (var->name.var_cu[0] == '$') {                                    // SSVN?
        t = SS_Get(var, ret_buffer);                                            // attempt to get the data
        if (t >= 0) return t;                                                   // if we got data, return it
        if ((t == -ERRM38) || (t == -ERRM7)) t = 0;                             // flag undefined SSVN
    } else {                                                                    // for a global var
        memcpy(&partab.jobtab->last_ref, var, sizeof(var_u) + 5 + var->slen);   // update naked
        t = DB_Get(var, ret_buffer);                                            // attempt to get the data
        if (t >= 0) return t;                                                   // if we got data, return it
        if (t == -ERRM7) t = 0;                                                 // flag undefined global var
    }

    if (t != 0) return t;                                                       // if an error, return it
    memmove(&ret_buffer[0], &expr->buf[0], expr->len);                          // copy here
    ret_buffer[expr->len] = '\0';                                               // ensure null terminated
    return expr->len;                                                           // and return the length
}

// $INCREMENT(variable)
short Dincrement1(u_char *ret_buffer, mvar *var)
{
    u_char  tmp[MAX_NUM_BYTES];                                                 // some temp storage for arithmetic
    cstring *cptr;                                                              // for the call

    cptr = (cstring *) tmp;                                                     // point at the space
DISABLE_WARN(-Warray-bounds)
    cptr->len = 1;                                                              // length of one
    cptr->buf[0] = '1';                                                         // default to 1
    cptr->buf[1] = '\0';                                                        // null terminated
ENABLE_WARN
    return Dincrement2(ret_buffer, var, cptr);                                  // do it below
}

// $INCREMENT(variable[,numexpr])
short Dincrement2(u_char *ret_buffer, mvar *var, cstring *numexpr)
{
    int    t;                                                                   // for return values
    u_char temp[MAX_STR_LEN];                                                   // some temp storage for arithmetic
    u_char *tmp;

    tmp = temp;

    if (var->uci == UCI_IS_LOCALVAR) {                                          // for a local var
        t = ST_Get(var, temp);                                                  // attempt to get the data
    } else {                                                                    // for a global var
        memcpy(&partab.jobtab->last_ref, var, sizeof(var_u) + 5 + var->slen);   // update naked
        t = SemOp(SEM_ATOMIC, SEM_WRITE);                                       // take an atomic write
        if (t < 0) return (short) t;                                            // if we got an error then return it
        t = DB_Get(var, temp);                                                  // attempt to get the data
    }

    if ((t == -ERRM6) || (t == -ERRM7)) {                                       // use '0' for undefined var
        ret_buffer[0] = '0';
        t = 0;
    } else if (t < 0) {
        if (var->uci != UCI_IS_LOCALVAR) SemOp(SEM_ATOMIC, -SEM_WRITE);         // release the atomic write
        return (short) t;
    } else {
        t = ncopy(&tmp, ret_buffer);
    }

    if (t < 0) {
        if (var->uci != UCI_IS_LOCALVAR) SemOp(SEM_ATOMIC, -SEM_WRITE);         // release the atomic write
        return (short) t;
    }

    t = runtime_add((char *) numexpr->buf, (char *) ret_buffer);

    if (t < 0) {
        if (var->uci != UCI_IS_LOCALVAR) SemOp(SEM_ATOMIC, -SEM_WRITE);         // release the atomic write
        return (short) t;
    }

DISABLE_WARN(-Warray-bounds)
    numexpr->len = t;
ENABLE_WARN
    memcpy(&ret_buffer[0], &numexpr->buf[0], numexpr->len);                     // copy here
    ret_buffer[numexpr->len] = '\0';                                            // ensure null terminated
    if (var->uci == UCI_IS_LOCALVAR) return (short) ST_Set(var, numexpr);       // set it back and return
    t = DB_Set(var, numexpr);                                                   // set it back
    SemOp(SEM_ATOMIC, -SEM_WRITE);                                              // release the atomic write
    return (short) t;                                                           // and return
}

// $JUSTIFY(expr,int1[,int2])
int Djustify2(u_char *ret_buffer, cstring *expr, int size)
{
    int adj;                                                                    // adjust required
    int i;                                                                      // for loops

    if (size > MAX_STR_LEN) return -ERRM75;                                     // complain if too long
    adj = size - (int) expr->len;                                               // get number of spaces
    if (adj < 0) adj = 0;                                                       // ensure positive
    for (i = 0; i < adj; i++) ret_buffer[i] = ' ';                              // for each required space, copy in a space
    memmove(&ret_buffer[adj], &expr->buf[0], expr->len);                        // copy here
    i = expr->len + adj;                                                        // get the new size
    ret_buffer[i] ='\0';                                                        // nul terminate it
    return i;                                                                   // and return it
}                                                                               // end 2 arg $J()

// NOTE: We must have been passed a canonic number
int Djustify3(u_char *ret_buffer, cstring *expr, int size, int round)
{
    int i;
    int j = 0;
    int spc;                                                                    // leading space count
    int zer = 0;                                                                // leading zero required flag
    int len;
    int ru = -2;                                                                // round up flag
    int dp = -2;                                                                // decimal point
    int cop;

    if (round < 0) return -ERRM28;                                              // that's an error
    if (size > MAX_STR_LEN) return -ERRM75;                                     // complain if too long
    if (size < 0) size = 0;                                                     // if negative size then make it zero
    len = expr->len;

    for (i = 0; i < expr->len; i++) {
        if (expr->buf[i] == '.') {                                              // search for DP
            dp = i;
            break;
        }
    }

    if (!dp || ((dp == 1) && (expr->buf[0] == '-'))) zer = 1;                   // need to add a zero

    if (!round) {
        if (dp != -2) {
            len = dp;
            if (expr->buf[dp + 1] > '4') ru = dp - 1;
        }
    } else {
        if (dp != -2) {
            len = dp + round + 1;
            if ((len < expr->len) && (expr->buf[len] > '4')) ru = len - 1;
        } else {
            len += round + 1;
        }
    }

    spc = size - len - zer;                                                     // spaces required
    if (spc < 0) spc = 0;
    for (i = 0; i < spc; ret_buffer[i++] = ' ') continue;                       // copy in spaces

    if (expr->buf[0] == '-') {
        ret_buffer[i++] = '-';                                                  // copy minus
        j = 1;                                                                  // where copy starts
    }

    for (cop = 0; cop < zer; cop++) ret_buffer[i++] = '0';                      // possible leading zero
    cop = expr->len - j;
    if (len < expr->len) cop = len - j;
    memmove(&ret_buffer[i], &expr->buf[j], cop);                                // copy the rest
    i += cop;
    len += zer + spc;
    if ((dp == -2) && (i < len)) ret_buffer[i++] = '.';
    while (i < len) ret_buffer[i++] = '0';                                      // possible trailing zeroes
    ret_buffer[len] = '\0';                                                     // null terminate

    if (ru != -2) {
        ru += zer + spc;                                                        // adjust round up

        while (TRUE) {
            ret_buffer[ru]++;                                                   // increment it
            if (ret_buffer[ru] <= '9') break;                                   // stop when done
            ret_buffer[ru] = '0';                                               // change back to zero
            ru--;                                                               // decrement ru
            if (ret_buffer[ru] == '.') ru--;                                    // skip the DP
            if (ret_buffer[ru] == ' ') ret_buffer[ru] = '0';

            if ((ret_buffer[ru] == '-') && (ret_buffer[0] == ' ')) {            // Check for the 2016 case
                ret_buffer[ru--] = '1';
                ret_buffer[ru] = '-';
                break;
            }                                                                   // end 2016 patch

            if (ru >= j) continue;
            memmove(&ret_buffer[j + 1], &ret_buffer[j], len - j + 1);
            ret_buffer[j] = '1';
            len++;
            break;
        }
    }

    for (i = 0; i < len; i++) {
        if (ret_buffer[i] == '-') {
            for (j = i + 1; j < len; j++) {
                if ((ret_buffer[j] != '0') && (ret_buffer[j] != '.')) break;
            }

            if (j == len) {                                                     // found nothing useful
                if ((i != 0) || (len == size)) {
                    ret_buffer[i] = ' ';                                        // remove the minus
                    break;
                }

                memmove(&ret_buffer[0], &ret_buffer[1], len--);                 // or this way
                break;
            }
        } else if (ret_buffer[i] != ' ') {
            break;
        }
    }

    return len;
}

// $LENGTH(expr1[,expr2])
short Dlength1(u_char *ret_buffer, cstring *expr)
{
    return (short) ultocstring(ret_buffer, expr->len);                          // just do it
}

short Dlength2(u_char *ret_buffer, cstring *expr, cstring *delim)
{
    return (short) ltocstring(ret_buffer, Dlength2x(expr, delim));              // copy to buf and ret len
}

int Dlength2x(cstring *expr, cstring *delim)
{
    int i;                                                                      // temp
    int j;                                                                      // index for delim
    int pce = 1;                                                                // for version 2

    if (delim->len == 0) return 0;                                              // special case, return zero

    for (i = 0; i < (int) expr->len; i++) {                                     // scan expr
        for (j = 0; j != (int) delim->len; j++) {
            if (expr->buf[i + j] != delim->buf[j]) break;                       // quit if not the same

            if ((j + 1) == (int) delim->len) {                                  // if at end of delim
                pce++;                                                          // count a piece
                i += j;                                                         // move i on a bit
            }                                                                   // end 'piece matches'
        }                                                                       // end compare loop
    }                                                                           // end of expr

    return pce;                                                                 // and return count
}

// $NAME(variable[,int])
short Dname1(u_char *ret_buffer, mvar *var)
{
    return Dname2(ret_buffer, var, MAX_NUM_SUBS);                               // use Dname2()
}

short Dname2(u_char *ret_buffer, mvar *var, int sub)
{
    if (sub < 0) return -ERRM39;                                                // Invalid $NAME argument
    return UTIL_String_Mvar(var, ret_buffer, sub);                              // do it elsewhere
}

// $ORDER(subscripted variable[,int])
short Dorder1(u_char *ret_buffer, mvar *var)
{
    return Dorder2(ret_buffer, var, 1);                                         // use Dorder2()
}

short Dorder2(u_char *ret_buffer, mvar *var, int dir)
{
    int   i = -1;                                                               // dir patch flag
    short s;
    int   realdir;

    if ((dir != 1) && (dir != -1) && ((dir != 2) && (systab->historic & HISTORIC_DNOK))) { // validate direction for $NEXT
        return -(ERRZ12 + ERRMLAST);                                            // complain on error
    }

    realdir = dir;
    if (dir == 2) realdir = 1;

    if (dir == -1) {                                                            // is it backwards?
        if ((var->key[var->slen - 1] == '\0') && (var->key[var->slen - 2] == '\0')) { // is it a nul?
            i = var->slen - 2;                                                  // position of first 0
            var->key[i] = '\377';                                               // change to 255
        }
    }

    if (var->uci == UCI_IS_LOCALVAR) {
        s = ST_Order(var, ret_buffer, realdir);                                 // for local var
    } else if (var->name.var_cu[0] == '$') {                                    // SSVN?
        s = SS_Order(var, ret_buffer, realdir);                                 // yes
    } else {
        memcpy(&partab.jobtab->last_ref, var, sizeof(var_u) + 5 + var->slen);   // update naked
        if (i != -1) partab.jobtab->last_ref.key[i] = '\0';                     // unfix from above
        s = DB_Order(var, ret_buffer, realdir);                                 // else it's global
    }

    if ((dir == 2) && (s == 0)) {                                               // last for $NEXT
        memcpy(ret_buffer, "-1\0", 3);                                          // change to -1
        s = 2;
    }

    return s;
}

// $PIECE(expr1,expr2[,int1[,int2]])
int Dpiece2(u_char *ret_buffer, cstring *expr, cstring *delim)
{
    return Dpiece4(ret_buffer, expr, delim, 1, 1);                              // use Dpiece4()
}

int Dpiece3(u_char *ret_buffer, cstring *expr, cstring *delim, int i1)
{
    return Dpiece4(ret_buffer, expr, delim, i1, i1);                            // use Dpiece4()
}

int Dpiece4(u_char *ret_buffer, cstring *expr, cstring *delim, int i1, int i2)
{
    int beg = 0;                                                                // start copy from
    int end;                                                                    // copy to
    int pce = 1;                                                                // current piece
    int f;                                                                      // found flag
    int j;                                                                      // for delim scan
    // NOTE: support negative offsets
    //int np;                                                                     // number of pieces

    ret_buffer[0] = '\0';                                                       // just in case
    if (delim->len == 0) return 0;                                              // null delimiter -> nul str
    /* NOTE: support negative offsets
    np = Dlength2x(expr, delim);                                                // get number of pieces
    if (i1 < 0) i1 += np + 1;                                                   // support negative arguments
    if (i2 < 0) i2 += np + 1;                                                   // support negative arguments
    */
    if (i1 < 0) i1 = 0;                                                         // minus makes no sense
    if (i2 < 0) i2 = 0;                                                         // minus makes no sense
    if ((i1 == 0) && (i2 == 0)) return 0;                                       // piece 0 is null str
    if (i1 > i2) return 0;                                                      // that's also null

    for (end = 0; end < expr->len; end++) {                                     // scan expr
        if (expr->buf[end] == delim->buf[0]) {                                  // if first char matches
            f = 1;                                                              // set found flag

            for (j = 1; j < delim->len; j++) {                                  // scan rest of delimiter
                if (expr->buf[end + j] != delim->buf[j]) {                      // if we have a mismatch
                    f = 0;                                                      // clear found flag
                    break;                                                      // and quit
                }
            }                                                                   // end delim scan

            if (f == 1) {                                                       // just quit the if on fail
                if (pce == i2) {                                                // if this is last piece
                    end--;                                                      // point at last required char
                    break;                                                      // and quit for loop
                }                                                               // end last piece processing

                pce++;                                                          // increment current piece
                end += delim->len - 1;                                          // point at last char of delim
                if (pce == i1) beg = end + 1;                                   // if this is the first pce
            }                                                                   // end found code
        }                                                                       // end of got match
    }                                                                           // end of expr scan

    if (pce < i1) return 0;                                                     // didn't find anything
    if (end == expr->len) end--;                                                // don't point past end
    j = end - beg + 1;                                                          // number of bytes we want
    memmove(ret_buffer, &expr->buf[beg], j);                                    // copy here
    ret_buffer[j] = '\0';                                                       // null terminate it
    return j;                                                                   // return count
}

// $QUERY(variable[,int])
short Dquery1(u_char *ret_buffer, mvar *var)
{
    return Dquery2(ret_buffer, var, 1);                                         // use Dquery2()
}

short Dquery2(u_char *ret_buffer, mvar *var, int dir)
{
    int i = -1;                                                                 // dir patch flag

    if ((dir != 1) && (dir != -1)) return -(ERRZ12 + ERRMLAST);                 // validate direction or complain on error

    if (dir == -1) {                                                            // is it backwards?
        if ((var->key[var->slen - 1] == '\0') && (var->key[var->slen - 2] == '\0')) { // is it a nul?
            i = var->slen - 2;                                                  // position of first 0
            var->key[i] = '\377';                                               // change to 255
        }
    }

    if (var->uci == UCI_IS_LOCALVAR) return ST_Query(var, ret_buffer, dir);     // for local var
    if (var->name.var_cu[0] == '$') return -ERRM38;                             // SSVN? then no such
    memcpy(&partab.jobtab->last_ref, var, sizeof(var_u) + 5 + var->slen);       // update naked
    if (i != -1) partab.jobtab->last_ref.key[i] = '\0';                         // unfix from above
    return DB_Query(var, ret_buffer, dir);                                      // else it's global
}

// $RANDOM(int)
short Drandom(u_char *ret_buffer, int seed)
{
    if (seed < 1) return -ERRM3;                                                // an error
    seed = random() % seed;                                                     // get a random number
    return ltocstring(ret_buffer, seed);                                        // convert answer to string
}

// $REVERSE(expr)
int Dreverse(u_char *ret_buffer, cstring *expr)
{
    int j = 0;                                                                  // destination

    for (int i = (int) expr->len - 1; i >= 0; i--) ret_buffer[j++] = expr->buf[i]; // for each character copy it
    ret_buffer[j] = '\0';                                                       // terminate it
    return expr->len;                                                           // and return count
}

/*
 * $STACK(int[,code])
 *
 * Retn:  $ST(-1) returns the largest value for which $ST(value) returns a
 *          non-empty string.
 *        $ST(0) returns an implementation specific value indicating how this
 *          process was started. RUN or JOB.
 *        $ST(n) where n is 1 to $ST(-1) returns how this level of process stack
 *          was created (one of DO, XECUTE, $$ or an error code like ",M6,").
 *
 *        While int is zero or greater, the following codes may be used:
 *        ECODE  the list of ecodes added at this level
 *        MCODE  the source line of code identified by "PLACE" below
 *        PLACE  the location of a command at this stack level as follows:
 *               a) if int is not equal to $STACK and $ST(int,"ECODE") is
 *                  empty, the last command executed.
 *               b) if int is equal to $STACK and $ST(int,"ECODE") is
 *                  empty, the currently executing command.
 *               c) if $ST(int,"ECODE") is not empty, the last command to
 *                  start execution while $ST(int,"ECODE") was empty.
 */
short Dstack1(u_char *ret_buffer, int level)
{
    return Dstack1x(ret_buffer, level, (partab.jobtab - partab.job_table));
}

short Dstack1x(u_char *ret_buffer, int level, int job)
{
    int i;                                                                      // a useful int

    ret_buffer[0] = '\0';                                                       // null terminate
    if (level < -1) return 0;                                                   // junk
    i = partab.job_table[job].cur_do;                                           // default

    if (partab.job_table[job].error_frame > partab.job_table[job].cur_do) {
        i = partab.job_table[job].error_frame;                                  // ensure we have the error bit
    }

    if (level > i) return 0;                                                    // nothing there
    if (level == -1) return ltocstring(ret_buffer, i);                          // return the number

    if (level == 0) {
        if (partab.job_table[job].dostk[0].type == TYPE_JOB) {
            return (short) mcopy((u_char *) "JOB", ret_buffer, 3);              // for a JOB command
        }

        return (short) mcopy((u_char *) "RUN", ret_buffer, 3);                  // normal run
    }

    if (level == partab.job_table[job].error_frame) level = STM1_FRAME;         // error frame adjust
    i = partab.job_table[job].dostk[level].type & 127;                          // get the type (what was high bit going to be for?)
    if (i == TYPE_RUN) return (short) mcopy((u_char *) "BREAK", ret_buffer, 5);
    if (i == TYPE_DO) return (short) mcopy((u_char *) "DO", ret_buffer, 2);
    if (i == TYPE_EXTRINSIC) return (short) mcopy((u_char *) "$$", ret_buffer, 2);
    if (i == TYPE_XECUTE) return (short) mcopy((u_char *) "XECUTE", ret_buffer, 6);
    ret_buffer[0] = '\0';
    return 0;                                                                   // else nothing
}

int Dstack2(u_char *ret_buffer, int level, cstring *code)
{
    return Dstack2x(ret_buffer, level, code, (partab.jobtab - partab.job_table));
}

int Dstack2x(u_char *ret_buffer, int level, cstring *code, int job)
{
    int     arg2 = 0;                                                           // arg 2 - 1 = ECODE, 2 = MCODE, 3 = PLACE
    var_u   *rounam;                                                            // routine name
    int     line;                                                               // line number
    int     i;                                                                  // a handy int
    u_char  *p;                                                                 // a handy pointer
    mvar    *var;                                                               // for ^$ROUTINE()
    u_char  temp[VAR_LEN + 4];                                                  // ditto
    cstring *cptr;                                                              // ditto
    int     t;                                                                  // ditto

    ret_buffer[0] = '\0';                                                       // null terminate
    if (level < 0) return 0;                                                    // junk
    i = partab.job_table[job].cur_do;                                           // default

    if (partab.job_table[job].error_frame > partab.job_table[job].cur_do) {
        i = partab.job_table[job].error_frame;                                  // ensure we have the error bit
    }

    if (level > i) return 0;                                                    // nothing there

    if (strncasecmp((const char *) code->buf, "ecode\0", 6) == 0) {
        arg2 = 1;
    } else if (strncasecmp((const char *) code->buf, "mcode\0", 6) == 0) {
        arg2 = 2;
    } else if (strncasecmp((const char *) code->buf, "place\0", 6) == 0) {
        arg2 = 3;
    } else {
        return -(ERRZ50 + ERRMLAST);                                            // junk
    }

    if (arg2 == 1) {                                                            // "ECODE"
        ret_buffer[0] = '\0';                                                   // assume nothing
        if (job != (partab.jobtab - partab.job_table)) return 0;                // can't find
        var = (mvar *) ret_buffer;                                              // use same space for mvar
        VAR_CLEAR(var->name);
        memcpy(&var->name.var_cu[0], "$ECODE", 6);                              // copy in $ECODE
        var->volset = 0;
        var->uci = UCI_IS_LOCALVAR;
        cptr = (cstring *) temp;                                                // some spare space
DISABLE_WARN(-Warray-bounds)
        cptr->len = ltocstring(cptr->buf, level);                               // setup for subscript
ENABLE_WARN
        var->slen = UTIL_Key_Build(cptr, &var->key[0]);
        t = ST_Get(var, ret_buffer);                                            // get and return
        if (t == -ERRM6) t = 0;                                                 // allow for not there
        return t;
    }

    if (level && (level == partab.job_table[job].error_frame)) level = STM1_FRAME; // error frame adjust

    if ((((partab.job_table[job].dostk[level].type & 127) == TYPE_XECUTE) ||
      ((partab.job_table[job].dostk[level].type & 127) == TYPE_RUN) ||
      ((partab.job_table[job].dostk[level].type & 127) == TYPE_JOB)) &&
      (var_empty(partab.job_table[job].dostk[level].rounam))) {
        if (arg2 == 2) {                                                        // "MCODE"
            ret_buffer[0] = '\0';                                               // JIC

            // no can do
            if (partab.job_table[job].cur_do < ((level == STM1_FRAME) ? partab.job_table[job].error_frame : level)) {
                return 0;
            }

            if (job != (partab.jobtab - partab.job_table)) return 0;            // can't find
            p = (u_char *) SOA(partab.job_table[job].dostk[level].routine);
            if (p == NULL) return 0;                                            // nothing there
            for (i = 0; (ret_buffer[i] = p[i]); i++) continue;                  // copy it
            return i;                                                           // return the count
        }

        return mcopy((u_char *) "@", ret_buffer, 1);                            // "PLACE"
    }

    rounam = &partab.job_table[job].dostk[level].rounam;                        // point at routine name
    line = partab.job_table[job].dostk[level].line_num;                         // get line number

    if (arg2 == 2) {                                                            // "MCODE"
        var = (mvar *) ret_buffer;                                              // use same space for mvar
        VAR_CLEAR(var->name);
        memcpy(&var->name.var_cu[0], "$ROUTINE", 8);                            // copy in $ROUTINE
        var->volset = partab.job_table[job].rvol;                               // volume number
        var->uci = partab.job_table[job].ruci;                                  // UCI number
        if (rounam->var_cu[0] == '%') var->uci = 1;                             // check for a percent routine
        cptr = (cstring *) temp;                                                // some spare space

        for (i = 0; i < VAR_LEN; i++) {                                         // copy name
            if (rounam->var_cu[i] == 0) break;                                  // quit when done
DISABLE_WARN(-Warray-bounds)
            cptr->buf[i] = rounam->var_cu[i];                                   // copy
        }

        cptr->buf[i] = '\0';                                                    // null terminate
        cptr->len = i;                                                          // save the length
        t = UTIL_Key_Build(cptr, &var->key[0]);                                 // make a key from it
        if (t < 0) return t;                                                    // die on error
        var->slen = (u_char) t;                                                 // save the length
        cptr->len = ltocstring(cptr->buf, line);                                // make a string from int
ENABLE_WARN
        t = UTIL_Key_Build(cptr, &var->key[var->slen]);                         // make a key from it
        if (t < 0) return t;                                                    // die on error
        var->slen += (u_char) t;                                                // save the length
        t = Dget1(ret_buffer, var);                                             // get data
        if (t < 0) t = 0;                                                       // ignore errors
        ret_buffer[t] = '\0';                                                   // null terminate
        return t;                                                               // and return
    }

    i = 0;                                                                      // the start
    ret_buffer[i++] = '+';                                                      // add plus
    i += ltocstring(&ret_buffer[i], line);                                      // add the line number
    ret_buffer[i++] = '^';                                                      // the name indicator

    for (arg2 = 0; arg2 < VAR_LEN; arg2++) {                                    // copy name
        if ((ret_buffer[i++] = rounam->var_cu[arg2]) == 0) break;
    }

    if (ret_buffer[i - 1] == '\0') i--;                                         // back up over null
    ret_buffer[i] = '\0';                                                       // null terminate
    return i;                                                                   // return length
}

/*
 * $TEXT(entryref)
 *   The entire string "entryref" is passed in one variable, eval it here
 */
int Dtext(u_char *ret_buffer, cstring *str)
{
    int     i = 0;                                                              // a handy int
    int     j = 0;                                                              // and another
    u_char  slen;                                                               // saved length
    int     t;                                                                  // for functions
    int     off = 1;                                                            // line offset
    u_char  rou[VAR_LEN + 4];                                                   // routine name
    u_char  tag[VAR_LEN + 4];                                                   // the tag
    cstring *cr;                                                                // the routine
    cstring *ct;                                                                // and the tag

    ret_buffer[0] = '\0';                                                       // JIC
    ct = (cstring *) &tag[0];                                                   // use it this way
    cr = (cstring *) &rou[0];                                                   // ditto
DISABLE_WARN(-Warray-bounds)
    ct->len = 0;                                                                // assume no tag
    cr->len = 0;                                                                // no routine for now
ENABLE_WARN

    if (memcmp(str->buf, "+0\0", 3) == 0) {                                     // $T(+0) ?
        for (i = 0; i < VAR_LEN; i++) {                                         // copy routine name
            if (!partab.jobtab->dostk[partab.jobtab->cur_do].rounam.var_cu[i]) {
                break;                                                          // quit when done
            }

            ret_buffer[i] = partab.jobtab->dostk[partab.jobtab->cur_do].rounam.var_cu[i]; // copy
        }

        ret_buffer[i] = '\0';                                                   // null terminate
        return i;                                                               // and exit
    }

    if ((str->buf[i] != '+') && (str->buf[i] != '^')) {                         // is there a tag
        while (j < VAR_LEN) {
            if ((i == 0) && (str->buf[i] == '%')) {                             // leading %
DISABLE_WARN(-Warray-bounds)
                ct->buf[j++] = str->buf[i++];                                   // copy it
                continue;                                                       // and go for more
            }

            if (isalnum(str->buf[i]) == 0) break;                               // done
            ct->buf[j++] = str->buf[i++];                                       // copy it
        }

        ct->buf[j] = '\0';                                                      // null terminate tag
        ct->len = j;                                                            // save the length
ENABLE_WARN
        off = 0;                                                                // change offset to zero

        while ((str->buf[i] != '+') && (str->buf[i] != '^') && (str->buf[i] != '\0')) {
            i++;                                                                // skip to + ^ or null
        }
    }                                                                           // end tag processing

    if (str->buf[i] == '+') {                                                   // if we have a plus
        off = 0;                                                                // clear offset
        i++;                                                                    // skip the +
        while (isdigit(str->buf[i]) != 0) off = (off * 10) + (str->buf[i++] - '0'); // for all digits, extract the offset
    }                                                                           // end offset stuff

    if ((str->buf[i] != '^') && (str->buf[i] != '\0')) return -(ERRZ12 + ERRMLAST); // complain
    j = 0;                                                                      // clear routine pointer

    if (str->buf[i] == '^') {                                                   // routine name
        i++;                                                                    // skip the ^

        while (j < VAR_LEN) {
            if ((j == 0) && (str->buf[i] == '%')) {                             // leading %
DISABLE_WARN(-Warray-bounds)
                cr->buf[j++] = str->buf[i++];                                   // copy it
                continue;                                                       // and go for more
            }

            if (isalnum(str->buf[i]) == 0) break;                               // done
            cr->buf[j++] = str->buf[i++];                                       // copy it
        }

        cr->buf[j] = '\0';                                                      // null terminate routine
        cr->len = j;                                                            // save the length
    } else {                                                                    // we need the current routine
        for (j = 0; j < VAR_LEN; j++) {
            if ((cr->buf[j] = partab.jobtab->dostk[partab.jobtab->cur_do].rounam.var_cu[j]) == '\0') {
                break;                                                          // copy till done
            }
        }

        cr->buf[j] = '\0';                                                      // null terminate routine
        cr->len = j;                                                            // save the length
    }

    if (cr->len == 0) return 0;                                                 // no routine supplied -> null
    VAR_CLEAR(partab.src_var.name);
    memcpy(&partab.src_var.name.var_cu[0], "$ROUTINE", 8);                      // setup for DB_Get
    partab.src_var.volset = partab.jobtab->rvol;                                // volume
    partab.src_var.uci = partab.jobtab->ruci;                                   // UCI
    if (cr->buf[0] == '%') partab.src_var.uci = 1;                              // manager routine? then point there
    partab.src_var.slen = 0;                                                    // init key size
    t = UTIL_Key_Build(cr, &partab.src_var.key[0]);                             // first key
    if (t < 0) return t;                                                        // die on error
    slen = (u_char) t;                                                          // save key size

    if (ct->len == 0) {                                                         // no tag?
        ct->len = ltocstring(ct->buf, off);                                     // cstring off
        t = UTIL_Key_Build(ct, &partab.src_var.key[slen]);                      // next key
        if (t < 0) return t;                                                    // die on error
        partab.src_var.slen = t + slen;                                         // save key size
        t = DB_Get(&partab.src_var, ret_buffer);                                // get it

        if (t < 0) {
            ret_buffer[0] = '\0';                                               // nothing
            t = 0;                                                              // zero length
        } else if (!off) {                                                      // just the name required?
            return mcopy(cr->buf, ret_buffer, cr->len);                         // return the name
        }

        return t;                                                               // and return it
    }

    for (j = 1; ; j++) {                                                        // need to read all lines
        cr->len = ltocstring(cr->buf, j);                                       // cstring j
        t = UTIL_Key_Build(cr, &partab.src_var.key[slen]);                      // next key
        if (t < 0) return t;                                                    // die on error
        partab.src_var.slen = t + slen;                                         // save key size
        t = DB_Get(&partab.src_var, ret_buffer);                                // get it

        if (t < 0) {
            ret_buffer[0] = '\0';                                               // nothing
            return 0;                                                           // zero length
        }

        for (i = 0; i < ct->len; i++) {                                         // check the tag
            if (ret_buffer[i] != ct->buf[i]) break;                             // quit if different
        }

        if (i < ct->len) continue;                                              // go for next if no match

        // must be space or ( or null
        if ((ret_buffer[i] != ' ') && (ret_buffer[i] != '(') && (ret_buffer[i] != '\0')) {
            continue;
        }

        if (off == 0) return t;                                                 // no offset - all done
        j += off;                                                               // add the offset
        cr->len = ltocstring(cr->buf, j);                                       // cstring j
ENABLE_WARN
        t = UTIL_Key_Build(cr, &partab.src_var.key[slen]);                      // next key
        if (t < 0) return t;                                                    // die on error
        partab.src_var.slen = t + slen;                                         // save key size
        t = DB_Get(&partab.src_var, ret_buffer);                                // get it

        if (t < 0) {
            ret_buffer[0] = '\0';                                               // nothing
            t = 0;                                                              // zero length
        }

        return t;                                                               // done
    }
}

// $TRANSLATE(expr1,expr2[,expr3])
int Dtranslate2(u_char *ret_buffer, cstring *expr1, cstring *expr2)
{
    u_short us = 0;                                                             // arg 3

    return Dtranslate3(ret_buffer, expr1, expr2, (cstring *) &us);
}

int Dtranslate3(u_char *ret_buffer, cstring *expr1, cstring *expr2, cstring *expr3)
{
    u_int i1;                                                                   // for expr1
    u_int i2;                                                                   // for expr2
    int   p = 0;                                                                // pointer to ret_buffer

    for (i1 = 0; i1 != expr1->len; i1++) {                                      // scan expr1
        short found = FALSE;                                                    // assume no match

        for (i2 = 0; i2 != expr2->len; i2++) {                                  // scan expr2 for char
            if (expr1->buf[i1] == expr2->buf[i2]) {                             // if we have a match
                found = TRUE;                                                   // say so
                if (i2 < expr3->len) ret_buffer[p++] = expr3->buf[i2];          // match in expr3? copy in replacement char
                break;                                                          // and quit this loop
            }
        }

        if (!found) ret_buffer[p++] = expr1->buf[i1];                           // copy character
    }

    ret_buffer[p] = '\0';                                                       // terminate it
    return p;                                                                   // and return count
}

// $VIEW(channel#,location[,size[,value]])
int Dview(u_char *ret_buffer, int chan, int loc, int size, cstring *value)
{
    u_char *vb;                                                                 // view buffer address

    if ((chan > -1) || (chan < -MAX_VOL)) return -(ERRZ63 + ERRMLAST);          // must be negative for now
    chan = (-chan) - 1;                                                         // negate it and 0 base
    if (partab.jobtab->view[chan] == NULL) return -(ERRZ63 + ERRMLAST);         // got a block? no - die
    vb = (u_char *) SOA(SOA(partab.jobtab->view[chan])->mem);                   // get block memory address

    if ((loc < 0) || (size < 1) || ((loc + size) > (int) SOA(partab.vol[chan]->vollab)->block_size)) {
        return -(ERRZ63 + ERRMLAST);                                            // out of range - die
    }

    vb += loc;                                                                  // offset to location

    if (value == NULL) {                                                        // a read?
        if (size == 1) return ultocstring(ret_buffer, *vb);                     // one byte
        if (size == 2) return ultocstring(ret_buffer, *((u_short *) vb));       // two bytes
        if (size == 4) return ultocstring(ret_buffer, *((u_int *) vb));         // four bytes
        return mcopy(vb, ret_buffer, size);                                     // return the string
    }

    ret_buffer[0] = '\0';                                                       // null terminate

    if ((size == 1) || (size == 2) || (size == 4)) {                            // int type?
        int i;

        i = cstringtoi(value);                                                  // make int of it

        if (size == 1) {
            *vb = (u_char) i;
        } else if (size == 2) {
            *((u_short *) vb) = (u_short) i;
        } else {
            *((u_int *) vb) = i;                                                // set some int type
        }
    } else {
        if (size != value->len) return -(ERRZ63 + ERRMLAST);                    // junk
        memcpy(vb, value->buf, size);                                           // copy whatever
    }

    return 0;                                                                   // return OK
}

// set $EXTRACT
int DSetextract(u_char *tmp, cstring *cptr, mvar *var, int i1, int i2)
{
    cstring *vptr;                                                              // where the variable goes
    int     t;                                                                  // for the functions

    if (i1 < 1) i1 = 1;                                                         // ensure i1 positive
    if (i1 > i2) return 0;                                                      // ignore that, it's junk
    if (i2 > MAX_STR_LEN) return -ERRM75;                                       // complain if too long
    vptr = (cstring *) tmp;                                                     // where it goes
    t = Dget1(vptr->buf, var);                                                  // get current value
    if (t < 0) return t;                                                        // die on error
    vptr->len = (u_short) t;                                                    // save the size
    for (int i = t; i < i1; vptr->buf[i++] = ' ') continue;                     // ensure enough spaces

    if (t <= i2) {                                                              // if no trailing left
        t = mcopy(cptr->buf, &vptr->buf[i1 - 1], cptr->len);                    // copy it in
        if (t < 0) return t;                                                    // check for overflow
        vptr->len = i1 - 1 + cptr->len;                                         // the new length
        if (var->uci == UCI_IS_LOCALVAR) return ST_Set(var, vptr);              // set it back and return
        return DB_Set(var, vptr);                                               // set it back and return
    }

    if ((i2 - i1 + 1) != cptr->len) {                                           // not an exact fit?
        t = mcopy(&vptr->buf[i2], &vptr->buf[i1 - 1 + cptr->len], vptr->len - i2 + 2); // move tail here
        if (t < 0) return t;                                                    // check overflow
    }

    memmove(&vptr->buf[i1 - 1], cptr->buf, cptr->len);                          // can't use mcopy() here
    vptr->len = vptr->len - (i2 - i1 + 1) + cptr->len;
    if (var->uci == UCI_IS_LOCALVAR) return ST_Set(var, vptr);                  // set it back and return
    return DB_Set(var, vptr);                                                   // set it back and return
}

// set $PIECE
int DSetpiece(u_char *tmp, cstring *cptr, mvar *var, cstring *dptr, int i1, int i2)
{
    cstring *vptr;                                                              // where the variable goes
    int     t;                                                                  // for the functions
    int     beg = 0;                                                            // start copy from
    int     end;                                                                // copy to
    int     pce = 1;                                                            // current piece
    int     f;                                                                  // found flag
    int     j;                                                                  // for delim scan
    int     np;                                                                 // number of pieces

    if (i1 < 1) i1 = 1;                                                         // ensure i1 positive
    if (i1 > i2) return 0;                                                      // ignore that, it's junk
    vptr = (cstring *) tmp;                                                     // where it goes
    t = Dget1(vptr->buf, var);                                                  // get current value
    if (t < 0) return t;                                                        // die on error
    vptr->len = t;                                                              // save the size

    if (dptr->len == 0) {                                                       // null delimiter ?
        t = mcopy(cptr->buf, &vptr->buf[vptr->len], cptr->len);                 // copy at end
        if (t < 0) return t;                                                    // die on error
        vptr->len += cptr->len;                                                 // the new length
        if (var->uci == UCI_IS_LOCALVAR) return ST_Set(var, vptr);              // set it back and return
        return DB_Set(var, vptr);                                               // set it back and return
    }

    np = Dlength2x(vptr, dptr);                                                 // get number of pieces

    if (np < i1) {                                                              // current < = start
        f = i1 - np;                                                            // delimiters required

        for (j = 0; j < f; j++) {                                               // for each required delimiter
            t = mcopy(dptr->buf, &vptr->buf[vptr->len], dptr->len);             // copy 1 delim
            if (t < 0) return t;                                                // check for overflow
            if ((vptr->len + t) > MAX_STR_LEN) return -ERRM75;
            vptr->len += t;                                                     // add to length
        }

        t = mcopy(cptr->buf, &vptr->buf[vptr->len], cptr->len);                 // copy in source
        if ((vptr->len + t) > MAX_STR_LEN) return -ERRM75;
        vptr->len += t;                                                         // add to length
        if (var->uci == UCI_IS_LOCALVAR) return ST_Set(var, vptr);              // set it back and return
        return DB_Set(var, vptr);                                               // set it back and return
    }

    for (end = 0; end < vptr->len; end++) {                                     // scan expr
        if (vptr->buf[end] == dptr->buf[0]) {                                   // if first char matches
            f = 1;                                                              // set found flag

            for (j = 1; j < dptr->len; j++) {                                   // scan rest of delimiter
                if (vptr->buf[end + j] != dptr->buf[j]) {                       // if we have a mismatch
                    f = 0;                                                      // clear found flag
                    break;                                                      // and quit
                }
            }                                                                   // end delim scan

            if (f == 1) {                                                       // just quit the if on fail
                if (pce == i2) {                                                // if this is last piece
                    end--;                                                      // point at last required char
                    break;                                                      // and quit for loop
                }                                                               // end last piece processing

                pce++;                                                          // increment current piece
                end += dptr->len - 1;                                           // point at last char of delim
                if (pce == i1) beg = end + 1;                                   // if this is the first pce
            }                                                                   // end found code
        }                                                                       // end of got match
    }                                                                           // end of expr scan

    if (np == i1) {                                                             // replace last piece
        t = mcopy(cptr->buf, &vptr->buf[beg], cptr->len);                       // copy it
        if (t < 0) return t;                                                    // check overflow
        vptr->len = beg + cptr->len;                                            // fixup length
        if (var->uci == UCI_IS_LOCALVAR) return ST_Set(var, vptr);              // set it back and return
        return DB_Set(var, vptr);                                               // set it back and return
    }

    if (end >= vptr->len) end = vptr->len - 1;                                  // don't point past end
    i1 = beg;                                                                   // start of cut
    i2 = end;                                                                   // end of cut

    if ((i2 - i1 + 1) != cptr->len) {                                           // not an exact fit?
        t = mcopy(&vptr->buf[i2 + 1], &vptr->buf[i1 + cptr->len], vptr->len - i2 + 2); // move tail here
        if (t < 0) return t;                                                    // check overflow
    }

    if (cptr->len) memmove(&vptr->buf[i1], cptr->buf, cptr->len);               // can't use mcopy() here
    vptr->len = vptr->len - (i2 - i1 + 1) + cptr->len;                          // don't reduce to -=
    if (var->uci == UCI_IS_LOCALVAR) return ST_Set(var, vptr);                  // set it back and return
    return DB_Set(var, vptr);                                                   // set it back and return
}
