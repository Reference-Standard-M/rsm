/*
 * Package:  Reference Standard M
 * File:     rsm/util/key.c
 * Summary:  module database - Key Utilities
 *
 * David Wicksell <dlw@linux.com>
 * Copyright © 2020-2021 Fourth Watch Software LC
 * https://gitlab.com/Reference-Standard-M/rsm
 *
 * Based on MUMPS V1 by Raymond Douglas Newman
 * Copyright (c) 1999-2017
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
#include <string.h>                                                             // for memcpy/memcmp
#include <strings.h>
#include <sys/types.h>                                                          // for u_char def
#include "rsm.h"                                                                // standard includes
#include "proto.h"                                                              // standard prototypes
#include "error.h"                                                              // errors
#include "compile.h"                                                            // for RBD definition

/*
 * Function: UTIL_Key_Build - Build a key from an ASCII source
 * Key is in order: NEGATIVE NUMBER -> ZERO -> POSITIVE NUMBER -> STRING
 */
short UTIL_Key_Build(cstring *src, u_char *dest)                                // locn of source string and where to put it
{
    int minus = 0;                                                              // minus flag
    int dp = -1;                                                                // decimal point flag
    int to = 0;                                                                 // for dst[]
    int idx = 0;                                                                // for src->buf[]
    int i;                                                                      // for loops

    if (src->len > MAX_SUB_LEN) return -(ERRZ1 + ERRMLAST);                     // > MAX_SUB_LEN is illegal so complain

    if (src->len == 0) {                                                        // test for null string
        dest[to++] = '\0';                                                      // move in a null
        dest[to++] = '\0';                                                      // and another
        dest[to] = '\0';                                                        // null terminate it
        return (short) to;                                                      // return the count
    }                                                                           // end null string code

    if ((src->len == 1) && (src->buf[0] == '0')) {                              // test for a zero
        dest[to++] = 64;                                                        // move in $C(64)
        dest[to++] = '\0';                                                      // and the null
        dest[to] = '\0';                                                        // null terminate it
        return (short) to;                                                      // return the count
    }                                                                           // end zero code

    if (src->buf[idx] == '-') {                                                 // if it's negative
        idx++;                                                                  // increment pointer
        minus = 1;                                                              // flag minus
    }                                                                           // end skip "-"

    if (src->buf[idx] == '.') {
        dp = 0;                                                                 // check for starting dp
    } else if ((src->buf[idx] < '1') || (src->buf[idx] > '9')) {
        goto string;                                                            // check for a string key
    }

    for (i = idx + 1; i < src->len; i++) {                                      // remainder of string
        if (src->buf[i] == '.') {                                               // check for decmal point
            if (dp != -1) goto string;                                          // second dp is out
            dp = i - minus;                                                     // save dp position
        } else if ((src->buf[i] < '0') || (src->buf[i] > '9')) {
            goto string;                                                        // exit if not numeric
        }
    }                                                                           // end numeric check

    if ((dp != -1) && (src->buf[src->len - 1] == '0')) goto string;             // check trailing 0 after dp
    if (dp == (src->len - 1)) goto string;                                      // or dp is last char in str
    if (dp == -1) dp = (src->len - minus);                                      // get dp posn (assumed)
    if (dp > 63) goto string;                                                   // max 63 digits b4 dp

    if (!minus) {                                                               // do a positive number
        dest[to++] = (u_char) (dp + 64);                                        // copy in the count + flag
        for (i = 0; i < dp; i++) dest[to++] = src->buf[i];                      // copy up to dp
        for (i = dp + 1; i < src->len; i++) dest[to++] = src->buf[i];           // copy remainder
        dest[to++] = '\0';                                                      // trailing null for +ve
        dest[to] = '\0';                                                        // null terminate it
        return (short) to;                                                      // return the count
    }                                                                           // end of positive code

    dest[to++] = (u_char) (63 - dp);                                            // copy in one's complement of count

    for (i = idx; i < src->len; i++) {                                          // go thru the string
        if (src->buf[i] != '.') dest[to++] = (u_char) (57 - src->buf[i] + 48);  // ignore the dp and use nine's complement
    }

    dest[to++] = 255;                                                           // trailing -1
    dest[to] = '\0';                                                            // null terminate it
    return (short) to;                                                          // return the count

string:                                                                         // do a string key
    // The following is the string code
    dest[to++] = (u_char) 128;                                                  // copy in the flag

    for (i = 0; i < src->len; i++) {                                            // for each char in string
        if ((dest[to++] = src->buf[i]) == 0) return -(ERRZ5 + ERRMLAST);        // copy it and complain if null
    }

    dest[to++] = '\0';                                                          // trailing null
    dest[to] = '\0';                                                            // null terminate it
    return (short) to;                                                          // return the count
}                                                                               // end of Key_Build()

/*
 * Function: UTIL_Key_Extract - Extract a key from a key record
 * Updates cnt (count of characters at key).
 * Arguments:
 *   key: Where the key is
 *   str: The location of the destination string
 *   cnt: The address of the count of chars used
 *
 * NOTE: if cnt is passed in non-zero, double the quotes(")
 */
short UTIL_Key_Extract(u_char *key, u_char *str, int *cnt)
{
    int s;                                                                      // size
    int i = 0;                                                                  // index
    int j = 0;                                                                  // string count
    int idx = 0;                                                                // and another
    int flg;                                                                    // flag for quotes in string

    flg = *cnt;                                                                 // get the flag
    s = *key++;                                                                 // get first char

    if (((s == 0) || (s == 255)) && (*key == 0)) {                              // check for null str
        *cnt = 2;                                                               // used 2 bytes
        str[0] = '\0';                                                          // nul term
        return 0;
    }

    if (s & 128) {                                                              // if it's a string
        for (i = 0; key[i] != 0; i++) {                                         // loop thru
            str[j++] = key[i];                                                  // copy till done
            if ((key[i] == '"') && flg) str[j++] = '"';                         // double quote if reqd
            if (i > MAX_SUB_LEN) return -(ERRZ1 + ERRMLAST);                    // check size
        }

        str[j] = 0;                                                             // null terminate
        *cnt = i + 2;                                                           // store bytes used
        return (short) j;                                                       // return string count
    }                                                                           // end of string processing

    if (s & 64) {                                                               // if it's a positive number
        s &= 63;                                                                // extract dp position

        if ((*key == '\0') && (s == 0)) {                                       // check for numeric 0
            str[idx++] = '0';                                                   // add zero
            str[idx] = '\0';                                                    // null terminate
            *cnt = 2;                                                           // used 2
            return (short) idx;                                                 // return length (1)
        }

        for (i = 0; i < s; i++) str[idx++] = *key++;                            // copy to dp
        str[idx] = 0;                                                           // null term (in case)
        *cnt = s + 2;                                                           // assume no dp, save count
        if (*key == '\0') return (short) idx;                                   // if char 0, all done
        str[idx++] = '.';                                                       // add the dp
        while ((str[idx++] = *key++)) s++;                                      // move to NULL, counting
        --idx;                                                                  // back to point at NULL
        if (s > MAX_SUB_LEN) return -(ERRZ1 + ERRMLAST);                        // check size
        *cnt = s + 2;                                                           // update count
        return (short) idx;                                                     // return string count
    }                                                                           // end of positive number

    s = 63 - s;                                                                 // get negative count
    str[idx++] = '-';                                                           // save minus sign
    for (i = 0; i < s; i++) str[idx++] = ('9' + '0' - *key++);                  // copy to dp, doing a nine's complement
    str[idx] = 0;                                                               // null term (in case)
    *cnt = s + 2;                                                               // update the count
    if (*key == 255) return (short) idx;                                        // if char 255, all done
    str[idx++] = '.';                                                           // add the dp

    while (TRUE) {                                                              // loop for end
        if (*key == 255) break;                                                 // check for end of string
        s++;                                                                    // count character
        str[idx++] = ('9' + '0' - *key++);                                      // copy a nine's complement
    }                                                                           // end while

    if (s > MAX_SUB_LEN) return -(ERRZ1 + ERRMLAST);                            // check size
    str[idx] = 0;                                                               // null term
    *cnt = s + 2;                                                               // update count
    return (short) idx;                                                         // return new key pointer
}

/* Function: UTIL_String_Key - Extract all keys to string
 * Arguments:
 *   key:      Where the key is -> count
 *   str:      The location of destination string
 *   max_subs: The max number of subscripts
 */
short UTIL_String_Key(u_char *key, u_char *str, int max_subs)
{
    int   count = 1;                                                            // bytes used and quote flag
    int   len;                                                                  // bytes in key
    int   idx = 0;                                                              // key index
    int   clen = 0;                                                             // len of returned string
    int   string = 0;                                                           // string indicator
    short ret;                                                                  // return value

    len = (int) key[idx++];                                                     // get key length
    str[clen++] = '(';                                                          // open bracket

    while (len > 1) {                                                           // while there are chars in key
        string = 0;                                                             // clear string ind

        if (key[idx] & 128) {                                                   // if it's a string
            string = 1;                                                         // flag it
            str[clen++] = '"';                                                  // add leading quote
        }

        ret = UTIL_Key_Extract(&key[idx], &str[clen], &count);                  // get one key
        if (ret < 0) return ret;                                                // die on error

        if (ret == 0) {                                                         // nul key
            string = 1;                                                         // flag it as a string
            str[clen++] = '"';                                                  // add leading quote
        }

        clen = clen + (int) ret;                                                // add to string length
        if (string == 1) str[clen++] = '"';                                     // add trailing quote
        len = len - count;                                                      // subtract used bytes
        idx = idx + count;                                                      // adjust key index
        str[clen++] = ',';                                                      // add a comma
        max_subs--;                                                             // count subscript
        if (max_subs < 1) break;                                                // give up if all done
    }

    clen--;                                                                     // last comma
    str[clen++] = ')';                                                          // replace with )
    str[clen] = '\0';                                                           // null terminate
    return (short) clen;                                                        // and return the length
}

int UTIL_Key_Last(mvar *var)                                                    // point at last subs in mvar
{
    int last = -1;                                                              // return value
    int i = 0;                                                                  // idx into var->key[]

    while (i < var->slen) {                                                     // while any there
        last = i;                                                               // save beginning

        if (var->key[i++] < 64) {                                               // negative number
            while ((var->key[i++] != 255) && (i < var->slen)) continue;         // scan to end
        } else {                                                                // positive or string
            while ((var->key[i++] != 0) && (i < var->slen)) continue;           // scan to end
        }
    }

    return last;                                                                // return the index
}

/* Function: UTIL_String_Mvar
 * Arguments:
 *   var:      Address of the mvar
 *   str:      The location of destination string
 *   max_subs: The max number of subscripts
 */
short UTIL_String_Mvar(mvar *var, u_char *str, int max_subs)
{
    int     i;                                                                  // for loops
    int     p = 0;                                                              // string pointer
    int     vol;                                                                // for volset
    uci_tab up;                                                                 // ptr to UCI tab
    var_u   *vt;                                                                // var table pointer
    rbd     *r;                                                                 // a handy pointer
    u_char  *ptr;                                                               // string ptr

    if (var->uci != UCI_IS_LOCALVAR) {                                          // if it's a global var
        str[p++] = '^';                                                         // lead off with the caret

        if (var->uci != 0) {                                                    // if an environment specified
            str[p++] = '[';                                                     // open bracket
            str[p++] = '"';                                                     // a leading quote
            vol = var->volset;                                                  // get vol
            if (vol == 0) vol = partab.jobtab->vol;                             // if none, get default
            up = systab->vol[vol - 1]->vollab->uci[var->uci - 1];               // UCI tab pointer

            for (i = 0; i < VAR_LEN; i++) {                                     // for each possible character
                if (up.name.var_cu[i] == '\0') break;                           // done if we hit a null
                str[p++] = up.name.var_cu[i];                                   // copy the character
            }

            str[p++] = '"';                                                     // a trailing quote

            if (var->volset != 0) {                                             // volset specified?
                str[p++] = ',';                                                 // copy in a comma
                str[p++] = '"';                                                 // a leading quote
                ptr = systab->vol[var->volset - 1]->vollab->volnam.var_cu;

                for (i = 0; i < VAR_LEN; i++) {                                 // for each possible character
                    if (ptr[i] == '\0') break;                                  // done if we hit a null
                    str[p++] = ptr[i];                                          // copy the character
                }

                str[p++] = '"';                                                 // a trailing quote
            }

            str[p++] = ']';                                                     // closing bracket
        }                                                                       // end environment stuff
    }                                                                           // end global specific stuff

    if ((var->uci == UCI_IS_LOCALVAR) && var->volset) {                         // special index type
        r = (rbd *) partab.jobtab->dostk[partab.jobtab->cur_do].routine;
        vt = (var_u *) (((u_char *) r) + r->var_tbl);                           // point at var table
        VAR_COPY(var->name, vt[var->volset - 1]);                               // get the var name
        var->volset = 0;                                                        // clear the volset
    }

    for (i = 0; i < VAR_LEN; i++) {                                             // now the name
        if (var->name.var_cu[i] == '\0') break;                                 // quit when done
        str[p++] = var->name.var_cu[i];                                         // copy a byte
    }

    if ((var->slen != 0) && (max_subs > 0)) {                                   // if there are subscripts
        i = UTIL_String_Key(&var->slen, &str[p], max_subs);                     // do the subscripts
        if (i < 0) return (short) i;                                            // quit on error
        p = p + i;                                                              // add to length
    }

    str[p] = '\0';                                                              // null terminate
    return (short) p;                                                           // return the length
}

/* Returns number of subscripts or negative error message
 * Arguments:
 *   src: The string
 *   var: The destination mvar
 */
short UTIL_MvarFromCStr(cstring *src, mvar *var)
{
    int     i;                                                                  // a handy int
    int     q;                                                                  // for quotes
    short   s;                                                                  // for functions
    int     subs = 0;                                                           // number of subscripts
    u_char  *ptr;                                                               // a handy pointer
    cstring *kb;                                                                // for key builds
    var_u   nam;                                                                // for name comparisons
    var_u   vol;                                                                // ditto
    u_char  tmp[MAX_KEY_SIZE + 5];                                              // temp area for subscripts

    kb = (cstring *) tmp;                                                       // make it a cstring
    var->volset = 0;                                                            // clear volset
    var->uci = UCI_IS_LOCALVAR;                                                 // assume local variable
    var->slen = 0;                                                              // and no subscripts
    VAR_CLEAR(var->name);                                                       // clear the name
    ptr = src->buf;                                                             // point at the source

    if (*ptr == '^') {                                                          // global?
        ptr++;                                                                  // skip the ^
        var->uci = 0;                                                           // not a local var

        if (*ptr == '[') {                                                      // environment specified?
            ptr++;                                                              // skip the [
            VAR_CLEAR(nam);                                                     // clear quadword
            if (*ptr++ != '"') return -(ERRZ12 + ERRMLAST);                     // must be a quote so complain
            i = 0;                                                              // clear an index

            while (*ptr != '"') {                                               // scan to end of literal
                if (i == VAR_LEN)                                               // check for too many
                return -(ERRZ12 + ERRMLAST);                                    // complain
                nam.var_cu[i++] = *ptr++;                                       // copy a byte
            }

            ptr++;                                                              // go past closing "

            if (*ptr == ',') {                                                  // vol name too ?
                ptr++;                                                          // skip the ,
                VAR_CLEAR(vol);                                                 // clear quadword
                if (*ptr++ != '"') return -(ERRZ12 + ERRMLAST);                 // must be a quote so complain
                i = 0;                                                          // clear an index

                while (*ptr != '"') {                                           // scan to end of literal
                    if (i == VAR_LEN) return -(ERRZ12 + ERRMLAST);              // check for too many, if so, complain
                    vol.var_cu[i++] = *ptr++;                                   // copy a byte
                }

                ptr++;                                                          // go past closing "

                for (i = 0; i < MAX_VOL; i++) {                                 // scan vol list
                    if (systab->vol[i] != NULL) {                               // vol here ?
                        if (var_equal(systab->vol[i]->vollab->volnam, vol)) break; // quit if found
                    }
                }

                if (i == MAX_VOL) return -ERRM26;                               // no such, complain
                var->volset = i + 1;                                            // store the vol#
            }

            if (var->volset == 0) var->volset = partab.jobtab->vol;             // default

            for (i = 0; i < UCIS; i++) {                                        // scan UCI list (vol 0)
                if (var_equal(systab->vol[var->volset - 1]->vollab->uci[i].name, nam)) {
                    break;                                                      // quit if found
                }
            }

            if (i == UCIS) return -ERRM26;                                      // no such, complain
            var->uci = i + 1;                                                   // store the UCI#
            if (*ptr++ != ']') return -ERRM26;                                  // don't allow volume for now so give error instead
        }
    }                                                                           // end special global stuff

    for (i = 0; i < VAR_LEN; i++) {                                             // now the name
        if ((*ptr == '(') || (*ptr == '\0')) break;                             // subs or end of str? then quit
        var->name.var_cu[i] = *ptr++;                                           // copy a byte
    }

    if (*ptr == '\0') return 0;                                                 // end of string - all done
    if (*ptr++ != '(') return -(ERRZ12 + ERRMLAST);                             // must be a (, if not complain

    while (TRUE) {                                                              // till we run out of subs
        if (*ptr == '\0') return -(ERRZ12 + ERRMLAST);                          // junk
        q = (*ptr == '"');                                                      // check for quotes
        if (q) ptr++;                                                           // skip the quote
        i = 0;                                                                  // init index

        while (TRUE) {                                                          // move 1 subs
            if (*ptr == '\0') return -(ERRZ12 + ERRMLAST);                      // junk

            if (*ptr == '"' && q) {                                             // quote
                ptr++;                                                          // skip it

                if (*ptr != '"') {                                              // next not a quote
                    if (*ptr == ',') break;                                     // done
                    if (*ptr == ')') break;                                     // also done
                    return -(ERRZ12 + ERRMLAST);                                // junk
                }
            }                                                                   // end quote processing

            if (i == 255) return -(ERRZ12 + ERRMLAST);                          // junk
            if (!q && ((*ptr == ',') || (*ptr == ')'))) break;                  // end numeric subs
DISABLE_WARN(-Warray-bounds)
            kb->buf[i++] = *ptr++;                                              // copy one character
        }                                                                       // end copy 1 subs

        kb->buf[i] = '\0';                                                      // null terminate
        kb->len = (u_short) i;                                                  // save the length
ENABLE_WARN
        s = UTIL_Key_Build(kb, &var->key[var->slen]);                           // do one key
        if (s < 0) return s;                                                    // got an error
        if ((s + var->slen) > 255) return -(ERRZ12 + ERRMLAST);                 // junk
        if ((var->key[var->slen] == 128) && !q) return -(ERRZ12 + ERRMLAST);    // got a string + no quotes, that's junk
        subs++;                                                                 // count a subscript
        var->slen = s + var->slen;                                              // save new length

        if (*ptr == ',') {                                                      // comma?
            ptr++;                                                              // skip it
            continue;                                                           // and keep going
        }

        if (*ptr == ')') {                                                      // trailing bracket
            ptr++;                                                              // skip it
            break;                                                              // and quit
        }

        return -(ERRZ12 + ERRMLAST);                                            // junk
    }

    if (*ptr != '\0') return -(ERRZ12 + ERRMLAST);                              // junk
    return (short) subs;                                                        // all OK
}

/*
 * Function:      UTIL_Key_KeyCmp
 * Description:   Compares the keys
 * Return Values: K2_LESSER, K2_GREATER, KEQUAL.
 */
int UTIL_Key_KeyCmp(u_char *key1, u_char *key2, int kleng1, int kleng2)
{
    int cmpvar;                                                                 // comparison variable

    cmpvar = memcmp(key1, key2, ((kleng1 < kleng2) ? kleng1 : kleng2));         // compare keys

    if (!cmpvar) {                                                              // if start of keys is same
        if (kleng1 == kleng2) return KEQUAL;                                    // and lengths are = keys are the same
        if (kleng1 > kleng2) return K2_LESSER;                                  // if length of key 1 if > key1 sorts after key2
        return K2_GREATER;                                                      // key2 sorts after key1
    }

    if (cmpvar > 0) return K2_LESSER;                                           // if value of key1 is > key1 sorts after key2
    return K2_GREATER;                                                          // key2 sorts after key1
}

/*
 * Function:      Chars_In_Subs
 * Description:   Count the chars in a number of subscripts of a key.
 *                Will also return number of subs and copy those subs
 *                if either (int *) or last (char *) is not NULL.
 * Return Values: Number of chars in subscripts counted.
 */
int UTIL_Key_Chars_In_Subs(char *Key, int keylen, int maxsubs, int *subs, char *KeyBuffer)
{
    int cnt;                                                                    // subs counts
    int i;                                                                      // char counts

    cnt = 0;                                                                    // initialize
    i = 0;                                                                      // these two

    while ((i < keylen) && (cnt < maxsubs)) {                                   // while still in key
        if ((Key[i] & 128) || (Key[i] & 64)) {                                  // if +ve no. or string
            for (i++; Key[i]; i++) continue;                                    // loop til find NULL
            i++;                                                                // skip NULL char
        } else {                                                                // else if -ve
            for (i++; (Key[i] != -1); i++) continue;                            // loop til find $C(255)
            i++;                                                                // skip past 255
        }

        cnt++;                                                                  // increment subs count
    }

    if (subs != NULL) *subs = cnt;                                              // if we should remember subs, then copy
    if (KeyBuffer != NULL) bcopy(Key, KeyBuffer, i);                            // if we want the chars, then copy them
    return i;                                                                   // return no. of chars
}
