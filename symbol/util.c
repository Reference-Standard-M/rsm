/*
 * Package: Reference Standard M
 * File:    rsm/symbol/util.c
 * Summary: module symbol - symbol table utilities
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

#include "symbol.h"                                                             // our definitions
#include "compile.h"                                                            // for routine buffer stuff
#include "error.h"                                                              // errors
#include "init.h"                                                               // init prototypes
#include "proto.h"                                                              // standard prototypes
#include <ctype.h>
#include <stdlib.h>                                                             // always include
#include <string.h>                                                             // for string ops

short         st_hash[ST_HASH + 1];                                             // allocate hashing table
symtab_struct symtab[ST_MAX + 1];                                               // and symbol table

/*
 * Function: FixData(ST_data *old, ST_data *new, int count)
 * When the data pointer changes, this fixes count pointers to it
 * Only called internally from this file
 */
static void FixData(const ST_data *old, ST_data *new, int count)
{
    int       i;                                                                // for loops
    ST_newtab *newtab;                                                          // for NEW tables

    for (i = 0; i < ST_MAX; i++) {                                              // scan symtab[]
        if (symtab[i].data == old) {                                            // same as our old one
            symtab[i].data = new;                                               // change to new one
            count--;                                                            // decrement the count
            if (count == 0) return;                                             // quit when done
        }
    }

    i = partab.jobtab->cur_do;                                                  // get current do level

    while (i) {                                                                 // for each one
        newtab = (ST_newtab *) partab.jobtab->dostk[i--].newtab;                // get newtab

        while (newtab != NULL) {                                                // for each table
            for (int c = 0; c < newtab->count_new; c++) {                       // for each variable
                if (newtab->locdata[c].data == old) {                           // if this is one
                    newtab->locdata[c].data = new;                              // copy in new value
                    count--;                                                    // count that
                    if (count == 0) return;                                     // quit when done
                }
            }

            newtab = newtab->fwd_link;                                          // get next
        }
    }
}

/*
 * Function: ST_Hash - Create a hash from a variable name
 * returns hash number
 */
static short ST_Hash(var_u var)                                                 // var name in a quad
{
    int ret = 0;                                                                // return value

    const int p[32] = {
        3, 5, 7, 11, 13, 17, 19, 23,
        29, 31, 37, 41, 43, 47, 53, 59,
        61, 67, 71, 73, 79, 83, 89, 97,
        101, 103, 107, 109, 113, 127, 131, 137
    };                                                                          // odd primes

    for (int i = 0; i < VAR_LEN; i++) {                                         // for each character
        if (var.var_cu[i] == 0) break;
        ret += var.var_cu[i] * p[i];
    }

    return (short) (ret % ST_HASH);                                             // return mod hash value
}                                                                               // end of ST_Hash

/*
 * Function: ST_Free - free varname entry in symbol table
 * returns nothing - only called when var exists
 */
static void ST_Free(var_u var)                                                  // var name in a quad
{
    short hash;                                                                 // hash value
    short fwd;                                                                  // fwd link pointer
    short last;                                                                 // last entry encountered

    hash = ST_Hash(var);                                                        // get hash value
    last = -hash;                                                               // save last value
    fwd = st_hash[hash];                                                        // get pointer (if any)

    while (fwd != -1) {                                                         // while there are links
        if (var_equal(symtab[fwd].varnam, var)) break;                          // quit if we found it
        last = fwd;                                                             // save last address
        fwd = symtab[fwd].fwd_link;                                             // get next pointer (if any)
    }                                                                           // end search loop

    if (fwd == -1) return;                                                      // symbol wasn't there, nothing to free

    if (last == -hash) {                                                        // if it was top
        st_hash[hash] = symtab[fwd].fwd_link;                                   // remove this way
    } else {                                                                    // if it's a symtab entry
        symtab[last].fwd_link = symtab[fwd].fwd_link;                           // do it this way
    }

    symtab[fwd].data = ST_DATA_NULL;                                            // in case it hasn't been removed
    VAR_CLEAR(symtab[fwd].varnam);                                              // clear var name
    symtab[fwd].fwd_link = st_hash[ST_FREE];                                    // point at next free
    st_hash[ST_FREE] = fwd;                                                     // point free list at this
    return;                                                                     // all done
}                                                                               // end of ST_Free()

/*
 * Function: ST_Init - initialize an empty symbol table
 * returns nothing
 */
void ST_Init(void)                                                              // no arguments
{
    for (int i = 0; i < ST_HASH; i++) st_hash[i] = -1;                          // clear hash table, -1 means empty
    st_hash[ST_FREE] = 0;                                                       // head of free list

    for (int i = 0; i < ST_MAX; i++) {                                          // for each symbol entry
        symtab[i].fwd_link = i + 1;                                             // point to next entry
        symtab[i].usage = 0;                                                    // clear usage count
        symtab[i].data = ST_DATA_NULL;                                          // clear data pointer
        VAR_CLEAR(symtab[i].varnam);                                            // clear variable name
    }                                                                           // end symtab clear loop

    symtab[ST_MAX].fwd_link = -1;                                               // indicate end of list
    return;                                                                     // done
}                                                                               // end of ST_Init()

/*
 * Function: ST_Locate - locate varname in symbol table
 * Return the symtab entry number or -1 on fail
 */
short ST_Locate(var_u var)                                                      // var name in a quad
{
    int   hash;                                                                 // hash value
    short fwd;                                                                  // fwd link pointer

    hash = ST_Hash(var);                                                        // get hash value
    fwd = st_hash[hash];                                                        // get pointer (if any)

    while (fwd != -1) {                                                         // while there are links
        if (var_equal(symtab[fwd].varnam, var)) return fwd;                     // if var names match then return if we found it
        fwd = symtab[fwd].fwd_link;                                             // get next pointer (if any)
    }                                                                           // end search loop

    return -1;                                                                  // failed to find it
}                                                                               // end of ST_Locate()

/*
 * Function: ST_LocateIdx - locate in symbol table by index
 * Return the symtab entry number or negative error number
 */
short ST_LocateIdx(int idx)                                                     // var index
{
    short fwd;                                                                  // fwd link pointer
    var_u var;                                                                  // var name (if required)
    rbd   *p;                                                                   // for looking at routines
    var_u *vt;                                                                  // for the var table

    fwd = partab.jobtab->dostk[partab.jobtab->cur_do].symbol[idx];
    if (fwd > -1) return fwd;                                                   // got it
    p = (rbd *) SOA(partab.jobtab->dostk[partab.jobtab->cur_do].routine);
    vt = (var_u *) (((u_char *) p) + p->var_tbl);                               // point at var table
    VAR_COPY(var, vt[idx]);                                                     // get the var name
    fwd = ST_SymAtt(var);                                                       // attach and get index
    if (fwd < 0) return fwd;                                                    // error if none free
    partab.jobtab->dostk[partab.jobtab->cur_do].symbol[idx] = fwd;              // save idx
    return fwd;                                                                 // return index
}                                                                               // end of ST_LocateIdx()

/*
 * Function: ST_Create - create/locate varname in symtab
 * Return the symtab entry number or negative error number
 */
short ST_Create(var_u var)                                                      // var name in a quad
{
    int   hash;                                                                 // hash value
    short fwd;                                                                  // fwd link pointer

    hash = ST_Hash(var);                                                        // get hash value
    fwd = st_hash[hash];                                                        // get pointer (if any)

    while (fwd != -1) {                                                         // while there are links
        if (var_equal(symtab[fwd].varnam, var)) return fwd;                     // return if we found it
        fwd = symtab[fwd].fwd_link;                                             // get next pointer (if any)
    }                                                                           // end search loop

    fwd = st_hash[ST_FREE];                                                     // get next free

    if ((fwd == -1) || ((fwd == ST_MAX) && (var.var_q != 76159689901348))) {    // ST_MAX can be used for $ECODE
        return -(ERRZ56 + ERRMLAST);                                            // error if none free
    }

    st_hash[ST_FREE] = symtab[fwd].fwd_link;                                    // unlink from free list
    symtab[fwd].fwd_link = st_hash[hash];                                       // link previous after this
    st_hash[hash] = fwd;                                                        // link this first
    symtab[fwd].usage = 0;                                                      // no NEWs or routine att*
    VAR_COPY(symtab[fwd].varnam, var);                                          // copy in variable name
    symtab[fwd].data = ST_DATA_NULL;                                            // no data just yet
    return fwd;                                                                 // return the pointer
}                                                                               // end of ST_Create()

/*
 * Function: ST_Kill - KILL a variable
 * returns nothing
 */
short ST_Kill(mvar *var)                                                        // var name in a quad
{
    short     ptr;                                                              // for the pointer
    ST_data   *data;                                                            // and ptr to data block
    ST_depend *check = ST_DEPEND_NULL;                                          // working dependent pointer
    ST_depend *checkprev = ST_DEPEND_NULL;                                      // previous dependent pointer

    if (var->volset) {                                                          // if by index
        ptr = ST_LocateIdx(var->volset - 1);                                    // get it this way
    } else {                                                                    // else locate by name
        ptr = ST_Locate(var->name);                                             // locate the variable
    }

    if (ptr < 0) return 0;                                                      // just return if no such
    data = symtab[ptr].data;                                                    // get pointer to the data

    if (data != ST_DATA_NULL) {                                                 // data block exists
        if (var->slen == 0) {                                                   // killing a data block
            check = data->deplnk;                                               // point at 1st DP block

            while (check != ST_DEPEND_NULL) {                                   // for all DP blocks
                checkprev = check;                                              // save a copy
                check = check->deplnk;                                          // get next
                free(checkprev);                                                // free this one
            }

            data->last_key = ST_DEPEND_NULL;                                    // reset last used key
            data->deplnk = ST_DEPEND_NULL;                                      // clear pointer
            data->dbc = VAR_UNDEFINED;                                          // dong it
        } else {                                                                // end killing data block, start killing dep block
            check = data->deplnk;                                               // get first dep if any

            if (check != ST_DEPEND_NULL) {                                      // if deps exist
                ST_depend *lastkey = data->last_key;                            // pointer to last used key

                // start search at last used key, rather than at the beginning (var after lastkey)
                if ((lastkey != ST_DEPEND_NULL) &&
                  (UTIL_Key_KeyCmp(var->key, lastkey->bytes, var->slen, lastkey->keylen) == K2_LESSER)) {
                    check = lastkey;
                }

                // check key less than supplied (var after check)
                while ((check != ST_DEPEND_NULL) &&
                  (UTIL_Key_KeyCmp(var->key, check->bytes, var->slen, check->keylen) == K2_LESSER)) {
                    checkprev = check;                                          // save current to previous
                    check = check->deplnk;                                      // go to next
                }                                                               // end if we go past it, or end

                // valid remove
                if ((check != ST_DEPEND_NULL) && (UTIL_Key_Cmp(check->bytes, var->key, check->keylen, var->slen) == KEQUAL)) {
                    ST_RemDp(data, checkprev, check, var);                      // get rid of it
                }                                                               // end if valid remove found

                data->last_key = checkprev;                                     // add last used key
            }                                                                   // end if dep exists
        }                                                                       // end else killing a dep blk

        if ((data->deplnk == ST_DEPEND_NULL) && (data->attach <= 1) && (data->dbc == VAR_UNDEFINED)) { // none attached
            free(data);                                                         // free data block space
            symtab[ptr].data = ST_DATA_NULL;                                    // and clear the pointer
        }                                                                       // end freeing data block
    }                                                                           // end if data block exists

    if ((symtab[ptr].data == ST_DATA_NULL) && (symtab[ptr].usage == 0)) {       // if no data block and no attaches or NEWs
        ST_Free(symtab[ptr].varnam);                                            // free this entry in symtab
    }

    return 0;                                                                   // all done
}                                                                               // end of ST_Kill()

/*
 * Function: ST_RemDp - Free a dependent block, if appropriate
 * returns nothing
 */
void ST_RemDp(ST_data *dblk, ST_depend *prev, ST_depend *dp, mvar *mvardr)
{
    if (dp == ST_DEPEND_NULL) return;                                           // no dependents to check

    // next dependency, has part match key
    if ((dp->deplnk != ST_DEPEND_NULL) && (UTIL_Key_Cmp(dp->bytes, mvardr->key, dp->keylen, mvardr->slen) == KEQUAL)) {
        ST_RemDp(dblk, dp, dp->deplnk, mvardr);                                 // try to get rid of next one
    }                                                                           // end if keys part match

    if (UTIL_Key_Cmp(dp->bytes, mvardr->key, dp->keylen, mvardr->slen) == KEQUAL) { // keys match to slen
        if (prev != ST_DEPEND_NULL) {                                           // if not removing first dep
            prev->deplnk = dp->deplnk;                                          // bypass a dep killee
        } else {                                                                // end if !removing first dep - removing first dep
            dblk->deplnk = dp->deplnk;                                          // bypass a first dep killee
        }                                                                       // end else removing first dep

        free(dp);                                                               // get rid of this dep
    }                                                                           // end if keys match up to slen
}                                                                               // end function ST_RemDp

/*
 * Function: ST_Get - Retrieve data
 * returns length of data
 */
int ST_Get(mvar *var, u_char *buf)                                              // get data at var/subscript
{
    int     t;                                                                  // for return value
    cstring *data;                                                              // ptr for ST_GetAdd()

    t = ST_GetAdd(var, &data);                                                  // get address of data
    if (t < 0) return t;                                                        // if error, quit
    t = mcopy(&data->buf[0], buf, t);                                           // copy data (if any)
    return t;                                                                   // return the size (or error)
}                                                                               // end function ST_Get

/*
 * Function: ST_Set - Set a variable in memory, create or replace
 * returns length of data on success, negative otherwise
 */
int ST_Set(mvar *var, const cstring *data)                                      // set var to be data
{
    ST_depend *ptr1;                                                            // a dependent pointer
    ST_depend *newPtrDp = ST_DEPEND_NULL;                                       // a dependent pointer
    ST_data   *newPtrDt = ST_DATA_NULL;                                         // a data pointer
    ST_depend *prevPtr = ST_DEPEND_NULL;                                        // pointer to previous element
    int       n;                                                                // key length
    int       pad = 0;                                                          // extra space padding
    short     fwd;                                                              // position in symtab

    if ((var->slen & 1) != 0) pad = 1;                                          // set up for any extra space

    if (var->volset) {                                                          // if volset defined
        fwd = ST_LocateIdx(var->volset - 1);                                    // locate var by volset
    } else {                                                                    // if no volset or volset zero
        fwd = ST_Create(var->name);                                             // attempt to create new symbol table entry
    }

    if (fwd < 0) return fwd;                                                    // error if none free

    if (symtab[fwd].data == ST_DATA_NULL) {                                     // if not already exists
        u_int i;

        i = DTBLKSIZE + data->len;                                              // required memory
        if ((var->slen != 0) || (i < DTMINSIZE)) i = DTMINSIZE;                 // not required or too small so make it min size
        newPtrDt = malloc(i);                                                   // allocate necessary space
        if (newPtrDt == ST_DATA_NULL) return -(ERRZ56 + ERRMLAST);              // no memory available

        if (var->slen == 0) {                                                   // no subscript key
            newPtrDt->deplnk = ST_DEPEND_NULL;                                  // no dependents
            newPtrDt->last_key = ST_DEPEND_NULL;                                // no last used key
            newPtrDt->attach = 1;                                               // initialize attach count
            newPtrDt->dbc = data->len;                                          // initialize data bytes count
            memcpy(&newPtrDt->data, &data->buf[0], data->len + 1);              // copy data in
        } else {                                                                // end if - slen is zero, subscript defined
            newPtrDp = malloc(DPBLKSIZE + data->len + var->slen + pad);         // new dep blk

            if (newPtrDp == ST_DEPEND_NULL) {                                   // no memory available
                free(newPtrDt);                                                 // free memory
                return -(ERRZ56 + ERRMLAST);
            }

DISABLE_WARN(-Warray-bounds)
            newPtrDt->dbc = VAR_UNDEFINED;                                      // initialize data byte count
            newPtrDt->deplnk = newPtrDp;                                        // initialize link to dependent
            newPtrDt->last_key = newPtrDp;                                      // initialize last key to dependent
            newPtrDt->attach = 1;                                               // initialize attach count
ENABLE_WARN
            newPtrDp->deplnk = ST_DEPEND_NULL;                                  // no more dependents
            newPtrDp->keylen = var->slen;                                       // copy sub keylength
            n = var->slen;                                                      // get the key size
            memcpy(&newPtrDp->bytes[0], &var->key[0], n);                       // copy the key
            if (n & 1) n++;                                                     // ensure n is even
            memcpy(&newPtrDp->bytes[n], &data->len, sizeof(u_short));           // save the data length
            n += sizeof(u_short);                                               // point past the DBC
            memcpy(&newPtrDp->bytes[n], &data->buf[0], data->len + 1);          // data & term 0
        }                                                                       // end else-has subs

        symtab[fwd].data = newPtrDt;                                            // link it in
    } else {                                                                    // end if-no data block - data block already exists
        /*
         * This ELSE segment uses the logic that data is unlinked and a new
         * block DT or DP takes it's place.
         * POSSIBLE ALTERNATIVE *** Implement the replacement of existing data by
         * utilising the same block and replacing its elements
         */

        if (var->slen == 0) {                                                   // not dependent setting
            newPtrDt = realloc(symtab[fwd].data, DTBLKSIZE + data->len);        // allocate data block
            if (newPtrDt == ST_DATA_NULL) return -(ERRZ56 + ERRMLAST);          // no memory available

            if ((newPtrDt != symtab[fwd].data) && (newPtrDt->attach > 1)) {     // did it move? and has many attached
                FixData(symtab[fwd].data, newPtrDt, newPtrDt->attach);          // fix it
            } else {
                symtab[fwd].data = newPtrDt;                                    // or just do this one
            }

            newPtrDt->dbc = data->len;                                          // set data byte count
            memcpy(&newPtrDt->data, &data->buf[0], data->len + 1);              // copy the data in
        } else {                                                                // end if-not dependent setting - setting dependent
            newPtrDp = malloc(DPBLKSIZE + data->len + var->slen + pad);         // allocate DP blk
            if (newPtrDp == ST_DEPEND_NULL) return -(ERRZ56 + ERRMLAST);        // no memory available
            newPtrDp->deplnk = ST_DEPEND_NULL;                                  // init dependent pointer
            newPtrDp->keylen = var->slen;                                       // copy sub keylength
            n = var->slen;                                                      // get the key size
            memcpy(&newPtrDp->bytes[0], &var->key[0], n);                       // copy the key
            if (n & 1) n++;                                                     // ensure n is even
            memcpy(&newPtrDp->bytes[n], &data->len, sizeof(u_short));           // save the data length
            n += sizeof(u_short);                                               // point past the DBC
            memcpy(&newPtrDp->bytes[n], &data->buf[0], data->len + 1);          // copy data and trailing 0
            ptr1 = symtab[fwd].data->deplnk;                                    // go into dependents

            if (ptr1 != ST_DEPEND_NULL) {                                       // deps currently exist
                ST_depend *lastkey = symtab[fwd].data->last_key;                // pointer to last used key

                // start search at last used key, rather than at the beginning (var after lastkey)
                if ((lastkey != ST_DEPEND_NULL) &&
                  (UTIL_Key_KeyCmp(var->key, lastkey->bytes, var->slen, lastkey->keylen) == K2_LESSER)) {
                    ptr1 = lastkey;
                }

                // compare keys (var after ptr1)
                while ((ptr1 != ST_DEPEND_NULL) && (UTIL_Key_KeyCmp(var->key, ptr1->bytes, var->slen, ptr1->keylen) == K2_LESSER)) {
                    prevPtr = ptr1;                                             // save previous
                    ptr1 = ptr1->deplnk;                                        // get next
                }                                                               // end while-compare keys

                // replace data if var keys equal it means to replace data (var equal ptr1)
                if ((ptr1 != ST_DEPEND_NULL) && (UTIL_Key_KeyCmp(var->key, ptr1->bytes, var->slen, ptr1->keylen) == KEQUAL)) {
                    if (prevPtr == ST_DEPEND_NULL) {                            // if no prev pointer
                        newPtrDp->deplnk = ptr1->deplnk;                        // link to previous first dep
                        symtab[fwd].data->deplnk = newPtrDp;                    // link in as first dep
                    } else {                                                    // end if no prev ptr - if prev pointer is defined
                        newPtrDp->deplnk = ptr1->deplnk;                        // set new dependent link
                        prevPtr->deplnk = newPtrDp;                             // alter previous to link in
                    }                                                           // end else bypassing mid list

                    free(ptr1);                                                 // remove previous dep link
                // if we have a dependent, create more data (var before ptr1)
                } else if ((ptr1 != ST_DEPEND_NULL) &&
                  (UTIL_Key_KeyCmp(var->key, ptr1->bytes, var->slen, ptr1->keylen) == K2_GREATER)) {
                    if (prevPtr == ST_DEPEND_NULL) {                            // new first element
                        newPtrDp->deplnk = ptr1;                                // link in
                        symtab[fwd].data->deplnk = newPtrDp;                    // link in
                    } else {                                                    // insert new element, mid list
                        newPtrDp->deplnk = ptr1;                                // link in
                        prevPtr->deplnk = newPtrDp;                             // link in
                    }                                                           // end insert mid list
                } else if ((ptr1 == ST_DEPEND_NULL) && (prevPtr != ST_DEPEND_NULL)) { // end deps currently exist
                    newPtrDp->deplnk = ST_DEPEND_NULL;                          // link in
                    prevPtr->deplnk = newPtrDp;                                 // link in
                }                                                               // end add to end of list

                symtab[fwd].data->last_key = prevPtr;                           // add last used key
            } else {                                                            // end if elements exist - no elements curr exist
                symtab[fwd].data->deplnk = newPtrDp;                            // add as first element ever
            }                                                                   // end else no elements existed
        }                                                                       // end else-slen not zero
    }                                                                           // end else-data block not null

    return data->len;                                                           // return length of data
}                                                                               // end ST_Set

/*
 * Function: ST_Data - examine type of variable
 * returns pointer to length of type of data
 *
 * *buf is set to 0, 1, 10, 11 depending on the type of data *var is
 * returned is the length of *buf, not including the /0 terminator
 *      0 = The variable has no data and no descendants (i.e., undef)
 *      1 = The variable has data but no descendants
 *     10 = The variable has no data but has descendants
 *     11 = The variable has data and has descendants
 */
short ST_Data(mvar *var, u_char *buf)                                           // put var type in buf
{
    int       ptr1;                                                             // position in symtab
    ST_depend *depPtr = ST_DEPEND_NULL;                                         // active pointer
    ST_depend *prevPtr = ST_DEPEND_NULL;                                        // pointer to previous element

    if (var->volset) {                                                          // if by index
        ptr1 = ST_LocateIdx(var->volset - 1);                                   // get it this way
    } else {                                                                    // if no volset, use name
        ptr1 = ST_Locate(var->name);                                            // locate the variable by name
    }

    if (ptr1 == -1) {                                                           // not found
        memcpy(buf, "0\0", 2);
        return 1;                                                               // end if-no data&no descendants
    }                                                                           // end if not found

    if (symtab[ptr1].data == ST_DATA_NULL) {                                    // not found
        memcpy(buf, "0\0", 2);
        return 1;                                                               // return length of buf
    }                                                                           // end if not found

    if (var->slen > 0) {                                                        // going into dependents
        ST_depend *lastkey;

        lastkey = symtab[ptr1].data->last_key;                                  // pointer to last used key
        depPtr = symtab[ptr1].data->deplnk;                                     // get first dependent

        // start search at last used key, rather than at the beginning (var after lastkey)
        if ((lastkey != ST_DEPEND_NULL) && (UTIL_Key_KeyCmp(var->key, lastkey->bytes, var->slen, lastkey->keylen) == K2_LESSER)) {
            depPtr = lastkey;
        }

        // while not yet found or past
        while ((depPtr != ST_DEPEND_NULL) && (UTIL_Key_Cmp(depPtr->bytes, var->key, depPtr->keylen, var->slen) == K2_GREATER)) {
            prevPtr = depPtr;                                                   // save previous
            depPtr = depPtr->deplnk;                                            // go to next
        }                                                                       // end while

        symtab[ptr1].data->last_key = prevPtr;                                  // add last used key

        if (depPtr == ST_DEPEND_NULL) {                                         // if we ran out of deps
            memcpy(buf, "0\0", 2);
            return 1;                                                           // return same
        }                                                                       // end if

        // while more deps
        while ((depPtr != ST_DEPEND_NULL) && (UTIL_Key_KeyCmp(depPtr->bytes, var->key, depPtr->keylen, var->slen) == K2_GREATER)) {
            prevPtr = depPtr;                                                   // save previous
            depPtr = depPtr->deplnk;                                            // go to next
        }                                                                       // end while

        symtab[ptr1].data->last_key = prevPtr;                                  // add last used key again

        if (depPtr == ST_DEPEND_NULL) {                                         // if we ran out
            memcpy(buf, "0\0", 2);
            return 1;                                                           // return same
        }                                                                       // end if

        // if matches ok
        if ((depPtr != ST_DEPEND_NULL) && (UTIL_Key_Cmp(depPtr->bytes, var->key, depPtr->keylen, var->slen) == KEQUAL)) {
            if (depPtr->keylen == var->slen) {                                  // exact match
                prevPtr = depPtr;                                               // save previous
                depPtr = depPtr->deplnk;                                        // go to next
                symtab[ptr1].data->last_key = prevPtr;                          // add last used key again

                if (depPtr == ST_DEPEND_NULL) {                                 // have we run out
                    memcpy(buf, "1\0", 2);
                    return 1;                                                   // return same
                }                                                               // end if

                if (UTIL_Key_Cmp(depPtr->bytes, var->key, depPtr->keylen, var->slen) == KEQUAL) { // if match ok
                    memcpy(buf, "11\0", 3);
                    return 2;                                                   // return same
                } else {                                                        // end if
                    memcpy(buf, "1\0", 2);
                    return 1;                                                   // return same
                }
            } else {                                                            // end if exact match - beyond exact match
                memcpy(buf, "10\0", 3);
                return 2;                                                       // return same
            }                                                                   // end else beyond exact match
        } else {                                                                // end if match to i - no longer matches for i
            memcpy(buf, "0\0", 2);
            return 1;                                                           // return same
        }
    } else {                                                                    // end if-going to dependents - work on data block
        // dbc defined with data and no descendants
        if ((symtab[ptr1].data->dbc != VAR_UNDEFINED) && (symtab[ptr1].data->deplnk == ST_DEPEND_NULL)) {
            memcpy(buf, "1\0", 2);
            return 1;
        }                                                                       // end if-data&no descendants

        // dbc not defined with no data and descendants
        if ((symtab[ptr1].data->dbc == VAR_UNDEFINED) && (symtab[ptr1].data->deplnk != ST_DEPEND_NULL)) {
            memcpy(buf, "10\0", 3);
            return 2;
        }                                                                       // end if-no data&descendants

        // dbc defined with data and descendants
        if ((symtab[ptr1].data->dbc != VAR_UNDEFINED) && (symtab[ptr1].data->deplnk != ST_DEPEND_NULL)) {
            memcpy(buf, "11\0", 3);
            return 2;
        }                                                                       // end if-data&descendants
    }                                                                           // end else-ops on data block

    memcpy(buf, "0\0", 2);
    return 1;                                                                   // return
}                                                                               // end ST_Data

/*
 * Function: ST_Order - get next subscript in sequence, forward or reverse
 * returns pointer to length of next subscript
 */
short ST_Order(mvar *var, u_char *buf, int dir)
{
    int       ptr1;                                                             // position in symtab
    ST_depend *current = ST_DEPEND_NULL;                                        // active pointer
    ST_depend *prev = ST_DEPEND_NULL;                                           // pointer to previous element
    ST_depend *lastkey = ST_DEPEND_NULL;                                        // pointer to last used key
    int       pieces = 0;                                                       // subscripts in key
    int       subs;
    u_char    keysub[MAX_KEY_SIZE + 5];                                         // current key subscript
    u_char    upOneLev[MAX_KEY_SIZE + 5];
    u_char    crud[MAX_KEY_SIZE + 5];
    int       index = 0;                                                        // where up to in key extract
    short     ret = 0;                                                          // current position in key

    buf[0] = '\0';                                                              // JIC
    if (var->slen == 0) return 0;                                               // can't $ORDER a data block so return null

    if (var->volset) {                                                          // if by index
        ptr1 = ST_LocateIdx(var->volset - 1);                                   // get it this way
    } else {                                                                    // no volset, so use var name
        ptr1 = ST_Locate(var->name);                                            // locate the variable by name
    }

    if (ptr1 < 0) return 0;
    if (symtab[ptr1].data == ST_DATA_NULL) return 0;                            // no data
    UTIL_Key_Chars_In_Subs((char *) var->key, (int) var->slen, MAX_NUM_SUBS, &pieces, (char *) NULL); // Return num of sub in pieces

    /*
     * Return characters in all but last subscript, number of subscripts in subs
     * and key string (less last subscript) at upOneLev[1] on for upOneLev[0] bytes
     */
    upOneLev[0] = (u_char) UTIL_Key_Chars_In_Subs((char *) var->key, (int) var->slen, pieces - 1, &subs, (char *) &upOneLev[1]);

    current = symtab[ptr1].data->deplnk;                                        // go to first dependent
    lastkey = symtab[ptr1].data->last_key;                                      // pointer to last used key

    // start search at last used key, rather than at the beginning (var after lastkey)
    if ((lastkey != ST_DEPEND_NULL) &&
      (UTIL_Key_KeyCmp(var->key, lastkey->bytes, var->slen, lastkey->keylen) == K2_LESSER) && (dir == 1)) {
        current = lastkey;
    }

    // compare keys - while we have dependent and key match fails (var after current)
    while ((current != ST_DEPEND_NULL) && (UTIL_Key_KeyCmp(var->key, current->bytes, var->slen, current->keylen) == K2_LESSER)) {
        prev = current;                                                         // set prev pointer
        current = current->deplnk;                                              // go to next dependent pointer
    }                                                                           // end while-compare keys

    symtab[ptr1].data->last_key = prev;                                         // add last used key
    if ((current == ST_DEPEND_NULL) && (dir == 1)) return 0;                    // nothing past our key - output same, return length

    if (dir == -1) {                                                            // reverse order
        current = prev;                                                         // save current
        if (current == ST_DEPEND_NULL) return 0;                                // if pointing nowhere then return length of zero
        crud[0] = UTIL_Key_Chars_In_Subs((char *) current->bytes, (int) current->keylen, pieces - 1, &subs, (char *) &crud[1]);

        if ((crud[0] != 0) && (upOneLev[0] != 0)) {
            if (crud[0] != upOneLev[0]) return 0;

            // Ensure higher level subscripts (if any) are equal
            if (UTIL_Key_Cmp(&crud[1], &upOneLev[1], crud[0], upOneLev[0]) != KEQUAL) return 0;
        }
    } else {                                                                    // end if reverse order - forward order
        // while we have dependents and key cmp fails
        while ((current != ST_DEPEND_NULL) && (UTIL_Key_Cmp(current->bytes, var->key, current->keylen, var->slen) == KEQUAL)) {
            current = current->deplnk;                                          // go to next dependent
        }                                                                       // end while

        if (current == ST_DEPEND_NULL) return 0;                                // if current points nowhere, return length of zero

        // compare keys, if compare fails to match (var before or after current)
        if (UTIL_Key_KeyCmp(var->key, current->bytes, var->slen, current->keylen) != KEQUAL) {
            crud[0] = UTIL_Key_Chars_In_Subs((char *) current->bytes, (int) current->keylen, pieces - 1, &subs, (char *) &crud[1]);

            if ((crud[0] != 0) && (upOneLev[0] != 0)) {                         // if lengths aren't 0
                // cmp fails then return a length of zero
                if (UTIL_Key_Cmp(&crud[1], &upOneLev[1], crud[0], upOneLev[0]) != KEQUAL) return 0;
            }                                                                   // end if slen's non zero
        }                                                                       // end if keys dont equal
    }                                                                           // end else-forward order

    if (current == ST_DEPEND_NULL) return 0;                                    // nothing past our key then return length

    for (int i = 1; i <= pieces; i++) {                                         // number of keys
        int upto = 0;                                                           // clear flag

        ret = UTIL_Key_Extract(&current->bytes[index], keysub, &upto);          // next key
        index += upto;                                                          // increment index
        if ((index >= current->keylen) && (i < pieces)) return 0;               // hit end of key & !found then return null
    }                                                                           // end for-pieces to level required

    // Now have ASCII key in desired position number, put the ASCII value of that key in *buf and return the length of it
    return (short) mcopy(keysub, buf, ret);
}                                                                               // end function - ST_Order

/*
 * Function: ST_Query - return next whole key in sequence, forward or reverse
 * returns pointer to length of next key
 */
short ST_Query(mvar *var, u_char *buf, int dir)
{
    int       ptr1;                                                             // position in symtab
    ST_depend *current = ST_DEPEND_NULL;                                        // active pointer
    ST_depend *prev = ST_DEPEND_NULL;                                           // pointer to previous element
    ST_depend *lastkey = ST_DEPEND_NULL;                                        // pointer to last used key
    mvar      outputVar = *var;                                                 // copy of supplied mvar

    if (var->volset) {                                                          // if by index
        ptr1 = ST_LocateIdx(var->volset - 1);                                   // get it this way
    } else {                                                                    // no volset, use var name
        ptr1 = ST_Locate(var->name);                                            // locate the variable by name
    }

    buf[0] = '\0';                                                              // JIC
    if (ptr1 < 0) return 0;
    if (symtab[ptr1].data == ST_DATA_NULL) return 0;                            // no data block, err
    current = symtab[ptr1].data->deplnk;                                        // first dependent pointed at

    if (current == ST_DEPEND_NULL) {                                            // not found
        if (dir == 1) return 0;

        // reverse order and root undefined or reverse order and is the root
        if ((symtab[ptr1].data->dbc == ROOT_UNDEFINED) || (var->slen == 0)) return 0;
        return UTIL_String_Mvar(&outputVar, buf, 0);                            // convert mvar and return length of key
    }

    lastkey = symtab[ptr1].data->last_key;                                      // pointer to last used key

    // start search at last used key, rather than at the beginning (var after lastkey)
    if ((lastkey != ST_DEPEND_NULL) &&
      (UTIL_Key_KeyCmp(var->key, lastkey->bytes, var->slen, lastkey->keylen) == K2_LESSER) && (dir == 1)) {
        current = lastkey;
    }

    // while more exist with keys that are larger, get next dep (var after current)
    while ((current != ST_DEPEND_NULL) && (UTIL_Key_KeyCmp(var->key, current->bytes, var->slen, current->keylen) == K2_LESSER)) {
        prev = current;                                                         // save prev pointer
        current = current->deplnk;                                              // go to next dependent pointer
    }                                                                           // end while-compare keys

    symtab[ptr1].data->last_key = prev;                                         // add last used key

    if (var->slen > 0) {                                                        // looking in dependents
        if (dir == -1) {                                                        // reverse order
            if (prev != ST_DEPEND_NULL) current = prev;                         // only if previous ptr defined, go back one
        } else {                                                                // end if reverse order - start forward order
            // not going past non exist last and keys are equal (var equal current)
            if ((current != ST_DEPEND_NULL) && (UTIL_Key_KeyCmp(var->key, current->bytes, var->slen, current->keylen) == KEQUAL)) {
                current = current->deplnk;                                      // go to next
            }                                                                   // end if exact match
        }                                                                       // end else-forward
    }                                                                           // finished looking

    if (current == ST_DEPEND_NULL) return 0;                                    // if we have gone past end, return length of zero
    if ((dir == -1) && (var->slen == 0)) return 0;                              // reverse dir and data block, return length of zero
    outputVar.slen = current->keylen;                                           // key and length of set
    memcpy(outputVar.key, current->bytes, (int) current->keylen);               // setup mvar

    if ((current == symtab[ptr1].data->deplnk) && (prev != current) && (dir == -1) && (var->slen > 0)) { // previous is a data block
        if (symtab[ptr1].data->dbc == ROOT_UNDEFINED) return 0;
        outputVar.slen = 0;                                                     // flag is as such
    }                                                                           // end if back to a data block

    return UTIL_String_Mvar(&outputVar, buf, MAX_NUM_SUBS);                     // convert mvar and return length of key
}                                                                               // end ST_Query

int ST_GetAdd(mvar *var, cstring **add)                                         // get local data address
{
    ST_depend *depPtr = ST_DEPEND_NULL;                                         // active pointer
    ST_depend *prev = ST_DEPEND_NULL;                                           // pointer to previous element
    int       ptr1;                                                             // position in symtab

    if (var->volset) {                                                          // if by index
        ptr1 = ST_LocateIdx(var->volset - 1);                                   // get it this way
    } else {                                                                    // no volset, use var name
        ptr1 = ST_Locate(var->name);                                            // locate the variable by name
    }

    if ((ptr1 > ST_MAX) || (ptr1 < -1)) {
        panic("ST_GetAdd: Junk pointer returned from ST_LocateIdx()");
    } else if (ptr1 >= 0) {                                                     // think we found it
        int i;                                                                  // generic counter

        if (symtab[ptr1].data == ST_DATA_NULL) return -ERRM6;                   // not found

        if (var->slen > 0) {                                                    // go to dependents
            ST_depend *lastkey = symtab[ptr1].data->last_key;                   // pointer to last used key

            depPtr = symtab[ptr1].data->deplnk;                                 // get first dependent

            // start search at last used key, rather than at the beginning (var after lastkey)
            if ((lastkey != ST_DEPEND_NULL) &&
              (UTIL_Key_KeyCmp(var->key, lastkey->bytes, var->slen, lastkey->keylen) == K2_LESSER)) {
                depPtr = lastkey;
            }

            while (depPtr != ST_DEPEND_NULL) {                                  // while dep ok, compare keys
                i = UTIL_Key_KeyCmp(var->key, depPtr->bytes, var->slen, depPtr->keylen);
                if (i == K2_GREATER) return -ERRM6;                             // error if we passed it (var before depPtr)
                if (i == KEQUAL) break;                                         // found it (var equal depPtr)
                prev = depPtr;                                                  // save previous pointer
                depPtr = depPtr->deplnk;                                        // get next
            }                                                                   // end while - compare keys

            if (depPtr == ST_DEPEND_NULL) return -ERRM6;                        // if no exist then return same
            i = (int) depPtr->keylen;                                           // get key length
            if (i & 1) i++;                                                     // ensure even
            *add = (cstring *) &depPtr->bytes[i];                               // send data addr as cstring
            symtab[ptr1].data->last_key = prev;                                 // add last used key
            return (*add)->len;                                                 // and return the length
        } else {                                                                // data block
            *add = (cstring *) &symtab[ptr1].data->dbc;                         // setup the address
            i = symtab[ptr1].data->dbc;                                         // get dbc
            if (i == VAR_UNDEFINED) return -ERRM6;                              // dbc not defined and no int so return same
            return i;                                                           // and return the count
        }                                                                       // finished with data block
    }                                                                           // end if - symtab posi valid

    return -ERRM6;                                                              // return if failed
}                                                                               // end ST_GetAdd

// Return next key in supplied mvar and data at buf
int ST_QueryD(mvar *var, u_char *buf)                                           // get next key and data
{
    cstring   *cdata;                                                           // temporary data access
    ST_depend *current = ST_DEPEND_NULL;                                        // active pointer
    ST_depend *prev = ST_DEPEND_NULL;                                           // pointer to previous element
    ST_depend *lastkey = ST_DEPEND_NULL;                                        // pointer to last used key
    int       ptr1;                                                             // position in symtab
    int       i;                                                                // generic counter

    if (var->volset) {                                                          // if by index
        ptr1 = ST_LocateIdx(var->volset - 1);                                   // get it this way
    } else {                                                                    // no volset, use name
        ptr1 = ST_Locate(var->name);                                            // locate the variable by name
    }

    buf[0] = '\0';                                                              // JIC
    if (ptr1 < 0) return -ERRM6;                                                // not found
    if (symtab[ptr1].data == ST_DATA_NULL) return -ERRM6;                       // no data, err
    current = symtab[ptr1].data->deplnk;                                        // first dependent pointed at
    if (current == ST_DEPEND_NULL) return -(ERRZ55 + ERRMLAST);                 // not found so no data below
    lastkey = symtab[ptr1].data->last_key;

    // start search at last used key, rather than at the beginning (var after lastkey)
    if ((lastkey != ST_DEPEND_NULL) && (UTIL_Key_KeyCmp(var->key, lastkey->bytes, var->slen, lastkey->keylen) == K2_LESSER)) {
        current = lastkey;
    }

    // more deps exist and key compare fails - compare keys (var after current)
    while ((current != ST_DEPEND_NULL) && (UTIL_Key_KeyCmp(var->key, current->bytes, var->slen, current->keylen) == K2_LESSER)) {
        prev = current;                                                         // set prev pointer
        current = current->deplnk;                                              // to next dependent pointer
    }                                                                           // end while-compare keys

    // while more deps exist and keys match exactly - (var equal current)
    if ((current != ST_DEPEND_NULL) && (UTIL_Key_KeyCmp(var->key, current->bytes, var->slen, current->keylen) == KEQUAL)) {
        prev = current;                                                         // set prev pointer
        current = current->deplnk;                                              // go to next
    }                                                                           // end if keys equal

    if (current == ST_DEPEND_NULL) return -(ERRZ55 + ERRMLAST);                 // if no more deps then out of data, error
    memcpy(var->key, current->bytes, current->keylen);                          // set up mvar
    var->slen = current->keylen;                                                // key and len
    i = (int) current->keylen;                                                  // get key length
    if ((i & 1) != 0) i++;                                                      // up it to next even boundary
    cdata = (cstring *) &current->bytes[i];                                     // convert to cstring
    symtab[ptr1].data->last_key = prev;                                         // add last used key
    return mcopy(cdata->buf, buf, cdata->len);                                  // get data into buf
}                                                                               // end ST_QueryD

// kill all local variables except those whose names appear in var_u *keep
short ST_KillAll(int count, var_u *keep)
{
    partab.src_var.uci = UCI_IS_LOCALVAR;                                       // init UCI as LOCAL
    partab.src_var.slen = 0;                                                    // init subscript length
    partab.src_var.volset = 0;                                                  // init volume set

    for (int i = 0; i < ST_MAX; i++) {                                          // for each entry in symbol table
        int j;                                                                  // generic counter

        if ((symtab[i].varnam.var_cu[0] == '$') || (symtab[i].varnam.var_cu[0] == '\0')) continue; // dont touch $ vars
        if (symtab[i].data == ST_DATA_NULL) continue;                           // ditto if it's undefined

        for (j = 0; j < count; j++) {                                           // scan the keep list
            if (var_equal(symtab[i].varnam, keep[j])) break;                    // if we want it then quit the loop
        }

        if (j < count) continue;                                                // continue if we want it
        VAR_COPY(partab.src_var.name, symtab[i].varnam);                        // init varnam
        ST_Kill(&partab.src_var);                                               // kill it and all under
    }                                                                           // end for all in symtab

    return 0;                                                                   // finished OK
}                                                                               // end ST_KillAll

/*
 * Locate variable 'var' - create the symtab entry if it doesn't exist.
 * Increment usage.
 * If ST_data block does not exist, create it.
 * Return the symtab entry number or negative error number
 */
short ST_SymAtt(var_u var)
{
    short pos;

    pos = ST_Create(var);                                                       // position in symtab - locate/create variable
    if (pos >= 0) symtab[pos].usage++;                                          // if ok, increment usage
    return pos;                                                                 // return whatever we found
}

/*
 * For each symtab entry in list (ignoring those that are VAR_UNDEFINED),
 * decrement usage. Remove the entry if it is undefined.
 */
void ST_SymDet(int count, short *list)
{
    for (int i = 0; i < count; i++) {                                           // for all supplied vars
        if (list[i] >= 0) {                                                     // if this got attached
            symtab[list[i]].usage--;                                            // decrement usage
            if (symtab[list[i]].usage > 0) continue;                            // still NEWed or whatever

            if (symtab[list[i]].data != ST_DATA_NULL) {                         // data?
                if (symtab[list[i]].data->dbc != VAR_UNDEFINED) continue;
                if (symtab[list[i]].data->deplnk != ST_DEPEND_NULL) continue;
                free(symtab[list[i]].data);                                     // free the data block
                symtab[list[i]].data = ST_DATA_NULL;                            // and remember this
            }

            if (symtab[list[i]].data == ST_DATA_NULL) {                         // no data?
                ST_Free(symtab[list[i]].varnam);                                // not in use - free it
            }
        }
    }

    free(list);                                                                 // dump the memory
    return;
}                                                                               // end function ST_SymDet

// get local data - symtab entry number provided
/*
int ST_SymGet(short syment, u_char *buf)
{
    if (symtab[syment].data == ST_DATA_NULL) return -ERRM6;                     // if no data (undefined) then complain
    if (symtab[syment].data->dbc == VAR_UNDEFINED) return -ERRM6;               // complain
    return mcopy(symtab[syment].data->data, buf, symtab[syment].data->dbc);     // go to the data and copy data (if any)
}                                                                               // end function ST_SymGet
*/

// set local data - symtab entry number provided
short ST_SymSet(short pos, const cstring *data)
{
    u_int   u;                                                                  // a handy int
    ST_data *ptr;                                                               // and a pointer

    u = DTBLKSIZE + data->len;                                                  // size required
    if (u < DTMINSIZE) u = DTMINSIZE;                                           // check for minimum

    if (symtab[pos].data == ST_DATA_NULL) {                                     // if no data block
        symtab[pos].data = malloc(u);                                           // get some memory
        if (symtab[pos].data == ST_DATA_NULL) return -(ERRZ56 + ERRMLAST);      // no mem
        symtab[pos].data->last_key = ST_DEPEND_NULL;                            // init last used key
        symtab[pos].data->deplnk = ST_DEPEND_NULL;                              // init dep link
        symtab[pos].data->attach = 1;                                           // init attach count
    } else if (symtab[pos].data->dbc < data->len) {                             // enough space?
        ptr = realloc(symtab[pos].data, u);                                     // attempt to increase it
        if (ptr == ST_DATA_NULL) return -(ERRZ56 + ERRMLAST);                   // no memory available

        if ((ptr != symtab[pos].data) && (ptr->attach > 1)) {                   // did it move and many attached?
            FixData(symtab[pos].data, ptr, ptr->attach);                        // fix it
        } else {
            symtab[pos].data = ptr;                                             // or just do this one
        }
    }

    symtab[pos].data->dbc = data->len;                                          // set the dbc
    memcpy(&symtab[pos].data->data, &data->buf[0], data->len + 1);              // set it
    return 0;                                                                   // and exit
}                                                                               // end function ST_SymSet

// kill a local var - symtab entry number provided
short ST_SymKill(short pos)
{
    ST_depend *dptr;                                                            // dependent ptr
    ST_depend *fptr;                                                            // dependent ptr

    if (symtab[pos].data != ST_DATA_NULL) {                                     // there is data
        dptr = symtab[pos].data->deplnk;                                        // get dependent ptr
        symtab[pos].data->last_key = ST_DEPEND_NULL;                            // clear it
        symtab[pos].data->deplnk = ST_DEPEND_NULL;                              // clear it

        while (dptr != ST_DEPEND_NULL) {                                        // for each dependent
            fptr = dptr;                                                        // save this one
            dptr = fptr->deplnk;                                                // get next
            free(fptr);                                                         // free this
        }

        if (symtab[pos].data->attach <= 1) {                                    // if no more attached
            free(symtab[pos].data);                                             // free data block
            symtab[pos].data = ST_DATA_NULL;                                    // clear the pointer
        }
    }

    if (symtab[pos].usage < 1) ST_Free(symtab[pos].varnam);                     // any NEWs etc.? if no - dong it
    return 0;                                                                   // and exit
}

// 0 to ST_MAX - 1 (i.e., ((ST_HASH + 1) * 3))
short ST_Dump(void)                                                             // dump entire symbol table to $IO
{
    int       j;                                                                // generic counter
    int       t;                                                                // for functions
    int       escape;
    int       string;
    int       dot;
    cstring   *cdata;                                                           // variable data gets dumped
    cstring   *ckey;                                                            // variable key data gets dumped
    u_char    dump[VAR_LEN + MAX_KEY_SIZE + MAX_NUM_SUBS + 12];                 // variable name gets dumped
    u_char    dumpk[VAR_LEN + MAX_KEY_SIZE + MAX_NUM_SUBS + 12];                // variable key name gets dumped
    ST_depend *depPtr = ST_DEPEND_NULL;                                         // active dependent ptr

    for (int i = 0; i < ST_MAX; i++) {                                          // for each entry in symbol table
        if (symtab[i].data == ST_DATA_NULL) continue;                           // get out if nothing to dump
        if (symtab[i].varnam.var_cu[0] == '$') continue;                        // dont spit out $ vars
        VAR_COPY(partab.src_var.name, symtab[i].varnam);                        // init var name
        partab.src_var.uci = UCI_IS_LOCALVAR;                                   // init UCI as LOCAL
        partab.src_var.slen = 0;                                                // init subscript length
        partab.src_var.volset = 0;                                              // init volume set
        cdata = (cstring *) &dump[0];                                           // make it a cstring
        escape = FALSE;
        string = FALSE;
        dot = 0;

        if (symtab[i].data->dbc != VAR_UNDEFINED) {                             // valid dbc
            t = UTIL_String_Mvar(&partab.src_var, cdata->buf, MAX_NUM_SUBS);    // get var name and dump data block
            if (t < 0) return (short) t;                                        // die on error
            cdata->len = t;
DISABLE_WARN(-Warray-bounds)
            cdata->buf[cdata->len++] = '=';                                     // tack on equal sign
ENABLE_WARN
            t = SQ_Write(cdata);                                                // dump var name =
            if (t < 0) return (short) t;                                        // die on error

            for (int k = 0; k < symtab[i].data->dbc; k++) {
                if (symtab[i].data->data[k] == '.') {
                    dot++;

                    if ((dot > 1) || (k == (symtab[i].data->dbc - 1))) {
                        string = TRUE;
                        break;
                    }
                } else if ((symtab[i].data->dbc > 1) && (k == 0) && (symtab[i].data->data[k] == '0')) {
                    string = TRUE;
                    break;
                } else if ((symtab[i].data->dbc > 1) && dot &&
                  (k == (symtab[i].data->dbc - 1)) && (symtab[i].data->data[k] == '0')) {
                    string = TRUE;
                    break;
                } else if ((k == 0) && (symtab[i].data->data[k] == '-')) {
                    if (symtab[i].data->data[k + 1] != '0') continue;
                    string = TRUE;
                    break;
                } else if (!isdigit(symtab[i].data->data[k])) {
                    string = TRUE;
                    break;
                }
            }

            for (int k = 0; k < symtab[i].data->dbc; k++) {
                if (!isprint(symtab[i].data->data[k])) {
                    if (escape) {
DISABLE_WARN(-Warray-bounds)
                        cdata->len = 1;
ENABLE_WARN
                        strcpy((char *) cdata->buf, ",");
                    } else if (k == 0) {
DISABLE_WARN(-Warray-bounds)
                        cdata->len = 3;
ENABLE_WARN
                        strcpy((char *) cdata->buf, "$C(");
                    } else {
DISABLE_WARN(-Warray-bounds)
                        cdata->len = 5;
ENABLE_WARN
                        strcpy((char *) cdata->buf, "\"_$C(");
                    }

                    t = SQ_Write(cdata);                                        // dump data character
                    if (t < 0) return (short) t;                                // die on error
DISABLE_WARN(-Warray-bounds)
                    cdata->len = ltocstring(cdata->buf, symtab[i].data->data[k]);
ENABLE_WARN
                    t = SQ_Write(cdata);                                        // dump data character
                    if (t < 0) return (short) t;                                // die on error

                    if (k == (symtab[i].data->dbc - 1)) {
DISABLE_WARN(-Warray-bounds)
                        cdata->len = 1;
ENABLE_WARN
                        strcpy((char *) cdata->buf, ")");
                        t = SQ_Write(cdata);                                    // dump data character
                        if (t < 0) return (short) t;                            // die on error
                    }

                    escape = TRUE;
                } else {
                    if (string && (k == 0)) {
DISABLE_WARN(-Warray-bounds)
                        cdata->len = 1;
ENABLE_WARN
                        strcpy((char *) cdata->buf, "\"");
                        t = SQ_Write(cdata);                                    // dump data character
                        if (t < 0) return (short) t;                            // die on error
                    }

                    if (escape) {
DISABLE_WARN(-Warray-bounds)
                        cdata->len = 3;
ENABLE_WARN
                        strcpy((char *) cdata->buf, ")_\"");
                        t = SQ_Write(cdata);                                    // dump data character
                        if (t < 0) return (short) t;                            // die on error
                    } else if (symtab[i].data->data[k] == '"') {
DISABLE_WARN(-Warray-bounds)
                        cdata->len = 1;
ENABLE_WARN
                        strcpy((char *) cdata->buf, "\"");
                        t = SQ_Write(cdata);                                    // dump data character
                        if (t < 0) return (short) t;                            // die on error
                    }

                    t = SQ_WriteStar(symtab[i].data->data[k]);                  // dump data character
                    if (t < 0) return (short) t;                                // die on error
                    escape = FALSE;

                    if (string && (k == (symtab[i].data->dbc - 1))) {
DISABLE_WARN(-Warray-bounds)
                        cdata->len = 1;
ENABLE_WARN
                        strcpy((char *) cdata->buf, "\"");
                        t = SQ_Write(cdata);                                    // dump data character
                        if (t < 0) return (short) t;                            // die on error
                    }
                }
            }

            if (!symtab[i].data->dbc) {
DISABLE_WARN(-Warray-bounds)
                cdata->len = 2;
ENABLE_WARN
                strcpy((char *) cdata->buf, "\"\"");
                t = SQ_Write(cdata);                                            // dump data character
                if (t < 0) return (short) t;                                    // die on error
            }

            t = SQ_WriteFormat(SQ_LF);                                          // line feed
            if (t < 0) return (short) t;                                        // die on error
        }                                                                       // end if valid dbc

        cdata = NULL;                                                           // nullify the cstring
        ckey = NULL;                                                            // nullify the cstring
        depPtr = symtab[i].data->deplnk;                                        // get first dependent

        while (depPtr != ST_DEPEND_NULL) {                                      // while dependents exist
            int     paren = 0;
            u_short datalen;

            escape = FALSE;
            string = FALSE;
            dot = 0;
            VAR_COPY(partab.src_var.name, symtab[i].varnam);                    // init var name
            partab.src_var.uci = UCI_IS_LOCALVAR;                               // init UCI as LOCAL
            partab.src_var.slen = depPtr->keylen;                               // init subscript length
            partab.src_var.volset = 0;                                          // init volume set
            memcpy(partab.src_var.key, depPtr->bytes, depPtr->keylen);          // init key
            cdata = (cstring *) &dump[0];                                       // get into a cstring
            ckey = (cstring *) &dumpk[0];                                       // get into a cstring
            t = UTIL_String_Mvar(&partab.src_var, cdata->buf, MAX_NUM_SUBS);    // get var name and dump dependent block
            if (t < 0) return (short) t;                                        // die on error
            cdata->len = t;
DISABLE_WARN(-Warray-bounds)
            cdata->buf[cdata->len++] = '=';                                     // tack on an equal sign

            for (int k = 0; k < cdata->len; k++) {
                if (cdata->buf[k] == '(') paren = k;
ENABLE_WARN

DISABLE_WARN(-Warray-bounds)
                if (!isprint(cdata->buf[k])) {
                    if (escape) {
                        ckey->len = 1;
ENABLE_WARN
                        strcpy((char *) ckey->buf, ",");
                    } else {
                        if (k == (paren + 2)) {
DISABLE_WARN(-Warray-bounds)
                            ckey->len = 3;
ENABLE_WARN
                            strcpy((char *) ckey->buf, "$C(");
                        } else {
DISABLE_WARN(-Warray-bounds)
                            ckey->len = 5;
ENABLE_WARN
                            strcpy((char *) ckey->buf, "\"_$C(");
                        }
                    }

                    t = SQ_Write(ckey);                                         // dump data character
                    if (t < 0) return (short) t;                                // die on error
DISABLE_WARN(-Warray-bounds)
                    ckey->len = ltocstring(ckey->buf, cdata->buf[k]);
ENABLE_WARN
                    t = SQ_Write(ckey);                                         // dump data character
                    if (t < 0) return (short) t;                                // die on error
                    escape = TRUE;
                } else {
                    if (escape) {
DISABLE_WARN(-Warray-bounds)
                        if (k == (cdata->len - 3)) {
                            ckey->len = 1;
ENABLE_WARN
                            strcpy((char *) ckey->buf, ")");
                        } else {
DISABLE_WARN(-Warray-bounds)
                            ckey->len = 3;
ENABLE_WARN
                            strcpy((char *) ckey->buf, ")_\"");
                        }

                        t = SQ_Write(ckey);                                     // dump data character
                        if (t < 0) return (short) t;                            // die on error
                    }

DISABLE_WARN(-Warray-bounds)
                    if (((k == (paren + 1)) && (!isprint(cdata->buf[k + 1]))) ||
                      ((k == (cdata->len - 3)) && (!isprint(cdata->buf[k - 1])))) {
ENABLE_WARN
                        escape = FALSE;
                        continue;
                    }

DISABLE_WARN(-Warray-bounds)
                    t = SQ_WriteStar(cdata->buf[k]);                            // dump data character
ENABLE_WARN
                    if (t < 0) return (short) t;                                // die on error
                    escape = FALSE;
                }
            }

            j = (int) depPtr->keylen;                                           // find key length
            if ((j & 1) != 0) j++;                                              // up it to next even boudary
            memcpy(&datalen, &depPtr->bytes[j], sizeof(u_short));               // find data length

            for (int k = j + 2; k < datalen + j + 2; k++) {
                if (depPtr->bytes[k] == '.') {
                    dot++;

                    if ((dot > 1) || (k == (datalen + j + 1))) {
                        string = TRUE;
                        break;
                    }
                } else if ((datalen > 1) && (k == (j + 2)) && (depPtr->bytes[k] == '0')) {
                    string = TRUE;
                    break;
                } else if ((datalen > 1) && dot && (k == (datalen + j + 1)) && (depPtr->bytes[k] == '0')) {
                    string = TRUE;
                    break;
                } else if ((k == (j + 2)) && (depPtr->bytes[k] == '-')) {
                    if (depPtr->bytes[k + 1] != '0') continue;
                    string = TRUE;
                    break;
                } else if (!isdigit(depPtr->bytes[k])) {
                    string = TRUE;
                    break;
                }
            }

            for (int k = j + 2; k < datalen + j + 2; k++) {
                if (!isprint(depPtr->bytes[k])) {
                    if (escape) {
DISABLE_WARN(-Warray-bounds)
                        cdata->len = 1;
ENABLE_WARN
                        strcpy((char *) cdata->buf, ",");
                    } else if (k == (j + 2)) {
DISABLE_WARN(-Warray-bounds)
                        cdata->len = 3;
ENABLE_WARN
                        strcpy((char *) cdata->buf, "$C(");
                    } else {
DISABLE_WARN(-Warray-bounds)
                        cdata->len = 5;
ENABLE_WARN
                        strcpy((char *) cdata->buf, "\"_$C(");
                    }

                    t = SQ_Write(cdata);                                        // dump data character
                    if (t < 0) return (short) t;                                // die on error
DISABLE_WARN(-Warray-bounds)
                    cdata->len = ltocstring(cdata->buf, depPtr->bytes[k]);
ENABLE_WARN
                    t = SQ_Write(cdata);                                        // dump data character
                    if (t < 0) return (short) t;                                // die on error

                    if (k == (datalen + j + 1)) {
DISABLE_WARN(-Warray-bounds)
                        cdata->len = 1;
ENABLE_WARN
                        strcpy((char *) cdata->buf, ")");
                        t = SQ_Write(cdata);                                    // dump data character
                        if (t < 0) return (short) t;                            // die on error
                    }

                    escape = TRUE;
                } else {
                    if (string && (k == (j + 2))) {
DISABLE_WARN(-Warray-bounds)
                        cdata->len = 1;
ENABLE_WARN
                        strcpy((char *) cdata->buf, "\"");
                        t = SQ_Write(cdata);                                    // dump data character
                        if (t < 0) return (short) t;                            // die on error
                    }

                    if (escape) {
DISABLE_WARN(-Warray-bounds)
                        cdata->len = 3;
ENABLE_WARN
                        strcpy((char *) cdata->buf, ")_\"");
                        t = SQ_Write(cdata);                                    // dump data character
                        if (t < 0) return (short) t;                            // die on error
                    } else if (depPtr->bytes[k] == '"') {
DISABLE_WARN(-Warray-bounds)
                        cdata->len = 1;
ENABLE_WARN
                        strcpy((char *) cdata->buf, "\"");
                        t = SQ_Write(cdata);                                    // dump data character
                        if (t < 0) return (short) t;                            // die on error
                    }

                    t = SQ_WriteStar(depPtr->bytes[k]);                         // dump data character
                    if (t < 0) return (short) t;                                // die on error
                    escape = FALSE;

                    if (string && (k == (datalen + j + 1))) {
DISABLE_WARN(-Warray-bounds)
                        cdata->len = 1;
ENABLE_WARN
                        strcpy((char *) cdata->buf, "\"");
                        t = SQ_Write(cdata);                                    // dump data character
                        if (t < 0) return (short) t;                            // die on error
                    }
                }
            }

            if (!datalen) {
DISABLE_WARN(-Warray-bounds)
                cdata->len = 2;
ENABLE_WARN
                strcpy((char *) cdata->buf, "\"\"");
                t = SQ_Write(cdata);                                            // dump data character
                if (t < 0) return (short) t;                                    // die on error
            }

            t = SQ_WriteFormat(SQ_LF);                                          // write a line feed
            if (t < 0) return (short) t;                                        // die on error
            depPtr = depPtr->deplnk;                                            // get next if any
        }                                                                       // end while dependents exist
    }                                                                           // end for all symtab entries

    return 0;                                                                   // finished successfully
}                                                                               // end function ST_Dump

// copy all variables in as subscripts to specified global
short ST_DumpV(mvar *global)
{
    int       j;                                                                // generic counter
    int       t;                                                                // for functions
    short     gs;                                                               // global slen save value
    u_char    gks[255];
    cstring   *cdata;                                                           // variable data gets dumped
    u_char    dump[1024];                                                       // variable name gets dumped
    ST_depend *depPtr = ST_DEPEND_NULL;                                         // active dependent ptr

    cdata = (cstring *) dump;                                                   // make it a cstring
    partab.src_var.uci = UCI_IS_LOCALVAR;                                       // init UCI as LOCAL
    partab.src_var.volset = 0;                                                  // init volume set
    gs = global->slen;                                                          // save original sub length
    memcpy(gks, global->key, global->slen);                                     // save original key

    for (int i = 0; i < ST_MAX; i++) {                                          // for each entry in symbol table
        if (symtab[i].data == ST_DATA_NULL) continue;                           // get out if nothing to dump
        if (symtab[i].varnam.var_cu[0] == '$') continue;                        // no $ vars
        if (var_empty(symtab[i].varnam)) continue;                              // ensure something there
        VAR_COPY(partab.src_var.name, symtab[i].varnam);                        // init var name
        partab.src_var.slen = 0;                                                // init subscript length

        if (symtab[i].data->dbc != VAR_UNDEFINED) {                             // if data exists
            t = UTIL_String_Mvar(&partab.src_var, cdata->buf, MAX_NUM_SUBS);
            if (t < 0) return (short) t;                                        // if error, quit
DISABLE_WARN(-Warray-bounds)
            cdata->len = t;
ENABLE_WARN
            memcpy(global->key, gks, gs);                                       // restore initial key
            global->slen = gs;                                                  // restore initial length
            global->slen = global->slen + UTIL_Key_Build(cdata, &global->key[gs]);

            // set rest of global key and len
            t = DB_Set(global, (cstring *) &symtab[i].data->dbc);               // try to set it

            // block overhead - header (20 or 44) + index (2) + chunk (2) + CCC (1) + UCC (1) + key (~34) + DBC (2) + alignment (~4)
            if (t == -ERRM75) {                                                 // if string too long
                j = symtab[i].data->dbc;                                        // save this
                symtab[i].data->dbc = 934;                                      // that should work (1024 - 90 overhead)
                //symtab[i].data->dbc = SOA(partab.vol[global->volset - 1]->vollab)->block_size - 90; // that should work
                t = DB_Set(global, (cstring *) &symtab[i].data->dbc);           // try again
                symtab[i].data->dbc = j;                                        // restore this
            }
        }                                                                       // end if data exists

        depPtr = symtab[i].data->deplnk;                                        // get first dependent

        while (depPtr != ST_DEPEND_NULL) {                                      // while dependents exist
            partab.src_var.slen = depPtr->keylen;                               // init subscript length
            memcpy(partab.src_var.key, depPtr->bytes, depPtr->keylen);          // init key
            cdata = (cstring *) &dump[0];                                       // get it into a cstring
            t = UTIL_String_Mvar(&partab.src_var, cdata->buf, MAX_NUM_SUBS);
            if (t < 0) return (short) t;                                        // if error, quit
DISABLE_WARN(-Warray-bounds)
            cdata->len = t;
ENABLE_WARN
            j = (int) depPtr->keylen;                                           // find key length
            if ((j & 1) != 0) j++;                                              // up it to next even boudary
            memcpy(global->key, gks, gs);                                       // restore initial key
            global->slen = gs;                                                  // restore initial length
            global->slen += UTIL_Key_Build(cdata, &global->key[gs]);

            // set up global key
            t = DB_Set(global, (cstring *) &depPtr->bytes[j]);                  // try to set it
            if (t < 0) return (short) t;
            depPtr = depPtr->deplnk;                                            // get next if any
        }                                                                       // end while dependents exist
    }                                                                           // end for all symtab entries

    return 0;                                                                   // finished successfully
}                                                                               // end function DumpV
