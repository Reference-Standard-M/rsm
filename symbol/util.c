/*
 * Package:  Reference Standard M
 * File:     rsm/symbol/util.c
 * Summary:  module symbol - symbol table utilities
 *
 * David Wicksell <dlw@linux.com>
 * Copyright © 2020-2023 Fourth Watch Software LC
 * https://gitlab.com/Reference-Standard-M/rsm
 *
 * Based on MUMPS V1 by Raymond Douglas Newman
 * Copyright (c) 1999-2016
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
#include <string.h>                                                             // for string ops
#include "rsm.h"                                                                // standard includes
#include "symbol.h"                                                             // our definitions
#include "error.h"                                                              // errors
#include "init.h"                                                               // init prototypes
#include "proto.h"                                                              // standard prototypes
#include "compile.h"                                                            // for routine buffer stuff

short         st_hash[ST_HASH + 1];                                             // allocate hashing table
symtab_struct symtab[ST_MAX + 1];                                               // and symbol table

/*
 * Function: ST_Hash - Create a hash from a variable name
 * returns hash number
 */
short ST_Hash(var_u var)                                                        // var name in a quad
{
    int       i;                                                                // for the loop
    int       ret = 0;                                                          // return value

    const int p[32] = {
        3, 5, 7, 11, 13, 17, 19, 23,
        29, 31, 37, 41, 43, 47, 53, 59,
        61, 67, 71, 73, 79, 83, 89, 97,
        101, 103, 107, 109, 113, 127, 131, 137
    };                                                                          // odd primes

    for (i = 0; i < VAR_LEN; i++) {                                             // for each character
        if (var.var_cu[i] == 0) break;
        ret = ((var.var_cu[i] * p[i]) + ret);
    }

    return (short) (ret % ST_HASH);                                             // return mod hash value
}                                                                               // end of ST_Hash

/*
 * Function: ST_Init - initialize an empty symbol table
 * returns nothing
 */
void ST_Init(void)                                                              // no arguments
{
    int i;                                                                      // for loops

    for (i = 0; i < ST_HASH; i++) st_hash[i] = -1;                              // clear hash table, -1 means empty
    st_hash[ST_FREE] = 0;                                                       // head of free list

    for (i = 0; i < ST_MAX; i++) {                                              // for each symbol entry
        symtab[i].fwd_link = (i + 1);                                           // point to next entry
        symtab[i].usage = 0;                                                    // clear usage count
        symtab[i].data = ST_DATA_NULL;                                          // clear data pointer
        VAR_CLEAR(symtab[i].varnam);                                            // clear variable name
    }                                                                           // end symtab clear loop

    symtab[ST_MAX].fwd_link = -1;                                               // indicate end of list
    return;                                                                     // done
}                                                                               // end of ST_Init()

/*
 * Function: ST_Locate - locate varname in symbol table
 * returns short pointer or -1 on fail
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
 * returns short pointer or -1 on fail
 */
short ST_LocateIdx(int idx)                                                     // var index
{
    short fwd;                                                                  // fwd link pointer
    var_u var;                                                                  // var name (if required)
    rbd   *p;                                                                   // for looking at routines
    var_u *vt;                                                                  // for the var table

    fwd = partab.jobtab->dostk[partab.jobtab->cur_do].symbol[idx];
    if (fwd > -1) return fwd;                                                   // got it
    p = (rbd *) partab.jobtab->dostk[partab.jobtab->cur_do].routine;
    vt = (var_u *) (((u_char *) p) + p->var_tbl);                               // point at var table
    VAR_COPY(var, vt[idx]);                                                     // get the var name
    fwd = ST_SymAtt(var);                                                       // attach and get index
    if (fwd < 0) return fwd;                                                    // error if none free
    partab.jobtab->dostk[partab.jobtab->cur_do].symbol[idx] = fwd;              // save idx
    return fwd;                                                                 // return index
}                                                                               // end of ST_LocateIdx()

/*
 * Function: ST_Free - free varname entry in symbol table
 * returns nothing - only called when var exists
 */
void ST_Free(var_u var)                                                         // var name in a quad
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
 * Function: ST_Create - create/locate varname in symtab
 * returns short pointer or -1 on error
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
                if ((lastkey != ST_DEPEND_NULL) && (UTIL_Key_KeyCmp(var->key, lastkey->bytes, var->slen, lastkey->keylen) > 0)) {
                    check = lastkey;
                }

                // check key less than supplied (var after check)
                while ((check != ST_DEPEND_NULL) && (UTIL_Key_KeyCmp(var->key, check->bytes, var->slen, check->keylen) > 0)) {
                    checkprev = check;                                          // save current to previous
                    check = check->deplnk;                                      // go to next
                }                                                               // end if we go past it, or end

                if ((check != ST_DEPEND_NULL) && (memcmp(check->bytes, var->key, var->slen) == 0)) { // valid remove
                    ST_RemDp(data, checkprev, check, var);                      // get rid of it
                }                                                               // end if valid remove found
            }                                                                   // end if dep exists

            data->last_key = checkprev;                                         // add last used key
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

    if ((dp->deplnk != ST_DEPEND_NULL) && (mvardr->slen == 0)) {                // kill DT more dep - (not currently called)
        ST_RemDp(dblk, dp, dp->deplnk, mvardr);                                 // try to get rid of next one
    } else {                                                                    // end if more to do - kill DP or run out of deps
        if ((dp->deplnk != ST_DEPEND_NULL) && (memcmp(dp->bytes, mvardr->key, mvardr->slen) == 0)) { // next dep, has part match key
            ST_RemDp(dblk, dp, dp->deplnk, mvardr);                             // try to get rid of next one
        }                                                                       // end if keys part match
    }                                                                           // end if more to do

    if (mvardr->slen == 0) {                                                    // killing a data block - (not currently called)
        if (prev != ST_DEPEND_NULL) {                                           // prev is defined
            prev->deplnk = ST_DEPEND_NULL;                                      // unlink all deps regardless
        } else {                                                                // end if prev defined - prev not defined
            dblk->deplnk = ST_DEPEND_NULL;                                      // unlink one and only dep
        }                                                                       // end if prev not defined

        free(dp);                                                               // get rid of dep
    } else {                                                                    // end if killing a data block - killing a dep tree
        if (memcmp(dp->bytes, mvardr->key, mvardr->slen) == 0) {                // keys match to slen
            if (prev != ST_DEPEND_NULL) {                                       // if not removing first dep
                prev->deplnk = dp->deplnk;                                      // bypass a dep killee
            } else {                                                            // end if !removing first dep - removing first dep
                dblk->deplnk = dp->deplnk;                                      // bypass a first dep killee
            }                                                                   // end else removing first dep

            free(dp);                                                           // get rid of this dep
        }                                                                       // end if keys match up to slen
    }                                                                           // end else killing a dep
}                                                                               // end function ST_RemDp

/*
 * Function: ST_Get - Retrieve data
 * returns length of data
 */
int ST_Get(mvar *var, u_char *buf)                                              // get data at var/subscript
{
    int     s;                                                                  // for return value
    cstring *data;                                                              // ptr for ST_GetAdd()

    s = ST_GetAdd(var, &data);                                                  // get address of data
    if (s < 0) return s;                                                        // if error, quit
    s = mcopy(&data->buf[0], buf, s);                                           // copy data (if any)
    return s;                                                                   // return the size (or error)
}                                                                               // end function ST_Get

/*
 * Function: FixData(ST_data *old, ST_data *new, int count)
 * When the data pointer changes, this fixes count pointers to it
 * Only called internally from this file
 */
void FixData(const ST_data *old, ST_data *new, int count)
{
    int       i;                                                                // for loops
    int       c;                                                                // a counter
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
            for (c = 0; c < newtab->count_new; c++) {                           // for each variable
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
 * Function: ST_Set - Set a variable in memory, create or replace
 * returns length of data on success, negative otherwise
 */
int ST_Set(mvar *var, cstring *data)                                            // set var to be data
{
    ST_depend *ptr1;                                                            // a dependent pointer
    ST_depend *newPtrDp = ST_DEPEND_NULL;                                       // a dependent pointer
    ST_data   *newPtrDt = ST_DATA_NULL;                                         // a data pointer
    ST_depend *prevPtr = ST_DEPEND_NULL;                                        // pointer to previous element
    int       n;                                                                // key length
    int       pad = 0;                                                          // extra space padding
    u_short   *ptr2ushort;                                                      // needed for u_short into char
    short     fwd;                                                              // position in symtab

    if ((var->slen & 1) != 0) pad = 1;                                          // set up for any extra space

    if (var->volset) {                                                          // if volset defined
        fwd = ST_LocateIdx(var->volset - 1);                                    // locate var by volset
    } else {                                                                    // if no volset or volset zero
        fwd = ST_Create(var->name);                                             // attempt to create new ST ent
    }

    if (fwd < 0) return fwd;                                                    // error if none free

    if (symtab[fwd].data == ST_DATA_NULL) {                                     // if not already exists
        u_int i = DTBLKSIZE + data->len;                                        // required memory

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
            ptr2ushort = (u_short *) &newPtrDp->bytes[n];                       // get a (u_short *) to here
            *ptr2ushort = data->len;                                            // save the data length
            n += 2;                                                             // point past the DBC
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
            ptr2ushort = (u_short *) &newPtrDp->bytes[n];                       // get a (u_short *) to here
            *ptr2ushort = data->len;                                            // save the data length
            n += 2;                                                             // point past the DBC
            memcpy(&newPtrDp->bytes[n], &data->buf[0], data->len + 1);          // copy data and trailing 0
            ptr1 = symtab[fwd].data->deplnk;                                    // go into dependents

            if (ptr1 != ST_DEPEND_NULL) {                                       // deps currently exist
                ST_depend *lastkey = symtab[fwd].data->last_key;                // pointer to last used key

                // start search at last used key, rather than at the beginning (var after lastkey)
                if ((lastkey != ST_DEPEND_NULL) && (UTIL_Key_KeyCmp(var->key, lastkey->bytes, var->slen, lastkey->keylen) > 0)) {
                    ptr1 = lastkey;
                }

                // compare keys (var after ptr1)
                while ((ptr1 != ST_DEPEND_NULL) && (UTIL_Key_KeyCmp(var->key, ptr1->bytes, var->slen, ptr1->keylen) > 0)) {
                    prevPtr = ptr1;                                             // save previous
                    ptr1 = ptr1->deplnk;                                        // get next
                    if (ptr1 == ST_DEPEND_NULL) break;                          // gone beyond last
                }                                                               // end while-compare keys

                // replace data if var keys equal it means to replace data (var equal ptr1)
                if ((ptr1 != ST_DEPEND_NULL) && (UTIL_Key_KeyCmp(var->key, ptr1->bytes, var->slen, ptr1->keylen) == 0)) {
                    if (prevPtr == ST_DEPEND_NULL) {                            // if no prev pointer
                        newPtrDp->deplnk = ptr1->deplnk;                        // link to previous first dep
                        symtab[fwd].data->deplnk = newPtrDp;                    // link in as first dep
                    } else {                                                    // end if no prev ptr - if prev pointer is defined
                        newPtrDp->deplnk = ptr1->deplnk;                        // set new dependent link
                        prevPtr->deplnk = newPtrDp;                             // alter previous to link in
                    }                                                           // end else bypassing mid list

                    free(ptr1);                                                 // remove previous dep link
                // if we have a dependent, create more data (var before ptr1)
                } else if ((ptr1 != ST_DEPEND_NULL) && (UTIL_Key_KeyCmp(var->key, ptr1->bytes, var->slen, ptr1->keylen) < 0)) {
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
            } else {                                                            // end if elements exist - no elements curr exist
                symtab[fwd].data->deplnk = newPtrDp;                            // add as first element ever
            }                                                                   // end else no elements existed

            symtab[fwd].data->last_key = prevPtr;                               // add last used key
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
        int i = var->slen;                                                      // get the length
        ST_depend *lastkey = symtab[ptr1].data->last_key;                       // pointer to last used key

        depPtr = symtab[ptr1].data->deplnk;                                     // get first dependent

        if (depPtr != ST_DEPEND_NULL) {                                         // only if we should go on
            if (depPtr->keylen < i) i = depPtr->keylen;                         // adjust length if needed
        }                                                                       // end if

        // start search at last used key, rather than at the beginning (var after lastkey)
        if ((lastkey != ST_DEPEND_NULL) && (UTIL_Key_KeyCmp(var->key, lastkey->bytes, var->slen, lastkey->keylen) > 0)) {
            depPtr = lastkey;
        }

        while ((depPtr != ST_DEPEND_NULL) && (memcmp(depPtr->bytes, var->key, i) < 0)) { // while not yet found or past
            depPtr = depPtr->deplnk;                                            // go to next
            if (depPtr == ST_DEPEND_NULL) break;                                // have we run out
            i = var->slen;                                                      // get the length again
            if (depPtr->keylen < i) i = depPtr->keylen;                         // adjust length if needed
        }                                                                       // end while

        if (depPtr == ST_DEPEND_NULL) {                                         // if we ran out of deps
            memcpy(buf, "0\0", 2);
            return 1;                                                           // return same
        }                                                                       // end if

        i = var->slen;                                                          // get the length again
        if (depPtr->keylen < i) i = depPtr->keylen;                             // adjust length if needed

        while (((depPtr != ST_DEPEND_NULL) && (memcmp(depPtr->bytes, var->key, i) == 0) && // while more deps and matches ok for i
          (depPtr->keylen < var->slen)) || ((depPtr != ST_DEPEND_NULL) &&       // an exact match and var slen still longer
          (memcmp(depPtr->bytes, var->key, i) < 0))) {                          // while more deps and
            depPtr = depPtr->deplnk;                                            // go to next
            if (depPtr == ST_DEPEND_NULL) break;                                // have we run out
            i = var->slen;                                                      // get the length again
            if (depPtr->keylen < i) i = depPtr->keylen;                         // adjust length if needed
        }                                                                       // end while

        if (depPtr == ST_DEPEND_NULL) {                                         // if we ran out
            memcpy(buf, "0\0", 2);
            return 1;                                                           // return same
        }                                                                       // end if

        if ((depPtr != ST_DEPEND_NULL) && (memcmp(depPtr->bytes, var->key, i) == 0)) { // if matches ok for i
            if (depPtr->keylen == var->slen) {                                  // exact match
                depPtr = depPtr->deplnk;                                        // go to next

                if (depPtr == ST_DEPEND_NULL) {                                 // have we run out
                    memcpy(buf, "1\0", 2);
                    return 1;                                                   // return same
                }                                                               // end if

                i = var->slen;                                                  // get the length
                if (depPtr->keylen < i) i = depPtr->keylen;                     // adjust len if needed

                if (memcmp(depPtr->bytes, var->key, i) == 0) {                  // if match ok for i
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
    int       i = 0;                                                            // generic counter
    char      keysub[256];                                                      // current key subscript
    u_char    upOneLev[256];
    u_char    crud[256];
    int       index = 0;                                                        // where up to in key extract
    short     ret = 0;                                                          // current position in key

    if (var->volset) {                                                          // if by index
        ptr1 = ST_LocateIdx(var->volset - 1);                                   // get it this way
    } else {                                                                    // no volset, so use var name
        ptr1 = ST_Locate(var->name);                                            // locate the variable by name
    }

    buf[0] = '\0';                                                              // JIC
    if (ptr1 < 0) return 0;
    if (var->slen == 0) return 0;                                               // if can't $ORDER a data block then return null
    UTIL_Key_Chars_In_Subs((char *) var->key, (int) var->slen, 255, &pieces, (char *) NULL); // Return num of sub in pieces

    /*
     * Return characters in all but last subscript, number of subscripts in subs
     * and key string (less last subscript) at upOneLev[1] on for upOneLev[0] bytes
     */
    upOneLev[0] = (u_char) UTIL_Key_Chars_In_Subs((char *) var->key, (int) var->slen, pieces - 1, &subs, (char *) &upOneLev[1]);

    if (symtab[ptr1].data == ST_DATA_NULL) return 0;                            // no data
    current = symtab[ptr1].data->deplnk;                                        // go to first dependent
    lastkey = symtab[ptr1].data->last_key;                                      // pointer to last used key

    // start search at last used key, rather than at the beginning (var after lastkey)
    if ((lastkey != ST_DEPEND_NULL) && (UTIL_Key_KeyCmp(var->key, lastkey->bytes, var->slen, lastkey->keylen) > 0) && (dir == 1)) {
        current = lastkey;
    }

    // compare keys - while we have dependent and key match fails (var after current)
    while ((current != ST_DEPEND_NULL) && (UTIL_Key_KeyCmp(var->key, current->bytes, var->slen, current->keylen) > 0)) {
        prev = current;                                                         // set prev pointer
        current = current->deplnk;                                              // go to next dependent pointer
    }                                                                           // end while-compare keys

    if (current == ST_DEPEND_NULL) {                                            // nothing past our key
        if (dir == 1) return 0;
    }                                                                           // output same, return length

    if (dir == -1) {                                                            // reverse order
        current = prev;                                                         // save current
        if (current == ST_DEPEND_NULL) return 0;                                // if pointing nowhere then return length of zero
        crud[0] = UTIL_Key_Chars_In_Subs((char *) current->bytes, (int) current->keylen, pieces - 1, &subs, (char *) &crud[1]);

        if ((crud[0] != 0) && (upOneLev[0] != 0)) {
            if (crud[0] != upOneLev[0]) return 0;
            if (memcmp(&crud[1], &upOneLev[1], upOneLev[0]) != 0) return 0;
            // Ensure higher level subscripts (if any) are equal
        }
    } else {                                                                    // end if reverse order - forward order
        // while we have dependents and key cmp fails
        while ((current != ST_DEPEND_NULL) && (memcmp(current->bytes, var->key, var->slen) == 0)) {
            current = current->deplnk;                                          // go to next dependent
        }                                                                       // end while

        if (current == ST_DEPEND_NULL) return 0;                                // if current points nowhere, return length of zero

        // compare keys, if compare fails to match (var before or after current)
        if (UTIL_Key_KeyCmp(var->key, current->bytes, var->slen, current->keylen) != 0) {
            crud[0] = UTIL_Key_Chars_In_Subs((char *) current->bytes, (int) current->keylen, pieces - 1, &subs, (char *) &crud[1]);

            if ((crud[0] != 0) && (upOneLev[0] != 0)) {                         // if lengths aren't 0
                if (memcmp(&crud[1], &upOneLev[1], upOneLev[0]) != 0) return 0; // & cmp fails then return a length of zero
            }                                                                   // end if slen's non zero
        }                                                                       // end if keys dont equal
    }                                                                           // end else-forward order

    if (current == ST_DEPEND_NULL) return 0;                                    // nothing past our key then return length

    for (i = 1; i <= pieces; i++) {                                             // number of keys
        int upto = 0;                                                           // clear flag

        ret = UTIL_Key_Extract(&current->bytes[index], (u_char *) keysub, &upto); // next key
        index += upto;                                                          // increment index
        if ((index >= current->keylen) && (i < pieces)) return 0;               // hit end of key & !found then return null
    }                                                                           // end for-pieces to level required

    symtab[ptr1].data->last_key = prev;                                         // add last used key

    // Now have ASCII key in desired position number, put the ASCII value of that key in *buf and return the length of it
    return (short) mcopy((u_char *) keysub, buf, ret);
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
    short     askeylen = 0;                                                     // length of *askey
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
    if (current == ST_DEPEND_NULL) return 0;                                    // not found
    lastkey = symtab[ptr1].data->last_key;                                      // pointer to last used key

    // start search at last used key, rather than at the beginning (var after lastkey)
    if ((lastkey != ST_DEPEND_NULL) && (UTIL_Key_KeyCmp(var->key, lastkey->bytes, var->slen, lastkey->keylen) > 0) && (dir == 1)) {
        current = lastkey;
    }

    // while more exist with keys that are larger, get next dep (var after current)
    while ((current != ST_DEPEND_NULL) && (UTIL_Key_KeyCmp(var->key, current->bytes, var->slen, current->keylen) > 0)) {
        prev = current;                                                         // save prev pointer
        current = current->deplnk;                                              // go to next dependent pointer
    }                                                                           // end while-compare keys

    if (var->slen > 0) {                                                        // looking in dependents
        if (dir == -1) {                                                        // reverse order
            if (prev != ST_DEPEND_NULL) current = prev;                         // only if previous ptr defined, go back one
        } else {                                                                // end if reverse order - start forward order
            // not going past non exist last and keys are equal (var equal current)
            if ((current != ST_DEPEND_NULL) && (UTIL_Key_KeyCmp(var->key, current->bytes, var->slen, current->keylen) == 0)) {
                current = current->deplnk;                                      // go to next
            }                                                                   // end if exact match
        }                                                                       // end else-forward
    }                                                                           // finished looking

    if (current == ST_DEPEND_NULL) return 0;                                    // if we have gone past end, return length of zero
    if ((dir == -1) && (var->slen == 0)) return 0;                              // reverse dir and data block, return length of zero
    outputVar.slen = current->keylen;                                           // key and length of set
    memcpy(outputVar.key, current->bytes, (int) current->keylen);               // setup mvar

    if ((current == symtab[ptr1].data->deplnk) && (prev != current) && (dir == -1) && (var->slen > 0)) { // previous is a data block
        outputVar.slen = 0;                                                     // flag is as such
    }                                                                           // end if back to a data block

    symtab[ptr1].data->last_key = prev;                                         // add last used key
    askeylen = UTIL_String_Mvar(&outputVar, buf, MAX_NUM_SUBS);                 // convert mvar
    return askeylen;                                                            // return length of key
}                                                                               // end ST_Query

int ST_GetAdd(mvar *var, cstring **add)                                         // get local data address
{
    int       ptr1;                                                             // position in symtab
    ST_depend *depPtr = ST_DEPEND_NULL;                                         // active pointer
    int       i;                                                                // generic counter

    if (var->volset) {                                                          // if by index
        ptr1 = ST_LocateIdx(var->volset - 1);                                   // get it this way
    } else {                                                                    // no volset, use var name
        ptr1 = ST_Locate(var->name);                                            // locate the variable by name
    }

    if ((ptr1 > ST_MAX) || (ptr1 < -1)) {
        panic("ST_GetAdd: Junk pointer returned from ST_LocateIdx");
    } else if (ptr1 >= 0) {                                                     // think we found it
        if (symtab[ptr1].data == ST_DATA_NULL) return -ERRM6;                   // not found

        if (var->slen > 0) {                                                    // go to dependents
            ST_depend *lastkey = symtab[ptr1].data->last_key;                   // pointer to last used key

            depPtr = symtab[ptr1].data->deplnk;                                 // get first dependent

            // start search at last used key, rather than at the beginning (var after lastkey)
            if ((lastkey != ST_DEPEND_NULL) && (UTIL_Key_KeyCmp(var->key, lastkey->bytes, var->slen, lastkey->keylen) > 0)) {
                depPtr = lastkey;
            }

            while (depPtr != ST_DEPEND_NULL) {                                  // while dep ok, compare keys
                i = UTIL_Key_KeyCmp(var->key, depPtr->bytes, var->slen, depPtr->keylen);
                if (i == K2_GREATER) return -ERRM6;                             // error if we passed it (var before depPtr)
                if (i == KEQUAL) break;                                         // found it (var equal depPtr)
                depPtr = depPtr->deplnk;                                        // get next
            }                                                                   // end while - compare keys

            if (depPtr == ST_DEPEND_NULL) return -ERRM6;                        // if no exist then return same
            i = (int) depPtr->keylen;                                           // get key length
            if (i & 1) i++;                                                     // ensure even
            *add = (cstring *) &depPtr->bytes[i];                               // send data addr as cstring
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

// 0 to ST_MAX - 1 (i.e., ((ST_HASH + 1) * 3))
short ST_Dump(void)                                                             // dump entire ST to $I
{
    int       i;                                                                // generic counter
    int       j;                                                                // generic counter
    int       s;                                                                // for functions
    cstring   *cdata;                                                           // variable data gets dumped
    u_char    dump[512];                                                        // variable name gets dumped
    ST_depend *depPtr = ST_DEPEND_NULL;                                         // active dependent ptr

    for (i = 0; i < ST_MAX; i++) {                                              // for each entry in ST
        if (symtab[i].data == ST_DATA_NULL) continue;                           // get out if nothing to dump
        if (symtab[i].varnam.var_cu[0] == '$') continue;                        // dont spit out $ vars
        VAR_COPY(partab.src_var.name, symtab[i].varnam);                        // init var name
        partab.src_var.uci = UCI_IS_LOCALVAR;                                   // init UCI as LOCAL
        partab.src_var.slen = 0;                                                // init subscript length
        partab.src_var.volset = 0;                                              // init volume set
        cdata = (cstring *) &dump[0];                                           // make it a cstring

        if (symtab[i].data->dbc != VAR_UNDEFINED) {                             // valid dbc
            cdata->len = UTIL_String_Mvar(&partab.src_var, cdata->buf, MAX_NUM_SUBS); // get var name and dump data block
DISABLE_WARN(-Warray-bounds)
            cdata->buf[cdata->len++] = '=';                                     // tack on equal sign
ENABLE_WARN
            s = SQ_Write(cdata);                                                // dump var name =
            if (s < 0) return (short) s;                                        // die on error
            s = SQ_Write((cstring *) &symtab[i].data->dbc);                     // dump data block
            if (s < 0) return (short) s;                                        // die on error
            s = SQ_WriteFormat(SQ_LF);                                          // line feed
            if (s < 0) return (short) s;                                        // die on error
        }                                                                       // end if valid dbc

        cdata = NULL;                                                           // nullify the cstring
        depPtr = symtab[i].data->deplnk;                                        // get first dependent

        while (depPtr != ST_DEPEND_NULL) {                                      // while dependents exist
            VAR_COPY(partab.src_var.name, symtab[i].varnam);                    // init var name
            partab.src_var.uci = UCI_IS_LOCALVAR;                               // init UCI as LOCAL
            partab.src_var.slen = depPtr->keylen;                               // init subscript length
            partab.src_var.volset = 0;                                          // init volume set
            memcpy(partab.src_var.key, depPtr->bytes, depPtr->keylen);          // init key
            cdata = (cstring *) &dump[0];                                       // get into a cstring
DISABLE_WARN(-Warray-bounds)
            cdata->len = UTIL_String_Mvar(&partab.src_var, cdata->buf, MAX_NUM_SUBS); // get var name and dump dependent block
            cdata->buf[cdata->len++] = '=';                                     // tack on an equal sign
ENABLE_WARN
            s = SQ_Write(cdata);                                                // dump var name =
            if (s < 0) return (short) s;                                        // die on error
            j = (int) depPtr->keylen;                                           // find key length
            if ((j & 1) != 0) j++;                                              // up it to next even boudary
            s = SQ_Write((cstring *) &depPtr->bytes[j]);                        // write out the data
            if (s < 0) return (short) s;                                        // return if error occurred
            s = SQ_WriteFormat(SQ_LF);                                          // write a line feed
            if (s < 0) return (short) s;                                        // die on error
            depPtr = depPtr->deplnk;                                            // get next if any
        }                                                                       // end while dependents exist
    }                                                                           // end for all symtab entries

    return 0;                                                                   // finished successfully
}                                                                               // end function ST_Dump

// Return next key in supplied mvar and data at buf
int ST_QueryD(mvar *var, u_char *buf)                                           // get next key and data
{
    int       ptr1;                                                             // position in symtab
    cstring   *cdata;                                                           // temporary data access
    ST_depend *current = ST_DEPEND_NULL;                                        // active pointer
    ST_depend *prev = ST_DEPEND_NULL;                                           // pointer to previous element
    ST_depend *lastkey = ST_DEPEND_NULL;                                        // pointer to last used key
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
    if ((lastkey != ST_DEPEND_NULL) && (UTIL_Key_KeyCmp(var->key, lastkey->bytes, var->slen, lastkey->keylen) > 0)) {
        current = lastkey;
    }

    // more deps exist and key compare fails - compare keys (var after current)
    while ((current != ST_DEPEND_NULL) && (UTIL_Key_KeyCmp(var->key, current->bytes, var->slen, current->keylen) > 0)) {
        prev = current;                                                         // set prev pointer
        current = current->deplnk;                                              // to next dependent pointer
    }                                                                           // end while-compare keys

    // while more deps exist and keys match exactly - (var equal current)
    if ((current != ST_DEPEND_NULL) && (UTIL_Key_KeyCmp(var->key, current->bytes, var->slen, current->keylen) == 0)) {
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

// copy all variables in as subscripts to specified global
short ST_DumpV(mvar *global)
{
    int       i;                                                                // generic counter
    int       j;                                                                // generic counter
    short     s;                                                                // for functions
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

    for (i = 0; i < ST_MAX; i++) {                                              // for each entry in ST
        if (symtab[i].data == ST_DATA_NULL) continue;                           // get out if nothing to dump
        if (symtab[i].varnam.var_cu[0] == '$') continue;                        // no $ vars
        if (var_empty(symtab[i].varnam)) continue;                              // ensure something there
        VAR_COPY(partab.src_var.name, symtab[i].varnam);                        // init var name
        partab.src_var.slen = 0;                                                // init subscript length

        if (symtab[i].data->dbc != VAR_UNDEFINED) {                             // if data exists
            s = UTIL_String_Mvar(&partab.src_var, cdata->buf, MAX_NUM_SUBS);
            if (s < 0) return s;                                                // if error, quit
DISABLE_WARN(-Warray-bounds)
            cdata->len = s;
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
                //symtab[i].data->dbc = systab->vol[global->volset - 1]->vollab->block_size - 90; // that should work
                t = DB_Set(global, (cstring *) &symtab[i].data->dbc);           // try again
                symtab[i].data->dbc = j;                                        // restore this
            }
        }                                                                       // end if data exists

        depPtr = symtab[i].data->deplnk;                                        // get first dependent

        while (depPtr != ST_DEPEND_NULL) {                                      // while dependents exist
            partab.src_var.slen = depPtr->keylen;                               // init subscript length
            memcpy(partab.src_var.key, depPtr->bytes, depPtr->keylen);          // init key
            cdata = (cstring *) &dump[0];                                       // get it into a cstring
            s = UTIL_String_Mvar(&partab.src_var, cdata->buf, MAX_NUM_SUBS);
            if (s < 0) return s;                                                // if error, quit
DISABLE_WARN(-Warray-bounds)
            cdata->len = s;
ENABLE_WARN
            j = (int) depPtr->keylen;                                           // find key length
            if ((j & 1) != 0) j++;                                              // up it to next even boudary
            memcpy(global->key, gks, gs);                                       // restore initial key
            global->slen = gs;                                                  // restore initial length
            global->slen = global->slen + UTIL_Key_Build(cdata, &global->key[gs]);

            // set up global key
            t = DB_Set(global, (cstring *) &depPtr->bytes[j]);                  // try to set it
            if (t < 0) return (short) t;
            depPtr = depPtr->deplnk;                                            // get next if any
        }                                                                       // end while dependents exist
    }                                                                           // end for all symtab entries

    return 0;                                                                   // finished successfully
}                                                                               // end function DumpV

// kill all local variables except those whose names appear in var_u *keep
short ST_KillAll(int count, var_u *keep)
{
    int i;                                                                      // generic counter
    int j;                                                                      // generic counter

    partab.src_var.uci = UCI_IS_LOCALVAR;                                       // init UCI as LOCAL
    partab.src_var.slen = 0;                                                    // init subscript length
    partab.src_var.volset = 0;                                                  // init volume set

    for (i = 0; i < ST_MAX; i++) {                                              // for each entry in ST
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
    short pos = ST_Create(var);                                                 // position in symtab - locate/create variable

    if (pos >= 0) symtab[pos].usage++;                                          // if ok, increment usage
    return pos;                                                                 // return whatever we found
}

/*
 * For each symtab entry in list (ignoring those that are VAR_UNDEFINED),
 * decrement usage. Remove the entry if it is undefined.
 */
void ST_SymDet(int count, short *list)
{
    int i;                                                                      // a handy int

    for (i = 0; i < count; i++) {                                               // for all supplied vars
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
short ST_SymSet(short pos, cstring *data)
{
    u_int   i;                                                                  // a handy int
    ST_data *ptr;                                                               // and a pointer

    i = DTBLKSIZE + data->len;                                                  // size required
    if (i < DTMINSIZE) i = DTMINSIZE;                                           // check for minimum

    if (symtab[pos].data == ST_DATA_NULL) {                                     // if no data block
        symtab[pos].data = malloc(i);                                           // get some memory
        if (symtab[pos].data == ST_DATA_NULL) return -(ERRZ56 + ERRMLAST);      // no mem
        symtab[pos].data->deplnk = ST_DEPEND_NULL;                              // init dep link
        symtab[pos].data->attach = 1;                                           // init attach count
    } else if (symtab[pos].data->dbc < data->len) {                             // enough space?
        ptr = realloc(symtab[pos].data, i);                                     // attempt to increase it
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
