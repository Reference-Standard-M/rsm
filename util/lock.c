/*
 * Package: Reference Standard M
 * File:    rsm/util/lock.c
 * Summary: module database - lock utilities
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

#include "compile.h"                                                            // for RBD definition
#include "error.h"                                                              // errors
#include "proto.h"                                                              // standard prototypes
#include <limits.h>                                                             // for SHRT_MAX
#include <stdio.h>                                                              // always include
#include <string.h>                                                             // for memcpy/memcmp
#include <time.h>                                                               // for ctime

static locktab *luptr = NULL;                                                   // last used pointer
static short   lujob = 0;                                                       // last used job

// Used by failed
typedef struct LCK_ADD {
    int     count;
    int     to;
    int     done;
    int     tryagain;
    time_t  currtime;
    time_t  strttime;
    short   x;
    locktab *lptr;
} lck_add;

static int failed(lck_add *pctx)                                                // common code
{
    pctx->done = pctx->count + 1;                                               // begin again from scratch
    if (pctx->to == 0) pctx->tryagain = 0;                                      // if no timeout then flag as if timeout expired

    if (pctx->to > 0) {                                                         // if timeout value specified
        pctx->currtime = current_time(FALSE);                                   // get current time
        if ((pctx->strttime + pctx->to) < pctx->currtime) pctx->tryagain = 0;   // flag if time expired
    }                                                                           // end if timeout specified

    if (pctx->tryagain == 1) {
        pctx->x = SemOp(SEM_LOCK, -SEM_WRITE);                                  // unlock SEM_LOCK
        SchedYield(TRUE);                                                       // give up slice (or sleep)
    }

    if (pctx->tryagain == 0) partab.jobtab->test = 0;                           // flag failure to lock

    if (partab.jobtab->attention) {
        if (partab.jobtab->trap & (SIG_CC | SIG_QUIT | SIG_TERM | SIG_STOP)) {
            if (pctx->tryagain == 0) pctx->x = SemOp(SEM_LOCK, -SEM_WRITE);     // unlock SEM_LOCK
            return -(ERRZ51 + ERRMLAST);
        }
    }

    pctx->lptr = NULL;
    return 0;
}

/* Arguments:
 *   var: Address of the lock entry
 *   str: Location of the destination string
 */
short UTIL_String_Lock(locktab *var, u_char *str)
{
    int          i;                                                             // for loops
    int          p = 0;                                                         // string pointer
    int          slen;                                                          // subscript length
    const u_char *vp;                                                           // ptr to vol name

    if (var->uci != UCI_IS_LOCALVAR) {                                          // if it's a global var
        uci_tab up;                                                             // ptr to UCI tab

        str[p++] = '^';                                                         // lead off with the caret
        str[p++] = '[';                                                         // open bracket
        str[p++] = '"';                                                         // a leading quote
        up = SOA(partab.vol[var->vol - 1]->vollab)->uci[var->uci - 1];          // UCI pointer

        for (i = 0; i < VAR_LEN; i++) {                                         // for each possible character
            if (up.name.var_cu[i] == '\0') break;                               // done if we hit a null
            str[p++] = up.name.var_cu[i];                                       // copy the character
        }

        str[p++] = '"';                                                         // a trailing quote
        str[p++] = ',';                                                         // comma
        str[p++] = '"';                                                         // start quote for vol
        vp = SOA(partab.vol[var->vol - 1]->vollab)->volnam.var_cu;              // point at name

        for (i = 0; i < VAR_LEN; i++) {                                         // for each possible character
            if (vp[i] == '\0') break;                                           // done if we hit a null
            str[p++] = vp[i];                                                   // copy the character
        }

        str[p++] = '"';                                                         // a trailing quote
        str[p++] = ']';                                                         // closing bracket
    }                                                                           // end global specific stuff

    for (i = 0; i < VAR_LEN; i++) {                                             // now the name
        if (var->name.var_cu[i] == '\0') break;                                 // quit when done
        str[p++] = var->name.var_cu[i];                                         // copy a byte
    }

    slen = var->byte_count - sizeof(var_u) - (2 * sizeof(u_char));              // subs length

    if (slen != 0) {                                                            // if there are subscripts
        u_char save;

        save = var->name.var_cu[VAR_LEN - 1];                                   // save that value
        var->name.var_cu[VAR_LEN - 1] = (u_char) slen;                          // put len there for call
        i = UTIL_String_Key(&var->name.var_cu[VAR_LEN - 1], &str[p], MAX_NUM_SUBS); // do the subscripts
        var->name.var_cu[VAR_LEN - 1] = save;                                   // restore saved value
        if (i < 0) return (short) i;                                            // quit on error
        p += i;                                                                 // add to length
    }

    str[p] = '\0';                                                              // null terminate
    return (short) p;                                                           // return the length
}

short UTIL_mvartolock(mvar *var, u_char *buf)                                   // convert mvar to string
{
    short s;                                                                    // a handy short

    if (var->uci == UCI_IS_LOCALVAR) {                                          // if local
        if (var->volset) {                                                      // if index type
            var_u *vt;                                                          // var table pointer
            rbd *p;

            p = (rbd *) SOA(partab.jobtab->dostk[partab.jobtab->cur_do].routine);
            vt = (var_u *) (((u_char *) p) + p->var_tbl);                       // point at var table
            VAR_COPY((*((var_u *) &buf[2])), vt[var->volset - 1]);              // get the var name
        } else {                                                                // non index type
            VAR_COPY((*((var_u *) &buf[2])), var->name);                        // copy name
        }

        memmove(&buf[2 + sizeof(var_u)], &var->key[0], var->slen);              // copy key
        s = var->slen + sizeof(var_u) + 2;                                      // how big it is
        buf[0] = 0;                                                             // no vol set
        buf[1] = UCI_IS_LOCALVAR;                                               // setup as local
        buf[s] = '\0';                                                          // ensure null terminated
        return s;                                                               // return the length
    }                                                                           // end of local var code

    if (var->name.var_cu[0] == '$') {                                           // SSVN?
        s = SS_Norm(var);                                                       // normalize it
        if (s == -ERRM59) return s;                                             // quit on error
    }

    buf[0] = var->volset;                                                       // copy volset
    buf[1] = var->uci;                                                          // copy UCI
    VAR_COPY((*((var_u *) &buf[2])), var->name);                                // copy varname
    if (!buf[0]) buf[0] = partab.jobtab->lvol;                                  // if no volset, set default

    if (!buf[1]) {                                                              // if no UCI
        if (var->name.var_cu[0] == '%') {                                       // if % var
            buf[1] = 1;                                                         // manager UCI
        } else {
            buf[1] = partab.jobtab->luci;                                       // default
        }
    }

    if ((var->volset == 0) && (var->uci == 0)) {                                // no vol or UCI
        for (int i = 0; i < systab->max_tt; i++) {                              // scan trantab
            if ((buf[0] == systab->tt[i].from_vol) && (buf[1] == systab->tt[i].from_uci) &&
              (var_equal(var->name, systab->tt[i].from_global))) {
                buf[0] = systab->tt[i].to_vol;                                  // copy this
                buf[1] = systab->tt[i].to_uci;                                  // and this
                VAR_COPY((*((var_u *) &buf[2])), systab->tt[i].to_global);      // and this
                break;                                                          // done
            }
        }                                                                       // end found one
    }                                                                           // end trantab lookup

    memmove(&buf[sizeof(var_u) + 2], &var->key[0], var->slen);                  // copy key
    s = var->slen + sizeof(var_u) + 2;                                          // how big it is
    buf[s] = '\0';                                                              // ensure null terminated
    return s;                                                                   // return the length
}

static short LCK_Combine(locktab *ptr)
{
    locktab *next;                                                              // a handy pointer

    if (ptr == NULL) {
        panic("LCK_Combine: Null pointer passed");
    } else {
        while (TRUE) {
            next = (locktab *) (((u_char *) ptr) + ptr->size);                  // where next

            if (luptr == next) {
                luptr = NULL;                                                   // reset last used pointer
                lujob = 0;                                                      // reset last used job
            }

            if (((char *) next) >= (((char *) SOA(systab->lockstart)) + systab->locksize)) break; // quit when done
            if (next != SOA(ptr->fwd_link)) break;                              // quit if next not free
            if (next->job > -1) panic("LCK_Combine: Attempt to combine non-free locks");
            ptr->size += next->size;                                            // increment the new free space
            ptr->fwd_link = next->fwd_link;                                     // new block now points correct
        }                                                                       // prev block/this block merge
    }

    return 0;                                                                   // finished OK
}                                                                               // end function LCK_Combine()

// Note, ptr has been removed from the lockhead list when we get here
static short LCK_Free(locktab *ptr)
{
    locktab *currptr;
    locktab *prevptr = NULL;                                                    // handy pointer

    if (ptr == NULL) panic("LCK_Free: Null pointer passed");                    // die

    if (luptr == ptr) {
        luptr = NULL;                                                           // reset last used pointer
        lujob = 0;                                                              // reset last used job
    }

    currptr = SOA(systab->lockfree);                                            // start here

    if ((ptr < currptr) || (currptr == NULL)) {                                 // free at start or end of memory
        ptr->fwd_link = SBA(currptr);                                           // this block becomes new
        systab->lockfree = SBA(ptr);                                            // head of free list
        return LCK_Combine(SOA(systab->lockfree));                              // see if we can combine the 2
    }

    while ((currptr != NULL) && (ptr > currptr)) {                              // while more to look at
        prevptr = currptr;                                                      // save current to previous
        currptr = SOA(currptr->fwd_link);                                       // move on
    }

    ptr->fwd_link = SBA(currptr);                                               // point at next or NULL
    prevptr->fwd_link = SBA(ptr);                                               // link us in here
    return LCK_Combine(prevptr);                                                // see if we can combine the 2
}

// returns pointer to free space
static locktab *LCK_Insert(int size)                                            // go through share section for chunk of size
{
    locktab *free_curr = NULL;                                                  // locktab traverse pointer
    locktab *free_prev = NULL;                                                  // previous locktab pointer
    locktab *ptr = NULL;                                                        // pointer to free space
    locktab *p = NULL;                                                          // handy pointer
    locktab *prevptr = NULL;                                                    // another handy pointer
    int     ret;

    ret = systab->locksize + 1;                                                 // size comparator
    free_curr = SOA(systab->lockfree);                                          // start here

    while (free_curr != NULL) {                                                 // while more locktabs
        if ((free_curr->size < ret) && (free_curr->size >= size)) {             // if this space
            ptr = free_curr;                                                    // more appropriate
            prevptr = free_prev;                                                // save to previous
            ret = free_curr->size;                                              // than last space, use
        }                                                                       // this one instead

        free_prev = free_curr;                                                  // save previous pointer
        free_curr = SOA(free_curr->fwd_link);                                   // check next one
    }                                                                           // end while

    if ((ptr == NULL) || (ret == (systab->locksize + 1))) {                     // if neither has changed
        return NULL;                                                            // no space available
    } else {                                                                    // and try again if both changed
        // 52 is the smallest size a lock table entry can be currently, so don't leave anything smaller
        if ((size + 52) <= ptr->size) {                                         // if way too big
            p = (locktab *) ((u_char *) ptr + size);                            // setup for new block
            p->fwd_link = ptr->fwd_link;                                        // point it at same
            p->size = ptr->size - size;                                         // init the size
            p->job = -1;                                                        // mark as free
            p->lock_count = 0;                                                  // initialize
            p->byte_count = 0;                                                  // these two also

            if (prevptr == NULL) {
                systab->lockfree = SBA(p);                                      // new head of freelist
            } else {
                prevptr->fwd_link = SBA(p);                                     // else link as usual
            }

            ptr->size = size;                                                   // set this NOW
        } else {                                                                // size just right
            if (prevptr == NULL) {
                systab->lockfree = ptr->fwd_link;                               // link here
            } else {
                prevptr->fwd_link = ptr->fwd_link;                              // or here
            }
        }                                                                       // end else size OK

        return ptr;                                                             // tell them where
    }                                                                           // end else get some space

    return NULL;                                                                // finished NO SPACE - can't get here
}                                                                               // end LCK_Insert

short LCK_Order(const cstring *ent, u_char *buf, int dir)                       // get next/prev entry
{
    locktab *lptr;                                                              // locktab entry we are doing
    locktab *plptr = NULL;                                                      // previous locktab entry
    short   s;                                                                  // for functions
    short   x;                                                                  // for SEM's

    x = SemOp(SEM_LOCK, SEM_READ);                                              // read lock SEM_LOCK
    if (x < 0) return x;                                                        // return error
    lptr = SOA(systab->lockhead);                                               // get the list head

    if (luptr != NULL) {
        if (luptr->job != lujob) {                                              // another job removed the lock
            luptr = NULL;                                                       // reset last used pointer
            lujob = 0;                                                          // reset last used job
        } else if (lptr != NULL) {
            // start search at last used lock pointer, rather than at the beginning (ent after luptr)
            if (UTIL_Key_KeyCmp(ent->buf, &luptr->vol, ent->len, luptr->byte_count) == K2_LESSER) {
                lptr = luptr;
            }
        }
    }

    while (lptr != NULL) {                                                      // while more locktabs
        int i;                                                                  // for entry lengths

        i = UTIL_Key_KeyCmp(ent->buf, &lptr->vol, ent->len, lptr->byte_count);  // compare them
        if ((dir > 0) && (i == K2_GREATER)) break;                              // found in fwd direction

        // found in the back direction & ent is not ^$LOCK("")
        if ((dir < 0) && (i != K2_LESSER) && (memcmp(ent->buf, "\000\377\000", 3) != 0)) {
            lptr = plptr;                                                       // point at previous
            break;                                                              // and quit
        }                                                                       // end if found in back dir

        plptr = lptr;                                                           // remember that one
        lptr = SOA(lptr->fwd_link);                                             // get the next one
    }                                                                           // end while more locktabs

    if (plptr != NULL) {
        luptr = plptr;                                                          // set last used pointer
        lujob = luptr->job;                                                     // set last used job
    }

    if ((dir < 0) && (lptr == NULL)) lptr = plptr;                              // adjust for last on ,-1)
    buf[0] = '\0';                                                              // null terminate
    s = 0;                                                                      // return value
    if (lptr != NULL) s = UTIL_String_Lock(lptr, buf);                          // got something? then convert it
    SemOp(SEM_LOCK, -SEM_READ);                                                 // release SEM_LOCK
    return s;                                                                   // and return
}

short LCK_Get(const cstring *ent, u_char *buf)                                  // get job#,lock_count
{
    locktab *lptr;                                                              // locktab entry we are doing
    locktab *plptr;                                                             // previous locktab entry
    short   s = 0;                                                              // return value
    short   x;                                                                  // for SEM's

    buf[0] = '\0';                                                              // JIC
    x = SemOp(SEM_LOCK, SEM_READ);                                              // read lock SEM_LOCK
    if (x < 0) return x;                                                        // return the error
    lptr = SOA(systab->lockhead);                                               // init current locktab pointer
    plptr = NULL;                                                               // init previous pointer

    if (luptr != NULL) {
        if (luptr->job != lujob) {                                              // another job removed the lock
            luptr = NULL;                                                       // reset last used pointer
            lujob = 0;                                                          // reset last used job
        } else if (lptr != NULL) {
            // start search at last used lock pointer, rather than at the beginning (ent after luptr)
            if (UTIL_Key_KeyCmp(ent->buf, &luptr->vol, ent->len, luptr->byte_count) == K2_LESSER) {
                lptr = luptr;
            }
        }
    }

    while (lptr != NULL) {                                                      // while more lock tabs
        if (UTIL_Key_KeyCmp(ent->buf, &lptr->vol, ent->len, lptr->byte_count) == KEQUAL) { // is there a match ?
            s = ltocstring(buf, lptr->job);                                     // convert job to string
            buf[s++] = ',';                                                     // copy in comma
            s += ltocstring(&buf[s], lptr->lock_count);                         // convert count to str
            break;                                                              // found it
        }                                                                       // end if exact match

        plptr = lptr;                                                           // remember that one
        lptr = SOA(lptr->fwd_link);                                             // get next
    }                                                                           // end while more lock tabs

    if (plptr != NULL) {
        luptr = plptr;                                                          // set last used pointer
        lujob = luptr->job;                                                     // set last used job
    }

    SemOp(SEM_LOCK, -SEM_READ);                                                 // release SEM_LOCK
    return s;                                                                   // return the count
}                                                                               // end function LCK_Get()

short LCK_Kill(const cstring *ent)                                              // remove an entry
{
    locktab *lptr;                                                              // locktab entry we are doing
    locktab *plptr = NULL;                                                      // previous locktab entry

    lptr = SOA(systab->lockhead);                                               // init current locktab pointer

    if (luptr != NULL) {
        if (luptr->job != lujob) {                                          // another job removed the lock
            luptr = NULL;                                                   // reset last used pointer
            lujob = 0;                                                      // reset last used job
        } else if (lptr != NULL) {
            // start search at last used lock pointer, rather than at the beginning (ent after luptr)
            if (UTIL_Key_KeyCmp(ent->buf, &luptr->vol, ent->len, luptr->byte_count) == K2_LESSER) {
                lptr = luptr;
            }
        }
    }

    while (lptr != NULL) {                                                      // while more lock tabs
        if (UTIL_Key_KeyCmp(ent->buf, &lptr->vol, ent->len, lptr->byte_count) == KEQUAL) { // is there a match ?
            if (plptr == NULL) {                                                // killing head of lock tabs
                systab->lockhead = lptr->fwd_link;                              // bypass it
            } else {                                                            // killing mid list
                plptr->fwd_link = lptr->fwd_link;                               // bypass it
            }                                                                   // end killing mid list

            lptr->job = -1;                                                     // flag it as free
            LCK_Free(lptr);                                                     // add to the free list
            break;                                                              // finished OK
        }                                                                       // end if byte counts match

        plptr = lptr;                                                           // make current, previous
        lptr = SOA(lptr->fwd_link);                                             // check next locktab
    }                                                                           // end while more lock tabs

    if (plptr != NULL) {
        luptr = plptr;                                                          // set last used pointer
        lujob = luptr->job;                                                     // set last used job
    }

    return 0;                                                                   // finished OK
}                                                                               // end function LCK_Kill()

void LCK_Remove(int job)                                                        // remove all locks for a job
{
    locktab *lptr;                                                              // locktab entry we are doing
    locktab *plptr;                                                             // previous locktab entry
    short   x;                                                                  // for SEM's

    if (!job) job = partab.jobtab - partab.job_table + 1;                       // current job
    x = SemOp(SEM_LOCK, SEM_WRITE);                                             // write lock SEM_LOCK
    if (x < 0) return;                                                          // return on error
    lptr = SOA(systab->lockhead);                                               // init current locktab pointer
    plptr = NULL;                                                               // init prev locktab pointer

    while (lptr != NULL) {                                                      // while more lock tabs
        if (lptr->job == job) {                                                 // if we own it
            if (plptr == NULL) {                                                // remove top node
                systab->lockhead = lptr->fwd_link;                              // link in new head lock node
                lptr->job = -1;                                                 // flag it as free
                LCK_Free(lptr);                                                 // add to the free list
                lptr = SOA(systab->lockhead);                                   // point at next
                plptr = NULL;                                                   // prev ptr still NULL
            }                                                                   // end if removing top node

            if ((plptr != NULL) && (lptr != NULL)) {                            // if both pointers defined
                plptr->fwd_link = lptr->fwd_link;                               // bypass it
                lptr->job = -1;                                                 // flag it as free
                LCK_Free(lptr);                                                 // add to the free list
                lptr = SOA(plptr->fwd_link);                                    // point at next
            }                                                                   // end if both pointers defined
        } else {                                                                // byte counts don't match
            plptr = lptr;                                                       // make current, previous
            lptr = SOA(lptr->fwd_link);                                         // check next locktab
        }                                                                       // end else job numbers !=
    }                                                                           // end while more lock tabs

    SemOp(SEM_LOCK, -SEM_WRITE);                                                // unlock SEM_LOCK
    return;                                                                     // return
}                                                                               // end function LCK_Remove()

short LCK_Old(int count, cstring *list, int to)                                 // old style lock
{
    LCK_Remove(0);                                                              // remove all locks for job

    if (partab.jobtab->trap & (SIG_CC | SIG_QUIT | SIG_TERM | SIG_STOP)) {      // quit
        return -(ERRZ51 + ERRMLAST);
    }

    if (count < 1) return 0;                                                    // just return if none
    return LCK_Add(count, list, to);                                            // add all locks in list
}                                                                               // end function LCK_Old()

short LCK_Add(int p_count, cstring *list, int p_to)                             // lock plus
{
    cstring *current;                                                           // temp cstring
    cstring *tempc;                                                             // temp cstring
    int     removedone;                                                         // flag
    u_int   posr;                                                               // position in *list
    int     toremove;                                                           // number of vars to remove
    locktab *plptr;                                                             // previous locktab
    locktab *nlptr;                                                             // new locktab
    locktab *slptr;                                                             // save locktab for tests
    int     ret;                                                                // for function returns
    int     reqd;                                                               // space required for lock
    lck_add ctx, *pctx;

    pctx = &ctx;
    pctx->count = p_count;
    pctx->to = p_to;
    pctx->done = 0;
    pctx->tryagain = 1;
    pctx->strttime = current_time(FALSE);                                       // save op start time

    while (pctx->tryagain) {                                                    // while we should give it a go
        u_int size;                                                             // size of entry count
        int   pos = 0;                                                          // position indicator

        pctx->tryagain = 0;                                                     // reset retry flag
        if (pctx->to > -1) partab.jobtab->test = 1;                             // flag successful locking
        pctx->done = 0;                                                         // init
        pctx->x = SemOp(SEM_LOCK, SEM_WRITE);                                   // write lock SEM_LOCK
        if (pctx->x < 0) return pctx->x;                                        // return the error

        while ((pctx->done < pctx->count) && (pctx->tryagain == 0)) {           // while more to do
            current = (cstring *) &((u_char *) list)[pos];                      // extract this entry
            reqd = sizeof(short) * 3 + sizeof(int) + sizeof(locktab *) + current->len;
            pctx->lptr = SOA(systab->lockhead);                                 // start at first locktab
            plptr = NULL;                                                       // init previous pointer

            if (pctx->lptr == NULL) {                                           // add first lock
                SOA(systab->lockfree)->fwd_link = NULL;                         // make sure JIC
                nlptr = LCK_Insert(reqd);                                       // try and get some space

                if (nlptr == NULL) {
                    pctx->tryagain = 1;                                         // set retry flag
                    toremove = pctx->done;                                      // save this value
                    removedone = 0;                                             // init this counter
                    posr = 0;                                                   // init position indicator

                    while (removedone < toremove) {                             // while more removals to do
                        LCK_Kill((cstring *) &((u_char *) list)[posr]);         // remove entry at posr
                        tempc = (cstring *) &((u_char *) list)[posr];           // extract this entry
                        size = sizeof(u_short) + tempc->len;                    // find size
                        if (size & 1) size++;                                   // ensure even
                        posr += size;                                           // posr to start of next entry
                        removedone++;                                           // increment removal counter
                    }                                                           // end while when no more to do

                    if ((ret = failed(pctx))) return ret;
                } else {
                    nlptr->job = partab.jobtab - partab.job_table + 1;          // init job number
                    nlptr->lock_count = 1;                                      // init lock count
                    nlptr->byte_count = current->len;                           // init data length
                    memcpy(&nlptr->vol, current->buf, current->len);            // copy in data
                    nlptr->fwd_link = systab->lockhead;                         // link it in sorted order
                    systab->lockhead = SBA(nlptr);                              // link it in sorted order
                }
            }                                                                   // end if first lock

            if (luptr != NULL) {
                if (luptr->job != lujob) {                                      // another job removed the lock
                    luptr = NULL;                                               // reset last used pointer
                    lujob = 0;                                                  // reset last used job
                } else if (pctx->lptr != NULL) {
                    // start search at last used lock pointer, rather than at the beginning (current after luptr)
                    if (UTIL_Key_KeyCmp(current->buf, &luptr->vol, current->len, luptr->byte_count) == K2_LESSER) {
                        pctx->lptr = luptr;
                    }
                }
            }

            while (pctx->lptr != NULL) {                                        // while more locktabs to see
                // more to see until found or past
                while ((pctx->lptr != NULL) &&
                  (UTIL_Key_Cmp(current->buf, &pctx->lptr->vol, current->len, pctx->lptr->byte_count) == K2_LESSER)) {
                    plptr = pctx->lptr;                                         // save current to previous
                    pctx->lptr = SOA(pctx->lptr->fwd_link);                     // get next locktab
                    if (pctx->lptr == NULL) break;                              // run out of locktabs
                }                                                               // end while more/found/past

                // exists as sub/exact/superset
                if ((pctx->lptr != NULL) &&
                  (UTIL_Key_Cmp(current->buf, &pctx->lptr->vol, current->len, pctx->lptr->byte_count) == KEQUAL)) {
                    if (pctx->lptr->job == (partab.jobtab - partab.job_table + 1)) { // we MUST own
                        int i = 0;

                        while ((pctx->lptr != NULL) &&                          // more to look at
                          (UTIL_Key_KeyCmp(current->buf, &pctx->lptr->vol, current->len, pctx->lptr->byte_count) == K2_LESSER)) {
                            plptr = pctx->lptr;
                            pctx->lptr = SOA(pctx->lptr->fwd_link);             // have a look at next one
                            if (pctx->lptr == NULL) break;                      // run out of locktabs
                        }                                                       // end while conditions true

                        slptr = pctx->lptr;
                        i = FALSE;

                        while ((slptr != NULL) &&                               // more to look at
                          (UTIL_Key_Cmp(current->buf, &slptr->vol, current->len, slptr->byte_count) == KEQUAL)) {
                            if (slptr->job != (partab.jobtab - partab.job_table + 1)) { // we MUST own
                                i = TRUE;                                       // set length to flag other job owns
                                break;
                            }

                            slptr = SOA(slptr->fwd_link);                       // have a look at next one
                            if (slptr == NULL) break;                           // run out of locktabs
                        }                                                       // end while conditions true

                        if (i == TRUE) {                                        // another job owns child
                            pctx->tryagain = 1;                                 // set retry flag
                            toremove = pctx->done;                              // save this value
                            removedone = 0;                                     // init this counter
                            posr = 0;                                           // init position indicator

                            while (removedone < toremove) {                     // while more removals to do
                                LCK_Kill((cstring *) &((u_char *) list)[posr]); // remove entry at posr
                                tempc = (cstring *) &((u_char *) list)[posr];   // extract this entry
                                size = sizeof(u_short) + tempc->len;            // find size
                                if (size & 1) size++;                           // ensure even
                                posr += size;                                   // posr to start of next entry
                                removedone++;                                   // increment removal counter
                            }                                                   // end while when no more to do

                            if ((ret = failed(pctx))) return ret;
                        } else {
                            if (pctx->lptr != NULL) {                           // if pctx->lptr defined
                                // if data matches
                                if (UTIL_Key_Cmp(current->buf, &pctx->lptr->vol, current->len, pctx->lptr->byte_count) == KEQUAL) {
                                    if (pctx->lptr->byte_count == current->len) { // if exact length match
                                        if ((pctx->lptr->lock_count + 1) > SHRT_MAX) {
                                            SemOp(SEM_LOCK, -SEM_WRITE);        // unlock SEM_LOCK
                                            return -(ERRZ78 + ERRMLAST);
                                        }

                                        pctx->lptr->lock_count++;               // increment the lock count
                                        pctx->lptr = NULL;                      // NULLIFY the ptr, finished
                                    } else {                                    // gone past it
                                        nlptr = LCK_Insert(reqd);               // get some space

                                        if (nlptr == NULL) {
                                            pctx->tryagain = 1;                 // set retry flag
                                            toremove = pctx->done;              // save this value
                                            removedone = 0;                     // init this counter
                                            posr = 0;                           // init position indicator

                                            while (removedone < toremove) {     // while more removals to do
                                                LCK_Kill((cstring *) &((u_char *) list)[posr]); // remove entry
                                                tempc = (cstring *) &((u_char *) list)[posr]; // extract
                                                size = sizeof(u_short) + tempc->len; // size
                                                if (size & 1) size++;           // ensure even
                                                posr += size;                   // posr to start of next entry
                                                removedone++;                   // increment removal counter
                                            }                                   // end while when no more to do

                                            if ((ret = failed(pctx))) return ret;
                                        } else {
                                            nlptr->job = partab.jobtab - partab.job_table + 1; // job#
                                            nlptr->lock_count = 1;              // init lock count
                                            nlptr->byte_count = current->len;   // length of data
                                            memcpy(&nlptr->vol, current->buf, current->len); // copy data

                                            if (plptr != NULL) {                // not inserting at list head
                                                plptr->fwd_link = SBA(nlptr);   // link it in this way
                                            } else {                            // inserting at list head
                                                systab->lockhead = SBA(nlptr);  // so link it in this way
                                            }

                                            nlptr->fwd_link = SBA(pctx->lptr);  // link it in
                                            pctx->lptr = NULL;                  // NULLIFY ptr, finished entry
                                        }
                                    }                                           // end else gone past it
                                } else {                                        // UTIL_Key_Cmp no longer == KEQUAL
                                    nlptr = LCK_Insert(reqd);                   // get some space

                                    if (nlptr == NULL) {
                                        pctx->tryagain = 1;                     // set retry flag
                                        toremove = pctx->done;                  // save this value
                                        removedone = 0;                         // init this counter
                                        posr = 0;                               // init position indicator

                                        while (removedone < toremove) {         // while more removals to do
                                            LCK_Kill((cstring *) &((u_char *) list)[posr]); // remove entry at posr
                                            tempc = (cstring *) &((u_char *) list)[posr]; // extract this entry
                                            size = sizeof(u_short) + tempc->len; // find size
                                            if (size & 1) size++;               // ensure even
                                            posr += size;                       // posr to start of next entry
                                            removedone++;                       // increment removal counter
                                        }                                       // end while when no more to do

                                        if ((ret = failed(pctx))) return ret;
                                    } else {
                                        nlptr->job = partab.jobtab - partab.job_table + 1; // init job
                                        nlptr->lock_count = 1;                  // init lock count
                                        nlptr->byte_count = current->len;       // length of data
                                        memcpy(&nlptr->vol, current->buf, current->len); // copy data
                                        plptr->fwd_link = SBA(nlptr);           // link it in
                                        nlptr->fwd_link = SBA(pctx->lptr);      // link it in
                                        pctx->lptr = NULL;                      // NULLIFY pointer, finished entry
                                    }
                                }                                               // end else UTIL_Key_Cmp no longer KEQUAL
                            } else {                                            // end if pctx->lptr defined
                                nlptr = LCK_Insert(reqd);                       // get some space

                                if (nlptr == NULL) {
                                    pctx->tryagain = 1;                         // set retry flag
                                    toremove = pctx->done;                      // save this value
                                    removedone = 0;                             // init this counter
                                    posr = 0;                                   // init position indicator

                                    while (removedone < toremove) {             // while more removals to do
                                        LCK_Kill((cstring *) &((u_char *) list)[posr]); // remove entry at posr
                                        tempc = (cstring *) &((u_char *) list)[posr]; // extract this entry
                                        size = sizeof(u_short) + tempc->len;    // find size
                                        if (size & 1) size++;                   // ensure even
                                        posr += size;                           // posr to start of next entry
                                        removedone++;                           // increment removal counter
                                    }                                           // end while when no more to do

                                    if ((ret = failed(pctx))) return ret;
                                } else {
                                    nlptr->job = partab.jobtab - partab.job_table + 1; // job#
                                    nlptr->lock_count = 1;                      // init lock count
                                    nlptr->byte_count = current->len;           // length of data
                                    memcpy(&nlptr->vol, current->buf, current->len); // copy data
                                    plptr->fwd_link = SBA(nlptr);               // link it in
                                    nlptr->fwd_link = SBA(pctx->lptr);          // link it in
                                    pctx->lptr = NULL;                          // NULLIFY ptr, finished entry
                                }
                            }                                                   // end else, pctx->lptr not defined
                        }
                    } else {                                                    // we don't own it - remove all locks thus far
                        pctx->tryagain = 1;                                     // set retry flag
                        toremove = pctx->done;                                  // save this value
                        removedone = 0;                                         // init this counter
                        posr = 0;                                               // init position indicator

                        while (removedone < toremove) {                         // while more removals to do
                            LCK_Kill((cstring *) &((u_char *) list)[posr]);     // remove entry at posr
                            tempc = (cstring *) &((u_char *) list)[posr];       // extract this entry
                            size = sizeof(u_short) + tempc->len;                // find size
                            if (size & 1) size++;                               // ensure even
                            posr += size;                                       // posr to start of next entry
                            removedone++;                                       // increment removal counter
                        }                                                       // end while when no more to do

                        if ((ret = failed(pctx))) return ret;
                    }                                                           // end else we don't own it
                } else {                                                        // end if exists in super/sub - doesn't exist
                    if ((pctx->lptr != NULL) &&
                      (UTIL_Key_Cmp(current->buf, &pctx->lptr->vol, current->len, pctx->lptr->byte_count) == K2_GREATER)) {
                        nlptr = LCK_Insert(reqd);                               // try get some space

                        if (nlptr == NULL) {
                            pctx->tryagain = 1;                                 // set retry flag
                            toremove = pctx->done;                              // save this value
                            removedone = 0;                                     // init this counter
                            posr = 0;                                           // init position indicator

                            while (removedone < toremove) {                     // while more removals to do
                                LCK_Kill((cstring *) &((u_char *) list)[posr]); // remove entry at posr
                                tempc = (cstring *) &((u_char *) list)[posr];   // extract this entry
                                size = sizeof(u_short) + tempc->len;            // find size
                                if (size & 1) size++;                           // ensure even
                                posr += size;                                   // posr to start of next entry
                                removedone++;                                   // increment removal counter
                            }                                                   // end while when no more to do

                            if ((ret = failed(pctx))) return ret;
                        } else {
                            nlptr->job = partab.jobtab - partab.job_table + 1;  // init job#
                            nlptr->lock_count = 1;                              // init lock count
                            nlptr->byte_count = current->len;                   // length of data
                            memcpy(&nlptr->vol, current->buf, current->len);    // copy the data
                            nlptr->fwd_link = SBA(pctx->lptr);                  // link it in
                            pctx->lptr = NULL;                                  // NULLIFY ptr, finished entry

                            if (plptr == NULL) {                                // insert as new top node
                                systab->lockhead = SBA(nlptr);                  // link this way
                            } else {                                            // end if insert as new top node - insert mid list
                                plptr->fwd_link = SBA(nlptr);                   // or link this way
                            }                                                   // end else insert mid list
                        }
                    } else {                                                    // end if match result greater - add to end of list
                        if (pctx->lptr == NULL) {                               // run out of locktabs
                            nlptr = LCK_Insert(reqd);                           // get some space

                            if (nlptr == NULL) {
                                pctx->tryagain = 1;                             // set retry flag
                                toremove = pctx->done;                          // save this value
                                removedone = 0;                                 // init this counter
                                posr = 0;                                       // init position indicator

                                while (removedone < toremove) {                 // while more removals to do
                                    LCK_Kill((cstring *) &((u_char *) list)[posr]); // remove entry at posr
                                    tempc = (cstring *) &((u_char *) list)[posr]; // extract this entry
                                    size = sizeof(u_short) + tempc->len;        // find size
                                    if (size & 1) size++;                       // ensure even
                                    posr += size;                               // posr to start of next entry
                                    removedone++;                               // increment removal counter
                                }                                               // end while when no more to do

                                if ((ret = failed(pctx))) return ret;
                            } else {
                                nlptr->job = partab.jobtab - partab.job_table + 1; // init job
                                nlptr->lock_count = 1;                          // init lock count
                                nlptr->byte_count = current->len;               // length of data
                                memcpy(&nlptr->vol, current->buf, current->len); // copy data
                                nlptr->fwd_link = SBA(pctx->lptr);              // link it in
                                pctx->lptr = NULL;                              // NULLIFY ptr, finished entry

                                if (plptr == NULL) {                            // insert as new top node
                                    systab->lockhead = SBA(nlptr);              // link it in this way
                                } else {                                        // end if new top node - insert mid list
                                    plptr->fwd_link = SBA(nlptr);               // or link it in this way
                                }                                               // end else insert mid list
                            }
                        }                                                       // end if run out of locktabs
                    }                                                           // end else add to end of list
                }                                                               // end else not already in
            }                                                                   // end while pctx->lptr not null

            if (pctx->tryagain == 0) {
                size = sizeof(u_short) + current->len;                          // calculate length of entry
                if (size & 1) size++;                                           // pad to even boundary
                pos += size;                                                    // find next start pos
                pctx->done++;                                                   // number done + 1

                if (plptr != NULL) {
                    luptr = plptr;                                              // set last used pointer
                    lujob = luptr->job;                                         // set last used job
                }
            }
        }                                                                       // end while more to do
    }                                                                           // end while try again

    SemOp(SEM_LOCK, -SEM_WRITE);                                                // unlock SEM_LOCK
    return 0;                                                                   // finished OK
}                                                                               // end function LCK_Add()

short LCK_Sub(int count, cstring *list)                                         // lock minus
{
    u_int   pos = 0;                                                            // position in *list
    int     done = 0;                                                           // number of entries completed
    locktab *lptr;                                                              // locktab pointer
    locktab *plptr = NULL;                                                      // previous locktab entry
    short   x;                                                                  // for SEM's

    x = SemOp(SEM_LOCK, SEM_WRITE);                                             // write lock SEM_LOCK
    if (x < 0) return x;                                                        // return the error

    while (done < count) {                                                      // while more to do
        const cstring *current;                                                 // temp cstring
        u_int         size;                                                     // size of entry in *list

        current = (cstring *) &((u_char *) list)[pos];                          // extract this entry
        lptr = SOA(systab->lockhead);                                           // start at first locktab

        if (luptr != NULL) {
            if (luptr->job != lujob) {                                          // another job removed the lock
                luptr = NULL;                                                   // reset last used pointer
                lujob = 0;                                                      // reset last used job
            } else if (lptr != NULL) {
                // start search at last used lock pointer, rather than at the beginning (current after luptr)
                if (UTIL_Key_KeyCmp(current->buf, &luptr->vol, current->len, luptr->byte_count) == K2_LESSER) {
                    lptr = luptr;
                }
            }
        }

        while (lptr != NULL) {                                                  // while more locktabs to see
            // while more locktabs to go until found or past
            while ((lptr != NULL) && (UTIL_Key_Cmp(current->buf, &lptr->vol, current->len, lptr->byte_count) == K2_LESSER)) {
                plptr = lptr;                                                   // remember that one
                lptr = SOA(lptr->fwd_link);                                     // get next locktab
            }                                                                   // end while not found/past

            // in subset/exact/superset form
            if ((lptr != NULL) && (UTIL_Key_Cmp(current->buf, &lptr->vol, current->len, lptr->byte_count) == KEQUAL)) {
                if (lptr->job == (partab.jobtab - partab.job_table + 1)) {      // we MUST own
                    // more to see - first bit match and length is smaller
                    while ((lptr != NULL) &&
                      (UTIL_Key_KeyCmp(current->buf, &lptr->vol, current->len, lptr->byte_count) == K2_LESSER)) {
                        plptr = lptr;                                           // remember that one
                        lptr = SOA(lptr->fwd_link);                             // look at next one
                    }                                                           // end while

                    if (lptr != NULL) {                                         // if lptr still defined
                        if (UTIL_Key_KeyCmp(current->buf, &lptr->vol, current->len, lptr->byte_count) == KEQUAL) { // exact match
                            lptr->lock_count--;                                 // decrement the lockcount
                            if (lptr->lock_count <= 0) LCK_Kill(current);       // if becomes un-wanted, kill it
                            lptr = NULL;                                        // done with this one
                        }
                    }                                                           // end if still point at lock
                }
            }

            if (lptr != NULL) {
                plptr = lptr;                                                   // remember that one
                lptr = SOA(lptr->fwd_link);                                     // NULLIFY ptr, finished entry
            }
        }                                                                       // end while more locktabs

        if (plptr != NULL) {
            luptr = plptr;                                                      // set last used pointer
            lujob = luptr->job;                                                 // set last used job
        }

        size = sizeof(u_short) + current->len;                                  // calculate length of entry
        if (size & 1) size++;                                                   // pad to even boundary
        pos += size;                                                            // find next start pos
        done++;                                                                 // number done + 1
    }                                                                           // successful

    SemOp(SEM_LOCK, -SEM_WRITE);                                                // unlock SEM_LOCK
    return 0;                                                                   // finished OK
}                                                                               // end function LCK_Sub()

// The following are internal only (first called from $&DEBUG())
void Dump_ltd(void)
{
    locktab *lptr;                                                              // locktab pointer
    locktab *used;                                                              // loop through lock used space
    int     size = 0;                                                           // actual size of used lock space
    short   x;
    u_char  keystr[MAX_KEY_SIZE + 5];
    u_char  workstr[MAX_KEY_SIZE + 5];
    time_t  t;                                                                  // for time

    x = SemOp(SEM_LOCK, SEM_WRITE);                                             // write lock SEM_LOCK
    if (x < 0) return;                                                          // return the error
    lptr = (locktab *) SOA(systab->lockstart);
    t = current_time(FALSE);
    printf("Dump of all Lock Table Descriptors on %s\r\n", ctime(&t));
    printf("Lock Head starts at %p\r\n", (void *) SOA(systab->lockhead));
    printf("Lock Free starts at %p\r\n", (void *) SOA(systab->lockfree));
    used = SOA(systab->lockhead);

    while (used != NULL) {
        size += used->size;
        used = SOA(used->fwd_link);
    }

    printf("Using %d of %d bytes of Lock Table Space\r\n\r\n", size, systab->locksize);
    printf("  Lock Pointer    Forward Link      Size   Job  Lock Cnt  Byte Cnt  VOL  UCI  Variable(Key)\r\n");

    while (lptr != NULL) {
        keystr[0] = '\0';

        if (lptr->byte_count > (VAR_LEN + 2)) {
            workstr[0] = lptr->byte_count - (VAR_LEN + 2);
            memcpy(&workstr[1], lptr->key, workstr[0]);
            x = UTIL_String_Key(workstr, keystr, MAX_NUM_SUBS);
            if (x < 0) snprintf((char *) keystr, MAX_KEY_SIZE + 5, " ERROR: %d", x);
        }

        if (lptr->job == -1) {                                                  // only display full stats for real locks
            printf("%14p %15p %9d %5d %9d %9d %4d %4d  %.32s%s\r\n",
                   (void *) lptr, (void *) SOA(lptr->fwd_link), lptr->size, lptr->job, 0, 0, 0, 0, "", "");
        } else {
            printf("%14p %15p %9d %5d %9d %9d %4d %4d  %.32s%s\r\n",
                   (void *) lptr, (void *) SOA(lptr->fwd_link), lptr->size, lptr->job, lptr->lock_count,
                   lptr->byte_count, lptr->vol, lptr->uci, lptr->name.var_cu, keystr);
        }

        lptr = (locktab *) (((u_char *) lptr) + lptr->size);

        if ((u_char *) lptr >= ((u_char *) SOA(systab->lockstart)) + systab->locksize) {
            break;
        }
    }

    SemOp(SEM_LOCK, -SEM_WRITE);                                                // unlock SEM_LOCK
    return;                                                                     // finished OK
}
