/*
 * Package:  Reference Standard M
 * File:     rsm/util/lock.c
 * Summary:  module database - lock utilities
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
#include <string.h>                                                             // for memcpy/memcmp
#include <sys/types.h>                                                          // for u_char def
#include "rsm.h"                                                                // standard includes
#include "proto.h"                                                              // standard prototypes
#include "error.h"                                                              // errors
#include "compile.h"                                                            // for RBD definition
#include <unistd.h>                                                             // for sleep
#include <sys/ipc.h>                                                            // semaphore stuff
#include <sys/sem.h>                                                            // semaphore stuff

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
        pctx->currtime = current_time(TRUE);                                    // get current time
        if ((pctx->strttime + pctx->to) < pctx->currtime) pctx->tryagain = 0;   // flag if time expired
    }                                                                           // end if timeout specified

    if (pctx->tryagain == 1) {
        pctx->x = SemOp(SEM_LOCK, systab->maxjob);                              // unlock SEM_LOCK
        sleep(1);
    }

    if (pctx->tryagain == 0) partab.jobtab->test = 0;                           // flag failure to lock

    if (partab.jobtab->attention) {
        if (partab.jobtab->trap & (SIG_CC | SIG_QUIT | SIG_TERM | SIG_STOP)) {
            if (pctx->tryagain == 0) pctx->x = SemOp(SEM_LOCK, systab->maxjob); // unlock SEM_LOCK
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
    int     i;                                                                  // for loops
    int     p = 0;                                                              // string pointer
    int     slen;                                                               // subscript length
    u_char *vp;                                                                 // ptr to vol name

    if (var->uci != UCI_IS_LOCALVAR) {                                          // if it's a global var
        uci_tab up;                                                             // ptr to UCI tab

        str[p++] = '^';                                                         // lead off with the caret
        str[p++] = '[';                                                         // open bracket
        str[p++] = '"';                                                         // a leading quote
        up = systab->vol[var->vol - 1]->vollab->uci[var->uci - 1];              // UCI pointer

        for (i = 0; i < VAR_LEN; i++) {                                         // for each possible character
            if (up.name.var_cu[i] == '\0') break;                               // done if we hit a null
            str[p++] = up.name.var_cu[i];                                       // copy the character
        }

        str[p++] = '"';                                                         // a trailing quote
        str[p++] = ',';                                                         // comma
        str[p++] = '"';                                                         // start quote for vol
        vp = systab->vol[var->vol - 1]->vollab->volnam.var_cu;                  // point at name

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

    slen = var->byte_count - sizeof(var_u) - (2 * sizeof(u_char));              // subs len

    if (slen != 0) {                                                            // if there are subscripts
        u_char save = var->name.var_cu[VAR_LEN - 1];                            // save that value

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

            rbd *p = (rbd *) partab.jobtab->dostk[partab.jobtab->cur_do].routine;

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
        if (s < 0) return s;                                                    // quit on error
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

short LCK_Combine(locktab *ptr)
{
    locktab *next;                                                              // a handy pointer

    if (ptr == NULL) {
        panic("Null pointer passed to LCK_Combine()");
    } else {
        while (TRUE) {
            next = (locktab *) (((u_char *) ptr) + ptr->size);                  // where next
            if (((char *) next) >= (((char *) systab->lockstart) + systab->locksize)) break; // quit when done
            if (next != ptr->fwd_link) break;                                   // quit if next not free
            if (next->job > -1) panic("Attempt to combine non-free in LCK_Combine()");
            ptr->size += ptr->fwd_link->size;                                   // increment the new free space
            ptr->fwd_link = next->fwd_link;                                     // new block now points correct
        }                                                                       // prev block/this block merge
    }

    return 0;                                                                   // finished OK
}                                                                               // end function LCK_Combine()

// Note, ptr has been removed from the lockhead list when we get here
short LCK_Free(locktab *ptr)
{
    locktab *currptr;
    locktab *prevptr = NULL;                                                    // handy ptr

    if (ptr == NULL) panic("Null pointer passed to LCK_Free()");                // die
    currptr = systab->lockfree;                                                 // start here

    if ((ptr < currptr) || (currptr == NULL)) {                                 // free at start or end of memory
        ptr->fwd_link = currptr;                                                // this block becomes new
        systab->lockfree = ptr;                                                 // head of free list
        return LCK_Combine(systab->lockfree);                                   // see if we can combine the 2
    }

    while ((currptr != NULL) && (ptr > currptr)) {                              // while more to look at
        prevptr = currptr;                                                      // save current to prev
        currptr = currptr->fwd_link;                                            // move on
    }

    ptr->fwd_link = currptr;                                                    // point at next or NULL
    prevptr->fwd_link = ptr;                                                    // link us in here
    return LCK_Combine(prevptr);                                                // see if we can combine the 2
}

// returns ptr to free space
locktab *LCK_Insert(int size)                                                   // go through share section for chunk of size
{
    locktab *free_curr = NULL;                                                  // locktab traverse ptr
    locktab *free_prev = NULL;                                                  // prev locktab ptr
    locktab *ptr = NULL;                                                        // pointer to free spc
    locktab *p = NULL;                                                          // handy ptr
    locktab *prevptr = NULL;                                                    // another handy ptr
    int     ret = systab->locksize + 1;                                         // size comparator

    free_curr = systab->lockfree;                                               // start here

    while (free_curr != NULL) {                                                 // while more locktabs
        if ((free_curr->size < ret) && (free_curr->size > size)) {              // if this space
            ptr = free_curr;                                                    // more appropriate
            prevptr = free_prev;                                                // save to previous
            ret = free_curr->size;                                              // than last space, use
        }                                                                       // this one instead

        free_prev = free_curr;                                                  // save prev ptr
        free_curr = free_curr->fwd_link;                                        // check next one
    }                                                                           // end while

    if ((ptr == NULL) || (ret == (systab->locksize + 1))) {                     // if neither has chgd
        return NULL;                                                            // no space available
    } else {                                                                    // and try again if both changed
        // the 64 on the next line will be # defined shortly
        if ((size + 64) < ptr->size) {                                          // if way too big
            p = (locktab *) ((u_char *) ptr + size);                            // setup for new block
            p->fwd_link = ptr->fwd_link;                                        // point it at same
            p->size = ptr->size - size;                                         // init the size
            p->job = -1;                                                        // mark as free
            p->lock_count = 0;                                                  // initialize
            p->byte_count = 0;                                                  // these two also

            if (prevptr == NULL) {
                systab->lockfree = p;                                           // new head of freelist
            } else {
                prevptr->fwd_link = p;                                          // else link as usual
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

    return NULL;                                                                // finished NO SPACE
}                                                                               // end LCK_Insert

short LCK_Order(cstring *ent, u_char *buf, int dir)                             // get next/prev entry
{
    locktab *lptr;                                                              // locktab entry we are doing
    locktab *plptr;                                                             // previous locktab entry
    short   s;                                                                  // for functions
    short   x;                                                                  // for SEM's

    x = SemOp(SEM_LOCK, -1);                                                    // read lock SEM_LOCK
    if (x < 0) return x;                                                        // return error
    lptr = systab->lockhead;                                                    // get the list head
    plptr = NULL;                                                               // init previous ptr

    while (lptr != NULL) {                                                      // while more locktabs
        int i = ent->len;                                                       // length of entry

        if (i > lptr->byte_count) i = lptr->byte_count;                         // but if locktab is less
        i = memcmp(ent->buf, &lptr->vol, i);                                    // compare them

        if ((i == 0) && (lptr->byte_count != ent->len)) {                       // if first part the same
            i = 1;                                                              // assume passed in greater
            if (lptr->byte_count > ent->len) i = -1;                            // no - other way
        }                                                                       // end if first part the same

        if ((dir > 0) && (i < 0)) break;                                        // found in fwd direction

        if ((dir < 0) && (i <= 0)) {                                            // found in the back direction
            lptr = plptr;                                                       // point at previous
            break;                                                              // and quit
        }                                                                       // end if found in back dir

        plptr = lptr;                                                           // remember that one
        lptr = lptr->fwd_link;                                                  // get the next one
    }                                                                           // end while more locktabs

    if ((dir < 0) && (lptr == NULL)) lptr = plptr;                              // adjust for last on ,-1)
    buf[0] = '\0';                                                              // null terminate
    s = 0;                                                                      // return value
    if (lptr != NULL) s = UTIL_String_Lock(lptr, buf);                          // got something? then convert it
    SemOp(SEM_LOCK, 1);                                                         // release SEM_LOCK
    return s;                                                                   // and return
}

short LCK_Get(cstring *ent, u_char *buf)                                        // get job#,lock_count
{
    locktab *lptr;                                                              // locktab entry we are doing
    int     i;                                                                  // a handy int
    short   s = 0;                                                              // return value
    short   x;                                                                  // for SEM's

    buf[0] = '\0';                                                              // JIC
    x = SemOp(SEM_LOCK, -1);                                                    // read lock SEM_LOCK
    if (x < 0) return x;                                                        // return the error
    lptr = systab->lockhead;                                                    // init current locktab pointer
    i = ent->len;                                                               // get length of supplied entry

    while (lptr != NULL) {                                                      // while more lock tabs
        if ((i == lptr->byte_count) && (memcmp(ent->buf, &lptr->vol, i) == 0)) { // if bytes counts match, is there a match ?
            s = itocstring(buf, lptr->job);                                     // convert job to str
            buf[s++] = ',';                                                     // copy in comma
            s += itocstring(&buf[s], lptr->lock_count);                         // convert count to str
            break;                                                              // found it
        }                                                                       // end if exact match

        lptr = lptr->fwd_link;                                                  // get next
    }                                                                           // end while more lock tabs

    SemOp(SEM_LOCK, 1);                                                         // release SEM_LOCK
    return s;                                                                   // return the count
}                                                                               // end function LCK_Get()

short LCK_Kill(cstring *ent)                                                    // remove an entry
{
    locktab *lptr;                                                              // locktab entry we are doing
    locktab *plptr;                                                             // previous locktab entry
    int     i;                                                                  // a handy int

    lptr = systab->lockhead;                                                    // init current locktab pointer
    plptr = NULL;                                                               // init prev locktab pointer
    i = ent->len;                                                               // get length of supplied entry

    while (lptr != NULL) {                                                      // while more lock tabs
        if (i == lptr->byte_count) {                                            // if bytes counts match
            if (memcmp(ent->buf, &lptr->vol, i) == 0) {                         // is there a match ?
                if (plptr == NULL) {                                            // killing head of lock tabs
                    systab->lockhead = lptr->fwd_link;                          // bypass it
                } else {                                                        // killing mid list
                    plptr->fwd_link = lptr->fwd_link;                           // bypass it
                }                                                               // end killing mid list

                lptr->job = -1;                                                 // flag it as free
                LCK_Free(lptr);                                                 // add to the free list
                break;                                                          // finished OK
            }                                                                   // end if match
        }                                                                       // end if byte counts match

        plptr = lptr;                                                           // make current, previous
        lptr = lptr->fwd_link;                                                  // check next locktab
    }                                                                           // end while more lock tabs

    return 0;                                                                   // finished OK
}                                                                               // end function LCK_Kill()

void LCK_Remove(int job)                                                        // remove all locks for a job
{
    locktab *lptr;                                                              // locktab entry we are doing
    locktab *plptr;                                                             // previous locktab entry
    short   x;                                                                  // for SEM's

    if (!job) job = partab.jobtab - systab->jobtab + 1;                         // current job
    x = SemOp(SEM_LOCK, -systab->maxjob);                                       // write lock SEM_LOCK
    if (x < 0) return;                                                          // return on error
    lptr = systab->lockhead;                                                    // init current locktab pointer
    plptr = NULL;                                                               // init prev locktab pointer

    while (lptr != NULL) {                                                      // while more lock tabs
        if (lptr->job == job) {                                                 // if we own it
            if (plptr == NULL) {                                                // remove top node
                systab->lockhead = lptr->fwd_link;                              // link in new head lock node
                lptr->job = -1;                                                 // flag it as free
                LCK_Free(lptr);                                                 // add to the free list
                lptr = systab->lockhead;                                        // point at next
                plptr = NULL;                                                   // prev ptr still NULL
            }                                                                   // end if removing top node

            if ((plptr != NULL) && (lptr != NULL)) {                            // if both ptrs defined
                plptr->fwd_link = lptr->fwd_link;                               // bypass it
                lptr->job = -1;                                                 // flag it as free
                LCK_Free(lptr);                                                 // add to the free list
                lptr = plptr->fwd_link;                                         // point at next
            }                                                                   // end if both pointers defined
        } else {                                                                // byte counts don't match
            plptr = lptr;                                                       // make current, previous
            lptr = lptr->fwd_link;                                              // check next locktab
        }                                                                       // end else job numbers !=
    }                                                                           // end while more lock tabs

    SemOp(SEM_LOCK, systab->maxjob);                                            // unlock SEM_LOCK
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
    int     i;                                                                  // handy int
    int     reqd;                                                               // space required for lock
    lck_add ctx, *pctx;

    pctx = &ctx;
    pctx->count = p_count;
    pctx->to = p_to;
    pctx->done = 0;
    pctx->tryagain = 1;
    pctx->strttime = current_time(TRUE);                                        // save op start time

    while (pctx->tryagain) {                                                    // while we should give it a go
        u_int size;                                                             // size of entry count
        int pos = 0;                                                            // position indicator

        pctx->tryagain = 0;                                                     // reset retry flag
        if (pctx->to > -1) partab.jobtab->test = 1;                             // flag successful locking
        pctx->done = 0;                                                         // init
        pctx->x = SemOp(SEM_LOCK, -systab->maxjob);                             // write lock SEM_LOCK
        if (pctx->x < 0) return pctx->x;                                        // return the error

        while ((pctx->done < pctx->count) && (pctx->tryagain == 0)) {           // while more to do
            current = (cstring *) &((u_char *) list)[pos];                      // extract this entry
            reqd = sizeof(short) * 3 + sizeof(int) + sizeof(locktab *) + current->len;
            pctx->lptr = systab->lockhead;                                      // start at first locktab
            plptr = NULL;                                                       // init previous pointer

            if (pctx->lptr == NULL) {                                           // add first lock
                systab->lockfree->fwd_link = NULL;                              // make sure JIC
                nlptr = LCK_Insert(reqd);                                       // try and get some space

                if (nlptr == NULL) {
                    pctx->tryagain = 1;                                         // set retry flag
                    toremove = pctx->done;                                      // save this value
                    removedone = 0;                                             // init this counter
                    posr = 0;                                                   // init position indicator

                    while (removedone < toremove) {                             // while more removals to do
                        LCK_Kill((cstring *) &((u_char *) list)[posr]);         // rem entry at posr
                        tempc = (cstring *) &((u_char *) list)[posr];           // extract this entry
                        size = sizeof(u_short) + tempc->len;                    // find size
                        if (size & 1) size++;                                   // ensure even
                        posr += size;                                           // posr to start of next entry
                        removedone++;                                           // increment removal counter
                    }                                                           // end while when no more to do

                    if (failed(pctx)) return -(ERRZ51 + ERRMLAST);
                } else {
                    nlptr->job = ((partab.jobtab - systab->jobtab) + 1);        // init job number
                    nlptr->lock_count = 1;                                      // init lock count
                    nlptr->byte_count = current->len;                           // init data length
                    memcpy(&nlptr->vol, current->buf, current->len);            // copy in data
                    nlptr->fwd_link = systab->lockhead;                         // link it in sorted order
                    systab->lockhead = nlptr;                                   // link it in sorted order
                }
            }                                                                   // end if first lock

            while (pctx->lptr != NULL) {                                        // while more locktabs to see
                i = current->len;                                               // get the length of this one
                if (pctx->lptr->byte_count < i) i = pctx->lptr->byte_count;     // find shortest length

                while ((pctx->lptr != NULL) && (memcmp(current->buf, &pctx->lptr->vol, i) > 0)) { // more to see until found or past
                    plptr = pctx->lptr;                                         // save current to previous
                    pctx->lptr = pctx->lptr->fwd_link;                          // get next locktab
                    if (pctx->lptr == NULL) break;                              // run out of locktabs
                    i = current->len;                                           // get length of this one
                    if (pctx->lptr->byte_count < i) i = pctx->lptr->byte_count; // find shortest length
                }                                                               // end while more/found/past

                if ((pctx->lptr != NULL) && (memcmp(current->buf, &pctx->lptr->vol, i) == 0)) { // exists as sub/exact/superset
                    if (pctx->lptr->job == (partab.jobtab - systab->jobtab) + 1) { // we MUST own
                        while ((pctx->lptr != NULL) &&                          // more to look at
                          (memcmp(current->buf, &pctx->lptr->vol, i) == 0) &&   // still matches
                          (pctx->lptr->byte_count < current->len)) {            // length is less
                            plptr = pctx->lptr;
                            pctx->lptr = pctx->lptr->fwd_link;                  // have a look at next one
                            if (pctx->lptr == NULL) break;                      // run out of locktabs
                            i = current->len;                                   // get length
                            if (pctx->lptr->byte_count < i) i = pctx->lptr->byte_count; // get shortest
                        }                                                       // end while conditions true

                        if (pctx->lptr != NULL) {                               // if pctx->lptr defined
                            if (memcmp(current->buf, &pctx->lptr->vol, i) == 0) { // if data matches
                                if (pctx->lptr->byte_count == current->len) {   // if exact length match
                                    pctx->lptr->lock_count++;                   // increment the lock count
                                    pctx->lptr = NULL;                          // NULLIFY the ptr, finished
                                } else {                                        // gone past it
                                    nlptr = LCK_Insert(reqd);                   // get some space

                                    if (nlptr == NULL) {
                                        pctx->tryagain = 1;                     // set retry flag
                                        toremove = pctx->done;                  // save this value
                                        removedone = 0;                         // init this counter
                                        posr = 0;                               // init position indicator

                                        while (removedone < toremove) {         // while more removals to do
                                            LCK_Kill((cstring *) &((u_char *) list)[posr]); // rem entry
                                            tempc = (cstring *) &((u_char *) list)[posr]; // extract
                                            size = sizeof(u_short) + tempc->len; // size
                                            if (size & 1) size++;               // ensure even
                                            posr += size;                       // posr to start of next entry
                                            removedone++;                       // increment removal counter
                                        }                                       // end while when no more to do

                                        if (failed(pctx)) return -(ERRZ51 + ERRMLAST);
                                    } else {
                                        nlptr->job = ((partab.jobtab - systab->jobtab) + 1); // job#
                                        nlptr->lock_count = 1;                  // init lock count
                                        nlptr->byte_count = current->len;       // length of data
                                        memcpy(&nlptr->vol, current->buf, current->len); // copy data

                                        if (plptr != NULL) {                    // not inserting at list head
                                            plptr->fwd_link = nlptr;            // link it in this way
                                        } else {                                // inserting at list head
                                            systab->lockhead = nlptr;           // so link it in this way
                                        }

                                        nlptr->fwd_link = pctx->lptr;           // link it in
                                        pctx->lptr = NULL;                      // NULLIFY ptr, finished entry
                                    }
                                }                                               // end else gone past it
                            } else {                                            // memcmp no longer == 0
                                nlptr = LCK_Insert(reqd);                       // get some space

                                if (nlptr == NULL) {
                                    pctx->tryagain = 1;                         // set retry flag
                                    toremove = pctx->done;                      // save this value
                                    removedone = 0;                             // init this counter
                                    posr = 0;                                   // init position indicator

                                    while (removedone < toremove) {             // while more removals to do
                                        LCK_Kill((cstring *) &((u_char *) list)[posr]); // rem entry at posr
                                        tempc = (cstring *) &((u_char *) list)[posr]; // extract this entry
                                        size = sizeof(u_short) + tempc->len;    // find size
                                        if (size & 1) size++;                   // ensure even
                                        posr += size;                           // posr to start of next entry
                                        removedone++;                           // increment removal counter
                                    }                                           // end while when no more to do

                                    if (failed(pctx)) return -(ERRZ51 + ERRMLAST);
                                } else {
                                    nlptr->job = ((partab.jobtab - systab->jobtab) + 1); // init job
                                    nlptr->lock_count = 1;                      // init lock count
                                    nlptr->byte_count = current->len;           // length of data
                                    memcpy(&nlptr->vol, current->buf, current->len); // copy data
                                    plptr->fwd_link = nlptr;                    // link it in
                                    nlptr->fwd_link = pctx->lptr;               // link it in
                                    pctx->lptr = NULL;                          // NULLIFY ptr, finished entry
                                }
                            }                                                   // end else memcmp no longer 0
                        } else {                                                // end if pctx->lptr defined
                            nlptr = LCK_Insert(reqd);                           // get some space

                            if (nlptr == NULL) {
                                pctx->tryagain = 1;                             // set retry flag
                                toremove = pctx->done;                          // save this value
                                removedone = 0;                                 // init this counter
                                posr = 0;                                       // init position indicator

                                while (removedone < toremove) {                 // while more removals to do
                                    LCK_Kill((cstring *) &((u_char *) list)[posr]); // rem entry at posr
                                    tempc = (cstring *) &((u_char *) list)[posr]; // extract this entry
                                    size = sizeof(u_short) + tempc->len;        // find size
                                    if (size & 1) size++;                       // ensure even
                                    posr += size;                               // posr to start of next entry
                                    removedone++;                               // increment removal counter
                                }                                               // end while when no more to do

                                if (failed(pctx)) return -(ERRZ51 + ERRMLAST);
                            } else {
                                nlptr->job = ((partab.jobtab - systab->jobtab) + 1); // job no.
                                nlptr->lock_count = 1;                          // init lock count
                                nlptr->byte_count = current->len;               // length of data
                                memcpy(&nlptr->vol, current->buf, current->len); // copy data
                                plptr->fwd_link = nlptr;                        // link it in
                                nlptr->fwd_link = pctx->lptr;                   // link it in
                                pctx->lptr = NULL;                              // NULLIFY ptr, finished entry
                            }
                        }                                                       // end else, pctx->lptr not defined
                    } else {                                                    // we don't own it - remove all locks thus far
                        pctx->tryagain = 1;                                     // set retry flag
                        toremove = pctx->done;                                  // save this value
                        removedone = 0;                                         // init this counter
                        posr = 0;                                               // init position indicator

                        while (removedone < toremove) {                         // while more removals to do
                            LCK_Kill((cstring *) &((u_char *) list)[posr]);     // rem entry at posr
                            tempc = (cstring *) &((u_char *) list)[posr];       // extract this entry
                            size = sizeof(u_short) + tempc->len;                // find size
                            if (size & 1) size++;                               // ensure even
                            posr += size;                                       // posr to start of next entry
                            removedone++;                                       // increment removal counter
                        }                                                       // end while when no more to do

                        if (failed(pctx)) return -(ERRZ51 + ERRMLAST);
                    }                                                           // end else we dont own it
                } else {                                                        // end if exists in super/sub - doesn't exist
                    if ((pctx->lptr != NULL) && (memcmp(current->buf, &pctx->lptr->vol, i) < 0)) {
                        nlptr = LCK_Insert(reqd);                               // try get some space

                        if (nlptr == NULL) {
                            pctx->tryagain = 1;                                 // set retry flag
                            toremove = pctx->done;                              // save this value
                            removedone = 0;                                     // init this counter
                            posr = 0;                                           // init position indicator

                            while (removedone < toremove) {                     // while more removals to do
                                LCK_Kill((cstring *) &((u_char *) list)[posr]); // rem entry at posr
                                tempc = (cstring *) &((u_char *) list)[posr];   // extract this entry
                                size = sizeof(u_short) + tempc->len;            // find size
                                if (size & 1) size++;                           // ensure even
                                posr += size;                                   // posr to start of next entry
                                removedone++;                                   // increment removal counter
                            }                                                   // end while when no more to do

                            if (failed(pctx)) return -(ERRZ51 + ERRMLAST);
                        } else {
                            nlptr->job = ((partab.jobtab - systab->jobtab) + 1); // init job no.
                            nlptr->lock_count = 1;                              // init lock count
                            nlptr->byte_count = current->len;                   // length of data
                            memcpy(&nlptr->vol, current->buf, current->len);    // copy the data
                            nlptr->fwd_link = pctx->lptr;                       // link it in
                            pctx->lptr = NULL;                                  // NULLIFY ptr, finished entry

                            if (plptr == NULL) {                                // insert as new top node
                                systab->lockhead = nlptr;                       // link this way
                            } else {                                            // end if insert as new top node - insert mid list
                                plptr->fwd_link = nlptr;                        // or link this way
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
                                    LCK_Kill((cstring *) &((u_char *) list)[posr]); // rem entry at posr
                                    tempc = (cstring *) &((u_char *) list)[posr]; // extract this entry
                                    size = sizeof(u_short) + tempc->len;        // find size
                                    if (size & 1) size++;                       // ensure even
                                    posr += size;                               // posr to start of next entry
                                    removedone++;                               // increment removal counter
                                }                                               // end while when no more to do

                                if (failed(pctx)) return -(ERRZ51 + ERRMLAST);
                            } else {
                                nlptr->job = ((partab.jobtab - systab->jobtab) + 1); // init job
                                nlptr->lock_count = 1;                          // init lock count
                                nlptr->byte_count = current->len;               // length of data
                                memcpy(&nlptr->vol, current->buf, current->len); // copy data
                                nlptr->fwd_link = pctx->lptr;                   // link it in
                                pctx->lptr = NULL;                              // NULLIFY ptr, finished entry

                                if (plptr == NULL) {                            // insert as new top node
                                    systab->lockhead = nlptr;                   // link it in this way
                                } else {                                        // end if new top node - insert mid list
                                    plptr->fwd_link = nlptr;                    // or link it in this way
                                }                                               // end else insert mid list
                            }
                        }                                                       // end if run out of locktabs
                    }                                                           // end else add to end of list
                }                                                               // end else not already in
            }                                                                   // end while pctx->lptr not null

            if (pctx->tryagain == 0) {
                size = sizeof(u_short) + current->len;                          // calc len of entry
                if (size & 1) size += 1;                                        // pad to even boundary
                pos += size;                                                    // find next start pos
                pctx->done++;                                                   // number done + 1
            }
        }                                                                       // end while more to do
    }                                                                           // end while try again

    pctx->x = SemOp(SEM_LOCK, systab->maxjob);                                  // unlock SEM_LOCK
    return 0;                                                                   // finished OK
}                                                                               // end function LCK_Add()

short LCK_Sub(int count, cstring *list)                                         // lock minus
{
    int     i;                                                                  // a handy int
    u_int   pos = 0;                                                            // position in *list
    int     done = 0;                                                           // number of entries completed
    cstring *current;                                                           // temp cstring
    locktab *lptr;                                                              // locktab pointer
    short   x;                                                                  // for SEM's

    x = SemOp(SEM_LOCK, -systab->maxjob);                                       // write lock SEM_LOCK
    if (x < 0) return x;                                                        // return the error

    while (done < count) {                                                      // while more to do
        u_int size;                                                             // size of entry in *list

        current = (cstring *) &((u_char *) list)[pos];                          // extract this entry
        lptr = systab->lockhead;                                                // start at first locktab

        while (lptr != NULL) {                                                  // while more locktabs to see
            i = current->len;                                                   // get the length of this one
            if (lptr->byte_count < i) i = lptr->byte_count;                     // find shortest length

            while ((lptr != NULL) && (memcmp(current->buf, &lptr->vol, i) > 0)) { // while more locktabs do go until found or past
                lptr = lptr->fwd_link;                                          // get next locktab
                if (lptr == NULL) break;                                        // if we run out, stop
                i = current->len;                                               // get length of this one
                if (lptr->byte_count < i) i = lptr->byte_count;                 // find shortest length
            }                                                                   // end while not found/past

            if ((lptr != NULL) && (memcmp(current->buf, &lptr->vol, i) == 0)) { // in sub/exact/superset form
                if (lptr->job == ((partab.jobtab - systab->jobtab) + 1)) {      // we MUST own
                    // more to see - first bit match and length is smaller
                    while ((lptr != NULL) && (memcmp(current->buf, &lptr->vol, i) == 0) && (lptr->byte_count < current->len)) {
                        lptr = lptr->fwd_link;                                  // look at next one
                        if (lptr == NULL) break;                                // run out of locktabs, stop
                        i = current->len;                                       // save length
                        if (lptr->byte_count < i) i = lptr->byte_count;         // get shorter one
                    }                                                           // end while

                    if (lptr != NULL) {                                         // if lptr still defined
                        if (memcmp(current->buf, &lptr->vol, i) == 0) {         // first bit matches
                            if (lptr->byte_count == current->len) {             // exact match
                                lptr->lock_count--;                             // decrement the lockcount

                                if (lptr->lock_count <= 0) {                    // if becomes un-wanted
                                    LCK_Kill(current);                          // Kill it
                                    lptr = lptr->fwd_link;                      // NULLIFY ptr, finished entry
                                } else {                                        // end if kill-able
                                    lptr = lptr->fwd_link;                      // NULLIFY ptr, finished entry
                                }
                            } else {                                            // end if exact match
                                lptr = lptr->fwd_link;                          // NULLIFY ptr, finished entry
                            }
                        } else {                                                // end if entries match length
                            lptr = lptr->fwd_link;                              // NULLIFY ptr, finished entry
                        }
                    }                                                           // end if still point at lock
                } else {                                                        // end if we own it
                    lptr = lptr->fwd_link;                                      // NULLIFY ptr, finished entry
                }
            } else {                                                            // end if we match to length
                if (lptr != NULL) lptr = lptr->fwd_link;                        // NULLIFY ptr, finished entry
            }
        }                                                                       // end while more locktabs

        size = sizeof(u_short) + current->len;                                  // calc length of entry
        if (size & 1) size += 1;                                                // pad to even boundary
        pos += size;                                                            // find next start pos
        done++;                                                                 // number done + 1
    }                                                                           // successful

    SemOp(SEM_LOCK, systab->maxjob);                                            // unlock SEM_LOCK
    return 0;                                                                   // finished OK
}                                                                               // end function LCK_Sub()

void Dump_lt(void)
{
    locktab *lptr;                                                              // locktab pointer
    short   x;
    u_char  keystr[MAX_KEY_SIZE + 5];
    u_char  workstr[MAX_KEY_SIZE + 5];

    x = SemOp(SEM_LOCK, -systab->maxjob);                                       // write lock SEM_LOCK
    if (x < 0) return;                                                          // return the error
    lptr = (locktab *) systab->lockstart;
    printf("Dump of Lockspace starting at %p\r\n\r\n", lptr);
    printf("Lock Head starts at %p\r\n", systab->lockhead);
    printf("Lock Free starts at %p\r\n", systab->lockfree);
    printf("      Lock_Ptr       Fwd_Link    Size    Job Lock_Cnt Byte_Cnt  VOL  UCI  Var(Key)\r\n");

    while (lptr != NULL) {
        keystr[0] = '\0';

        if (lptr->byte_count > (VAR_LEN + 2)) {
            workstr[0] = lptr->byte_count - (VAR_LEN + 2);
            memcpy(&workstr[1], lptr->key, workstr[0]);
            x = UTIL_String_Key(workstr, keystr, MAX_NUM_SUBS);
            if (x < 0) sprintf((char *) keystr, " ERROR: %d", x);
        }

        if (lptr->job == -1) {                                                  // only display full stats for real locks
            printf("%10p %14p %7d %6d %8d %8d %4d %4d  %.32s%s\r\n",
                   lptr, lptr->fwd_link, lptr->size, lptr->job, 0, 0, 0, 0, "", "");
        } else {
            printf("%10p %14p %7d %6d %8d %8d %4d %4d  %.32s%s\r\n",
                   lptr, lptr->fwd_link, lptr->size, lptr->job, lptr->lock_count,
                   lptr->byte_count, lptr->vol, lptr->uci, lptr->name.var_cu, keystr);
        }

        lptr = (locktab *) (((u_char *) lptr) + lptr->size);

        if ((u_char *) lptr >= ((u_char *) systab->lockstart) + systab->locksize) {
            break;
        }
    }

    SemOp(SEM_LOCK, systab->maxjob);                                            // unlock SEM_LOCK
    return;                                                                     // finished OK
}
