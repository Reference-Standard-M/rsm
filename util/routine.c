/*
 * Package: Reference Standard M
 * File:    rsm/util/routine.c
 * Summary: module RSM routine - routine functions
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

#include <stdio.h>                                                              // always include
#include <stdlib.h>                                                             // these two
#include <errno.h>                                                              // error stuff
#include <string.h>                                                             // for memcpy
#include <time.h>                                                               // for ctime
#include <sys/types.h>                                                          // for u_char def
#include "rsm.h"                                                                // standard includes
#include "compile.h"                                                            // RBD structures
#include "proto.h"                                                              // the prototypes

// The following is called ONLY from rsm/init/start.c and rsm/database/mount.c
void Routine_Init(int vol)                                                      // setup rbd for this vol
{
    rbd   *rou;                                                                 // a routine pointer
    u_int i;                                                                    // an int

    for (i = 0; i < RBD_HASH; i++) partab.vol[vol]->rbd_hash[i] = NULL;         // the hash table, need to clear it out
    rou = (rbd *) SOA(partab.vol[vol]->rbd_head);                               // free space entry
    partab.vol[vol]->rbd_hash[RBD_HASH] = SBA(rou);                             // head of free list
    i = (char *) SOA(partab.vol[vol]->rbd_end) - (char *) SOA(partab.vol[vol]->rbd_head); // memory available
    rou->fwd_link = NULL;                                                       // no forward link
    rou->chunk_size = i;                                                        // size of this bit of free
    rou->attached = 0;                                                          // nothing attached
    rou->last_access = 0;                                                       // not used
    VAR_CLEAR(rou->rnam);                                                       // no routine
    rou->uci = 0;                                                               // no UCI
    rou->vol = 0;                                                               // no vol
    rou->rou_size = 0;                                                          // no routine here
    return;                                                                     // done
}

int Routine_Hash(var_u routine)                                                 // return hash code
{
    int hash = 0;                                                               // for the return
    int i;                                                                      // a handy int
    int j;                                                                      // another handy int

    int p[4][8] = {
        {3, 5, 7, 11, 13, 17, 19, 23},
        {29, 31, 37, 41, 43, 47, 53, 59},
        {61, 67, 71, 73, 79, 83, 89, 97},
        {101, 103, 107, 109, 113, 127, 131, 137}
    };                                                                          // odd primes

    for (i = 0; i < (VAR_LEN / 8); i++) {
        if (routine.var_qu[i] == 0) break;
        for (j = 0; j < 8; j++) {                                               // for each character
            if (routine.var_qu[i] == 0) break;
            hash = (((routine.var_qu[i] & 0xFF) * p[i][j]) + hash);             // add that char
            routine.var_qu[i] = (routine.var_qu[i] >> 8);                       // right shift one byte
        }                                                                       // end hash loop
    }

    return (hash % RBD_HASH);                                                   // return the code
}

void Routine_Combine(rbd *pointer)                                              // combine with following block
{
    rbd *ptr;                                                                   // a handy pointer
    rbd *p;                                                                     // and another

    ptr = (rbd *) ((u_char *) pointer + pointer->chunk_size);                   // point at next
    p = SOA(partab.vol[partab.jobtab->rvol - 1]->rbd_hash[RBD_HASH]);           // see where it points

    if (p == ptr) {                                                             // if that's us
        partab.vol[partab.jobtab->rvol - 1]->rbd_hash[RBD_HASH] = ptr->fwd_link; // point at our fwd link
    } else {                                                                    // else find our entry
        while (SOA(p->fwd_link) != ptr) p = SOA(p->fwd_link);                   // find previous entry
        p->fwd_link = ptr->fwd_link;                                            // point it at our forward link
    }

    pointer->chunk_size += ptr->chunk_size;                                     // add the two sizes together
    return;                                                                     // and exit
}

void Routine_Free(rbd *pointer)                                                 // call internally, RBD locked
{
    rbd *ptr;                                                                   // a handy pointer
    rbd *p;                                                                     // and another
    int hash;                                                                   // hash pointer

    pointer->rou_size = 0;                                                      // flag 'not used'
    hash = Routine_Hash(pointer->rnam);                                         // get the hash
    ptr = SOA(partab.vol[partab.jobtab->rvol - 1]->rbd_hash[hash]);             // see where it points

    if (ptr == pointer) {                                                       // if that's us
        partab.vol[partab.jobtab->rvol - 1]->rbd_hash[hash] = pointer->fwd_link; // point at our forward link
    } else {                                                                    // else find our entry
        while (TRUE) {
            if (ptr == NULL) {                                                  // if end of list
                ptr = SOA(partab.vol[partab.jobtab->rvol - 1]->rbd_hash[RBD_HASH]); //point at first free

                if (pointer == ptr) {
                    partab.vol[partab.jobtab->rvol - 1]->rbd_hash[RBD_HASH] = pointer->fwd_link;
                } else {
                    while (ptr != NULL) {                                       // scan free list
                        if (SOA(ptr->fwd_link) == pointer) {                    // if in freelist
                            ptr->fwd_link = pointer->fwd_link;                  // take it out
                            break;                                              // and quit
                        }

                        ptr = SOA(ptr->fwd_link);                               // point at next
                        // if (ptr == NULL) we should search else where!!!
                    }
                }

                break;
            }

            if (SOA(ptr->fwd_link) == pointer) {                                // if found
                ptr->fwd_link = pointer->fwd_link;                              // point it at our forward link
                break;                                                          // and quit
            }

            ptr = SOA(ptr->fwd_link);                                           // point to next
        }
    }

    pointer->fwd_link = partab.vol[partab.jobtab->rvol - 1]->rbd_hash[RBD_HASH]; // point at first free
    partab.vol[partab.jobtab->rvol - 1]->rbd_hash[RBD_HASH] = SBA(pointer);     // add to free list
    VAR_CLEAR(pointer->rnam);                                                   // zot rou name
    pointer->uci = 0;                                                           // and UCI
    pointer->vol = 0;                                                           // and vol
    pointer->last_access = (time_t) 0;                                          // and access

    while (TRUE) {                                                              // until end of list
        ptr = (rbd *) ((u_char *) pointer + pointer->chunk_size);               // point at next
        if (ptr >= (rbd *) SOA(partab.vol[partab.jobtab->rvol - 1]->rbd_end)) break; // quit when done

        if (ptr->rou_size == 0) {                                               // if there is no routine
            Routine_Combine(pointer);                                           // combine it in
        } else {
            break;                                                              // else done
        }
    }

    while (TRUE) {                                                              // look for previous
        ptr = (rbd *) SOA(partab.vol[partab.jobtab->rvol - 1]->rbd_head);       // start of RBDs
        if (ptr == pointer) return;                                             // same - all done

        while (TRUE) {                                                          // scan for previous
            p = (rbd *) ((u_char *) ptr + ptr->chunk_size);                     // point at next
            if (p == pointer) break;                                            // found it (in ptr)
            ptr = p;                                                            // remember this one
        }

        if (ptr->rou_size == 0) {                                               // if unused
            pointer = ptr;                                                      // make this one active
            Routine_Combine(pointer);                                           // combine it in
        } else {
            return;                                                             // else all done
        }
    }
}

void Routine_Collect(time_t off)                                                // collect based on time
{
    rbd *ptr;                                                                   // a pointer

    off = current_time(TRUE) - off;                                             // get compare time
    ptr = (rbd *) SOA(partab.vol[partab.jobtab->rvol - 1]->rbd_head);           // head of RBDs

    while (TRUE) {                                                              // scan whole list
        // nothing attached and it fits the time and not already free
        if ((ptr->attached < 1) && (ptr->last_access < off) && (ptr->rou_size > 0)) {
            Routine_Free(ptr);                                                  // free it
            ptr = (rbd *) SOA(partab.vol[partab.jobtab->rvol - 1]->rbd_head);   // start from the begining
        }

        ptr = (rbd *) ((u_char *) ptr + ptr->chunk_size);                       // point at next
        if (ptr >= (rbd *) SOA(partab.vol[partab.jobtab->rvol - 1]->rbd_end)) break; // quit when done
    }

    return;                                                                     // all done
}

rbd *Routine_Find(u_int size)                                                   // find int bytes
{
    rbd *ptr;                                                                   // a pointer
    rbd *p;                                                                     // and another

    ptr = SOA(partab.vol[partab.jobtab->rvol - 1]->rbd_hash[RBD_HASH]);         // get head of free list

    while (ptr != NULL) {                                                       // while we have some
        if (ptr->chunk_size >= size) break;                                     // if big enough
        ptr = SOA(ptr->fwd_link);                                               // get next
    }

    if (ptr == NULL) {                                                          // found nothing
        Routine_Collect(RESERVE_TIME);                                          // do a collect using reserve
        ptr = SOA(partab.vol[partab.jobtab->rvol - 1]->rbd_hash[RBD_HASH]);     // get head of free list

        while (ptr != NULL) {                                                   // while we have some
            if (ptr->chunk_size >= size) break;                                 // if big enough
            ptr = SOA(ptr->fwd_link);                                           // get next
        }

        if (ptr == NULL) {                                                      // found nothing
            Routine_Collect(0);                                                 // do a collect using zero
            ptr = SOA(partab.vol[partab.jobtab->rvol - 1]->rbd_hash[RBD_HASH]); // get head of free list

            while (ptr != NULL) {                                               // while we have some
                if (ptr->chunk_size >= size) break;                             // if big enough
                ptr = SOA(ptr->fwd_link);                                       // get next
            }

            if (ptr == NULL) return NULL;                                       // found nothing - give up
        }
    }

    if ((size + SIZE_CLOSE) < ptr->chunk_size) {                                // if far too big
        p = (rbd *) ((u_char *) ptr + size);                                    // setup for another
        p->fwd_link = ptr->fwd_link;                                            // and forward link
        p->chunk_size = ptr->chunk_size - size;                                 // its size
        p->attached = 0;                                                        // clear this lot
        p->last_access = 0;                                                     // no time required
        VAR_CLEAR(p->rnam);                                                     // no routine name
        p->uci = 0;                                                             // no UCI
        p->vol = 0;                                                             // no vol
        p->rou_size = 0;                                                        // no routine (not in use)
        ptr->fwd_link = SBA(p);                                                 // point at new chunk
        ptr->chunk_size = size;                                                 // the new size
    }

    if (SOA(partab.vol[partab.jobtab->rvol - 1]->rbd_hash[RBD_HASH]) == ptr) {
        partab.vol[partab.jobtab->rvol - 1]->rbd_hash[RBD_HASH] = ptr->fwd_link; // new free bit
    } else {
        p = SOA(partab.vol[partab.jobtab->rvol - 1]->rbd_hash[RBD_HASH]);       // get head of free list
        while (SOA(p->fwd_link) != ptr) p = SOA(p->fwd_link);                   // find our pointer
        p->fwd_link = ptr->fwd_link;                                            // change to new one
    }

    return ptr;                                                                 // return the pointer
}

// The following are called from the general M code
rbd *Routine_Attach(var_u routine)                                              // attach to routine
{
    int     hash = 0;                                                           // for rbd_hash[]
    int     i;                                                                  // a handy int
    u_int   size;                                                               // size required
    short   s;                                                                  // a useful thing
    int     t;                                                                  // a useful thing
    rbd     *ptr;                                                               // a pointer for this
    rbd     *p;                                                                 // and another
    u_char  tmp[VAR_LEN + 4];                                                   // temp space
    cstring *cptr;                                                              // for making strings
    mvar    rouglob;                                                            // mvar for $ROUTINE
    u_char  uci;                                                                // current UCI
    u_char  vol;                                                                // current vol

    hash = Routine_Hash(routine);                                               // get the hash
    s = SemOp(SEM_ROU, SEM_WRITE);                                              // write lock the RBDs
    if (s < 0) return NULL;                                                     // say can't find on error
    p = SOA(partab.vol[partab.jobtab->rvol - 1]->rbd_hash[hash]);               // see where it points
    ptr = p;                                                                    // use it in ptr
    uci = partab.jobtab->ruci;                                                  // get current UCI
    vol = partab.jobtab->rvol;                                                  // and vol

    if (routine.var_cu[0] == '%') {                                             // check for a % routine
        vol = 1;
        uci = 1;
    }

    while (ptr != NULL) {                                                       // while we have something
        if (var_equal(ptr->rnam, routine) && (ptr->uci == uci) && (ptr->vol == vol)) { // if this is the right one
            ptr->attached++;                                                    // count an attach
            SemOp(SEM_ROU, -SEM_WRITE);                                         // release the lock
            return SBA(ptr);                                                    // and return the pointer
        }

        ptr = SOA(ptr->fwd_link);                                               // point at the next one
    }                                                                           // end while loop

    SemOp(SEM_ROU, -SEM_WRITE);                                                 // release the lock
    VAR_CLEAR(rouglob.name);
    memcpy(rouglob.name.var_cu, "$ROUTINE", 8);                                 // global name
    rouglob.volset = vol;                                                       // volume set
    rouglob.uci = uci;                                                          // UCI
    cptr = (cstring *) tmp;                                                     // get some temp space

    for (i = 0; i < VAR_LEN; i++) {                                             // loop thru the name
        if (routine.var_cu[i] == '\0') break;
DISABLE_WARN(-Warray-bounds)
        cptr->buf[i] = routine.var_cu[i];                                       // copy a byte
    }

    cptr->buf[i] = '\0';                                                        // terminate
    cptr->len = (u_short) i;                                                    // the count
    s = UTIL_Key_Build(cptr, rouglob.key);                                      // first subs
    rouglob.slen = (u_char) s;                                                  // save count so far
    cptr->buf[0] = '0';                                                         // now the zero
    cptr->buf[1] = '\0';                                                        // null terminate
    cptr->len = 1;                                                              // and the length
ENABLE_WARN
    s = UTIL_Key_Build(cptr, &rouglob.key[s]);                                  // second subs
    rouglob.slen += (u_char) s;                                                 // save count so far
    t = DB_GetLen(&rouglob, 0, NULL);                                           // get a possible length
    if (t < 1) return NULL;                                                     // no such
    s = SemOp(SEM_ROU, SEM_WRITE);                                              // write lock & try again
    if (s < 0) return NULL;                                                     // no such
    p = SOA(partab.vol[partab.jobtab->rvol - 1]->rbd_hash[hash]);               // see where it points
    ptr = p;                                                                    // use it in ptr

    while (ptr != NULL) {                                                       // while we have something
        if (var_equal(ptr->rnam, routine) && (ptr->uci == uci) && (ptr->vol == vol)) { // if this is the right one
            ptr->attached++;                                                    // count an attach
            SemOp(SEM_ROU, -SEM_WRITE);                                         // release the lock
            return SBA(ptr);                                                    // and return the pointer
        }

        p = ptr;                                                                // save for ron
        ptr = SOA(ptr->fwd_link);                                               // point at the next one
    }                                                                           // end while loop

    t = DB_GetLen(&rouglob, 1, NULL);                                           // lock the GBD

    if (t < 1) {                                                                // if it's gone
        DB_GetLen(&rouglob, -1, NULL);                                          // un-lock the GBD
        SemOp(SEM_ROU, -SEM_WRITE);                                             // release the lock
        return NULL;                                                            // say no such
    }

    size = t + RBD_OVERHEAD + 1;                                                // space required
    if (size & 7) size = (size & ~7) + 8;                                       // round up to 8 byte boundary
    ptr = Routine_Find(size);                                                   // find location

    if (ptr == NULL) {                                                          // no space mate!!
        SemOp(SEM_ROU, -SEM_WRITE);                                             // release the lock
        DB_GetLen(&rouglob, -1, NULL);                                          // un-lock the GBD
        return (rbd *) -1;                                                      // say no space
    }

    if (p == NULL) {                                                            // listhead for this hash
        partab.vol[partab.jobtab->rvol - 1]->rbd_hash[hash] = SBA(ptr);         // save it here
    } else {
        p->fwd_link = SBA(ptr);                                                 // or here
    }

    ptr->fwd_link = NULL;                                                       // ensure this is null
    ptr->attached = 1;                                                          // count the attach
    ptr->last_access = current_time(TRUE);                                      // and the current time
    VAR_COPY(ptr->rnam, routine);                                               // the routine name
    ptr->uci = uci;                                                             // the UCI
    ptr->vol = vol;                                                             // current volume
    ptr->rou_size = (u_short) t;                                                // save the size
    t = DB_GetLen(&rouglob, -1, (u_char *) &ptr->comp_ver);                     // get the routine
    if (t != ptr->rou_size) panic("routine load - size wrong");                 // DOUBLECHECK
    ptr->tag_tbl += RBD_OVERHEAD;                                               // adjust for RBD junk
    ptr->var_tbl += RBD_OVERHEAD;                                               // adjust for RBD junk
    ptr->code += RBD_OVERHEAD;                                                  // adjust for RBD junk

    if (ptr->comp_ver != COMP_VER) {                                            // check compiler version
        ptr->attached--;                                                        // decrement the count
        Routine_Free(ptr);                                                      // free the space
        SemOp(SEM_ROU, -SEM_WRITE);                                             // release the lock
        return (rbd *) -2;                                                      // yet another *magic* number
    }

    SemOp(SEM_ROU, -SEM_WRITE);                                                 // release the lock
    return SBA(ptr);                                                            // success
}

void Routine_Detach(rbd *pointer)                                               // Detach from routine
{
    short s;                                                                    // for SemOp() call

    while (SemOp(SEM_ROU, SEM_WRITE) < 0) continue;                             // lock the RBDs, check error
    if (pointer->attached > 0) pointer->attached--;                             // if not lost then decrement the count
    if ((pointer->uci == 0) && (pointer->attached == 0)) Routine_Free(pointer); // if invalid and nothing attached, free the space
    s = SemOp(SEM_ROU, -SEM_WRITE);                                             // release the lock
    if (s < 0) fprintf(stderr, "errno = %d - %s\n", errno, strerror(errno));
    return;                                                                     // done
}

// NOTE: Semaphore must be held BEFORE calling this routine
void Routine_Delete(var_u routine, int vol, int uci)                            // mark routine deleted
{
    int hash = 0;                                                               // for rbd_hash[]
    rbd *ptr;                                                                   // a pointer for this

    hash = Routine_Hash(routine);                                               // get the hash
    ptr = SOA(partab.vol[vol - 1]->rbd_hash[hash]);                             // see where it points

    while (ptr != NULL) {                                                       // while we have something
        if ((var_equal(ptr->rnam, routine)) && (ptr->uci == uci)) {             // if this is the right one
            ptr->uci = 0;                                                       // mark as deleted
            if (ptr->attached == 0) Routine_Free(ptr);                          // if not in use then free the space
            break;                                                              // and exit
        }

        ptr = SOA(ptr->fwd_link);                                               // point at the next one
    }                                                                           // end while loop

    return;                                                                     // done
}

// The following are internal only (first called from $&DEBUG())
void Dump_rbd(void)                                                             // dump RBDs
{
    int     i;                                                                  // an int
    short   s;                                                                  // for function returns
    rbd     *p;                                                                 // a pointer
    rbd     *free;                                                              // loop through routine free space
    u_int   free_size = 0;                                                      // actual size of free routine space
    u_int   size = 0;                                                           // size of routine space
    char    tmp[VAR_LEN + 1];                                                   // some space
    time_t  t;                                                                  // for time

    s = SemOp(SEM_ROU, SEM_WRITE);                                              // write lock the RBDs
    if (s < 0) return;                                                          // exit on error
    p = (rbd *) SOA(partab.vol[partab.jobtab->rvol - 1]->rbd_head);             // get the start
    t = current_time(FALSE);
    printf("Dump of all Routine Buffer Descriptors on %s [%lld]\r\n\r\n", strtok(ctime(&t), "\n"), (long long) t);
    free = SOA(partab.vol[partab.jobtab->rvol - 1]->rbd_hash[RBD_HASH]);
    printf("Routine Buffer Space Free at %p\r\n", free);

    while (free != NULL) {
        free_size += free->chunk_size;
        free = SOA(free->fwd_link);
    }

    size = (u_int) (SOA(partab.vol[partab.jobtab->rvol - 1]->rbd_end) - SOA(partab.vol[partab.jobtab->rvol - 1]->rbd_head));
    printf("Using %u of %u bytes of Routine Buffer Space\r\n\r\n", size - free_size, size);
    printf("       Address    Forward Link  Chunk Size      Attach  Last Access  VOL  UCI  Routine Size  Routine Name\r\n");

    while (TRUE) {                                                              // for all
        for (i = 0; i < VAR_LEN; i++) tmp[i] = ' ';                             // space fill tmp[]

        for (i = 0; i < VAR_LEN; i++) {
            if (p->rnam.var_cu[i] == 0) break;
            tmp[i] = p->rnam.var_cu[i];
        }

        tmp[i] = '\0';                                                          // null terminate name

        printf("%14p %15p %11u %11u %12lld %4d %4d %13d  %s\r\n", p, SOA(p->fwd_link), p->chunk_size,
               p->attached, (long long) p->last_access, p->vol, p->uci, p->rou_size, tmp);

        p = (rbd *) ((u_char *) p + p->chunk_size);                             // point at next
        if (p >= (rbd *) SOA(partab.vol[partab.jobtab->rvol - 1]->rbd_end)) break; // quit when done
    }

    SemOp(SEM_ROU, -SEM_WRITE);                                                 // release lock
    return;                                                                     // and exit
}
