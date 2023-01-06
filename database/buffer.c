/*
 * Package:  Reference Standard M
 * File:     rsm/database/buffer.c
 * Summary:  module database - buffer management database functions
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
#include <string.h>                                                             // for memset
#include <unistd.h>                                                             // for file reading
#include <ctype.h>                                                              // for GBD stuff
#include <errno.h>                                                              // error stuff
#include <fcntl.h>                                                              // for open()
#include <sys/types.h>                                                          // for semaphores
#include <sys/ipc.h>                                                            // for semaphores
#include <sys/sem.h>                                                            // for semaphores
#include "rsm.h"                                                                // standard includes
#include "database.h"                                                           // database protos
#include "proto.h"                                                              // standard prototypes
#include "error.h"                                                              // error strings

/* typedef struct GBD {                                                         // global buf desciptor
 *     int    block;                                                            // block number
 *     struct GBD *next;                                                        // next entry in list
 *     struct DB_BLOCK *mem;                                                    // memory addr of block
 *     struct GBD *dirty;                                                       // to write -> next
 *     time_t last_accessed;                                                    // last time used
 * } gbd;                                                                       // end GBD struct
 *
 * This is a table of the meanings for the following arrangements of member contents:
 *
 *                     --------------------------------------
 *                     |  block  |  *dirty  |  last_access  |
 *                     --------------------------------------
 * GBD is on freelist       0        NULL           0  (or should be)
 *
 * GBD is being read       SET       NULL           0
 *   in from disk.
 *
 * GBD been garbaged,      SET       SET            0
 *   but is on a dirty list.
 *
 * Note: if dirty pointer is less than (gbd *) 3 then this dirty pointer has
 * been set by a function to reserve this GBD.
 *
 * Note also that every GBD should always have its "mem" member set, not NULL.
 *
 * SEM_GBD is used for reading/writing GBDs and the hash table
 */

/*
 * Function: Get_block
 * Descript: Get specified block into blk[level] - get GBD first
 *           The GBD found is returned in blk[level]
 *           ->dirty is set to (gbd *) 1 if (writing)
 *           ->last_accessed is set to the current time
 *           The block pointers idx & iidx are setup, Index is set to IDX_START
 * Input(s): Block number to get
 * Return:   0 -> Ok, negative M error
 */
short Get_block(u_int blknum)                                                   // Get block
{
    int   i;                                                                    // a handy int
    short s = -1;                                                               // for functions
    off_t file_off;                                                             // for lseek()
    gbd   *ptr;                                                                 // a handy pointer

    systab->vol[volnum - 1]->stats.logrd++;                                     // update stats
    ptr = systab->vol[volnum - 1]->gbd_hash[blknum & (GBD_HASH - 1)];           // get head

    while (ptr != NULL) {                                                       // for entire list
        if (ptr->block == blknum) {                                             // found it?
            blk[level] = ptr;                                                   // save the ptr
            while (ptr->last_accessed == (time_t) 0) SchedYield();              // if being read then wait for it
            goto exit;                                                          // go common exit code
        }

        ptr = ptr->next;                                                        // point at next
    }                                                                           // end memory search

    if (!writing) {                                                             // if read mode
        SemOp(SEM_GLOBAL, -curr_lock);                                          // release read lock
        s = SemOp(SEM_GLOBAL, WRITE);                                           // get write lock
        if (s < 0) return s;                                                    // on error just return it
        ptr = systab->vol[volnum - 1]->gbd_hash[blknum & (GBD_HASH - 1)];       // get head

        while (ptr != NULL) {                                                   // for entire list
            if (ptr->block == blknum) {                                         // found it?
                blk[level] = ptr;                                               // save the ptr
                SemOp(SEM_GLOBAL, WR_TO_R);                                     // drop to read lock
                while (ptr->last_accessed == (time_t) 0) SchedYield();          // if being read then wait for it
                goto exit;                                                      // go common exit code
            }

            ptr = ptr->next;                                                    // point at next
        }                                                                       // end memory search
    }                                                                           // now have a write lck

    systab->vol[volnum - 1]->stats.phyrd++;                                     // update stats
    Get_GBD();                                                                  // get a GBD
    blk[level]->block = blknum;                                                 // set block number
    blk[level]->last_accessed = (time_t) 0;                                     // clear last access
    i = blknum & (GBD_HASH - 1);                                                // get hash entry
    blk[level]->next = systab->vol[volnum - 1]->gbd_hash[i];                    // link it in
    systab->vol[volnum - 1]->gbd_hash[i] = blk[level];
    if (!writing) SemOp(SEM_GLOBAL, WR_TO_R);                                   // if reading then drop to read lock
    file_off = (off_t) blknum - 1;                                              // block#

    file_off = (file_off * (off_t) systab->vol[volnum - 1]->vollab->block_size)
             + (off_t) systab->vol[volnum - 1]->vollab->header_bytes;

    if (volnum > 1) {
        if (volnum > MAX_VOL) return -ERRM26;                                   // Must be in range

        if (partab.vol_fds[volnum - 1] == 0) {                                  // if not open
            if (systab->vol[volnum - 1]->file_name[0] == 0) return -(ERRZ72 + ERRMLAST); // need a filename
            i = open(systab->vol[volnum - 1]->file_name, O_RDONLY);             // Open the volume
            if (i < 0) return -(ERRMLAST + ERRZLAST + errno);                   // Give up on error
            partab.vol_fds[volnum - 1] = i;                                     // make sure fd right
        } else {                                                                // check still there
            if (systab->vol[volnum - 1]->file_name[0] == 0) {
                close(partab.vol_fds[volnum - 1]);                              // close the file
                partab.vol_fds[volnum - 1] = 0;                                 // flag not there
                return -(ERRZ72 + ERRMLAST);                                    // exit complaining
            }
        }
    }

    file_off = lseek(partab.vol_fds[volnum - 1], file_off, SEEK_SET);           // Seek to blk
    if (file_off < 1) panic("Get_block: lseek() failed!");                      // if that failed then die
    i = read(partab.vol_fds[volnum - 1], blk[level]->mem, systab->vol[volnum - 1]->vollab->block_size);
    if (i < 0) panic("Get_block: read() failed!");                              // if read failed then die

exit:
    blk[level]->last_accessed = current_time(TRUE);                             // set access time
    if (writing && (blk[level]->dirty < (gbd *) 5)) blk[level]->dirty = (gbd *) 1; // if writing then reserve it
    Index = IDX_START;                                                          // first one
    idx = (u_short *) blk[level]->mem;                                          // point at the block
    iidx = (int *) blk[level]->mem;                                             // point at the block
    return 0;                                                                   // return success
}

/*
 * Function: New_block
 * Descript: Get new block into blk[level] - get GBD first
 *           The GBD found is returned in blk[level]
 *           ->dirty is set to (gbd *) -1
 *           ->last_accessed is set to the current time
 *           The entire block is zeroed
 *           The block pointers idx & iidx are setup, Index is set to IDX_START
 * Input(s): none
 * Return:   0 -> Ok, negative M error
 * Note:     curr_lock MUST be WRITE when calling this function
 */
short New_block(void)                                                           // get new block
{
    int    i;                                                                   // a handy int
    u_int  blknum;                                                              // the block#
    u_char *c;                                                                  // character ptr
    u_char *end;                                                                // end of map

    // Need to add a check for a dirty map scan in progress here
    Get_GBD();                                                                  // get a GBD
    Index = IDX_START;                                                          // first one
    c = (u_char *) systab->vol[volnum - 1]->first_free;                         // look at first_free
    end = ((u_char *) systab->vol[volnum - 1]->map) + (systab->vol[volnum - 1]->vollab->max_block >> 3); // start plus bits

    while (c <= end) {                                                          // scan map
        if (*c != 255) {                                                        // is there space
            blknum = (c - ((u_char *) systab->vol[volnum - 1]->map)) * 8;       // base number
            for (i = 0; ((1U << i) & *c); i++) continue;                        // find first free bit
            blknum += i;                                                        // add the little bit

            if (blknum <= systab->vol[volnum - 1]->vollab->max_block) {
                *c |= (1U << i);                                                // mark block as used
                systab->vol[volnum - 1]->stats.blkalloc++;                      // update stats
                systab->vol[volnum - 1]->map_dirty_flag++;                      // mark map dirty
                blk[level]->block = blknum;                                     // save in structure
                blk[level]->next = systab->vol[volnum - 1]->gbd_hash[blknum & (GBD_HASH - 1)];
                systab->vol[volnum - 1]->gbd_hash[blknum & (GBD_HASH - 1)] = blk[level]; // link it in
                memset(blk[level]->mem, 0, systab->vol[volnum - 1]->vollab->block_size);
                blk[level]->dirty = (gbd *) 1;                                  // reserve it
                blk[level]->last_accessed = current_time(TRUE);                 // accessed
                systab->vol[volnum - 1]->first_free = c;                        // save this
                return 0;                                                       // return success
            }
        }

        c++;                                                                    // point at next
    }                                                                           // end map scan

    Free_GBD(blk[level]);                                                       // give it back
    return -(ERRZ11 + ERRMLAST);                                                // error - no room
}

/*
 * Function: Get_GBDs
 * Descript: Ensure there are n available GBDs
 * Input(s): number of GBDs required
 * Return:   none
 * Note:     No lock is held when calling this function.
 *           When it completes, it returns with a write lock held.
 */
void Get_GBDs(int greqd)                                                        // get n free GBDs
{
    int    i;                                                                   // a handy int
    int    curr;                                                                // current count
    gbd    *ptr;                                                                // and pointer
    gbd    *last;                                                               // and another
    time_t now;                                                                 // current time
    int    pass = 0;                                                            // pass number

start:
    while (SemOp(SEM_GLOBAL, WRITE)) continue;                                  // get write semaphore lock
    ptr = systab->vol[volnum - 1]->gbd_hash[GBD_HASH];                          // head of free list
    curr = 0;                                                                   // clear current

    while (ptr != NULL) {                                                       // while some there
        curr++;                                                                 // count it
        if (curr >= greqd) return;                                              // if enough there then just exit
        ptr = ptr->next;                                                        // point at next
    }                                                                           // end while

    now = current_time(TRUE) + 1;                                               // get current time + 1
    i = (hash_start + 1) & (GBD_HASH - 1);                                      // where to start

    while (TRUE) {                                                              // loop
        ptr = systab->vol[volnum - 1]->gbd_hash[i];                             // get first entry
        last = NULL;                                                            // clear last

        while (ptr != NULL) {                                                   // while we have some
            if (ptr->block == 0) {                                              // if no block
                if (last == NULL) {                                             // if first one
                    systab->vol[volnum - 1]->gbd_hash[i] = ptr->next;           // hook it here
                } else {                                                        // not first one
                    last->next = ptr->next;                                     // then hook it here
                }

                ptr->next = systab->vol[volnum - 1]->gbd_hash[GBD_HASH];        // hook to free
                systab->vol[volnum - 1]->gbd_hash[GBD_HASH] = ptr;              // and this
                ptr->dirty = NULL;                                              // ensure clear
                ptr->last_accessed = (time_t) 0;                                // and this
                curr++;                                                         // count this
                if (curr >= greqd) return;                                      // if enough there then just exit

                if (last == NULL) {                                             // if first one
                    ptr = systab->vol[volnum - 1]->gbd_hash[i];                 // get next in list
                } else {
                    ptr = last->next;                                           // to allow for loop
                }

                continue;                                                       // next ptr
            }                                                                   // end - no block

            // if free and time expired and there is a time
            if ((ptr->dirty == NULL) && (ptr->last_accessed < now) && (ptr->last_accessed > 0)) {
                curr++;                                                         // count that
                if (curr >= greqd) return;                                      // if enough there then just exit
            }

            last = ptr;                                                         // remember last
            ptr = ptr->next;                                                    // point at next
        }                                                                       // end 1 hash list

        i = (i + 1) & (GBD_HASH - 1);                                           // next hash entry
        if (i == hash_start) break;                                             // where we started and done
    }                                                                           // end while (TRUE)

    SemOp(SEM_GLOBAL, -curr_lock);                                              // release our lock
    sleep(1);
    pass++;                                                                     // increment a pass
    if (pass > 60) panic("Get_GBDs: Can't get enough GBDs after 60 seconds");   // this is crazy!
    goto start;                                                                 // try again
}

/*
 * Function: Get_GBD
 * Descript: Get a GBD into blk[level]
 *           ->block, next, dirty and last_accessed are cleared
 *           The block pointers idx & iidx are setup
 *           The block is NOT zeroed
 * Input(s): none
 * Return:   none
 * Note:     curr_lock MUST be WRITE when calling this function
 */
void Get_GBD(void)                                                              // get a GBD
{
    int    i;                                                                   // a handy int
    time_t now;                                                                 // current time
    time_t exp;                                                                 // expiry time
    time_t old;                                                                 // oldest
    int    hash = -1;                                                           // for the table
    gbd    *ptr;                                                                // loop GBD ptr
    gbd    *oldptr = NULL;                                                      // remember oldest
    gbd    *last;                                                               // points to ptr

start:
    if (systab->vol[volnum - 1]->gbd_hash[GBD_HASH]) {                          // any free?
        blk[level] = systab->vol[volnum - 1]->gbd_hash[GBD_HASH];               // get one
        systab->vol[volnum - 1]->gbd_hash[GBD_HASH] = blk[level]->next;         // unlink it
        goto exit;                                                              // common exit code
    }

    now = current_time(TRUE);                                                   // get current time
    old = now + 1;                                                              // remember oldest
    exp = now - gbd_expired;                                                    // expired time
    i = (hash_start + 1) & (GBD_HASH - 1);                                      // where to start

    while (TRUE) {                                                              // loop
        ptr = systab->vol[volnum - 1]->gbd_hash[i];                             // get first entry
        last = NULL;                                                            // clear last

        while (ptr != NULL) {                                                   // while we have some
            // if no block OR if free AND time expired AND there is a time
            if ((ptr->block == 0) || ((ptr->dirty == NULL) && (ptr->last_accessed < exp) && (ptr->last_accessed > 0))) {
                if (last == NULL) {                                             // first one?
                    systab->vol[volnum - 1]->gbd_hash[i] = ptr->next;           // unlink from hash
                } else {                                                        // subsequent
                    last->next = ptr->next;                                     // unlink
                }

                hash_start = i;                                                 // remember this
                blk[level] = ptr;                                               // store where reqd
                goto exit;                                                      // common exit code
            }                                                                   // end found expired

            // if free AND less than oldest AND there is a time
            if ((ptr->dirty == NULL) && (ptr->last_accessed < old) && (ptr->last_accessed > 0)) {
                old = ptr->last_accessed;                                       // save time
                oldptr = ptr;                                                   // save the ptr
                hash = i;                                                       // and the hash
            }

            last = ptr;                                                         // save last
            ptr = ptr->next;                                                    // point at next
        }                                                                       // end 1 hash list

        i = (i + 1) & (GBD_HASH - 1);                                           // next hash entry
        if (i == hash_start) break;                                             // where we started and done
    }                                                                           // end while (TRUE)

    if (oldptr == NULL) {                                                       // did we get one
        if (writing) panic("Get_GBD: Failed to find an available GBD while writing"); // SET or KILL then die
        SemOp(SEM_GLOBAL, -curr_lock);                                          // release current
        sleep(1);                                                               // wait
        while (SemOp(SEM_GLOBAL, WRITE));                                       // re-get lock
        goto start;                                                             // and try again
    }

    ptr = systab->vol[volnum - 1]->gbd_hash[hash];                              // get the list

    if (ptr == oldptr) {                                                        // is this it
        systab->vol[volnum - 1]->gbd_hash[hash] = ptr->next;                    // unlink it
    } else {                                                                    // we gota look for it
        while (ptr->next != oldptr) ptr = ptr->next;                            // until we do we get the next
        ptr->next = oldptr->next;                                               // unlink it
    }

    blk[level] = oldptr;                                                        // store where reqd

exit:
    blk[level]->block = 0;                                                      // no block attached
    blk[level]->next = NULL;                                                    // clear link
    blk[level]->dirty = NULL;                                                   // clear dirty
    blk[level]->last_accessed = (time_t) 0;                                     // and time
    idx = (u_short *) blk[level]->mem;                                          // set this up
    iidx = (int *) blk[level]->mem;                                             // and this
    return;                                                                     // return
}

/*
 * Function: Free_GBD
 * Descript: Free specified GBD (if ->block non-zero, remove from hash table)
 * Input(s): GBD pointer
 * Return:   none
 * Note:     curr_lock MUST be WRITE when calling this function
 */
void Free_GBD(gbd *free)                                                        // Free a GBD
{
    gbd *ptr;                                                                   // a handy pointer

    if (free->block) {                                                          // if there is a blk#
        ptr = systab->vol[volnum - 1]->gbd_hash[free->block & (GBD_HASH - 1)];

        if (ptr == free) {                                                      // if this one
            systab->vol[volnum - 1]->gbd_hash[free->block & (GBD_HASH - 1)] = free->next; // unlink it
        } else {                                                                // look for it
            while (ptr->next != free) ptr = ptr->next;                          // til found get the next
            ptr->next = free->next;                                             // unlink it
        }
    }

    free->next = systab->vol[volnum - 1]->gbd_hash[GBD_HASH];                   // get free list
    systab->vol[volnum - 1]->gbd_hash[GBD_HASH] = free;                         // link it in
    free->block = 0;                                                            // clear this
    free->dirty = NULL;                                                         // and this
    free->last_accessed = (time_t) 0;                                           // and this
    return;                                                                     // and exit
}
