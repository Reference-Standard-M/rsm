/*
 * Package: Reference Standard M
 * File:    rsm/database/util.c
 * Summary: module database - database functions - utilities
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
#include <string.h>                                                             // for memcpy
#include <time.h>                                                               // for ctime
#include <unistd.h>                                                             // for file reading
#include <fcntl.h>                                                              // for expand
#include <ctype.h>                                                              // for GBD stuff
#include <errno.h>                                                              // for errors
#include <sys/param.h>                                                          // for realpath() function
#include <sys/types.h>                                                          // for semaphores
#include <sys/ipc.h>                                                            // for semaphores
#include <sys/sem.h>                                                            // for semaphores
#include <sys/stat.h>                                                           // for fchmod
#include "rsm.h"                                                                // standard includes
#include "database.h"                                                           // database protos
#include "proto.h"                                                              // standard prototypes
#include "error.h"                                                              // error strings

/*
 * Function: Insert
 * Summary:  Insert the supplied key and data in blk[level]
 * Input(s): Pointer the the key and data to insert
 * Return:   0 -> Ok, negative M error -(ERRZ62 + ERRMLAST)
 */
short Insert(u_char *key, const cstring *data)                                  // insert a node
{
    int    isdata;                                                              // data/ptr flag
    int    rs;                                                                  // required size
    u_char ccc;                                                                 // common char count
    u_char ucc;                                                                 // uncommon char count
    u_int  flags = 0;                                                           // for $GLOBAL

    isdata = ((SOA(blk[level]->mem)->type > 64) && level);                      // data block and not the directory

    if (SOA(blk[level]->mem)->last_idx > (IDX_START - 1)) {                     // if some data
        short s;

        s = Locate(key);                                                        // search for it

        if (s >= 0) {                                                           // if found
            return -(ERRZ61 + ERRMLAST);                                        // database stuffed
        } else if (s != -ERRM7) {                                               // for any other error
            return s;                                                           // exit
        }
    } else {                                                                    // empty block
        Index = IDX_START;                                                      // start
        idx = (u_short *) SOA(blk[level]->mem);                                 // point at the block
        iidx = (int *) SOA(blk[level]->mem);                                    // point at the block
    }

    if (!level) {                                                               // insert in global directory
        chunk = (cstring *) &iidx[idx[IDX_START]];                              // point at $GLOBAL chunk
        record = (cstring *) &chunk->buf[chunk->buf[1] + 2];
        Align_record();                                                         // align it
        flags = ((u_int *) record)[1];                                          // get default flags
        partab.jobtab->last_block_flags = flags;
    }

    keybuf[0] = 0;                                                              // clear keybuf

    for (u_int i = IDX_START; i < Index; i++) {                                 // for all previous Indexes (key compression)
        chunk = (cstring *) &iidx[idx[i]];                                      // point at the chunk
        memcpy(&keybuf[chunk->buf[0] + 1], &chunk->buf[2], chunk->buf[1]);      // update the key
        keybuf[0] = chunk->buf[0] + chunk->buf[1];                              // and the size
    }                                                                           // we insert after this

    ccc = 0;                                                                    // start here

    if (key[0] && keybuf[0]) {                                                  // if any there
        while (key[ccc + 1] == keybuf[ccc + 1]) {                               // while the same
            if ((ccc == key[0]) || (ccc == keybuf[0])) break;                   // at end of either we're done
            ccc++;                                                              // increment pointer
        }
    }

    ucc = key[0] - ccc;                                                         // and this
    rs = sizeof(u_short) + 2 + ucc + data->len;                                 // chunk + ccc/ucc + key + data

    if (isdata) {                                                               // if it's a data block
        rs += sizeof(u_short);                                                  // add the dbc size
    } else if (!level) {                                                        // if global directory
        rs += 4;                                                                // allow for flags
    }

    if (rs & 3) rs += (4 - (rs & 3));                                           // not even long word so round it up
    rs += 4;                                                                    // allow for the Index
    if (rs > MAX_STR_LEN) return -ERRM75;                                       // record size too large to fit in chunk len

    if (rs > ((SOA(blk[level]->mem)->last_free * 2 + 1 - SOA(blk[level]->mem)->last_idx) * 2)) {
        if (!(SOA(blk[level]->mem)->flags & BLOCK_DIRTY)) return -(ERRZ62 + ERRMLAST); // if block is clean then say no room
        Tidy_block();                                                           // tidy it

        if (rs > ((SOA(blk[level]->mem)->last_free * 2 + 1 - SOA(blk[level]->mem)->last_idx) * 2)) {
            return -(ERRZ62 + ERRMLAST);                                        // say no room
        }
    }                                                                           // it will now fit

    rs -= 4;                                                                    // rs now chunksize
    for (u_int i = SOA(blk[level]->mem)->last_idx; i >= Index; i--) idx[i + 1] = idx[i]; // the trailing ones get copied down
    idx[Index] = SOA(blk[level]->mem)->last_free - (rs / 4) + 1;                // where it goes
    chunk = (cstring *) &iidx[idx[Index]];                                      // as an address
    record = (cstring *) &chunk->buf[ucc + 2];                                  // where the data goes
    chunk->len = rs;                                                            // store chunk size
    chunk->buf[0] = ccc;                                                        // this bit
    chunk->buf[1] = ucc;                                                        // then this
    memcpy(&chunk->buf[2], &key[ccc + 1], ucc);                                 // then the key bit

    if (isdata) {                                                               // for data block
        record->len = data->len;                                                // copy the length
        memcpy(record->buf, data->buf, data->len);                              // then the data
    } else {                                                                    // it's a pointer
        Align_record();                                                         // align it
        memcpy(record, data->buf, sizeof(int));                                 // the block number
        if (!level) ((u_int *) record)[1] = flags;                              // if global directory then set/clear the flags
    }

    SOA(blk[level]->mem)->last_free -= (rs / 4);                                // redo last_free
    SOA(blk[level]->mem)->last_idx++;                                           // add to the index
    SOA(blk[level]->mem)->flags |= BLOCK_DIRTY;                                 // mark dirty
    return 0;                                                                   // done
}

/*
 * Function: Queit
 * Summary:  Queue the GBD at blk[level] for write - links already setup
 * Input(s): None
 * Return:   None
 * Note:     Must hold a write lock before calling this function
 */
void Queit(void)                                                                // queue a GBD for write
{
    int i;                                                                      // a handy int
    gbd *ptr;                                                                   // a handy pointer

    ptr = blk[level];                                                           // point at the block
    partab.vol[volnum]->stats.logwt++;                                          // increment logical

    while (SOA(ptr->dirty) != ptr) {                                            // check it
        ptr = SOA(ptr->dirty);                                                  // point at next
        partab.vol[volnum]->stats.logwt++;                                      // increment logical
    }

    i = partab.vol[volnum]->dirtyQw;                                            // where to put it

    while (partab.vol[volnum]->dirtyQ[i] != NULL) {                             // if slot not available
        sleep(1);                                                               // wait a bit
    }                                                                           // NOTE: The above CAN'T work!!!

    partab.vol[volnum]->dirtyQ[i] = SBA(blk[level]);                            // stuff it in
    partab.vol[volnum]->dirtyQw = (i + 1) & (NUM_DIRTY - 1);                    // reset pointer
    return;                                                                     // and exit
}

/*
 * Function: Garbit
 * Summary:  Queue the block passed in for garbage collection
 * Input(s): Block number
 * Return:   None
 * Note:     Must hold a write lock before calling this function
 */
void Garbit(u_int blknum)                                                       // queue a block for garb
{
    int i;                                                                      // a handy int

    i = partab.vol[volnum]->garbQw;                                             // where to put it

    for (int j = 0; ; j++) {
        if (partab.vol[volnum]->garbQ[i] == 0) break;                           // if slot available then exit
        if (j == 9) panic("Garbit: could not get a garbage slot after 10 seconds");
        sleep(1);                                                               // wait a bit
    }                                                                           // NOTE: I don't think this can work either

    partab.vol[volnum]->garbQ[i] = blknum;                                      // stuff it in
    partab.vol[volnum]->garbQw = (i + 1) & (NUM_GARB - 1);                      // reset pointer
    return;                                                                     // and exit
}

/*
 * Function: Free_block
 * Summary:  Remove the specified block from the map
 * Input(s): Block number
 * Return:   None
 * Note:     Must hold a write lock before calling this function
 */
void Free_block(u_int blknum)                                                   // free block in map
{
    int    i;                                                                   // a handy int
    int    off;                                                                 // and another
    u_char *map;                                                                // map pointer

    map = (u_char *) SOA(partab.vol[volnum]->map);                              // point at it
    i = blknum >> 3;                                                            // map byte
    off = blknum & 7;                                                           // bit number
    off = 1U << off;                                                            // convert to mask
    if ((map[i] & off) == 0) return;                                            // if it's already free then just exit
    map[i] &= ~off;                                                             // clear the bit

    if (SOA(partab.vol[volnum]->first_free) > (void *) &map[i]) {               // if earlier
        partab.vol[volnum]->first_free = &SBA(map)[i];                          // reset first free
    }

    partab.vol[volnum]->stats.blkdeall++;                                       // update stats
    partab.vol[volnum]->map_dirty_flag++;                                       // mark map dirty
    return;                                                                     // and exit
}

/*
 * Function: Used_block
 * Summary:  Add the specified block to the map
 * Input(s): Block number
 * Return:   None
 * Note:     Must hold a write lock before calling this function
 *           The caller must have ensured that, if there is a map
 *           scan in progress, this block is less than "upto"
 * This is only called from rsm/database/view.c
 */
void Used_block(u_int blknum)                                                   // set block in map
{
    int    i;                                                                   // a handy int
    int    off;                                                                 // and another
    u_char *map;                                                                // map pointer

    map = (u_char *) SOA(partab.vol[volnum]->map);                              // point at it
    i = blknum >> 3;                                                            // map byte
    off = blknum & 7;                                                           // bit number
    off = 1U << off;                                                            // convert to mask
    if (map[i] & off) return;                                                   // if it's already used then just exit
    map[i] |= off;                                                              // set the bit
    partab.vol[volnum]->stats.blkalloc++;                                       // update stats
    partab.vol[volnum]->map_dirty_flag++;                                       // mark map dirty
    return;                                                                     // and exit
}

/*
 * Function: Tidy_block
 * Summary:  Tidy the current block
 * Input(s): None
 * Return:   None
 * Note:     Must hold a write lock before calling this function
 *           This function omits records with dbc = NODE_UNDEFINED
 *           This function omits pointers with record = PTR_UNDEFINED
 */
void Tidy_block(void)                                                           // tidy current block
{
    gbd      *ptr;                                                              // a handy pointer
    DB_Block *btmp;                                                             // ditto

    ptr = blk[level];                                                           // remember current
    Get_GBD();                                                                  // get another
    memset(SOM(blk[level]->mem), 0, SOA(partab.vol[volnum]->vollab)->block_size); // zot
    SOA(blk[level]->mem)->type = SOA(ptr->mem)->type;                           // copy type
    if (!level) SOA(blk[level]->mem)->type |= 64;                               // if it's a global directory then ensure it's data
    SOA(blk[level]->mem)->right_ptr = SOA(ptr->mem)->right_ptr;                 // copy RL
    VAR_COPY(SOA(blk[level]->mem)->global, SOA(ptr->mem)->global);              // copy global name
    SOA(blk[level]->mem)->last_idx = IDX_START - 1;                             // unused block
    SOA(blk[level]->mem)->last_free = (SOA(partab.vol[volnum]->vollab)->block_size >> 2) - 1; // set this up
    Copy_data(ptr, IDX_START);                                                  // copy entire block
    btmp = blk[level]->mem;                                                     // save this
    blk[level]->mem = ptr->mem;                                                 // copy in this
    ptr->mem = btmp;                                                            // end swap 'mem'
    Free_GBD(blk[level]);                                                       // release it
    blk[level] = ptr;                                                           // restore the pointer
    idx = (u_short *) SOA(blk[level]->mem);                                     // set this up
    iidx = (int *) SOA(blk[level]->mem);                                        // and this
    return;                                                                     // and exit
}

/*
 * Function: Copy_data
 * Summary:  Copy data from "fptr" to blk[level]
 * Input(s): From GBD and index (or flag)
 * Return:   None
 * Note:     Must hold a write lock before calling this function
 *           All external variables describing blk[level] must be setup
 *           This function omits records with dbc = NODE_UNDEFINED
 *           This function omits pointers with record = PTR_UNDEFINED
 */
void Copy_data(gbd *fptr, int fidx)                                             // copy records
{
    u_short *sfidx;                                                             // for Indexes
    int     *fiidx;                                                             // int ver of Index
    u_char  fk[MAX_KEY_SIZE + 5];                                               // for keys
    int     isdata;                                                             // a flag
    cstring *c;                                                                 // reading from old
    u_char  ccc;                                                                // common char count
    u_char  ucc;                                                                // uncommon char count
    int     cs;                                                                 // new chunk size

    isdata = ((SOA(blk[level]->mem)->type > 64) && level);                      // block type
    sfidx = (u_short *) SOA(fptr->mem);                                         // point at it
    fiidx = (int *) SOA(fptr->mem);                                             // point at it
    keybuf[0] = 0;                                                              // clear this

    for (int i = IDX_START; i <= SOA(blk[level]->mem)->last_idx; i++) {         // scan to end to block
        chunk = (cstring *) &iidx[idx[i]];                                      // point at the chunk
        memcpy(&keybuf[chunk->buf[0] + 1], &chunk->buf[2], chunk->buf[1]);      // update the key
        keybuf[0] = chunk->buf[0] + chunk->buf[1];                              // and the size
    }

    for (int i = IDX_START; i <= SOA(fptr->mem)->last_idx; i++) {               // for each Index
        c = (cstring *) &fiidx[sfidx[i]];                                       // point at chunk
        memcpy(&fk[c->buf[0] + 1], &c->buf[2], c->buf[1]);                      // copy key
        fk[0] = c->buf[0] + c->buf[1];                                          // and the length
        if (i < fidx) continue;                                                 // copy this one? no - just continue
        c = (cstring *) &c->buf[c->buf[1] + 2];                                 // point at dbc/ptr

        if (isdata) {                                                           // if data
            if (c->len == NODE_UNDEFINED) continue;                             // junk record? then ignore it
        } else {                                                                // if a pointer
            if ((long) c & 3) c = (cstring *) ((long) c + (4 - ((long) c & 3))); // if not aligned then align
            if ((*(int *) c) == PTR_UNDEFINED) continue;                        // see if that's junk then ignore it
        }

        ccc = 0;                                                                // start here

        if (fk[0] && keybuf[0]) {                                               // if any there
            while (fk[ccc + 1] == keybuf[ccc + 1]) {                            // while the same
                if ((ccc == fk[0]) || (ccc == keybuf[0])) break;                // at end of either, done
                ccc++;                                                          // increment pointer
            }
        }

        ucc = fk[0] - ccc;                                                      // get the ucc
        cs = 4 + ucc + (isdata ? (c->len + 2) : 4);                             // chunk size = this
        if (!level) cs += 4;                                                    // if global directory then allow for flags
        if (cs & 3) cs += (4 - (cs & 3));                                       // but, round up

        if (cs >= ((SOA(blk[level]->mem)->last_free * 2 + 1 - SOA(blk[level]->mem)->last_idx) * 2)) {
            if (fidx == -1) return;
            panic("Copy_data: about to overflow block");
        }

        SOA(blk[level]->mem)->last_free -= (cs / 4);                            // reset free
        idx[++SOA(blk[level]->mem)->last_idx] = SOA(blk[level]->mem)->last_free + 1; // point at next chunk
        chunk = (cstring *) &iidx[SOA(blk[level]->mem)->last_free + 1];
        chunk->len = cs;                                                        // set the size
        chunk->buf[0] = ccc;                                                    // ccc
        chunk->buf[1] = ucc;                                                    // ucc
        memcpy(&chunk->buf[2], &fk[ccc + 1], ucc);                              // the key
        record = (cstring *) &chunk->buf[ucc + 2];                              // point at dbc/ptr

        if (isdata) {                                                           // for a data block
            record->len = c->len;                                               // copy dbc
            memcpy(record->buf, c->buf, c->len);                                // copy the data
            if (fidx == -1) c->len = NODE_UNDEFINED;
        } else {                                                                // for a pointer
            Align_record();                                                     // ensure aligned
            *(u_int *) record = *(u_int *) c;                                   // copy pointer
            if (fidx == -1) *(int *) c = PTR_UNDEFINED;

            if (!level) {                                                       // if global directory
                ((u_int *) record)[1] = ((u_int *) c)[1] & 3;                   // copy flags
                // NOTE: ABOVE ALL FLAGS EXCEPT (3) CLEARED !!!!!!!!
            }
        }

        memcpy(keybuf, fk, fk[0] + 1);                                          // save full key
    }                                                                           // end copy loop

    return;                                                                     // and exit
}

/*
 * Function: Align_record
 * Summary:  Ensure that record is on a four byte boundary
 * Input(s): None
 * Return:   None
 * Note:     Must only be called for pointer/directory blocks
 */
void Align_record(void)                                                         // align record (u_int)
{
    if ((long) record & 3) {                                                    // if not aligned
        record = (cstring *) ((long) record + (4 - ((long) record & 3)));       // align
    }

    return;                                                                     // exit
}

/*
 * Function: Compress1
 * Summary:  Compress one block union
 * Input(s): mvar * to the key to find the level to operate at
 * Return:   Zero or error
 */
short Compress1(void)
{
    int i;
    int curlevel;
    int t;

    Get_GBDs(MAXTREEDEPTH * 2);                                                 // ensure this many
    curlevel = level;
    writing = 1;                                                                // flag writing
    t = Get_data(curlevel);                                                     // get the data
    if ((t == -ERRM7) && (!db_var.slen)) t = 0;                                 // if top, it does exist

    if (t == -ERRM7) {                                                          // if gone missing
        while (level >= 0) {
            if (blk[level]->dirty == (gbd *) 1) blk[level]->dirty = NULL;
            level--;
        }

        return 0;                                                               // just exit
    }

    if (t < 0) return (short) t;                                                // any other error then return it

    if (!SOA(blk[level]->mem)->right_ptr) {                                     // if no more blocks
        if ((level == 2) && !db_var.slen) {                                     // and block 1 on level 2
            u_char gtmp[VAR_LEN + 4];                                           // to find glob

            level = 0;                                                          // look at the global directory
            gtmp[1] = 128;                                                      // start string key

            for (i = 0; i < VAR_LEN; i++) {                                     // for each char
                if (db_var.name.var_cu[i] == '\0') break;                       // check for null and break if found
                gtmp[i + 2] = db_var.name.var_cu[i];                            // copy char
            }

            i += 2;                                                             // correct count
            gtmp[i] = '\0';                                                     // null terminate
            gtmp[0] = (u_char) i;                                               // add the count
            t = Locate(gtmp);                                                   // search for it
            if (t < 0) return (short) t;                                        // failed? then return error
            Align_record();                                                     // if not aligned
            *((u_int *) record) = blk[2]->block;                                // new top level block

            if (blk[level]->dirty <= (gbd *) 3) {                               // if it needs queueing
                blk[level]->dirty = SBA(blk[level]);                            // terminate list
                Queit();                                                        // and queue it
            }

            // Now, we totally release the block at level 1 for this global
            SOA(blk[1]->mem)->type = 65;                                        // pretend it's data
            blk[1]->last_accessed = current_time(FALSE);                        // clear last access
            Garbit(blk[1]->block);                                              // queue for freeing
            memset(&partab.jobtab->last_ref, 0, sizeof(mvar));                  // clear last ref
            return 0;                                                           // and exit
        }

        while (level >= 0) {
            if (blk[level]->dirty == (gbd *) 1) blk[level]->dirty = NULL;
            level--;
        }

        return 0;                                                               // just exit
    }

    blk[level + 1] = blk[level];                                                // save that
    t = Get_block(SOA(blk[level]->mem)->right_ptr);

    if (t < 0) {                                                                // if error
        while (level >= 0) {
            if (blk[level]->dirty == (gbd *) 1) blk[level]->dirty = NULL;
            level--;
        }

        return (short) t;                                                       // just exit
    }

    i = ((SOA(blk[level + 1]->mem)->last_free * 2 + 1 - SOA(blk[level + 1]->mem)->last_idx) * 2)
      + ((SOA(blk[level]->mem)->last_free * 2 + 1 - SOA(blk[level]->mem)->last_idx) * 2);

    if (i < 1024) {                                                             // if REALLY not enough space (NOTE: make a param)
        level++;

        while (level >= 0) {
            if (blk[level]->dirty == (gbd *) 1) blk[level]->dirty = NULL;
            level--;
        }

        return (short) t;                                                       // just exit
    }

    Un_key();                                                                   // unkey RL block
    level++;                                                                    // point at left block
    Tidy_block();                                                               // ensure it's tidy
    Copy_data(blk[level - 1], -1);                                              // combine them
    if (blk[level]->dirty == (gbd *) 1) blk[level]->dirty = (gbd *) 2;          // if not queued then mark for queuing
    level--;                                                                    // point at RL
    Tidy_block();                                                               // ensure it's tidy

    if (SOA(blk[level]->mem)->last_idx < IDX_START) {                           // if it's empty
        SOA(blk[level]->mem)->type = 65;                                        // pretend it's data
        blk[level]->last_accessed = current_time(FALSE);                        // clear last access
        SOA(blk[level + 1]->mem)->right_ptr = SOA(blk[level]->mem)->right_ptr;  // copy RL
        Garbit(blk[level]->block);                                              // queue for freeing
        blk[level] = NULL;                                                      // ignore

        if (SOA(blk[level + 1]->mem)->right_ptr) {                              // if we have a RL
            Get_block(SOA(blk[level + 1]->mem)->right_ptr);                     // get it
        }                                                                       // and hope it worked
    } else {
        if (blk[level]->dirty == (gbd *) 1) {                                   // if not queued
            blk[level]->dirty = (gbd *) 2;                                      // mark to queue
            Add_rekey(blk[level]->block, level);                                // queue to re-key later
        }
    }

    if (blk[level] != NULL) {                                                   // if more to go
        if (blk[level]->dirty == (gbd *) 2) {                                   // if some left
            chunk = (cstring *) &iidx[idx[IDX_START]];                          // point at the first
            memcpy(&partab.jobtab->last_ref.slen, &chunk->buf[1], chunk->buf[1] + 1); // save the real key
        }
    } else {
        memset(&partab.jobtab->last_ref, 0, sizeof(mvar));                      // or clear it
    }

    level += 2;                                                                 // spare level
    blk[level] = NULL;                                                          // clear it

    for (i = level - 1; i >= 0; i--) {                                          // scan pointer blocks
        if (blk[i] != NULL) {
            if (blk[i]->dirty == (gbd *) 2) {                                   // if changed
                if (blk[level] == NULL) {                                       // list empty
                    blk[i]->dirty = SBA(blk[i]);                                // point at self
                } else {
                    blk[i]->dirty = SBA(blk[level]);                            // else point at previous
                }

                blk[level] = blk[i];                                            // remember this one
            } else if (blk[i]->dirty == (gbd *) 1) {                            // if reserved
                blk[i]->dirty = NULL;                                           // clear it
            }
        }
    }

    if (blk[level] != NULL) Queit();                                            // if something there, queue that lot
    return Re_key();                                                            // re-key and return
}

/*
 * Function: ClearJournal
 * Summary:  Create/clear the journal file
 * Input(s): Internal volume number
 * Return:   None
 * Note:     Must be called with a write lock
 */
void ClearJournal(int vol)                                                      // clear journal
{
    jrnrec      jj;                                                             // to write with
    int         jfd;                                                            // file descriptor
    label_block *vol_label;                                                     // current volume label
    char        fullpathvol[MAXPATHLEN];                                        // full pathname of vol file
    u_int       magic;
    off_t       free;

    vol_label = SOA(partab.vol[vol]->vollab);
    umask(0);                                                                   // set umask to 0000

    // Create journal file, with 0660 permissions, jobs write their own journal entries
    jfd = open(vol_label->journal_file, O_CREAT | O_TRUNC | O_WRONLY, S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP);

    if (jfd == -1) {                                                            // error
        fprintf(stderr, "Journal %s open error: %d - %s\n", vol_label->journal_file, errno, strerror(errno));
    } else {                                                                    // if OK
        if (realpath(vol_label->journal_file, fullpathvol) != NULL) {           // get full path
            if (strlen(fullpathvol) <= JNL_FILENAME_MAX) {                      // if can fit in our struct
                strcpy(vol_label->journal_file, fullpathvol);                   // copy this full path into the vol_def structure
            } else {                                                            // end if path will fit - otherwise
                int i;

                i = strlen(fullpathvol) - JNL_FILENAME_MAX;                     // copy as much as
                strcpy(vol_label->journal_file, &fullpathvol[i]);               // is possible, thats the best we can do
            }                                                                   // end length testing
        }

        magic = RSM_MAGIC - 1;

        if (write(jfd, (u_char *) &magic, sizeof(magic)) < (long) sizeof(magic)) { // write the journal header magic
            fprintf(stderr, "Journal %s write error: %d - %s\n", vol_label->journal_file, errno, strerror(errno));
        }

#if RSM_DBVER == 1
        free = 20;                                                              // next free byte
#else
        free = 24;                                                              // next free byte
#endif

        if (write(jfd, (u_char *) &free, sizeof(free)) < (long) sizeof(free)) { // write the journal header free
            fprintf(stderr, "Journal %s write error: %d - %s\n", vol_label->journal_file, errno, strerror(errno));
        }

#if RSM_DBVER == 1
        jj.size = 8;
        jj.time = current_time(TRUE);
#else
        jj.size = 12;
        jj.time = (u_int64) current_time(TRUE);
#endif
        jj.action = JRN_CREATE;
        jj.uci = 0;

        if (write(jfd, &jj, jj.size) < jj.size) {                               // write the create record
            fprintf(stderr, "Journal %s write error: %d - %s\n", vol_label->journal_file, errno, strerror(errno));
        }

        close(jfd);                                                             // and close it
        partab.vol[vol]->jrn_next = (off_t) free;                               // where it's upto
    }

    return;                                                                     // done
}

/*
 * Function: DoJournal
 * Summary:  Write a journal record
 * Input(s): Journal recors
 *           Data pointer (set only)
 * Return:   None
 * Note:     Must be called with a write lock and the date/time and size are filled in here
 */
void DoJournal(jrnrec *jj, cstring *data)                                       // Write journal
{
    int i;

    // address to location
    if (lseek(partab.jnl_fds[volnum], partab.vol[volnum]->jrn_next, SEEK_SET) != partab.vol[volnum]->jrn_next) {
        goto fail;                                                              // if failed
    }

    jj->size = 13 + sizeof(var_u) + jj->slen;
    if ((jj->action != JRN_SET) && (jj->action != JRN_KILL)) jj->size = 12;     // not SET or KILL, small size is 12
    i = jj->size;                                                               // store full size, but data is written below
    if (jj->action == JRN_SET) jj->size += (sizeof(short) + data->len);
    jj->time = (u_int64) current_time(TRUE);                                    // store the time
    if (write(partab.jnl_fds[volnum], jj, i) != i) goto fail;                   // write header

    if (jj->action == JRN_SET) {
        i = sizeof(short) + data->len;                                          // data size
        if (write(partab.jnl_fds[volnum], data, i) != i) goto fail;             // write data
    }

    if (jj->size & 3) jj->size += (4 - (jj->size & 3));                         // round it
    partab.vol[volnum]->jrn_next += jj->size;                                   // update next
    if (lseek(partab.jnl_fds[volnum], 4, SEEK_SET) != 4) goto fail;
    if (write(partab.jnl_fds[volnum], &partab.vol[volnum]->jrn_next, sizeof(off_t)) == -1) goto fail; // write next
    return;

fail:
    SOA(partab.vol[volnum]->vollab)->journal_available = 0;                     // turn it off
    close(partab.jnl_fds[volnum]);                                              // close the file
    return;                                                                     // and exit
}

// The following are internal only (first called from $&DEBUG())
void Dump_gbd(void)                                                             // dump GBDs
{
    u_int   i;                                                                  // for loops
    int     j;                                                                  // and another
    u_short len;
    u_short cnt = 0;
    short   s;                                                                  // for function returns
    char    uci;
    gbd     *p;                                                                 // a pointer
    gbd     *f;                                                                 // free pointer
    char    tmp[VAR_LEN + 1];                                                   // some space
    char    type[10];                                                           // for block type
    time_t  t;                                                                  // for time

    s = SemOp(SEM_GLOBAL, SEM_WRITE);                                           // write lock the globals
    if (s < 0) return;                                                          // exit on error
    p = SOA(partab.vol[partab.jobtab->vol - 1]->gbd_head);                      // get listhead
    t = current_time(FALSE);
    printf("Dump of all Global Buffer Descriptors on %s [%lld]\r\n\r\n", strtok(ctime(&t), "\n"), (long long) mktime(gmtime(&t)));

    for (i = 0; i < partab.vol[partab.jobtab->vol - 1]->num_gbd; i++) {         // for all
        if (!p[i].block) continue;                                              // skip empty buffers
        cnt++;
    }

    f = SOA(partab.vol[partab.jobtab->vol - 1]->gbd_hash[GBD_HASH]);
    printf("Global Buffers Free at %p --> %p\r\n", (void *) f, (f == NULL) ? NULL : (void *) SOA(f->mem));
    printf("Using %u of %u Global Buffers\r\n\r\n", cnt, partab.vol[partab.jobtab->vol - 1]->num_gbd);
    printf("       Address   Global Buffer       Block  Right Link  Block Type  Last Access  VOL  UCI  Global Name\r\n");

    for (i = 0; i < partab.vol[partab.jobtab->vol - 1]->num_gbd; i++) {         // for all
        if (!p[i].block) continue;                                              // skip empty buffers
        for (j = 0; j < VAR_LEN; j++) tmp[j] = ' ';                             // space fill tmp[]

        for (j = 0; j < VAR_LEN; j++) {
            if (SOA(p[i].mem)->global.var_cu[j] == 0) break;
            tmp[j] = SOA(p[i].mem)->global.var_cu[j];
        }

        tmp[j] = '\0';                                                          // null terminate name

        len = snprintf(type, 10, "%s", (!strncmp(tmp, "$GLOBAL\0", 8)) ? "Directory" :
              (SOA(p[i].mem)->type > UCIS) ? "Data" : "Pointer");

        type[len] = '\0';
        uci = SOA(p[i].mem)->type % 64;
        t = p[i].last_accessed;
        if (t > 0) t = mktime(gmtime(&t));

        printf("%14p %15p %11u %11u %11s %12lld %4d %4d  %s\r\n", (void *) &p[i], (void *) SOA(p[i].mem),
               p[i].block, SOA(p[i].mem)->right_ptr, type, (long long) t, partab.jobtab->vol, uci, tmp);
    }

    SemOp(SEM_GLOBAL, -SEM_WRITE);                                              // unlock the globals
    return;                                                                     // and exit
}
