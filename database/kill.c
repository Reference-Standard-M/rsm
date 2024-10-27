/*
 * Package: Reference Standard M
 * File:    rsm/database/kill.c
 * Summary: module database - database functions, kill
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
#include <unistd.h>                                                             // for file reading
#include <ctype.h>                                                              // for GBD stuff
#include <sys/types.h>                                                          // leopard seems to want this
#include "rsm.h"                                                                // standard includes
#include "database.h"                                                           // database protos
#include "proto.h"                                                              // standard prototypes
#include "error.h"                                                              // error strings

/*
 * Function: Kill_data
 * Summary:  Remove the sub-tree described by db_var
 * Input(s): None
 * Return:   0 -> Ok, negative M error
 */
short Kill_data(void)                                                           // remove tree
{
    int     t;                                                                  // for funcs
    int     i;                                                                  // a handy int
    u_int   j;                                                                  // a handy unsigned int
    gbd     *rblk[MAXTREEDEPTH];                                                // right side tree
    gbd     *leftblk;                                                           // save left side tree
    gbd     *ptr;                                                               // spare pointer
    int     rlevel;                                                             // level in rblk[]
    u_int   blknum;                                                             // for block numbers
    u_char  tmp[VAR_LEN + 4];                                                   // spare string
    int     top;                                                                // top in complex kill
    u_char  *p;                                                                 // a handy pointer
    cstring *c;                                                                 // and another

    memset(rekey_blk, 0, MAXREKEY * sizeof(u_int));                             // clear that table
    memset(rekey_lvl, 0, MAXREKEY * sizeof(int));                               // and that table
    SemOp(SEM_GLOBAL, -curr_lock);                                              // release read lock
    systab->last_blk_used[LBU_OFF(volnum)] = 0;                                 // clear last

start:
    Get_GBDs(MAXTREEDEPTH * 2);                                                 // ensure this many
    j = 0;                                                                      // clear counter

    for (i = 0; i < NUM_GARB; i++) {
        if (partab.vol[volnum]->garbQ[i] == 0) {
            if (j++ >= (NUM_GARB / 2)) goto cont;                               // ensure we have 1/2 table
        }
    }

    SemOp(SEM_GLOBAL, -curr_lock);                                              // release current lock
    sleep(1);
    goto start;

cont:
    writing = 1;                                                                // say we are killing
    level = 0;                                                                  // reset level
    t = Get_data(0);                                                            // attempt to get it
    if ((t < 0) && (t != -ERRM7)) return (short) t;                             // error, not undef then return it

    if ((SOA(partab.vol[volnum]->vollab)->journal_available) &&
      (SOA(partab.vol[volnum]->vollab)->journal_requested) &&
      (partab.jobtab->last_block_flags & GL_JOURNAL)) {                         // if journaling
        jrnrec jj;                                                              // jrn structure
        jj.action = JRN_KILL;                                                   // doing kill
        jj.uci = db_var.uci;                                                    // copy UCI
        VAR_COPY(jj.name, db_var.name);                                         // global name
        jj.slen = db_var.slen;                                                  // subs length
        memcpy(jj.key, db_var.key, jj.slen);                                    // copy key
        DoJournal(&jj, NULL);                                                   // and do it
    }

    if (db_var.slen == 0) {                                                     // full global kill?
        while (level) {                                                         // for each level
            if (blk[level]->dirty == (gbd *) 1) blk[level]->dirty = NULL;       // if reserved then clear it
            level--;                                                            // up a level
        }

        tmp[1] = 128;                                                           // start string key

        for (i = 0; i < VAR_LEN; i++) {                                         // for each char
            if (db_var.name.var_cu[i] == '\0') break;                           // check for null and break if found
            tmp[i + 2] = db_var.name.var_cu[i];                                 // copy char
        }

        i += 2;                                                                 // correct count
        tmp[i] = '\0';                                                          // null terminate
        tmp[0] = (u_char) i;                                                    // add the count
        t = Locate(tmp);                                                        // search for it

        if (t == -ERRM7) {
            if (blk[level]->dirty == (gbd *) 1) blk[level]->dirty = NULL;       // if reserved then clear it
            return 0;                                                           // nothing to do
        }

        Align_record();                                                         // align
        blknum = *(u_int *) record;                                             // remember the block
        *(u_int *) record = PTR_UNDEFINED;                                      // mark as junk
        Tidy_block();                                                           // and tidy it

        if (blk[level]->dirty == (gbd *) 1) {                                   // if reserved
            blk[level]->dirty = SBA(blk[level]);                                // set it
            Queit();                                                            // and queue for write
        }

        Garbit(blknum);                                                         // garbage the block
        memset(&systab->last_blk_used[0], 0, systab->maxjob * sizeof(u_int) * MAX_VOL); // zot all
        level--;                                                                // backup a level
        return 0;                                                               // and exit
    }                                                                           // end full kill

    systab->last_blk_used[LBU_OFF(volnum)] = 0;                                 // clear last

    while (level >= 0) {                                                        // what we just got
        if (blk[level]->dirty == (gbd *) 1) blk[level]->dirty = NULL;           // if reserved then clear it
        level--;                                                                // up a level
    }

    db_var.key[db_var.slen++] = 255;                                            // modify key
    t = Get_data(0);                                                            // attempt to get it
    if (t != -ERRM7) return -(ERRZ61 + ERRMLAST);                               // must be undefined so database stuffed
    db_var.slen--;                                                              // put count back
    rlevel = level;                                                             // number in right side
    for (i = 0; i <= level; i++) rblk[i] = blk[i];                              // for each level, copy GBD
    level = 0;                                                                  // reset level
    systab->last_blk_used[LBU_OFF(volnum)] = 0;                                 // clear last
    t = Get_data(-1);                                                           // get left side

    if ((t < 0) && (t != -ERRM7)) {                                             // error, not undef
        return (short) t;                                                       // return it
    }                                                                           // WARNING: This leaves blocks reserved

    if (rlevel != level) panic("Kill_data: left level not equal to right level"); // check this, if not correct, die

    for (level = 0; level < rlevel; level++) {                                  // scan the levels
        if (blk[level + 1] != rblk[level + 1]) break;                           // check following level and end loop
    }

    if (level == rlevel) {                                                      // all in 1 data block
        // NEVER first one
        i = Index;                                                              // start here

        while (i <= SOA(blk[level]->mem)->last_idx) {                           // while in block
            chunk = (cstring *) &iidx[idx[i]];                                  // point at the chunk
            memcpy(&keybuf[chunk->buf[0] + 1], &chunk->buf[2], chunk->buf[1]);  // fix the key
            keybuf[0] = chunk->buf[0] + chunk->buf[1];                          // and the size

            if ((keybuf[0] < db_var.slen) || (memcmp(&keybuf[1], &db_var.key, db_var.slen) != 0)) { // new key too small - different
                break;                                                          // quit loop
            }

            record = (cstring *) &chunk->buf[chunk->buf[1] + 2];                // point at record
            record->len = NODE_UNDEFINED;                                       // mark not required
            i++;                                                                // point at next
        }                                                                       // end removing recs

        Tidy_block();                                                           // tidy the block

        if (blk[level]->dirty == (gbd *) 1) {                                   // if reserved
            blk[level]->dirty = SBA(blk[level]);                                // set it
            Queit();                                                            // and queue for write
        }

        for (level = 0; level < rlevel; level++) {                              // scan the levels
            if (blk[level]->dirty == (gbd *) 1) blk[level]->dirty = NULL;       // reserved? if yes, clear it
        }

        return 0;                                                               // and exit
    }                                                                           // end all in 1

    /*
     * We need to do a multi-block kill - we now have:
     * top common block at [level] in both trees
     * left edge -> blk[]           rblk[] <- right edge
     * the bottom level is [rlevel] in both trees
     *
     * Note: It is possible that no killable nodes live in the left edge,
     *       and we will never point at Index IDX_START in the left edge
     *       BUT, the RL may have to be changed.
     */

    top = level;                                                                // save for ron

    for (i = 0; i < top; i++) {                                                 // scan upper bit
        if (blk[i]->dirty == (gbd *) 1) blk[i]->dirty = NULL;                   // reserved? if yes, clear it
    }

    for (level = top; level <= rlevel; level++) {                               // scan left edge
        t = Locate(&db_var.slen);                                               // locate the record

        if ((t < 0) && (t != -ERRM7)) {                                         // error?
            return (short) t;                                                   // give up
        }                                                                       // WARNING: This leaves blocks reserved

        for (i = Index; i <= SOA(blk[level]->mem)->last_idx; i++) {             // scan block
            chunk = (cstring *) &iidx[idx[i]];                                  // point at the chunk
            memcpy(&keybuf[chunk->buf[0] + 1], &chunk->buf[2], chunk->buf[1]);  // update the key
            keybuf[0] = chunk->buf[0] + chunk->buf[1];                          // and the size

            if ((keybuf[0] < db_var.slen) || (memcmp(&keybuf[1], &db_var.key, db_var.slen) != 0)) { // new key too small - different
                break;                                                          // quit loop
            }

            record = (cstring *) &chunk->buf[chunk->buf[1] + 2];                // point at the dbc

            if (level != rlevel) {                                              // if a pointer block
                Align_record();                                                 // align the pointer
                j = *(u_int *) record;                                          // get block#
                if (j != rblk[level + 1]->block) Garbit(j);                     // if not right edge then garbage it
                *(u_int *) record = PTR_UNDEFINED;                              // mark as junk
            } else {                                                            // it's a data block
                record->len = NODE_UNDEFINED;                                   // mark as junk
            }

            SOA(blk[level]->mem)->flags |= BLOCK_DIRTY;                         // mark it so
        }                                                                       // end block scan

        if (SOA(blk[level]->mem)->flags & BLOCK_DIRTY) Tidy_block();            // if we changed it then tidy it
        if (level > top) SOA(blk[level]->mem)->right_ptr = rblk[level]->block;  // not at top so hook to right edge
    }                                                                           // end left edge scan

    for (level = rlevel; level > top; level--) {                                // scan right edge (up)
        leftblk = blk[level];                                                   // save left here
        blk[level] = rblk[level];                                               // get right one
        idx = (u_short *) SOA(blk[level]->mem);                                 // point at the block
        iidx = (int *) SOA(blk[level]->mem);                                    // point at the block
        Index = IDX_START;                                                      // start at the start

        while (Index <= SOA(blk[level]->mem)->last_idx) {                       // scan the block
            chunk = (cstring *) &iidx[idx[Index]];                              // point at the chunk
            memcpy(&keybuf[chunk->buf[0] + 1], &chunk->buf[2], chunk->buf[1]);  // update the key
            keybuf[0] = chunk->buf[0] + chunk->buf[1];                          // and the size

            if ((keybuf[0] < db_var.slen) || (memcmp(&keybuf[1], &db_var.key, db_var.slen) != 0)) { // new key too small - different
                break;                                                          // quit loop
            }

            record = (cstring *) &chunk->buf[chunk->buf[1] + 2];                // point at the dbc

            if (level != rlevel) {                                              // if a pointer block
                Align_record();                                                 // align the pointer
                j = *(u_int *) record;                                          // get block#

                if (rblk[level + 1] != NULL) {                                  // if there is level up
                    if (j != rblk[level + 1]->block) Garbit(j);                 // if not right edge then garbage it
                } else {                                                        // no level up
                    Garbit(j);                                                  // garbage it anyway
                }

                *(u_int *) record = PTR_UNDEFINED;                              // mark as junk
            } else {                                                            // it's data
                record->len = NODE_UNDEFINED;                                   // mark as junk
            }

            SOA(blk[level]->mem)->flags |= BLOCK_DIRTY;                         // mark it so
            Index++;                                                            // next
        }                                                                       // end block scan

        if (SOA(blk[level]->mem)->flags & BLOCK_DIRTY) Tidy_block();            // if we changed it then tidy it

        if ((level < rlevel) && (rblk[level + 1] != NULL)) {                    // if in a pointer block AND is lower level
            idx = (u_short *) SOA(rblk[level + 1]->mem);                        // point at the block
            iidx = (int *) SOA(rblk[level + 1]->mem);                           // point at the block
            chunk = (cstring *) &iidx[idx[IDX_START]];                          // point at first chunk
            p = &chunk->buf[1];                                                 // point at the key
            t = Locate(p);                                                      // see if it's there

            if (t == -ERRM7) {                                                  // if it isn't
                c = (cstring *) tmp;                                            // point at this
DISABLE_WARN(-Warray-bounds)
                c->len = 4;                                                     // the size
ENABLE_WARN
                memcpy(c->buf, &rblk[level + 1]->block, sizeof(u_int));         // point the int here, get the block#
                t = Insert(p, c);                                               // insert the node

                if (t == -(ERRZ62 + ERRMLAST)) {
                    Add_rekey(rblk[level + 1]->block, level + 1);               // do it later
                } else if (t < 0) {
                    return (short) t;                                           // error!
                }
            }
        }                                                                       // end of insert pointer

        if ((((u_long) ((SOA(leftblk->mem)->last_free * 2 + 1 - SOA(leftblk->mem)->last_idx) * 2)
          + ((SOA(blk[level]->mem)->last_free * 2 + 1 - SOA(blk[level]->mem)->last_idx) * 2))
          > (SOA(partab.vol[volnum]->vollab)->block_size - sizeof(DB_Block)))   // if will fit in 1
          || (SOA(blk[level]->mem)->last_idx < IDX_START)) {                    // or empty block
            ptr = blk[level];                                                   // right edge
            blk[level] = leftblk;                                               // left edge
            idx = (u_short *) SOA(blk[level]->mem);                             // point at the block
            iidx = (int *) SOA(blk[level]->mem);                                // point at the block
            if (SOA(ptr->mem)->last_idx > (IDX_START - 1)) Copy_data(ptr, IDX_START); // if any data then copy to left edge
            SOA(blk[level]->mem)->right_ptr = SOA(ptr->mem)->right_ptr;         // copy right pointer
            SOA(ptr->mem)->type = 65;                                           // say type = data!!
            ptr->last_accessed = current_time(FALSE);                           // clear last access
            Garbit(ptr->block);                                                 // dump the block
            rblk[level] = NULL;                                                 // mark gone
        }                                                                       // end move to one

        blk[level] = leftblk;                                                   // restore left edge
    }                                                                           // end right edge scan

    // Now ensure that the right edge has a pointer in [top] - (level == top)
    if (rblk[top + 1] != NULL) {                                                // and there is level+1
        idx = (u_short *) SOA(rblk[top + 1]->mem);                              // point at the block
        iidx = (int *) SOA(rblk[top + 1]->mem);                                 // point at the block
        chunk = (cstring *) &iidx[idx[IDX_START]];                              // point at the chunk
        p = &chunk->buf[1];                                                     // point at the key
        t = Locate(p);                                                          // see if it's there

        if (t == -ERRM7) {                                                      // if it isn't
            c = (cstring *) tmp;                                                // point at this
DISABLE_WARN(-Warray-bounds)
            c->len = 4;                                                         // the size
ENABLE_WARN
            memcpy(c->buf, &rblk[level + 1]->block, sizeof(u_int));             // point the int here, get the block#
            t = Insert(p, c);                                                   // insert the node

            if (t == -(ERRZ62 + ERRMLAST)) {
                Add_rekey(rblk[level + 1]->block, level + 1);                   // do it later
            } else if (t < 0) {
                return (short) t;                                               // error!
            }
        }                                                                       // end of insert pointer
    }                                                                           // end pointer level

    level = MAXTREEDEPTH - 1;                                                   // a useful level
    blk[level] = NULL;                                                          // clear this

    for (i = top; i <= rlevel; i++) {                                           // scan left list
        if (blk[i]->dirty == (gbd *) 1) {                                       // reserved?
            if (blk[level] == NULL) {                                           // if list not started
                blk[i]->dirty = SBA(blk[i]);                                    // point at self
            } else {                                                            // end start of list // just add it in
                blk[i]->dirty = SBA(blk[level]);                                // point at previous
            }

            blk[level] = blk[i];                                                // remember this one
        }
    }                                                                           // end scan

    for (i = top + 1; i <= rlevel; i++) {                                       // scan right list
        if (rblk[i] != NULL) {                                                  // if anything there
            if (rblk[i]->dirty == (gbd *) 1) {                                  // reserved?
                if (blk[level] == NULL) {                                       // if list not started
                    rblk[i]->dirty = SBA(rblk[i]);                              // point at self
                } else {                                                        // end start of list - just add it in
                    rblk[i]->dirty = SBA(blk[level]);                           // point at previous
                }

                blk[level] = rblk[i];                                           // remember this one
            }
        }                                                                       // end not NULL
    }                                                                           // end scan

    if (blk[level] != NULL) {                                                   // anything to queue
        Queit();                                                                // yes - do so
    }                                                                           // end right edge stuff

    memset(&systab->last_blk_used[0], 0, systab->maxjob * sizeof(u_int) * MAX_VOL); // zot all
    return Re_key();                                                            // re-key and return
}
