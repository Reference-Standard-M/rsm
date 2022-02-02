/*
 * Package:  Reference Standard M
 * File:     rsm/database/uci.c
 * Summary:  module database - Database Functions, UCI manipulation
 *
 * David Wicksell <dlw@linux.com>
 * Copyright © 2020-2022 Fourth Watch Software LC
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
#include <string.h>                                                             // for bcopy
#include <strings.h>
#include <unistd.h>                                                             // for file reading
#include <ctype.h>                                                              // for GBD stuff
#include <sys/types.h>                                                          // for semaphores
#include <sys/ipc.h>                                                            // for semaphores
#include <sys/sem.h>                                                            // for semaphores
#include "rsm.h"                                                                // standard includes
#include "database.h"                                                           // database protos
#include "proto.h"                                                              // standard prototypes
#include "error.h"                                                              // error strings

/*
 * Function: DB_UCISet
 * Descript: Create UCI
 * Input(s): Vol#
 *           UCI#
 *           UCI Name
 * Return:   0 -> Ok, negative M error
 */
short DB_UCISet(int vol, int uci, var_u name)                                   // set UCI name
{
    short s;                                                                    // for functions

    if ((vol > MAX_VOL) || (vol < 1)) return -ERRM26;                           // within limits? no - error
    if ((uci > UCIS) || (uci < 1)) return -ERRM26;                              // too big
    volnum = vol;                                                               // set this

    while (systab->vol[volnum - 1]->writelock) {                                // check for write lock
        (void) sleep(5);                                                        // wait a bit
        if (partab.jobtab->attention) return -(ERRZ51 + ERRZLAST);              // for <Control><C>
    }                                                                           // end writelock check

    writing = 1;                                                                // writing
    level = 0;                                                                  // clear this
    s = SemOp(SEM_GLOBAL, WRITE);                                               // get write lock
    if (s < 0) return s;                                                        // on error then return it

    if (systab->vol[vol - 1] == NULL) {                                         // is it mounted?
        SemOp(SEM_GLOBAL, -curr_lock);
        return -ERRM26;                                                         // no - error
    }

    if (!systab->vol[vol - 1]->vollab->uci[uci-1].global) {                     // if no GD
        s = New_block();                                                        // get a new block

        if (s < 0) {                                                            // if failed
            SemOp(SEM_GLOBAL, -curr_lock);
            return s;                                                           // error
        }

        systab->vol[vol - 1]->vollab->uci[uci - 1].global = blk[level]->block;  // save block #
        blk[level]->mem->type = uci + 64;                                       // block type
        blk[level]->mem->last_idx = IDX_START;                                  // one index
        VAR_CLEAR(blk[level]->mem->global);
        bcopy("$GLOBAL", &blk[level]->mem->global, 7);                          // the global
        blk[level]->mem->last_free = (systab->vol[volnum - 1]->vollab->block_size >> 2) - 7; // use 6 words
        idx[IDX_START] = blk[level]->mem->last_free + 1;                        // the data
        chunk = (cstring *) &iidx[idx[IDX_START]];                              // point at it
        chunk->len = 24;                                                        // 5 words
        chunk->buf[0] = 0;                                                      // zero ccc
        chunk->buf[1] = 9;                                                      // ucc
        bcopy("\200$GLOBAL\0", &chunk->buf[2], 9);                              // the key
        record = (cstring *) &chunk->buf[chunk->buf[1] + 2];                    // setup record ptr
        Align_record();                                                         // align it
        *(u_int *) record = blk[level]->block;                                  // point at self
        bzero(&record->buf[2], sizeof(u_int));                                  // zero flags
        blk[level]->dirty = blk[level];                                         // setup for write
        Queit();                                                                // queue for write
    }                                                                           // end new block code

    VAR_COPY(systab->vol[vol - 1]->vollab->uci[uci - 1].name, name);            // set the new name
    SemOp(SEM_GLOBAL, -curr_lock);
    return 0;                                                                   // and exit
}

/*
 * Function: DB_UCIKill
 * Descript: Remove UCI
 * Input(s): Vol#
 *           UCI#
 * Return:   0 -> Ok, negative M error
 */
short DB_UCIKill(int vol, int uci)                                              // kill UCI entry
{
    short s;                                                                    // for functions
    u_int gb;                                                                   // block number

    if ((vol > MAX_VOL) || (vol < 1)) return -ERRM26;                           // within limits? no - error
    if ((uci > UCIS) || (uci < 1)) return -ERRM26;                              // too big
    volnum = vol;                                                               // set this

    while (systab->vol[volnum - 1]->writelock) {                                // check for write lock
        (void) sleep(5);                                                        // wait a bit
        if (partab.jobtab->attention) return -(ERRZ51 + ERRZLAST);              // for <Control><C>
    }                                                                           // end writelock check

    writing = 1;                                                                // writing
    level = 0;                                                                  // clear this
    s = SemOp(SEM_GLOBAL, WRITE);                                               // get write lock
    if (s < 0) return s;                                                        // on error, return it

    if (systab->vol[vol - 1] == NULL) {                                         // is it mounted?
        SemOp(SEM_GLOBAL, -curr_lock);
        return -ERRM26;                                                         // no - error
    }

    if (systab->vol[vol - 1]->vollab->uci[uci-1].name.var_cu[0] == '\0') {      // does UCI exits?
        SemOp(SEM_GLOBAL, -curr_lock);
        return 0;                                                               // no - just return
    }

    gb = systab->vol[vol - 1]->vollab->uci[uci - 1].global;                     // get global directory
    s = Get_block(gb);                                                          // get the block

    if (s < 0) {
        SemOp(SEM_GLOBAL, -curr_lock);
        return s;                                                               // error
    }

    if (blk[level]->dirty == (gbd *) 1) blk[level]->dirty = NULL;               // if reserved then clear it

    if (blk[level]->mem->last_idx > IDX_START) {                                // if any globals
        SemOp(SEM_GLOBAL, -curr_lock);
        return -ERRM29;                                                         // no can do
    }

    systab->vol[vol - 1]->vollab->uci[uci - 1].global = 0;                      // clear this
    VAR_CLEAR(systab->vol[vol - 1]->vollab->uci[uci - 1].name);                 // and this
    systab->vol[vol - 1]->map_dirty_flag = 1;                                   // mark map dirty
    blk[level]->mem->last_idx = IDX_START - 1;                                  // say no index
    Garbit(gb);                                                                 // garbage it
    bzero(&systab->last_blk_used[0], systab->maxjob * sizeof(int));             // zot all
    SemOp(SEM_GLOBAL, -curr_lock);
    return 0;                                                                   // exit
}
