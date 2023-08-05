/*
 * Package:  Reference Standard M
 * File:     rsm/database/uci.c
 * Summary:  module database - database functions, UCI manipulation
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
#include <string.h>                                                             // for memcpy
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
 * Summary:  Create UCI
 * Input(s): Vol#
 *           UCI#
 *           UCI Name
 * Return:   0 -> Ok, negative M error
 */
short DB_UCISet(int vol, int uci, var_u name)                                   // set UCI name
{
    short s;                                                                    // for functions

    if ((vol < 1) || (vol > MAX_VOL)) return -ERRM26;                           // not within volume range
    if ((uci < 1) || (uci > (UCIS - 1))) return -ERRM26;                        // not within UCI range
    if (systab->vol[vol - 1] == NULL) return -ERRM26;                           // volume not mounted

    for (int i = 0; i < (UCIS - 1); i++) {                                      // make sure no other UCIs with the same name
        if (i == (uci - 1)) continue;                                           // allow renaming UCIs

        if (var_equal(name, systab->vol[vol - 1]->vollab->uci[i].name)) {
            return -(ERRZ12 + ERRMLAST);                                        // exit with error
        }
    }

    while (systab->vol[vol - 1]->writelock) {                                   // check for write lock
        sleep(1);                                                               // wait a bit
        if (partab.jobtab->attention) return -(ERRZ51 + ERRZLAST);              // for <Control><C>
    }                                                                           // end writelock check

    s = SemOp(SEM_GLOBAL, WRITE);                                               // get write lock
    if (s < 0) return s;                                                        // on error then return it

    if (systab->vol[vol - 1] == NULL) {                                         // is it mounted?
        SemOp(SEM_GLOBAL, -curr_lock);
        return -ERRM26;                                                         // no - error
    }

    volnum = vol;                                                               // set this
    writing = 1;                                                                // writing
    level = 0;                                                                  // clear this

    if (!systab->vol[vol - 1]->vollab->uci[uci - 1].global) {                   // if no GD
        s = New_block();                                                        // get a new block

        if (s < 0) {                                                            // if failed
            SemOp(SEM_GLOBAL, -curr_lock);
            return s;                                                           // error
        }

        systab->vol[vol - 1]->vollab->uci[uci - 1].global = blk[level]->block;  // save block #
        blk[level]->mem->type = uci + 64;                                       // block type
        blk[level]->mem->last_idx = IDX_START;                                  // one index
        VAR_CLEAR(blk[level]->mem->global);
        memcpy(&blk[level]->mem->global, "$GLOBAL", 7);                         // the global
        blk[level]->mem->last_free = (systab->vol[vol - 1]->vollab->block_size >> 2) - 7; // minus extra for rec length
        idx[IDX_START] = blk[level]->mem->last_free + 1;                        // the data
        chunk = (cstring *) &iidx[idx[IDX_START]];                              // point at it
        chunk->len = 24;                                                        // 5 words
        chunk->buf[0] = 0;                                                      // zero ccc
        chunk->buf[1] = 9;                                                      // ucc
        memcpy(&chunk->buf[2], "\200$GLOBAL\0", 9);                             // the key
        record = (cstring *) &chunk->buf[chunk->buf[1] + 2];                    // setup record ptr
        Align_record();                                                         // align it
        *(u_int *) record = blk[level]->block;                                  // point at self
        memset(&record->buf[2], 0, sizeof(u_int));                              // zero flags
        blk[level]->dirty = blk[level];                                         // setup for write
        Queit();                                                                // queue for write
    }                                                                           // end new block code

    VAR_COPY(systab->vol[vol - 1]->vollab->uci[uci - 1].name, name);            // set the new name
    systab->vol[vol - 1]->map_dirty_flag = 1;                                   // mark map dirty
    SemOp(SEM_GLOBAL, -curr_lock);
    return 0;                                                                   // and exit
}

/*
 * Function: DB_UCIKill
 * Summary:  Remove UCI
 * Input(s): Vol#
 *           UCI#
 * Return:   0 -> Ok, negative M error
 */
short DB_UCIKill(int vol, int uci)                                              // kill UCI entry
{
    short s;                                                                    // for functions
    u_int gb;                                                                   // block number

    if ((vol < 1) || (vol > MAX_VOL)) return -ERRM26;                           // not within volume range
    if ((uci < 1) || (uci > (UCIS - 1))) return -ERRM26;                        // not within UCI range
    if (systab->vol[vol - 1] == NULL) return -ERRM26;                           // volume not mounted

    while (systab->vol[vol - 1]->writelock) {                                   // check for write lock
        sleep(1);                                                               // wait a bit
        if (partab.jobtab->attention) return -(ERRZ51 + ERRZLAST);              // for <Control><C>
    }                                                                           // end writelock check

    s = SemOp(SEM_GLOBAL, WRITE);                                               // get write lock
    if (s < 0) return s;                                                        // on error, return it

    if (systab->vol[vol - 1] == NULL) {                                         // is it mounted?
        SemOp(SEM_GLOBAL, -curr_lock);
        return -ERRM26;                                                         // no - error
    }

    if (systab->vol[vol - 1]->vollab->uci[uci - 1].name.var_cu[0] == '\0') {    // does UCI exits?
        SemOp(SEM_GLOBAL, -curr_lock);
        return 0;                                                               // no - just return
    }

    volnum = vol;                                                               // set this
    writing = 1;                                                                // writing
    level = 0;                                                                  // clear this
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
    memset(&systab->last_blk_used[0], 0, systab->maxjob * sizeof(int) * MAX_VOL); // zot all
    SemOp(SEM_GLOBAL, -curr_lock);
    return 0;                                                                   // exit
}
