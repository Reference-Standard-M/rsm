/*
 * Package: Reference Standard M
 * File:    rsm/database/uci.c
 * Summary: module database - database functions, UCI manipulation
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

        if (var_equal(name, SOA(partab.vol[vol - 1]->vollab)->uci[i].name)) {
            return -(ERRZ12 + ERRMLAST);                                        // exit with error
        }
    }

    while (partab.vol[vol - 1]->writelock) {                                    // check for write lock
        sleep(1);                                                               // wait a bit
        if (partab.jobtab->attention) return -(ERRZ51 + ERRZLAST);              // for <Control-C>
    }                                                                           // end writelock check

    s = SemOp(SEM_GLOBAL, SEM_WRITE);                                           // get write lock
    if (s < 0) return s;                                                        // on error then return it

    if (systab->vol[vol - 1] == NULL) {                                         // is it mounted?
        SemOp(SEM_GLOBAL, -curr_lock);
        return -ERRM26;                                                         // no - error
    }

    volnum = vol - 1;                                                           // set this
    writing = 1;                                                                // writing
    level = 0;                                                                  // clear this

    if (!SOA(partab.vol[vol - 1]->vollab)->uci[uci - 1].global) {               // if no global directory
        s = New_block();                                                        // get a new block

        if (s < 0) {                                                            // if failed
            SemOp(SEM_GLOBAL, -curr_lock);
            return s;                                                           // error
        }

        SOA(partab.vol[vol - 1]->vollab)->uci[uci - 1].global = blk[level]->block; // save block #
        SOA(blk[level]->mem)->type = uci + 64;                                  // block type
        SOA(blk[level]->mem)->last_idx = IDX_START;                             // one index
        VAR_CLEAR(SOA(blk[level]->mem)->global);
        memcpy(&SOA(blk[level]->mem)->global, "$GLOBAL", 7);                    // the global

        // minus extra for record length
        SOA(blk[level]->mem)->last_free = (SOA(partab.vol[vol - 1]->vollab)->block_size >> 2) - 7;
        idx[IDX_START] = SOA(blk[level]->mem)->last_free + 1;                   // the data
        chunk = (cstring *) &iidx[idx[IDX_START]];                              // point at it
        chunk->len = 24;                                                        // 5 words
        chunk->buf[0] = 0;                                                      // zero ccc
        chunk->buf[1] = 9;                                                      // ucc
        memcpy(&chunk->buf[2], "\200$GLOBAL\0", 9);                             // the key
        record = (cstring *) &chunk->buf[chunk->buf[1] + 2];                    // setup record pointer
        Align_record();                                                         // align it
        *(u_int *) record = blk[level]->block;                                  // point at self
        memset(&record->buf[2], 0, sizeof(u_int));                              // zero flags
        blk[level]->dirty = SBA(blk[level]);                                    // setup for write
        Queit();                                                                // queue for write
    }                                                                           // end new block code

    VAR_COPY(SOA(partab.vol[vol - 1]->vollab)->uci[uci - 1].name, name);        // set the new name
    partab.vol[vol - 1]->map_dirty_flag = 1;                                    // mark map dirty
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

    while (partab.vol[vol - 1]->writelock) {                                    // check for write lock
        sleep(1);                                                               // wait a bit
        if (partab.jobtab->attention) return -(ERRZ51 + ERRZLAST);              // for <Control-C>
    }                                                                           // end writelock check

    s = SemOp(SEM_GLOBAL, SEM_WRITE);                                           // get write lock
    if (s < 0) return s;                                                        // on error, return it

    if (systab->vol[vol - 1] == NULL) {                                         // is it mounted?
        SemOp(SEM_GLOBAL, -curr_lock);
        return -ERRM26;                                                         // no - error
    }

    if (SOA(partab.vol[vol - 1]->vollab)->uci[uci - 1].name.var_cu[0] == '\0') { // does UCI exist?
        SemOp(SEM_GLOBAL, -curr_lock);
        return 0;                                                               // no - just return
    }

    volnum = vol - 1;                                                           // set this
    writing = 1;                                                                // writing
    level = 0;                                                                  // clear this
    gb = SOA(partab.vol[vol - 1]->vollab)->uci[uci - 1].global;                 // get global directory
    s = Get_block(gb);                                                          // get the block

    if (s < 0) {
        SemOp(SEM_GLOBAL, -curr_lock);
        return s;                                                               // error
    }

    if (blk[level]->dirty == (gbd *) 1) blk[level]->dirty = NULL;               // if reserved then clear it

    if (SOA(blk[level]->mem)->last_idx > IDX_START) {                           // if any globals
        SemOp(SEM_GLOBAL, -curr_lock);
        return -ERRM29;                                                         // no can do
    }

    SOA(partab.vol[vol - 1]->vollab)->uci[uci - 1].global = 0;                  // clear this
    VAR_CLEAR(SOA(partab.vol[vol - 1]->vollab)->uci[uci - 1].name);             // and this
    partab.vol[vol - 1]->map_dirty_flag = 1;                                    // mark map dirty
    SOA(blk[level]->mem)->last_idx = IDX_START - 1;                             // say no index
    Garbit(gb);                                                                 // garbage it
    memset(&systab->last_blk_used[0], 0, systab->maxjob * sizeof(u_int) * MAX_VOL); // zot all
    SemOp(SEM_GLOBAL, -curr_lock);
    return 0;                                                                   // exit
}
