/*
 * Package: Reference Standard M
 * File:    rsm/database/get.c
 * Summary: module database - get database functions
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
#include <string.h>                                                             // for memcmp
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
 * Function: Get_data
 * Summary:  Locate and return data described in db_var
 * Input(s): Direction (flag), negative means backwards, 0 forward
 *           > 0 means stop at this level
 * Return:   String length -> Ok, negative M error
 *           Extern variables defined in rsm/database/main.c are also setup
 *             level -> pointer to current level in blk[]
 *             blk[] -> from 0 to level (how we got here)
 *                      unless blk[0] == NULL (for lastused)
 *
 *           This calls Locate() which sets up chunk, record, idx, iidx, keybuf, Index
 * NOTE:     lastused block is NOT used if dir != 0 or journaling is on and writing
 */
int Get_data(int dir)                                                           // locate a record
{
    u_int  block;                                                               // database block
    int    i;                                                                   // a handy int
    int    t;                                                                   // for function returns
    u_char tmp[VAR_LEN + 4];                                                    // spare string
    gbd    *ptr;                                                                // handy pointer

    if (!curr_lock) {                                                           // ensure locked
        t = SemOp(SEM_GLOBAL, SEM_READ);                                        // take a read lock
        if (t < 0) return t;                                                    // if we got an error then return it
    }

    if (systab->vol[volnum - 1] == NULL) return -ERRM26;                        // vol still mounted? if not - error

    if ((memcmp(&db_var.name.var_cu[0], "$GLOBAL\0", 8) == 0) || (dir != 0) ||  // if ^$GLOBAL or level or backward
      (SOA(partab.vol[volnum - 1]->vollab)->journal_available && writing)) {    // or journaling and writing
        systab->last_blk_used[LBU_OFF(volnum - 1)] = 0;                         // zot this
    } else {
        block = systab->last_blk_used[LBU_OFF(volnum - 1)];                     // get last used

        if (block && ((((u_char *) SOA(partab.vol[volnum - 1]->map))[block >> 3]) & (1U << (block & 7)))) { // if one there
            partab.vol[volnum - 1]->stats.lasttry++;                            // count a try
            ptr = SOA(partab.vol[volnum - 1]->gbd_hash[block & (GBD_HASH - 1)]); // get listhead

            while (ptr != NULL) {                                               // for each in list
                if (ptr->block == block) {                                      // found it
                    if ((!var_equal(SOA(ptr->mem)->global, db_var.name)) ||     // wrong global or
                      (SOA(ptr->mem)->type != (db_var.uci + 64)) ||             // wrong UCI/type or
                      (ptr->last_accessed == (time_t) 0)) {                     // not available
                        break;                                                  // exit the loop
                    }

                    level = LAST_USED_LEVEL;                                    // use this level
                    blk[level] = ptr;                                           // point at it
                    t = Locate(&db_var.slen);                                   // check for the key

                    // if found OR not found AND still in block AND not at beginning
                    if ((t >= 0) || ((t = -ERRM7) && (Index <= SOA(blk[level]->mem)->last_idx) && (Index > IDX_START))) {
                        partab.vol[volnum - 1]->stats.lastok++;                 // count success
                        blk[level]->last_accessed = current_time(TRUE);         // accessed
                        for (i = 0; i < level; blk[i++] = NULL) continue;       // zot these
                        if (!t) t = record->len;                                // if ok then get the dbc
                        if (writing && (blk[level]->dirty == NULL)) blk[level]->dirty = (gbd *) 1; // if writing then reserve it

                        // Is this the top node?
                        if (!db_var.slen && !t && ((partab.jobtab->last_block_flags & GL_TOP_DEFINED) == 0)) {
                            t = -ERRM7;
                        }

                        return t;                                               // and return
                    }

                    blk[level] = NULL;                                          // clear this
                    level = 0;                                                  // and this
                    break;                                                      // and exit loop
                }                                                               // end found block

                ptr = SOA(ptr->next);                                           // get next
            }                                                                   // end while ptr
        }                                                                       // end last used stuff

        systab->last_blk_used[LBU_OFF(volnum - 1)] = 0;                         // zot it
    }

    block = SOA(partab.vol[db_var.volset - 1]->vollab)->uci[db_var.uci - 1].global;

    // get directory blk#
    if (!block) return -ERRM26;                                                 // if no such then error
    level = 0;                                                                  // where it goes
    t = Get_block(block);                                                       // get the block
    if (t < 0) return t;                                                        // error? then give up

    if (memcmp(&db_var.name.var_cu[0], "$GLOBAL\0", 8) == 0) {                  // if ^$GLOBAL
        t = Locate(&db_var.slen);                                               // look for it
        if (t >= 0) Align_record();                                             // if found
        return t;                                                               // end ^$GLOBAL lookup
    }

    tmp[1] = 128;                                                               // start string key

    for (i = 0; i < VAR_LEN; i++) {                                             // for each char
        if (db_var.name.var_cu[i] == '\0') break;                               // check for null and break if found
        tmp[i + 2] = db_var.name.var_cu[i];                                     // copy char
    }

    i += 2;                                                                     // correct count
    tmp[i] = '\0';                                                              // null terminate
    tmp[0] = (u_char) i;                                                        // add the count
    t = Locate(tmp);                                                            // search for it
    if (t < 0) return t;                                                        // failed? then return error
    partab.jobtab->last_block_flags = 0;                                        // clear JIC
    Align_record();                                                             // if not aligned
    block = *(u_int *) record;                                                  // get block#
    if (!block) return -ERRM7;                                                  // none there? then say no such
    partab.jobtab->last_block_flags = ((u_int *) record)[1];                    // save flags

    if (partab.jobtab->last_block_flags > 3) {                                  // TEMP         ????
        partab.jobtab->last_block_flags &= 3;                                   // CLEAR UNUSED ????
        ((u_int *) record)[1] = partab.jobtab->last_block_flags;                // RESET        ????
    }

    level++;                                                                    // where we want it
    t = Get_block(block);                                                       // get the block
    if (t < 0) return t;                                                        // error? then give up

    while (SOA(blk[level]->mem)->type < 65) {                                   // while we have ptrs
        if (!var_equal(SOA(blk[level]->mem)->global, db_var.name)) return -(ERRZ61 + ERRMLAST); // database stuffed
        t = Locate(&db_var.slen);                                               // locate the key

        if (t == -ERRM7) {                                                      // failed to find?
            Index--;                                                            // yes, backup the Index
        } else if (t < 0) {                                                     // else if error
            return t;                                                           // return it
        } else if (dir < 0) {                                                   // if found and want -
            Index--;                                                            // backup the Index
            if (Index < IDX_START) panic("Get_data: Problem with negative direction"); // can't happen?
        }

        chunk = (cstring *) &iidx[idx[Index]];                                  // point at the chunk
        record = (cstring *) &chunk->buf[chunk->buf[1] + 2];                    // point at the dbc
        Align_record();                                                         // if not aligned
        if (level == dir) return t;                                             // stop here? if so - return result
        block = *(u_int *) record;                                              // get block#
        level++;                                                                // where it goes
        t = Get_block(block);                                                   // get the block
        if (t < 0) return t;                                                    // error? then give up
    }                                                                           // end while ptr

    if (!var_equal(SOA(blk[level]->mem)->global, db_var.name)) return -(ERRZ61 + ERRMLAST); // database stuffed
    t = Locate(&db_var.slen);                                                   // locate key in data

    // if not a pointer then set last used
    if (dir < 1) systab->last_blk_used[LBU_OFF(volnum - 1)] = block;

    if (!db_var.slen && !t && ((partab.jobtab->last_block_flags & GL_TOP_DEFINED) == 0)) { // check for top node
        if (!record->len) t = -ERRM7;
    }

    if (!t) t = record->len;                                                    // if ok then get the dbc
    return t;                                                                   // return result
}
