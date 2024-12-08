/*
 * Package: Reference Standard M
 * File:    rsm/database/locate.c
 * Summary: module database - locate database functions
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

#include "database.h"                                                           // database protos
#include "error.h"                                                              // error strings
#include "proto.h"                                                              // standard prototypes
#include <string.h>                                                             // for memcpy

/*
 * Function: Locate
 * Summary:  Locate passed in key in blk[level] updating extern vars
 *           Index, chunk, record and keybuf
 * Input(s): Pointer to key to find (key[0] -> length)
 * Return:   0 -> Ok, negative M error
 * Note:     On fail (-ERRM7), Index etc. points at the following record.
 *           External vars setup are:
 *             (cstring *)  chunk   points at the chunk in the block
 *             (u_short *)  idx     maps the block as an array
 *             (int *)      iidx    maps the block as an array
 *             (cstring *)  record  points at the data for the record
 *                                  (not aligned for ptr/GD)
 *             (u_char)     keybuf  the current full key
 */
short Locate(u_char *key)                                                       // find key
{
    idx = (u_short *) SOA(blk[level]->mem);                                     // point at the block
    iidx = (int *) SOA(blk[level]->mem);                                        // point at the block
    Index = IDX_START;                                                          // start at the start

    while (TRUE) {                                                              // loop
        int i;                                                                  // a handy int

        chunk = (cstring *) &iidx[idx[Index]];                                  // point at the chunk
        memcpy(&keybuf[chunk->buf[0] + 1], &chunk->buf[2], chunk->buf[1]);      // update the key
        keybuf[0] = chunk->buf[0] + chunk->buf[1];                              // and the size
        record = (cstring *) &chunk->buf[chunk->buf[1] + 2];                    // point at the dbc
        i = UTIL_Key_KeyCmp(&keybuf[1], &key[1], keybuf[0], key[0]);            // compare
        if (i == KEQUAL) return 0;                                              // same? then done
        if (i == K2_LESSER) return -ERRM7;                                      // passed it? then no such
        Index++;                                                                // point at next
        if (Index > SOA(blk[level]->mem)->last_idx) return -ERRM7;              // passed the end, there is no such
    }                                                                           // end locate loop
}

/*
 * Function: Locate_next
 * Summary:  Locate next key in blk[level] updating extern vars
 *           Index, chunk, record and keybuf
 * Input(s): None (extern vars must be setup)
 * Return:   0 -> Ok, negative M error
 * Note:     Must be be called with a read lock
 *           External vars setup as for Locate() above
 */
short Locate_next(void)                                                         // point at next key
{
    Index++;                                                                    // point at next

    if (Index > SOA(blk[level]->mem)->last_idx) {                               // passed end?
        int   i;                                                                // a handy int
        short s;                                                                // function returns

        if (!SOA(blk[level]->mem)->right_ptr) return -ERRM7;                    // any more there? if no, just exit
        i = SOA(blk[level]->mem)->right_ptr;                                    // get right block#
        s = Get_block(i);                                                       // attempt to get it
        if (s < 0) return s;                                                    // if we got an error then return it
    }                                                                           // end new block

    chunk = (cstring *) &iidx[idx[Index]];                                      // point at the chunk
    memcpy(&keybuf[chunk->buf[0] + 1], &chunk->buf[2], chunk->buf[1]);          // update the key
    keybuf[0] = chunk->buf[0] + chunk->buf[1];                                  // and the size
    record = (cstring *) &chunk->buf[chunk->buf[1] + 2];                        // point at the dbc
    return 0;                                                                   // all done
}
