/*
 * Package: Reference Standard M
 * File:    rsm/database/rekey.c
 * Summary: module database - database keying functions
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
 * Function: Set_key
 * Summary:  Set the supplied block number to location pointed to by db_var
 * Input(s): Block number to set, level to work on
 *           Re_key (below) has refreshed blk[] and db_var
 * Return:   0 -> Ok, negative M error
 */
static short Set_key(u_int ptr_blk, int this_level)                             // set a block#
{
    int      t;                                                                 // for returns
    u_char   tmp[8];                                                            // some space
    u_char   gtmp[VAR_LEN + 4];                                                 // to find glob
    u_int    i;                                                                 // a handy unsigned int
    cstring  *ptr;                                                              // spare pointer
    int      rs;                                                                // required space
    int      ts;                                                                // trailing size
    int      rls;                                                               // RL space
    u_int    trailings;                                                         // pointer to original trail
    gbd      *cblk[3];                                                          // current level blocks
    u_int    tgb;                                                               // top block in GD
    DB_Block *btmp;                                                             // ditto

    ptr = (cstring *) tmp;                                                      // point at this
DISABLE_WARN(-Warray-bounds)
    ptr->len = 4;                                                               // always
ENABLE_WARN
    memcpy(ptr->buf, &ptr_blk, sizeof(u_int));                                  // for pointers, copy this here
    level = this_level;                                                         // set current level

    if (!this_level) {                                                          // top level split
        gtmp[1] = 128;                                                          // start string key

        for (i = 0; i < VAR_LEN; i++) {                                         // for each char
            if (db_var.name.var_cu[i] == '\0') break;                           // check for null and break if found
            gtmp[i + 2] = db_var.name.var_cu[i];                                // copy char
        }

        i += 2;                                                                 // correct count
        gtmp[i] = '\0';                                                         // null terminate
        gtmp[0] = (u_char) i;                                                   // add the count
        t = Get_block(SOA(partab.vol[db_var.volset - 1]->vollab)->uci[db_var.uci - 1].global);
        if (t < 0) return (short) t;                                            // failed? then return error
        t = Locate(gtmp);                                                       // search for it
        if (t < 0) return (short) t;                                            // failed? then return error
        Align_record();                                                         // if not aligned
        tgb = *(u_int *) record;                                                // get block#
        level = 1;                                                              // at top level
        t = New_block();                                                        // get a new block
        if (t < 0) return (short) t;                                            // if that failed then return error
        SOA(blk[level]->mem)->type = db_var.uci;                                // pointer block
        SOA(blk[level]->mem)->last_idx = IDX_START;                             // first Index
        SOA(blk[level]->mem)->last_free = (SOA(partab.vol[volnum]->vollab)->block_size >> 2) - 3; // use 2 words
        memcpy(&SOA(blk[level]->mem)->global, &db_var.name.var_cu[0], VAR_LEN);
        idx[IDX_START] = SOA(blk[level]->mem)->last_free + 1;                   // the data
        chunk = (cstring *) &iidx[idx[IDX_START]];                              // point at it
        chunk->len = 8;                                                         // used two words
        chunk->buf[0] = 0;                                                      // ccc
        chunk->buf[1] = 0;                                                      // ucc
        record = (cstring *) &chunk->buf[chunk->buf[1] + 2];                    // setup record pointer
        *((u_int *) record) = tgb;                                              // first entry
        t = Insert(&db_var.slen, ptr);                                          // insert this one
        if (t < 0) return (short) t;                                            // failed? then return error
        level = 0;                                                              // point at GD
        t = Locate(gtmp);                                                       // search for it
        if (t < 0) return (short) t;                                            // failed? then return error
        Align_record();                                                         // if not aligned
        *((u_int *) record) = blk[1]->block;                                    // new top level block
        level = 1;
        blk[level]->dirty = SBA(blk[level]);                                    // hook to self

        if (blk[0]->dirty == (gbd *) 1) {                                       // if not queued
            blk[0]->dirty = SBA(blk[level]);                                    // hook it
            level = 0;                                                          // and clear level
        }

        Queit();                                                                // queue to write
        return 0;                                                               // end of level == 0
    }

    t = Get_data(this_level);                                                   // get the key block
    if (t >= 0) return -(ERRZ61 + ERRMLAST);                                    // if found there is a database problem
    if (t != -ERRM7) return (short) t;                                          // any other error then give up
    if (blk[level]->block == ptr_blk) return -(ERRZ61 + ERRMLAST);              // database problem
    Index++;                                                                    // point at insert

    // see Get_data() in rsm/database/get.c
    trailings = Index;                                                          // remember for later
    if (trailings < (IDX_START + 1)) return -(ERRZ61 + ERRMLAST);               // if junk then database stuffed
    t = Insert(&db_var.slen, ptr);                                              // attempt to insert

    if (t == 0) {                                                               // if that worked
        if (blk[level]->dirty == (gbd *) 1) {
            blk[level]->dirty = SBA(blk[level]);                                // hook to self
            Queit();                                                            // and queue
        }

        level--;                                                                // backup a level

        while (level >= 0) {                                                    // scan up
            if (blk[level]->dirty == (gbd *) 1) blk[level]->dirty = NULL;       // if reserved then release it
            level--;                                                            // up one
        }

        return 0;                                                               // exit **0**
    } else if (t != -(ERRZ62 + ERRMLAST)) {
        return (short) t;                                                       // error!
    }

    ts = 0;                                                                     // none yet

    if (trailings <= SOA(blk[level]->mem)->last_idx) {                          // if any
        i = trailings;                                                          // start here
        chunk = (cstring *) &iidx[idx[i]];                                      // point at first chunk
        ts = chunk->buf[0];                                                     // get ccc

        while (i <= SOA(blk[level]->mem)->last_idx) {                           // loop thru trailings
            chunk = (cstring *) &iidx[idx[i]];                                  // point at chunk
            ts += (chunk->len + 2);                                             // add the chunk + idx
            i++;                                                                // increment idx
        }
    }

    rs = 4 + db_var.slen + 4;                                                   // required key + pointer space
    if (rs & 3) rs += (4 - (rs & 3));                                           // if required then round up
    rs += 4;                                                                    // allow for index
    cblk[0] = blk[level];                                                       // remember this
    cblk[1] = NULL;                                                             // clear this
    cblk[2] = NULL;                                                             // and this
    rls = 0;                                                                    // nothing here yet

    if (SOA(blk[level]->mem)->right_ptr) {                                      // if there is one
        t = Get_block(SOA(blk[level]->mem)->right_ptr);                         // get it
        cblk[2] = blk[level];                                                   // remember address
        if (SOA(blk[level]->mem)->flags & BLOCK_DIRTY) Tidy_block();            // check state and ensure tidy
        rls = (SOA(blk[level]->mem)->last_free * 2 + 1 - SOA(blk[level]->mem)->last_idx) * 2;
    }

    if ((ts < rls) && ts) {                                                     // if trailings -> RL
        Un_key();                                                               // unlink RL key
        Get_GBD();                                                              // get another GBD
        memset(SOM(blk[level]->mem), 0, SOA(partab.vol[volnum]->vollab)->block_size); // zot
        SOA(blk[level]->mem)->type = SOA(cblk[2]->mem)->type;                   // copy type
        SOA(blk[level]->mem)->right_ptr = SOA(cblk[2]->mem)->right_ptr;         // copy RL
        VAR_COPY(SOA(blk[level]->mem)->global, SOA(cblk[2]->mem)->global);      // copy global name
        SOA(blk[level]->mem)->last_idx = IDX_START - 1;                         // unused block
        SOA(blk[level]->mem)->last_free = (SOA(partab.vol[volnum]->vollab)->block_size >> 2) - 1; // set this up
        keybuf[0] = 0;                                                          // clear this

        if ((ts + rs) < rls) {                                                  // if new record fits
            t = Insert(&db_var.slen, ptr);                                      // insert it
            if (t < 0) panic("Set_key: Insert in new block (RL) failed");       // failed ?
            memcpy(keybuf, &chunk->buf[1], chunk->buf[1] + 1);                  // save key
        }

        Copy_data(cblk[0], trailings);                                          // copy trailings
        Copy_data(cblk[2], IDX_START);                                          // and old RL
        btmp = blk[level]->mem;                                                 // save this
        blk[level]->mem = cblk[2]->mem;                                         // copy in this
        cblk[2]->mem = btmp;                                                    // end swap 'mem'
        Free_GBD(blk[level]);                                                   // give it back
        blk[level] = cblk[0];                                                   // original block again
        idx = (u_short *) SOA(blk[level]->mem);                                 // point at it
        iidx = (int *) SOA(blk[level]->mem);                                    // point at it

        for (i = trailings; i <= SOA(blk[level]->mem)->last_idx; i++) {
            chunk = (cstring *) &iidx[idx[i]];                                  // point at the chunk
            record = (cstring *) &chunk->buf[chunk->buf[1] + 2];                // point at the dbc
            Align_record();                                                     // align
            *(int *) record = PTR_UNDEFINED;                                    // mark as junk
        }

        Tidy_block();                                                           // tidy it
        if ((ts + rs) < rls) goto fix_keys;                                     // if new record done then exit **1**
        t = Insert(&db_var.slen, ptr);                                          // attempt to insert

        if (t >= 0) {                                                           // if OK
            goto fix_keys;                                                      // exit **2**
        } else if (t != -(ERRZ62 + ERRMLAST)) {
            return (short) t;                                                   // error!
        }

        t = New_block();                                                        // new block for insert
        if (t < 0) panic("Set_key: Failed to get new block for insert");        // if failed
        SOA(blk[level]->mem)->type = SOA(cblk[0]->mem)->type;                   // copy type
        SOA(blk[level]->mem)->right_ptr = SOA(cblk[0]->mem)->right_ptr;         // copy RL
        VAR_COPY(SOA(blk[level]->mem)->global, SOA(cblk[0]->mem)->global);      // copy global name
        SOA(blk[level]->mem)->last_idx = IDX_START - 1;                              // unused block
        SOA(blk[level]->mem)->last_free = (SOA(partab.vol[volnum]->vollab)->block_size >> 2) - 1; // set this up
        keybuf[0] = 0;                                                          // clear this
        SOA(cblk[0]->mem)->right_ptr = blk[level]->block;                       // point at it
        t = Insert(&db_var.slen, ptr);                                          // insert it
        if (t < 0) panic("Set_key: Insert in new block insert() failed");       // failed ?
        cblk[1] = blk[level];                                                   // remember this one
        goto fix_keys;                                                          // exit **3**
    }                                                                           // end trailings in RL

    if ((rs < rls) && !ts) {                                                    // if will fit in RL
        blk[level] = cblk[2];                                                   // point at RL
        Un_key();                                                               // delete current key
        t = Insert(&db_var.slen, ptr);                                          // insert it

        if (t >= 0) {                                                           // if OK
            goto fix_keys;                                                      // exit **5**
        } else if (t != -(ERRZ62 + ERRMLAST)) {
            return (short) t;                                                   // error!
        }
    } else if (cblk[2] != NULL) {                                               // if RL allocated
        if (cblk[2]->dirty == (gbd *) 1) cblk[2]->dirty = NULL;                 // if reserved then clear it
        cblk[2] = NULL;                                                         // flag not used
    }

    t = New_block();                                                            // new block for trail
    if (t < 0) panic("Set_key: Failed to get new block for trailings");         // if failed
    SOA(blk[level]->mem)->type = SOA(cblk[0]->mem)->type;                       // copy type
    SOA(blk[level]->mem)->right_ptr = SOA(cblk[0]->mem)->right_ptr;             // copy RL
    VAR_COPY(SOA(blk[level]->mem)->global, SOA(cblk[0]->mem)->global);          // copy global name
    SOA(blk[level]->mem)->last_idx = IDX_START - 1;                             // unused block
    SOA(blk[level]->mem)->last_free = (SOA(partab.vol[volnum]->vollab)->block_size >> 2) - 1; // set this up
    keybuf[0] = 0;                                                              // clear this
    SOA(cblk[0]->mem)->right_ptr = blk[level]->block;                           // point at it
    cblk[1] = blk[level];                                                       // save this one

    if (ts) {                                                                   // if any trailings
        Copy_data(cblk[0], trailings);                                          // copy trailings
        blk[level] = cblk[0];                                                   // original block again
        idx = (u_short *) SOA(blk[level]->mem);                                 // point at it
        iidx = (int *) SOA(blk[level]->mem);                                    // point at it

        for (i = trailings; i <= SOA(blk[level]->mem)->last_idx; i++) {
            chunk = (cstring *) &iidx[idx[i]];                                  // point at the chunk
            record = (cstring *) &chunk->buf[chunk->buf[1] + 2];                // point at the dbc
            Align_record();                                                     // align
            *(int *) record = PTR_UNDEFINED;                                    // mark as junk
        }

        Tidy_block();                                                           // tidy it
        t = Insert(&db_var.slen, ptr);                                          // attempt to insert

        if (t >= 0) {                                                           // if OK
            goto fix_keys;                                                      // exit **4**
        } else if (t != -(ERRZ62 + ERRMLAST)) {
            return (short) t;                                                   // error!
        }

        blk[level] = cblk[1];                                                   // new block again
        idx = (u_short *) SOA(blk[level]->mem);                                 // point at it
        iidx = (int *) SOA(blk[level]->mem);                                    // point at it
    }

    t = Insert(&db_var.slen, ptr);                                              // attempt to insert

    if (t >= 0) {                                                               // if OK
        goto fix_keys;                                                          // exit **5**
    } else if (t != -(ERRZ62 + ERRMLAST)) {
        return (short) t;                                                       // error!
    }

    panic("Set_key: Options 0->5 didn't work");                                 // die

fix_keys:
    blk[level] = NULL;                                                          // clear this

    for (int j = level - 1; j >= 0; j--) {                                      // scan pointer blocks
        if (blk[j]->dirty == (gbd *) 2) {                                       // if changed
            if (blk[level] == NULL) {                                           // list empty
                blk[j]->dirty = SBA(blk[j]);                                    // point at self
            } else {
                blk[j]->dirty = SBA(blk[level]);                                // else point at previous
            }

            blk[level] = blk[j];                                                // remember this one
        } else if (blk[j]->dirty == (gbd *) 1) {                                // if reserved
            blk[j]->dirty = NULL;                                               // clear it
        }
    }

    for (i = 0; i < 3; i++) {                                                   // scan cblk[]
        if (cblk[i] == NULL) continue;                                          // if empty then ignore it

        if (cblk[i]->dirty == (gbd *) 1) {                                      // not queued
            if (blk[level] == NULL) {                                           // list empty
                cblk[i]->dirty = SBA(cblk[i]);                                  // point at self
            } else {
                cblk[i]->dirty = SBA(blk[level]);                               // else point at previous
            }

            blk[level] = cblk[i];                                               // remember this one
        }
    }

    if (blk[level] != NULL) Queit();                                            // if something there then queue that lot

    for (i = 1; i < 3; i++) {                                                   // scan cblk[] again
        if (cblk[i] != NULL) Add_rekey(cblk[i]->block, this_level);             // if there then queue a fix
    }                                                                           // end fix key loop

    return 0;                                                                   // done
}

/*
 * Function: Add_rekey
 * Summary:  Set the supplied block number and level into rekey table
 * Input(s): Block number to set, level it is currently at
 * Return:   0
 */
short Add_rekey(u_int block, int level)                                         // add to re-key table
{
    for (int i = 0; i < MAXREKEY; i++) {                                        // scan table
        if (!rekey_blk[i]) {                                                    // if empty
            rekey_blk[i] = block;                                               // save block
            rekey_lvl[i] = level;                                               // and level
            return 0;                                                           // and exit
        }
    }

    panic("Add_rekey: Rekey table overflowed - database is corrupt");
    return 0;                                                                   // won't happen
}

/*
 * Function: Re_key
 * Summary:  Re-key all blocks in the re-key table
 * Input(s): None
 * Return:   0 or negative M error
 */
short Re_key(void)                                                              // re-key blocks
{
    while (TRUE) {                                                              // loop
        short s;                                                                // for functions
        int   low_level = -1;                                                   // lowest found
        int   low_index = -1;                                                   // where found

        for (int i = 0; i < MAXREKEY; i++) {                                    // search table
            if (rekey_blk[i]) {                                                 // if something there
                if (rekey_lvl[i] > low_level) {                                 // higher than got
                    low_level = rekey_lvl[i];                                   // save level
                    low_index = i;                                              // and the index
                }
            }
        }

        if (low_index == -1) return 0;                                          // if none found then all done
        partab.vol[volnum]->stats.blkreorg++;                                   // update stats
        level = 0;                                                              // clear level
        s = Get_block(rekey_blk[low_index]);                                    // get the block
        if (s < 0) return -(ERRZ61 + ERRMLAST);                                 // database stuffed
        chunk = (cstring *) &iidx[idx[IDX_START]];                              // point at first chunk
        memcpy(&db_var.slen, &chunk->buf[1], chunk->buf[1] + 1);                // copy key
        if (blk[level]->dirty == (gbd *) 1) blk[level]->dirty = NULL;           // if reserved then clear it
        s = Set_key(rekey_blk[low_index], low_level - 1);                       // do it
        if (s < 0) return s;                                                    // on fail
        rekey_lvl[low_index] = 0;                                               // clear this
        rekey_blk[low_index] = 0;                                               // and this

        if (low_level == 1) {                                                   // if a top split
            for (int i = 0; i < MAXREKEY; i++) {                                // search table
                if (rekey_lvl[i]) rekey_lvl[i]++;                               // if something there then increment it
            }
        }
    }                                                                           // end while (TRUE)

    return 0;                                                                   // can't get here
}

/*
 * Function: Un_key
 * Summary:  Un-key current blk[level]
 * Input(s): None
 * Return:   None
 */
void Un_key(void)
{
    int     this_level;
    int     save_level;
    int     xxx_level;
    u_char  cstr[8];                                                            // and another
    cstring *xptr;                                                              // spare pointer
    gbd     *save;                                                              // save a block
    u_char  *uptr;                                                              // a handy pointer
    u_char  *lptr;                                                              // a handy pointer
    u_int   blkno;                                                              // a block number

    this_level = level;                                                         // save for ron
    idx = (u_short *) SOA(blk[level]->mem);                                     // point at the block
    iidx = (int *) SOA(blk[level]->mem);                                        // point at the block
    chunk = (cstring *) &iidx[idx[IDX_START]];                                  // point at first chunk
    uptr = &chunk->buf[1];                                                      // point at key

    for (level = level - 1; level; level--) {                                   // for each above level
        short s;

        s = Locate(uptr);                                                       // look for key

        if (s == -ERRM7) {                                                      // if not found
            if (Index > SOA(blk[level]->mem)->last_idx) {                       // if ran off end
                save = blk[level];                                              // save this one
                s = Locate_next();                                              // get rl

                if (s == 0) {                                                   // if one there
                    s = Locate(uptr);                                           // look for key

                    if (s < 0) {                                                // if not found
                        if (blk[level]->dirty == (gbd *) 1) {                   // if reserved
                            blk[level]->dirty = NULL;                           // free it
                        }

                        blk[level] = save;                                      // put this one back
                    } else if (save->dirty == (gbd *) 1) {                      // if this is reserved
                        save->dirty = NULL;                                     // clear it
                    }
                }
            }
        }

        if (s == 0) {                                                           // if found
            Align_record();                                                     // align
            *(int *) record = PTR_UNDEFINED;                                    // mark as junk
            Tidy_block();                                                       // and tidy the block

            if (level < (this_level - 1)) {                                     // if up > 1 level
                if (SOA(blk[level + 1]->mem)->last_idx > (IDX_START - 1)) {     // and if lower not mt
                    idx = (u_short *) SOA(blk[level + 1]->mem);                 // point at the block
                    iidx = (int *) SOA(blk[level + 1]->mem);                    // point at the block
                    chunk = (cstring *) &iidx[idx[IDX_START]];                  // point at first chunk
                    lptr = &chunk->buf[1];                                      // point at key
                    xptr = (cstring *) cstr;                                    // point at spare
DISABLE_WARN(-Warray-bounds)
                    xptr->len = 4;                                              // one int
ENABLE_WARN
                    memcpy(xptr->buf, &blk[level + 1]->block, sizeof(u_int));   // point the int here, get block#
                    s = Insert(lptr, xptr);                                     // insert that

                    if (s == -(ERRZ62 + ERRMLAST)) {
                        Add_rekey(blk[level + 1]->block, level + 1);            // do it later
                    } else if (s < 0) {
                        panic("Un_Key: Insert returned fatal value");
                    }
                } else {                                                        // lower level is empty
                    save_level = level;                                         // remember where we at
                    blkno = 0;                                                  // clear block#

                    while (TRUE) {
                        s = Locate(uptr);                                       // find key - must fail
                        if (s != -ERRM7) panic("Un_key: Key locate at 'level' didn't return -ERRM7"); // if not - die

                        if (Index > IDX_START) {                                // if not first node
                            chunk = (cstring *) &iidx[idx[Index - 1]];          // point at previous
                            record = (cstring *) &chunk->buf[chunk->buf[1] + 2]; // point at it
                            Align_record();                                     // align
                            blkno = *(u_int *) record;                          // get the number
                            break;                                              // and exit loop
                        }

                        level--;                                                // up a level
                        if (!level) panic("Un_key: Failed to determine left edge"); // if not found
                    }                                                           // end while (TRUE)

                    while (level < save_level) {
                        xxx_level = level;                                      // remember this
                        level = MAXTREEDEPTH - 1;                               // use this one
                        s = Get_block(blkno);
                        if (s < 0) panic("Un_key: Get_block() failed in left block tree");
                        s = Locate(uptr);                                       // find key - must fail
                        if (s != -ERRM7) panic("Un_key: Key locate in left edge didn't return -ERRM7"); // if not - die
                        chunk = (cstring *) &iidx[idx[Index - 1]];              // point at previous
                        record = (cstring *) &chunk->buf[chunk->buf[1] + 2];    // point at it
                        Align_record();                                         // align
                        blkno = *(u_int *) record;                              // get the number
                        if (blk[level]->dirty == (gbd *) 1) blk[level]->dirty = NULL; // free GBD
                        level = xxx_level;                                      // restore level
                        level++;                                                // and increment it
                    }

                    xxx_level = MAXTREEDEPTH - 1;                               // use this one
                    level++;                                                    // point at MT block
                    blk[xxx_level] = blk[level];                                // remember that there
                    s = Get_block(blkno);
                    if (s < 0) panic("Un_key: Get_block() failed for left block");
                    SOA(blk[level]->mem)->right_ptr = SOA(blk[xxx_level]->mem)->right_ptr;
                    if (blk[level]->dirty == (gbd *) 1) blk[level]->dirty = (gbd *) 2; // if we changed it then mark for write
                    Garbit(blk[xxx_level]->block);                              // dump MT block
                    level = save_level;                                         // restore level
                }                                                               // end empty block proc
            }

            if (blk[level]->dirty == (gbd *) 1) blk[level]->dirty = (gbd *) 2;  // if we changed it then mark for write
        } else {
            break;                                                              // no more to find
        }
    }

    level = this_level;                                                         // restore level
    return;
}
