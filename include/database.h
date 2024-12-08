/*
 * Package: Reference Standard M
 * File:    rsm/include/database.h
 * Summary: module database header file - standard includes
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

#ifndef RSM_DATABASE_H
#define RSM_DATABASE_H

#include "rsm.h"                                                                // standard RSM includes

// Defines
#define NODE_UNDEFINED  VAR_UNDEFINED                                           // junk record (was -1 now MAX_STR_LEN + 1)
#define PTR_UNDEFINED   0                                                       // junk pointer

/*
 * Below is the maximum depth that the database code will search down to.
 *
 * NOTE: With a block size of 4 KiB a depth of 12 allows for
 *       potentially about 8.649E12 data blocks available.
 */
#define MAXTREEDEPTH    12                                                      // max level down
#define LAST_USED_LEVEL 3                                                       // when last used works
#define MAXREKEY        (MAXTREEDEPTH * 3)                                      // max re-keys
#define GBD_EXPIRED     60                                                      // seconds to expire

// DB_Block flags
#define BLOCK_DIRTY     1                                                       // block needs tidying

// Write Daemon defines
#define DOING_NOTHING   0                                                       // nothing
#define DOING_MAP       1                                                       // cleaning the map
#define DOING_WRITE     2                                                       // writing
#define DOING_GARB      3                                                       // garbage collect
#define DOING_DISMOUNT  4                                                       // dismounting

// Structures
typedef struct __attribute__ ((aligned(4), packed)) DB_BLOCK {                  // database block layout
    u_char  type;                                                               // block type
    u_char  flags;                                                              // flags
    u_short spare;                                                              // future
    u_int   right_ptr;                                                          // right pointer
    u_short last_idx;                                                           // last used index off
    u_short last_free;                                                          // last free lw in block
    var_u   global;                                                             // global name
} DB_Block;                                                                     // end block header

#define IDX_START   ((u_int) sizeof(DB_Block) / 2)

typedef struct __attribute__ ((__packed__)) GBD {                               // global buf desciptor
    u_int           block;                                                      // block number
    struct GBD      *next;                                                      // next entry in list
    struct DB_BLOCK *mem;                                                       // memory address of block
    struct GBD      *dirty;                                                     // to write -> next
    time_t          last_accessed;                                              // last time used
} gbd;                                                                          // end GBD struct

typedef struct __attribute__ ((__packed__)) JRNREC {                            // journal record
    u_short size;                                                               // size of record
    u_char  action;                                                             // what it is
    u_char  uci;                                                                // UCI number
#if RSM_DBVER == 1
    time_t  time;                                                               // now
#else
    u_int64 time;                                                               // now
#endif
    var_u   name;                                                               // global name
    u_char  slen;                                                               // subs length
    u_char  key[MAX_KEY_SIZE + 1];                                              // the key to MAX_KEY_SIZE + 1 char
    //short   dbc;                                                                // data byte count
    //u_char  data[MAX_STR_LEN];                                                  // bytes to MAX_STR_LEN
} jrnrec;                                                                       // end jrnrec struct

#define JRN_CREATE  0                                                           // create file
#define JRN_START   1                                                           // start/mount environ
#define JRN_STOP    2                                                           // stop journaling
#define JRN_ESTOP   3                                                           // stop/dism environ
#define JRN_SET     4                                                           // Set global
#define JRN_KILL    5                                                           // Kill global

/*
 * NOTE: The first 4 bytes (u_int) = (RSM_MAGIC - 1).
 *       The next 8 bytes (off_t) in the file point at the next free byte.
 *       Initially 12 and always rounded to the next 4 byte boundary.
 *       Journal file is only accessed while a write lock is held.
 */

// External declarations

// File: rsm/database/main.c
extern int     curr_lock;                                                       // lock on globals
extern int     gbd_expired;
extern mvar    db_var;                                                          // local copy of var
extern int     volnum;                                                          // current volume
extern gbd     *blk[MAXTREEDEPTH];                                              // current tree
extern int     level;                                                           // level in above
extern u_int   rekey_blk[MAXREKEY];                                             // to be re-keyed
extern int     rekey_lvl[MAXREKEY];                                             // from level
extern u_int   Index;                                                           // index # into above
extern cstring *chunk;                                                          // chunk at index
extern cstring *record;                                                         // record at index
extern u_char  keybuf[MAX_KEY_SIZE + 5];                                        // for storing keys
extern u_short *idx;                                                            // for indexes
extern int     *iidx;                                                           // int ver of index
extern int     writing;                                                         // set when writing
extern int     hash_start;                                                      // start searching here

// Function Prototypes

// File: rsm/database/buffer.c
short Get_block(u_int blknum);                                                  // Get block
short New_block(void);                                                          // get new block
void  Get_GBD(void);                                                            // get a GBD
void  Get_GBDs(int greqd);                                                      // get n free GBDs
void  Free_GBD(gbd *free);                                                      // Free a GBD

// File: rsm/database/get.c
int Get_data(int dir);                                                          // get db_var node

// File: rsm/database/ic.c
void ic_map(int flag);                                                          // check the map

// File: rsm/database/kill.c
short Kill_data(void);                                                          // remove tree

// File: rsm/database/locate.c
short Locate(u_char *key);                                                      // find key
short Locate_next(void);                                                        // point at next key

// File: rsm/database/rekey.c
short Add_rekey(u_int block, int level);                                        // add to re-key table
short Re_key(void);                                                             // re-key blocks
void  Un_key(void);                                                             // un-key blk[level]

// File: rsm/database/set.c
int Set_data(cstring *data);                                                    // set a record

// File: rsm/database/util.c
void  Align_record(void);                                                       // align record (int)
void  Copy_data(gbd *fptr, int fidx);                                           // copy records
void  DoJournal(jrnrec *jj, cstring *data);                                     // Write journal
void  Free_block(u_int blknum);                                                 // free blk in map
void  Garbit(u_int blknum);                                                     // queue a blk for garbage
short Insert(u_char *key, const cstring *data);                                 // insert a node
void  Queit(void);                                                              // queue a GBD for write
void  Tidy_block(void);                                                         // tidy current blk
void  Used_block(u_int blknum);                                                 // set blk in map
short Compress1(void);                                                          // compress 1 block

#endif
