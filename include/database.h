/*
 * Package:  Reference Standard M
 * File:     rsm/include/database.h
 * Summary:  module database header file - standard includes
 *
 * David Wicksell <dlw@linux.com>
 * Copyright © 2020 Fourth Watch Software LC
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

#ifndef _RSM_DATABASE_H_				// only do this once
#define _RSM_DATABASE_H_

// **** Defines ***************************************************************

#define READ		(-1)				// Locking defines makes
#define WRITE		(-systab->maxjob)		// easy reading code
#define WR_TO_R		(systab->maxjob-1)		// from write to read

#define NODE_UNDEFINED	-1				// junk record
#define PTR_UNDEFINED	0				// junk pointer

// Below is the maximum depth that the database code will search down to.
// (Note: With a minimum block size of 4kb a depth of 12 allows for
//        potentially about 8.649e+012 data blocks available.)

#define MAXTREEDEPTH	12				// max level down
#define LAST_USED_LEVEL	3				// when last used works

#define MAXREKEY	(MAXTREEDEPTH * 3)		// max re-keys

#define GBD_EXPIRED	60				// seconds to expire

// DB_Block flags
#define BLOCK_DIRTY	1				// block needs tidying

// Write Daemon defines
#define DOING_NOTHING	0				// nothing
#define DOING_MAP	1				// cleaning the map
#define DOING_WRITE	2				// writing
#define DOING_GARB	3				// garbage collect
#define DOING_DISMOUNT	4				// dismounting

// **** Structures ***********************************************************

typedef struct __attribute__ ((aligned(4), packed)) DB_BLOCK // database block layout
{ u_char type;						// block type
  u_char flags;						// flags
  u_short spare;					// future
  u_int right_ptr;					// right pointer
  u_short last_idx;					// last used index off
  u_short last_free;					// last free lw in block
  var_u global;						// global name
} DB_Block;						// end block header

#define IDX_START	(sizeof(DB_Block) / 2)

typedef struct __attribute__ ((__packed__)) GBD		// global buf desciptor
{ u_int block;						// block number
  struct GBD *next;					// next entry in list
  struct DB_BLOCK *mem;					// memory address of blk
  struct GBD *dirty;					// to write -> next
  time_t last_accessed;					// last time used
} gbd;							// end gbd struct

typedef struct __attribute__ ((__packed__)) JRNREC	// journal record
{ u_short size;						// size of record
  u_char action;					// what it is
  u_char uci;						// uci number
#if RSM_DBVER == 1
  time_t time;						// now
#else
  u_int64 time;						// now
#endif
  var_u name;						// global name
  u_char slen;						// subs length
  u_char key[256];					// the key to 256 char
//short dbc;						// data byte count
//u_char data[32767];					// bytes to 32767
} jrnrec;						// end jrnrec struct

#define JRN_CREATE	0				// create file
#define JRN_START	1				// start/mount environ
#define JRN_STOP	2				// stop journaling
#define JRN_ESTOP	3				// stop/dism environ
#define JRN_SET		4				// Set global
#define JRN_KILL	5				// Kill global

// Note: The first 4 bytes (u_int) = (RSM_MAGIC - 1).
//	 The next 8 bytes (off_t) in the file point at the next free byte.
//	 (Initially 12 and always rounded to the next 4 byte boundary).
//	 Journal file is only accessed while a write lock is held.
//	 Protection on the file is changed to g:rw by init_start

// **** External declarations ************************************************
// Defined in database/db_main.c

extern int curr_lock;					// lock on globals
extern int gbd_expired;
extern mvar db_var;					// local copy of var
extern int volnum;					// current volume

extern gbd *blk[MAXTREEDEPTH];				// current tree
extern int level;					// level in above
							// 0 = global dir
extern u_int rekey_blk[MAXREKEY];			// to be re-keyed
extern int   rekey_lvl[MAXREKEY];			// from level

extern int Index;					// index # into above
extern cstring *chunk;					// chunk at index
extern cstring *record;					// record at index
							// points at dbc
extern u_char keybuf[260];				// for storing keys
extern u_short *idx;					// for indexes
extern int *iidx;					// int ver of index

extern int writing;					// set when writing

extern int hash_start;					// start searching here

//**** Function Prototypes*****************************************************

// File: database/db_buffer.c
short Get_block(u_int blknum);				// Get block
short New_block();					// get new block
void Get_GBD();						// get a GBD
void Get_GBDs(int greqd);				// get n free GBDs
void Free_GBD(gbd *free);				// Free a GBD

// File: database/db_get.c
short Get_data(int dir);				// get db_var node

// File: database/db_kill.c
short Kill_data();					// remove tree

// File: database/db_locate.c
short Locate(u_char *key);				// find key
short Locate_next();					// point at next key

// File: database/db_rekey.c
short Add_rekey(u_int block, int level);		// add to re-key table
short Re_key();						// re-key blocks
void Un_key();						// un-key blk[level]

// File: database/db_set.c
short Set_data(cstring *data);				// set a record

// File: database/db_util.c
void Align_record();					// align record (int)
void Copy_data(gbd *fptr, int fidx);			// copy records
void DoJournal(jrnrec *jj, cstring *data); 		// Write journal
void Free_block(int blknum);				// free blk in map
void Garbit(int blknum);				// que a blk for garb
short Insert(u_char *key, cstring *data);		// insert a node
void Queit();						// que a gbd for write
void Tidy_block();					// tidy current blk
void Used_block(int blknum);				// set blk in map
short Compress1();					// compress 1 block

//*****************************************************************************

#endif							// !_RSM_DATABASE_H_
