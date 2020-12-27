/*
 * Package:  Reference Standard M
 * File:     rsm/database/db_util.c
 * Summary:  module database - Database Functions - Utilities
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

#include <stdio.h>					// always include
#include <stdlib.h>					// these two
#include <string.h>					// for bcopy
#include <strings.h>
#include <unistd.h>					// for file reading
#include <fcntl.h>					// for expand
#include <ctype.h>					// for gbd stuff
#include <errno.h>					// for errors
#include <sys/types.h>					// for semaphores
#include <sys/ipc.h>					// for semaphores
#include <sys/sem.h>					// for semaphores
#include <sys/stat.h>					// for fchmod
#include "rsm.h"					// standard includes
#include "database.h"					// database protos
#include "proto.h"					// standard prototypes
#include "error.h"					// error strings

//-----------------------------------------------------------------------------
// Function: Insert
// Descript: Insert the supplied key and data in blk[level]
// Input(s): Pointer the the key and data to insert
// Return:   String length -> Ok, negative M error -(ERRMLAST+ERRZ62)
//
short Insert(u_char *key, cstring *data)		// insert a node
{ int i;						// a handy int
  int isdata;						// data/ptr flag
  int rs;						// required size
  short s;						// for funcs
  u_char ccc;						// common char count
  u_char ucc;						// uncommon char count
  u_int flags = 0;					// for $GLOBAL

  isdata = ((blk[level]->mem->type > 64) &&		// data block and
	    (level));					// not the directory

  if (blk[level]->mem->last_idx > (IDX_START - 1))	// if some data
  { s = Locate(key);					// search for it
    if (s >= 0)						// if found
    { return -(ERRMLAST+ERRZ61);			// database stuffed
    }
    else if (s != -ERRM7)				// for any other error
    { return s;						// exit
    }
  }
  else							// empty block
  { Index = IDX_START;					// start
    idx = (u_short *) blk[level]->mem;			// point at the block
    iidx = (int *) blk[level]->mem;			// point at the block
  }
  if (!level)						// insert in GD
  { chunk = (cstring *) &iidx[idx[IDX_START]];		// point at $G chunk
    record = (cstring *) &chunk->buf[chunk->buf[1] + 2];
    Align_record();					// align it
    flags = ((u_int *) record)[1];			// get default flags
    partab.jobtab->last_block_flags = flags;
  }

  keybuf[0] = 0;					// clear keybuf
  for (i = IDX_START; i < Index; i++)			// for all prev Indexes
  { chunk = (cstring *) &iidx[idx[i]];			// point at the chunk
    bcopy(&chunk->buf[2], &keybuf[chunk->buf[0]+1], chunk->buf[1]); // update the key
    keybuf[0] = chunk->buf[0] + chunk->buf[1];		// and the size
  }							// we insert after this

  ccc = 0;						// start here
  if ((key[0]) && (keybuf[0]))				// if any there
  { while (key[ccc + 1] == keybuf[ccc + 1])		// while the same
    { if ((ccc == key[0]) || (ccc == keybuf[0]))	// at end of either
      { break;						// done
      }
      ccc++;						// increment ptr
    }
  }
  ucc = key[0] - ccc;					// and this
  rs = sizeof(short) + 2				// chunksiz + ccc + ucc
       + ucc + data->len;				// + key + data
  if (isdata)						// if it's a data blk
  { rs += sizeof(short);				// add the dbc size
  }
  else if (!level)					// if GD
  { rs += 4;						// allow for flags
  }
  if (rs & 3)						// not even long word
  { rs += (4 - (rs & 3));				// round it up
  }
  rs += 4;						// allow for the Index

  if (rs > ((blk[level]->mem->last_free * 2 + 1 - blk[level]->mem->last_idx) * 2))
  { if (!(blk[level]->mem->flags & BLOCK_DIRTY))	// if block is clean
    { return -(ERRMLAST+ERRZ62);			// say no room
    }
    Tidy_block();					// tidy it
    if (rs > ((blk[level]->mem->last_free * 2 + 1 - blk[level]->mem->last_idx) * 2))
    { return -(ERRMLAST+ERRZ62);			// say no room
    }
  }							// it will now fit
  rs -= 4;						// rs now chunksize

  for (i = blk[level]->mem->last_idx; i >= Index; i--)	// the trailing ones
  { idx[i + 1] = idx[i];				// get copied down
  }
  idx[Index] = blk[level]->mem->last_free - (rs / 4) + 1; // where it goes
  chunk = (cstring *) &iidx[idx[Index]];		// as an address
  record = (cstring *) &chunk->buf[ucc + 2];		// where the data goes
  chunk->len = rs;					// store chunk size
  chunk->buf[0] = ccc;					// this bit
  chunk->buf[1] = ucc;					// then this
  bcopy(&key[ccc + 1], &chunk->buf[2], ucc);		// then the key bit
  if (isdata)						// for data blk
  { record->len = data->len;				// copy the length
    bcopy(data->buf, record->buf, data->len);		// then the data
  }
  else							// it's a pointer
  { Align_record();					// align it
    bcopy(data->buf, record, sizeof(int));		// the block number
    if (!level)						// if GD
    { ((u_int *) record)[1] = flags;			// set/clear the flags
    }
  }
  blk[level]->mem->last_free -= (rs / 4);		// redo last_free
  blk[level]->mem->last_idx++;				// add to the index
  blk[level]->mem->flags |= BLOCK_DIRTY;		// mark dirty
  return 0;						// done
}

//-----------------------------------------------------------------------------
// Function: Queit
// Descript: Que the gbd at blk[level] for write - links already setup
// Input(s): none
// Return:   none
// Note:     Must hold a write lock before calling this function
//

void Queit()						// que a gbd for write
{ int i;						// a handy int
  gbd *ptr;						// a handy ptr

  ptr = blk[level];					// point at the block
  systab->vol[volnum-1]->stats.logwt++;			// incr logical
  while (ptr->dirty != ptr)				// check it
  { ptr = ptr->dirty;					// point at next
    systab->vol[volnum-1]->stats.logwt++;		// incr logical
  }

  i = systab->vol[volnum - 1]->dirtyQw;			// where to put it
  while (systab->vol[volnum - 1]->dirtyQ[i] != NULL)	// if slot not avbl
  { sleep(1);						// wait a bit
  }				// NOTE: The above CAN'T work!!!
  systab->vol[volnum - 1]->dirtyQ[i] = blk[level];	// stuff it in
  systab->vol[volnum - 1]->dirtyQw = (i + 1) & (NUM_DIRTY - 1); // reset ptr

  return;						// and exit
}

//-----------------------------------------------------------------------------
// Function: Garbit
// Descript: Que the block passed in for garbage collection
// Input(s): block number
// Return:   none
// Note:     Must hold a write lock before calling this function
//

void Garbit(int blknum)					// que a blk for garb
{ int i;						// a handy int
  int j;						// for loop

  i = systab->vol[volnum - 1]->garbQw;			// where to put it
  for (j = 0; ; j++)
  { if (systab->vol[volnum - 1]->garbQ[i] == 0)		// if slot avbl
    { break;						// exit
    }
    if (j == 9)
    { panic("Garbit: could not get a garbage slot after 10 seconds");
    }
    sleep(1);						// wait a bit
  }				// NOTE: I don't think this can work either
  systab->vol[volnum - 1]->garbQ[i] = blknum;		// stuff it in
  systab->vol[volnum - 1]->garbQw = (i + 1) & (NUM_GARB - 1); // reset ptr
  return;						// and exit
}

//-----------------------------------------------------------------------------
// Function: Free_block
// Descript: Remove the specified block from the map
// Input(s): block number
// Return:   none
// Note:     Must hold a write lock before calling this function
//

void Free_block(int blknum)				// free blk in map
{ int i;						// a handy int
  int off;						// and another
  u_char *map;						// map pointer

  map = ((u_char *) systab->vol[volnum-1]->map);	// point at it
  i = blknum >> 3;					// map byte
  off = blknum & 7;					// bit number
  off = 1U << off;					// convert to mask
  if ((map[i] & off) == 0)				// if it's already free
  { return;						// just exit
  }
  map[i] &= ~off;					// clear the bit
  if (systab->vol[volnum-1]->first_free > (void *) &map[i])	// if earlier
  { systab->vol[volnum-1]->first_free = &map[i]; // reset first free
  }
  systab->vol[volnum-1]->map_dirty_flag++;		// mark map dirty
  return;						// and exit
}

//-----------------------------------------------------------------------------
// Function: Used_block
// Descript: Add the specified block to the map
// Input(s): block number
// Return:   none
// Note:     Must hold a write lock before calling this function
//	     The caller must have ensured that, if there is a map
//		scan in progress, this block is less than "upto".
// This is only called from database/db_view.c
//

void Used_block(int blknum)				// set blk in map
{ int i;						// a handy int
  int off;						// and another
  u_char *map;						// map pointer

  map = ((u_char *) systab->vol[volnum-1]->map);	// point at it
  i = blknum >> 3;					// map byte
  off = blknum & 7;					// bit number
  off = 1U << off;					// convert to mask
  if ((map[i] & off))					// if it's already used
  { return;						// just exit
  }
  map[i] |= off;					// set the bit
  systab->vol[volnum-1]->map_dirty_flag++;		// mark map dirty
  return;						// and exit
}

//-----------------------------------------------------------------------------
// Function: Tidy_block
// Descript: Tidy the current block
// Input(s): none
// Return:   none
// Note:     Must hold a write lock before calling this function
//	     This function omits records with dbc = NODE_UNDEFINED
//	     This function omits pointers with record = PTR_UNDEFINED
//

void Tidy_block()					// tidy current blk
{ gbd *ptr;						// a handy pointer
  DB_Block *btmp;					// ditto

  ptr = blk[level];					// remember current
  Get_GBD();						// get another
  bzero(blk[level]->mem, systab->vol[volnum-1]->vollab->block_size); // zot
  blk[level]->mem->type = ptr->mem->type;		// copy type

  if (!level)						// if it's a GD
  { blk[level]->mem->type |= 64;			// ensure it's data
  }

  blk[level]->mem->right_ptr = ptr->mem->right_ptr;	// copy RL
  VAR_COPY(blk[level]->mem->global, ptr->mem->global);	// copy global name
  blk[level]->mem->last_idx = IDX_START - 1;		// unused block
  blk[level]->mem->last_free = (systab->vol[volnum-1]->vollab->block_size >> 2) - 1; // set this up
  Copy_data(ptr, IDX_START);				// copy entire block
  btmp = blk[level]->mem;				// save this
  blk[level]->mem = ptr->mem;				// copy in this
  ptr->mem = btmp;					// end swap 'mem'

  Free_GBD(blk[level]);					// release it
  blk[level] = ptr;					// restore the ptr
  idx = (u_short *) blk[level]->mem;			// set this up
  iidx = (int *) blk[level]->mem;			// and this
  return;						// and exit
}

//-----------------------------------------------------------------------------
// Function: Copy_data
// Descript: Copy data from "from" to blk[level]
// Input(s): from GBD and index (or flag)
// Return:   none
// Note:     Must hold a write lock before calling this function
//	     All external variables describing blk[level] must be setup
//	     This function omits records with dbc = NODE_UNDEFINED
//	     This function omits pointers with record = PTR_UNDEFINED
//

void Copy_data(gbd *fptr, int fidx)			// copy records
{ int i;						// a handy int
  u_short *sfidx;					// for Indexes
  int *fiidx;						// int ver of Index
  u_char fk[260];					// for keys
  int isdata;						// a flag
  cstring *c;						// reading from old
  u_char ccc;						// common char count
  u_char ucc;						// uncommon char count
  short cs;						// new chunk size

  isdata = ((blk[level]->mem->type > 64) && (level));	// block type
  sfidx = (u_short *) fptr->mem;			// point at it
  fiidx = (int *) fptr->mem;				// point at it

  keybuf[0] = 0;					// clear this
  for (i = IDX_START; i <= blk[level]->mem->last_idx; i++) // scan to end to blk
  { chunk = (cstring *) &iidx[idx[i]];			// point at the chunk
    bcopy(&chunk->buf[2], &keybuf[chunk->buf[0] + 1], chunk->buf[1]); // update the key
    keybuf[0] = chunk->buf[0] + chunk->buf[1];		// and the size
  }							// end update keybuf[]

  for (i = IDX_START; i <= fptr->mem->last_idx; i++)	// for each Index
  { c = (cstring *) &fiidx[sfidx[i]];			// point at chunk
    bcopy(&c->buf[2], &fk[c->buf[0] + 1], c->buf[1]);	// copy key
    fk[0] = c->buf[0] + c->buf[1];			// and the length
    if (i < fidx)					// copy this one
    { continue;						// no - just continue
    }
    c = (cstring *) &(c->buf[c->buf[1] + 2]);		// point at dbc/ptr
    if (isdata)						// if data
    { if (c->len == NODE_UNDEFINED)			// junk record?
      { continue;					// ignore it
      }
    }
    else						// if a pointer
    { if ((long) c & 3)					// if not aligned
      { c = (cstring *) &c->buf[2 - ((long) c & 3)];	// align
      }
      if ((*(int *) c) == PTR_UNDEFINED)		// see if that's junk
      { continue;					// ignore it
      }
    }
    ccc = 0;						// start here
    if ((fk[0]) && (keybuf[0]))				// if any there
    { while (fk[ccc + 1] == keybuf[ccc + 1])		// while the same
      { if ((ccc == fk[0]) || (ccc == keybuf[0]))	// at end of either
        { break;					// done
        }
        ccc++;						// increment ptr
      }
    }
    ucc = fk[0] - ccc;					// get the ucc
    cs = 4 + ucc + (isdata ? (c->len + 2) : 4);		// chunk size = this
    if (!level)						// if GD
    { cs += 4;						// allow for flags
    }
    if (cs & 3)						// but
    { cs += (4 - (cs & 3));				// round up
    }

    if (cs >=
        ((blk[level]->mem->last_free*2 + 1 - blk[level]->mem->last_idx)*2))
    { if (fidx == -1)
      { return;
      }
      panic("Copy_data: about to overflow block");
    }

    blk[level]->mem->last_free -= (cs / 4);		// reset free
    idx[++blk[level]->mem->last_idx]
      = blk[level]->mem->last_free + 1;			// point at next chunk
    chunk = (cstring *) &iidx[blk[level]->mem->last_free + 1];
    chunk->len = cs;					// set the size
    chunk->buf[0] = ccc;				// ccc
    chunk->buf[1] = ucc;				// ucc
    bcopy(&fk[ccc + 1], &chunk->buf[2], ucc);		// the key
    record = (cstring *) &chunk->buf[ucc + 2];		// point at dbc/ptr
    if (isdata)						// for a data block
    { record->len = c->len;				// copy dbc
      bcopy(c->buf, record->buf, c->len);		// copy the data
      if (fidx == -1)
      { c->len = NODE_UNDEFINED;
      }
    }
    else						// for a pointer
    { Align_record();					// ensure aligned
      *(u_int *) record = *(u_int *) c;			// copy ptr
      if (fidx == -1)
      { *(int *) c = PTR_UNDEFINED;
      }
      if (!level)					// if GD
      { ((u_int *) record)[1] = ((u_int *) c)[1] & 3;	// copy flags
			// NOTE: ABOVE ALL FLAGS EXCEPT (3) CLEARED !!!!!!!!
      }
    }
    bcopy(fk, keybuf, fk[0] + 1);			// save full key
  }							// end copy loop
  return;						// and exit
}

//-----------------------------------------------------------------------------
// Function: Align_record
// Descript: Ensure that record is on a four byte boundary
// Input(s): none
// Return:   none
// Note:     Must only be called for pointer/directory blocks
//

void Align_record()					// align record (int)
{ if ((long) record & 3)				// if not aligned
  { record = (cstring *) &record->buf[2 - ((long) record & 3)]; // align
  }
  return;						// exit
}

//-----------------------------------------------------------------------------
// Function: Compress1
// Descript: Compress one block union
// Input(s): mvar * to the key to find
//	     the level to operate at
// Return:   zero or error
//

short Compress1()
{ int i;
  int curlevel;
  short s;
  u_char gtmp[VAR_LEN + 4];				// to find glob

  Get_GBDs(MAXTREEDEPTH * 2);                           // ensure this many

  curlevel = level;
  writing = 1;						// flag writing
  s = Get_data(curlevel);				// get the data
  if ((s == -ERRM7) && (!db_var.slen))			// if top
  { s = 0;						// it does exist
  }
  if (s == -ERRM7)					// if gone missing
  { while (level >= 0)
    { if (blk[level]->dirty == (gbd *) 1)
      { blk[level]->dirty = NULL;
      }
      level--;
    }
    return 0;						// just exit
  }
  if (s < 0)						// any other error
  { return s;						// return it
  }
  if (!blk[level]->mem->right_ptr)			// if no more blocks
  { if ((level == 2) && (!db_var.slen))			// and blk 1 on level 2
    { level = 0;					// look at the GD
      gtmp[1] = 128;					// start string key
      for (i = 0; i < VAR_LEN; i++)			// for each char
      { if (db_var.name.var_cu[i] == '\0')		// check for null
        { break;					// break if found
        }
        gtmp[i + 2] = db_var.name.var_cu[i];		// copy char
      }
      i += 2;						// correct count
      gtmp[i] = '\0';					// null terminate
      gtmp[0] = (u_char) i;				// add the count
      s = Locate(gtmp);					// search for it
      if (s < 0)					// failed?
      { return s;					// return error
      }
      Align_record();					// if not aligned
      *((u_int *) record) = blk[2]->block;		// new top level blk
      if (blk[level]->dirty < (gbd *) 5)		// if it needs queing
      { blk[level]->dirty = blk[level];			// terminate list
	Queit();					// and queue it
      }
	// Now, we totally release the block at level 1 for this global
      blk[1]->mem->type = 65;				// pretend it's data
      blk[1]->last_accessed = current_time(TRUE);	// clear last access
      Garbit(blk[1]->block);				// que for freeing

      bzero(&partab.jobtab->last_ref, sizeof(mvar));	// clear last ref
      return 0;						// and exit
    }
    while (level >= 0)
    { if (blk[level]->dirty == (gbd *) 1)
      { blk[level]->dirty = NULL;
      }
      level--;
    }
    return 0;						// just exit
  }
  blk[level + 1] = blk[level];				// save that
  s = Get_block(blk[level]->mem->right_ptr);
  if (s < 0)						// if error
  { while (level >= 0)
    { if (blk[level]->dirty == (gbd *) 1)
      { blk[level]->dirty = NULL;
      }
      level--;
    }
    return s;						// just exit
  }
  i = ((blk[level + 1]->mem->last_free * 2 + 1 - blk[level + 1]->mem->last_idx) * 2)
    + ((blk[level]->mem->last_free * 2 + 1 - blk[level]->mem->last_idx) * 2);
  if (i < 1024)		// if REALLY not enough space (btw: make this a param)
  { level++;
    while (level >= 0)
    { if (blk[level]->dirty == (gbd *) 1)
      { blk[level]->dirty = NULL;
      }
      level--;
    }
    return s;						// just exit
  }
  Un_key();						// unkey RL block
  level++;						// point at left blk
  Tidy_block();						// ensure it's tidy
  Copy_data(blk[level - 1], - 1);			// combine them
  if (blk[level]->dirty == (gbd *) 1)			// if not queued
  { blk[level]->dirty = (gbd *) 2;			// mark for queuing
  }
  level--;						// point at rl
  Tidy_block();						// ensure it's tidy
  if (blk[level]->mem->last_idx < IDX_START)		// if it's empty
  { blk[level]->mem->type = 65;				// pretend it's data
    blk[level]->last_accessed = current_time(TRUE);	// clear last access
    blk[level + 1]->mem->right_ptr = blk[level]->mem->right_ptr; // copy RL
    Garbit(blk[level]->block);				// que for freeing
    blk[level] = NULL;					// ignore

    if (blk[level + 1]->mem->right_ptr)			// if we have a RL
    { s = Get_block(blk[level + 1]->mem->right_ptr);	// get it
    }							// and hope it worked

  }
  else
  { if (blk[level]->dirty == (gbd *) 1)			// if not queued
    { blk[level]->dirty = (gbd *) 2;			// mark to que
      s = Add_rekey(blk[level]->block, level);		// que to re-key later
    }
  }

  if (blk[level] != NULL)				// if more to go
  { if (blk[level]->dirty == (gbd *) 2)			// if some left
    { chunk = (cstring *) &iidx[idx[IDX_START]];	// point at the first
      bcopy(&chunk->buf[1], &partab.jobtab->last_ref.slen, chunk->buf[1] + 1); // save the real key
    }
  }
  else
  { bzero(&partab.jobtab->last_ref, sizeof(mvar));	// or clear it
  }

  level += 2;						// spare level
  blk[level] = NULL;					// clear it
  for (i = level - 1; i >= 0; i--)			// scan ptr blks
  { if (blk[i] != NULL)
    { if (blk[i]->dirty == (gbd *) 2)			// if changed
      { if (blk[level] == NULL)				// list empty
        { blk[i]->dirty = blk[i];			// point at self
        }
        else
        { blk[i]->dirty = blk[level];			// else point at prev
        }
        blk[level] = blk[i];				// remember this one
      }
      else if (blk[i]->dirty == (gbd *) 1)		// if reserved
      { blk[i]->dirty = NULL;				// clear it
      }
    }
  }
  if (blk[level] != NULL)				// if something there
  { Queit();						// que that lot
  }

  return Re_key();                                      // re-key and return

}

//-----------------------------------------------------------------------------
// Function: ClearJournal
// Descript: Create/clear the journal file
// Input(s): internal volume number
// Return:   none
// Note:     Must be called with a write lock
//
void ClearJournal(int vol)				// clear journal
{ jrnrec jj;						// to write with
  int jfd;						// file descriptor
  int i;						// a handy int

  struct __attribute__ ((__packed__))
  { u_int magic;
    off_t free;
  } tmp;

  jfd = open(systab->vol[vol]->vollab->journal_file, O_TRUNC | O_RDWR | O_CREAT, 0770);	// open it
  if (jfd > 0)						// if OK
  { tmp.magic = (RSM_MAGIC - 1);
#if RSM_DBVER == 1
    tmp.free = 20;					// next free byte
#else
    tmp.free = 24;					// next free byte
#endif
    i = write(jfd, (u_char *) &tmp, 12);
    if (i < 0) fprintf(stderr, "errno = %d %s\n", errno, strerror(errno));
#if RSM_DBVER == 1
    jj.size = 8;
#else
    jj.size = 12;
#endif
    jj.action = JRN_CREATE;
    jj.uci = 0;
#if RSM_DBVER == 1
    jj.time = current_time(TRUE);
#else
    jj.time = (u_int64) current_time(TRUE);
#endif
    i = write(jfd, &jj, jj.size);			// write the create rec
    if (i < 0) fprintf(stderr, "errno = %d %s\n", errno, strerror(errno));
    (void) fchmod(jfd, S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP); // make grp wrt
    (void) close(jfd);					// and close it
    systab->vol[vol]->jrn_next = (off_t) tmp.free;	// where it's upto
  }
  return;						// done
}

//-----------------------------------------------------------------------------
// Function: DoJournal
// Descript: Write a journal record
// Input(s): jrnrec structure
//	     data pointer (set only)
// Return:   none
// Note:     Must be called with a write lock
//	     the date/time and size are filled in here
//
void DoJournal(jrnrec *jj, cstring *data) 		// Write journal
{ off_t jptr;
  int i;
  int j;

  jptr = lseek(partab.jnl_fds[volnum - 1], systab->vol[volnum - 1]->jrn_next, SEEK_SET); // address to locn
  if (jptr != systab->vol[volnum  - 1]->jrn_next)	// if failed
  { goto fail;
  }
  jj->size = 13 + sizeof(var_u) + jj->slen;
  if ((jj->action != JRN_SET) && (jj->action != JRN_KILL)) // not SET of KILL
  { jj->size = 12;					// size is 12
  }
  i = jj->size;
  if (jj->action == JRN_SET)
  { jj->size += (sizeof(short) + data->len);
  }
  jj->time = (u_int64) current_time(TRUE);			// store the time
  j = write(partab.jnl_fds[volnum - 1], jj, i);		// write header
  if (j != i)						// if that failed
  { goto fail;
  }
  if (jj->action == JRN_SET)
  { i = (sizeof(short) + data->len);			// data size
    j = write(partab.jnl_fds[volnum - 1], data, i);	// write data
    if (j != i)						// if that failed
    { goto fail;
    }
  }
  if (jj->size & 3)
  { jj->size += (4 - (jj->size & 3));			// round it
  }
  systab->vol[volnum  - 1]->jrn_next += jj->size;	// update next
  jptr = lseek(partab.jnl_fds[volnum - 1], 4, SEEK_SET);
  if (jptr != 4)
  { goto fail;
  }
  j = write(partab.jnl_fds[volnum - 1], &systab->vol[volnum  - 1]->jrn_next, sizeof(off_t)); // write next
  if (j < 0)
  { goto fail;
  }
  return;

fail:
  systab->vol[volnum - 1]->vollab->journal_available = 0; // turn it off
  close(partab.jnl_fds[volnum - 1]);			// close the file
  return;						// and exit
}
