/*
 * Package:  Reference Standard M
 * File:     rsm/database/db_locate.c
 * Summary:  module database - Locate Database Functions
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
#include <time.h>					// for gbd stuff
#include <ctype.h>					// for gbd stuff
#include <sys/types.h>					// for semaphores
#include <sys/ipc.h>					// for semaphores
#include <sys/sem.h>					// for semaphores
#include "rsm.h"					// standard includes
#include "database.h"					// database protos
#include "proto.h"					// standard prototypes
#include "error.h"					// error strings

//-----------------------------------------------------------------------------
// Function: Locate
// Descript: Locate passed in key in blk[level] updating extern vars
//	     Index, chunk, record and keybuf
// Input(s): Pointer to key to find (key[0] -> length)
// Return:   0 -> Ok, negative M error
// Note:     On fail (-ERRM7), Index etc points at the following record.
//	     External vars setup are:
//		(cstring *)	chunk	points at the chunk in the block
//		(u_short *)	idx	maps the block as an array
//		(int *)		iidx	maps the block as an array
//		(cstring *)	record	points at the data for the record
//					(not aligned for ptr/GD)
//		(u_char)	keybuf	the current full key
//

short Locate(u_char *key)				// find key
{ int i;						// a handy int

  idx = (u_short *) blk[level]->mem;			// point at the block
  iidx = (int *) blk[level]->mem;			// point at the block
  Index = 10;						// start at the start
  while (TRUE)						// loop
  { chunk = (cstring *) &iidx[idx[Index]];		// point at the chunk
    bcopy(&chunk->buf[2], &keybuf[chunk->buf[0]+1],
	  chunk->buf[1]);				// update the key
    keybuf[0] = chunk->buf[0] + chunk->buf[1];		// and the size
    record = (cstring *) &chunk->buf[chunk->buf[1]+2];	// point at the dbc
    i = UTIL_Key_KeyCmp(&keybuf[1], &key[1], keybuf[0], key[0]); // compare
    if (i == KEQUAL)					// same?
    { return 0;						// done
    }
    if (i == K2_LESSER)					// passed it?
    { return -ERRM7;					// no such
    }
    Index++;						// point at next
    if (Index > blk[level]->mem->last_idx)		// passed the end
    { return -ERRM7;					// no such
    }
  }							// end locate loop
}

//-----------------------------------------------------------------------------
// Function: Locate_next
// Descript: Locate next key in blk[level] updating extern vars
//	     Index, chunk, record and keybuf
// Input(s): none (extern vars must be setup)
// Return:   0 -> Ok, negative M error
// Note:     Must be be called with a read lock
//	     External vars setup as for Locate() above.
//

short Locate_next()					// point at next key
{ int i;						// a handy int
  short s;						// function returns

  Index++;						// point at next
  if (Index > blk[level]->mem->last_idx)		// passed end?
  { if (!blk[level]->mem->right_ptr)			// any more there?
    { return -ERRM7;					// no, just exit
    }
    i = blk[level]->mem->right_ptr;			// get right block#
    s = Get_block(i);					// attempt to get it
    if (s < 0)                                        	// if we got an error
    { return s;                                       	// return it
    }
  }							// end new block

  chunk = (cstring *) &iidx[idx[Index]];		// point at the chunk
  bcopy(&chunk->buf[2], &keybuf[chunk->buf[0]+1],
	chunk->buf[1]);					// update the key
  keybuf[0] = chunk->buf[0] + chunk->buf[1];		// and the size
  record = (cstring *) &chunk->buf[chunk->buf[1]+2];	// point at the dbc
  return 0;						// all done
}

