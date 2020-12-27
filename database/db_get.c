/*
 * Package:  Reference Standard M
 * File:     rsm/database/db_get.c
 * Summary:  module database - Get Database Functions
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
#include <ctype.h>					// for gbd stuff
#include <sys/types.h>					// for semaphores
#include <sys/ipc.h>					// for semaphores
#include <sys/sem.h>					// for semaphores
#include "rsm.h"					// standard includes
#include "database.h"					// database protos
#include "proto.h"					// standard prototypes
#include "error.h"					// error strings

//-----------------------------------------------------------------------------
// Function: Get_data
// Descript: Locate and return data described in db_var
// Input(s): Direction (flag), negative means backwards, 0 forward
//	     > 0 means stop at this level.
// Return:   String length -> Ok, negative M error
//	     extern variables defined in db_main.c are also setup
//		level 	-> pointer to current level in blk[]
//		blk[]	-> from 0 to level (how we got here)
//			   unless blk[0] == NULL (for lastused)
//	     This calls Locate() which sets up chunk, record, idx,
//		iidx, keybuf Index.
// NOTE: lastused block is NOT used if dir != 0 or journaling is on and writing

short Get_data(int dir)					// locate a record
{ int i;						// a handy int
  short s;						// for function returns
  u_char tmp[VAR_LEN + 4];				// spare string
  gbd *ptr;						// handy pointer

  if (!curr_lock)					// ensure locked
  { s = SemOp(SEM_GLOBAL, READ);			// take a read lock
    if (s < 0)						// if we got an error
    { return s;						// return it
    }
  }

  if (systab->vol[db_var.volset - 1] == NULL)		// vol still mounted?
  { return (-ERRM26);					// no - error
  }
  if ((bcmp("$GLOBAL\0", &db_var.name.var_cu[0], 8) == 0) || // if ^$G
      (dir != 0) ||					// or level or backward
      ((systab->vol[volnum - 1]->vollab->journal_available) && // or journaling
       (writing)))					// and writing
  { systab->last_blk_used[partab.jobtab - systab->jobtab] = 0; // zot this
  }
			// NOTE - LASTUSED NEEDS TO BE BY VOLUME SET
  else
  { i = systab->last_blk_used[partab.jobtab - systab->jobtab]; // get last used
    if ((i) && ((((u_char *)systab->vol[volnum-1]->map)[i>>3]) &(1U<<(i&7))))
							// if one there
    { systab->vol[volnum-1]->stats.lasttry++;		// count a try
      ptr = systab->vol[volnum-1]->gbd_hash[i & (GBD_HASH - 1)]; // get listhead
      while (ptr != NULL)				// for each in list
      { if (ptr->block == i)				// found it
        { if ((!var_equal(ptr->mem->global, db_var.name)) || // wrong global or
	      (ptr->mem->type != (db_var.uci + 64)) ||	// wrong uci/type or
	      (ptr->last_accessed == (time_t) 0))	// not available
          { break;					// exit the loop
	  }
	  level = LAST_USED_LEVEL;			// use this level
	  blk[level] = ptr;				// point at it
	  s = Locate(&db_var.slen);			// check for the key
	  if ((s >= 0) ||				// if found or
	      ((s = -ERRM7) &&				// not found and
	       (Index <= blk[level]->mem->last_idx) &&	// still in block
	       (Index > IDX_START)))			// not at beginning
	  { systab->vol[volnum-1]->stats.lastok++;	// count success
	    blk[level]->last_accessed = current_time(TRUE); // accessed
            for (i = 0; i < level; blk[i++] = NULL);	// zot these
	    if (!s)					// if ok
	    { s = record->len;				// get the dbc
	    }
	    if ((writing) && (blk[level]->dirty == NULL)) // if writing
	    { blk[level]->dirty = (gbd *) 1;		// reserve it
	    }
	    if ((!db_var.slen) && (!s) &&
	        ((partab.jobtab->last_block_flags & GL_TOP_DEFINED) == 0))
	    { s = -ERRM7;				// check for top node
	    }
	    return s;					// and return
	  }
	  blk[level] = NULL;				// clear this
	  level = 0;					// and this
	  break;					// and exit loop
        }						// end found block
        ptr = ptr->next;				// get next
      }							// end while ptr
    }							// end last used stuff
    systab->last_blk_used[partab.jobtab - systab->jobtab] = 0; // zot it
  }

  i = systab->vol[db_var.volset-1]->vollab->uci[db_var.uci-1].global;
							// get directory blk#
  if (!i)						// if nosuch
  { return (-ERRM26);					// then error
  }

  level = 0;						// where it goes
  s = Get_block(i);					// get the block
  if (s < 0)						// error?
  { return s;						// give up
  }

  if (bcmp("$GLOBAL\0", &db_var.name.var_cu[0], 8) == 0) // if ^$G
  { s = Locate(&db_var.slen);				// look for it
    if (s >= 0)						// if found
    { Align_record();
    }
    return s;						// end ^$G() lookup
  }

  tmp[1] = 128;						// start string key
  for (i = 0; i < VAR_LEN; i++)				// for each char
  { if (db_var.name.var_cu[i] == '\0')			// check for null
    { break;						// break if found
    }
    tmp[i + 2] = db_var.name.var_cu[i];			// copy char
  }
  i += 2;						// correct count
  tmp[i] = '\0';					// null terminate
  tmp[0] = (u_char) i;					// add the count

  s = Locate(tmp);					// search for it
  if (s < 0)						// failed?
  { return s;						// return error
  }
  partab.jobtab->last_block_flags = 0;			// clear JIC
  Align_record();					// if not aligned
  i = *(int *) record;					// get block#
  if (!i)						// none there?
  { return -ERRM7;					// say nosuch
  }
  partab.jobtab->last_block_flags = ((u_int *) record)[1]; // save flags

  if (partab.jobtab->last_block_flags > 3)		// **** TEMP      ????
  { partab.jobtab->last_block_flags &= 3;		// CLEAR UNUSED   ????
    ((u_int *) record)[1] = partab.jobtab->last_block_flags; // RESET     ????
  }							//		  ????

  level++;						// where we want it
  s = Get_block(i);					// get the block
  if (s < 0)						// error?
  { return s;						// give up
  }
  while (blk[level]->mem->type < 65)			// while we have ptrs
  {
    if (!var_equal(blk[level]->mem->global, db_var.name))
    { return -(ERRMLAST+ERRZ61);			// database stuffed
    }
    s = Locate(&db_var.slen);				// locate the key
    if (s == -ERRM7)					// failed to find?
    { Index--;						// yes, backup the Index
    }
    else if (s < 0)					// else if error
    { return s;						// return it
    }
    else if (dir < 0)					// if found and want -
    { Index--;						// backup the Index
      if (Index < IDX_START)				// can't happen?
      { panic("Get_data: Problem with negative direction");
      }
    }

    chunk = (cstring *) &iidx[idx[Index]];		// point at the chunk
    record = (cstring *) &chunk->buf[chunk->buf[1]+2];	// point at the dbc
    Align_record();					// if not aligned
    if (level == dir)					// stop here?
    { return s;						// yes - return result
    }
    i = *(int *) record;				// get block#
    level++;						// where it goes
    s = Get_block(i);					// get the block
    if (s < 0)						// error?
    { return s;						// give up
    }
  }							// end while ptr

  if (!var_equal(blk[level]->mem->global, db_var.name))
  { return -(ERRMLAST+ERRZ61);				// database stuffed
  }
  s = Locate(&db_var.slen);				// locate key in data
  if (dir < 1)						// if not a pointer
  { systab->last_blk_used[partab.jobtab - systab->jobtab] = i; // set last used
  }
  if ((!db_var.slen) && (!s) && ((partab.jobtab->last_block_flags & GL_TOP_DEFINED) == 0))
  { if (!record->len)
    { s = -ERRM7;					// check for top node
    }
  }

  if (!s)						// if ok
  { s = record->len;					// get the dbc
  }
  return s;						// return result
}
