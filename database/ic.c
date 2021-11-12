/*
 * Package:  Reference Standard M
 * File:     rsm/database/db_ic.c
 * Summary:  module database - Database Functions, Integrity Check
 *
 * David Wicksell <dlw@linux.com>
 * Copyright © 2020-2021 Fourth Watch Software LC
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

int icerr;						// error count
int doing_full;						// type of ic

u_char wrt_buf[100];					// for output
cstring *outc;						// ditto

u_char *rlnk;						// for right links
u_char *dlnk;						// for down links
u_char *used;						// for the map
u_int volsiz;						// blocks in volume

void ic_full(void);					// full check
void ic_bits(u_int block, int flag, u_int points_at);	// check bits
u_int ic_block(u_int block, u_int points_at, u_char *kin, var_u global); // check block
void ic_map(int flag);					// check the map

extern int dbfd;					// global db file desc

//-----------------------------------------------------------------------------
// Function: DB_ic
// Descript: Do integrity check on vol according to flag
// Input(s): Volume number
//	     Check flag
// Return:   Number of errors found
//

int DB_ic(int vol, int block)                  		// integrity checker
{
  int uci;						// UCI#
  u_int b1;						// a block

  if (vol > MAX_VOL)					// within limits?
  { return -ERRM26;					// no - error
  }
  if (systab->vol[vol - 1] == NULL)			// is it mounted?
  { return -ERRM26;					// no - error
  }
  volnum = vol;						// save this
  curr_lock = 0;					// ensure this is clear
  writing = 0;						// clear this
  icerr = 0;						// clear errors
  doing_full = 0;					// and this
  outc = (cstring *) wrt_buf;				// for reporting
  used = ((u_char *) systab->vol[volnum - 1]->map);	// point at map
  volsiz = systab->vol[volnum - 1]->vollab->max_block;	// number of blocks
  gbd_expired = 0;					// clear this
  for (level = 0; level < MAXTREEDEPTH; blk[level++] = NULL)
    continue;

  if (block == 0)					// full check?
  { level = 0;
    ic_full();						// do it
    gbd_expired = GBD_EXPIRED;
    return icerr;					// and return
  }
  else if (block > 0)
  { level = 1;
    for (uci = 0; uci < UCIS; uci++)			// scan uci table
    { b1 = systab->vol[volnum - 1]->vollab->uci[uci].global; // get GD
      if (b1 == block)					// if block is GD
      { level = 0;
        break;
      }
    }
    ic_block(block, 0, NULL, (var_u) 0ull);		// check it
    gbd_expired = GBD_EXPIRED;
    return icerr;					// and return
  }
  dbfd = partab.vol_fds[volnum - 1];			// set this up
  ic_map(block);					// map check
  return icerr;						// and return
}

//-----------------------------------------------------------------------------
// Function: ic_full
// Descript: Do full integrity check on volnum (updates icerr)
// Input(s): none
// Return:   none
//

void ic_full(void)					// full check
{ int i;						// a handy int
  int j;						// and another
  int uci;						// uci#
  u_int b1;						// a block
  u_char off;						// offset
  u_char msg[20];					// for messages

  doing_full = 1;					// set this
  i = (volsiz / 8) + 1;					// number of bytes
  rlnk = malloc(i);					// for right links
  if (rlnk == NULL)					// if failed
  { panic("ic_full: can't get memory for rlnk");	// die
  }
  dlnk = malloc(i);					// for down links
  if (dlnk == NULL)					// if failed
  { panic("ic_full: can't get memory for dlnk");	// die
  }
  bzero(rlnk, i);					// clear this
  bzero(dlnk, i);					// and this
  rlnk[0] = 1;						// say blk 0 used
  dlnk[0] = 1;						// say blk 0 used

  for (uci = 0; uci < UCIS; uci++)			// scan uci table
  { b1 = systab->vol[volnum - 1]->vollab->uci[uci].global; // get GD
    if (b1 == 0)					// if none
    { continue;						// ignore it
    }
    if ((used[b1 / 8] & (1U << (b1 & 7))) == 0)		// if marked free
    { outc->len = sprintf((char *) &outc->buf[0], "%10u free (global directory for UCI %d) - skipped", b1, uci + 1); // error msg
      icerr++;						// count it
      (void) SQ_Write(outc);				// output it
      (void) SQ_WriteFormat(SQ_LF);			// and a !
      continue;						// ignore it
    }
    ic_bits(b1, 3, 0);					// set link bits
    level = 0;						// clear level
    ic_block(b1, 0, NULL, (var_u) 0ull);		// check the block
  }							// end main for loop

  for (i = 0; i < (volsiz / 8); i++)			// for each byte in map
  { for (j = 0; j < 8; j++)				// for each bit
    { off = (1U << j);					// setup offset
      b1 = ((u_int) i * 8) + j;				// and block#
      bcopy("both pointers\0", msg, 14);		// default msg
      if ((used[i] & off) != 0)				// if used
      { if (((rlnk[i] & off) == 0) ||			// if no RL
	    ((dlnk[i] & off) == 0))			// or no DL
	{ if ((rlnk[i] & off) != 0)			// if it has RL
	  { bcopy("down pointer\0", msg, 13);		// say down
	  }
	  else if ((dlnk[i] & off) != 0)		// if it has DL
	  { bcopy("right pointer\0", msg, 14);		// say right
	  }
	  outc->len = sprintf((char *) &outc->buf[0], "%10u is used, missing %s", b1, msg); // error msg
	  icerr++;					// count it
	  (void) SQ_Write(outc);				// output it
	  (void) SQ_WriteFormat(SQ_LF);			// and a !
	}						// end error code
      }							// end used block
      else if (((rlnk[i] & off) != 0) ||		// there is a RL
	       ((dlnk[i] & off) != 0))			// or a DL and NOT used
      { outc->len = sprintf((char *) &outc->buf[0], "%10u is UNUSED but is pointed to", b1);
	icerr++;					// count it
	(void) SQ_Write(outc);				// output it
	(void) SQ_WriteFormat(SQ_LF);			// and a !
      }
    }
  }

  free(rlnk);						// free that
  free(dlnk);						// and that
  return;						// and exit
}

//-----------------------------------------------------------------------------
// Function: ic_bits
// Descript: check/set bits in rlnk and dlnk
// Input(s): Block number to check
//	     flag: 1 = chk RL, 2 = chk DL, 3 = check both
//	     block that points at this block (if any)
// Return:   none
//

void ic_bits(u_int block, int flag, u_int points_at)	// check bits

{ u_int i;						// a handy int
  u_char off;						// bit offset

  i = block >> 3;					// byte number
  off = 1U << (block & 7);				// bit offset

  if (flag & 1)
  { if (rlnk[i] & off)					// check rlnk
    { outc->len = sprintf((char *) &outc->buf[0], "%10u <- %10u - duplicate right pointer", block, points_at); // error msg
      icerr++;						// count it
      (void) SQ_Write(outc);				// output it
      (void) SQ_WriteFormat(SQ_LF);			// and a !
    }
    else						// set the bit
    { rlnk[i] |= off;					// set
    }
  }
  if (flag & 2)
  { if (dlnk[i] & off)					// check dlnk
    { outc->len = sprintf((char *) &outc->buf[0], "%10u <- %10u - duplicate down pointer", block, points_at); // error msg
      icerr++;						// count it
      (void) SQ_Write(outc);				// output it
      (void) SQ_WriteFormat(SQ_LF);			// and a !
    }
    else						// set the bit
    { dlnk[i] |= off;					// set
    }
  }

  if ((points_at) &&					// points_at supplied
      ((used[i] & off) == 0))				// marked free?
  { outc->len = sprintf((char *) &outc->buf[0], "%10u <- %10u - block is free", block, points_at); // error msg
    icerr++;						// count it
    (void) SQ_Write(outc);				// output it
    (void) SQ_WriteFormat(SQ_LF);			// and a !
  }
  return;						// done
}

//-----------------------------------------------------------------------------
// Function: ic_block
// Descript: check supplied block
// Input(s): Block number to check
// Return:   none
//

u_int ic_block(u_int block, u_int points_at, u_char *kin, var_u global)	// check block
{ int i;						// a handy int
  short s;						// for funct
  int left_edge;					// a flag
  u_char emsg[80];					// for errors
  int isdata;						// blk type
  int Lidx;						// Local index
  int Llevel;						// local level
  int Llast;						// local last_idx
  gbd *Lgbd;						// and gbd
  u_int b1;						// a block
  u_char k[MAX_KEY_SIZE + 5];				// local key
  u_short *isx;						// a map
  u_int *iix;						// a map
  u_char k1[MAX_KEY_SIZE + 5];				// for keys
  u_char k2[MAX_KEY_SIZE + 5];				// for keys
  cstring *c;						// for chunk
  cstring *r;						// for record
  u_char *eob;						// end of block
  u_int lb;						// last block
  u_int brl;						// block rl

  while (SemOp(SEM_GLOBAL, READ))			// get a read lock
    continue;

  s = Get_block(block);					// get it
  if (s < 0)						// if that failed
  { s = UTIL_strerror(s, emsg);				// decode message
    outc->len = sprintf((char *) &outc->buf[0], "%10u <- %10u - error getting - %s", block, points_at, emsg); // error msg
    icerr++;						// count it
    (void) SQ_Write(outc);				// output it
    (void) SQ_WriteFormat(SQ_LF);			// and a !
    s = SemOp(SEM_GLOBAL, -curr_lock);			// release the lock
    return 0;						// and exit
  }

  if ((used[block / 8] & (1U << (block & 7))) == 0)	// if marked free
  { outc->len = sprintf((char *) &outc->buf[0], "%10u <- %10u marked free, type = %d", block, points_at, blk[level]->mem->type);
                                                        // error msg
      icerr++;						// count it
      (void) SQ_Write(outc);				// output it
      (void) SQ_WriteFormat(SQ_LF);			// and a !
      return 0;						// give up
  }

  eob = (u_char *) blk[level]->mem + systab->vol[volnum - 1]->vollab->block_size - 1;

  if (blk[level]->dirty == NULL)
  { blk[level]->dirty = (gbd *) 3;			// reserve it
  }

  isdata = ((blk[level]->mem->type > 64) && (level));	// blk type
  Llevel = level;					// save this
  Lgbd = blk[level];					// and this
  s = SemOp(SEM_GLOBAL, -curr_lock);			// release the lock

  if (!var_empty(global))
  { if (!var_equal(global, blk[level]->mem->global))	// check global
    { outc->len = sprintf((char *) &outc->buf[0], "%10u <- %10u - global is wrong", block, points_at); // error msg
      icerr++;						// count it
      (void) SQ_Write(outc);				// output it
      (void) SQ_WriteFormat(SQ_LF);			// and a !
    }
  }

  chunk = (cstring *) &iidx[idx[IDX_START]];		// point at 1st chunk
  left_edge = (!chunk->buf[1]);				// check for first
  if (chunk->buf[0])					// non-zero ccc
  { outc->len = sprintf((char *) &outc->buf[0], "%10u <- %10u - non-zero ccc on first key", block, points_at); // error msg
    icerr++;						// count it
    (void) SQ_Write(outc);				// output it
    (void) SQ_WriteFormat(SQ_LF);			// and a !
  }
  else if (kin != NULL)					// if key supplied
  { if (bcmp(kin, &chunk->buf[1], kin[0] + 1))		// if not the same
    { outc->len = sprintf((char *) &outc->buf[0], "%10u <- %10u - down link differs from first key", block, points_at); // error msg
      icerr++;						// count it
      (void) SQ_Write(outc);				// output it
      (void) SQ_WriteFormat(SQ_LF);			// and a !
    }
  }

  if (!isdata)						// if a pointer
  { Llast = blk[level]->mem->last_idx;			// remember this
    lb = 0;						// clear this
    brl = 0;						// and this
    for (Lidx = IDX_START; Lidx <= Llast; Lidx++)
    { level = Llevel;					// restore this
      if ((!level) && (Lidx == IDX_START))		// ignore entry
      { continue;					// for $GLOBAL in GD
      }
      blk[level] = Lgbd;				// and this
      idx = (u_short *) blk[level]->mem;		// point at the block
      iidx = (int *) blk[level]->mem;			// point at the block
      chunk = (cstring *) &iidx[idx[Lidx]];		// point at the chunk
      if (!level)					// a GD
      { k[0] = '\0';					// empty key
      }
      else						// pointer
      { bcopy(&chunk->buf[2], &k[chunk->buf[0] + 1], chunk->buf[1]); // update the key
        k[0] = chunk->buf[0] + chunk->buf[1];		// and the size
      }
      record = (cstring *) &chunk->buf[chunk->buf[1] + 2]; // point at the dbc
      Align_record();					// ensure aligned
      b1 = *(u_int *) record;				// get blk#
      if ((b1 > volsiz) || (!b1))			// out of range
      { outc->len = sprintf((char *) &outc->buf[0], "%10u <- %10u - (%d) block %u outside vol - skipped",
                    block, points_at, Lidx, b1);	// error msg
        icerr++;					// count it
        (void) SQ_Write(outc);				// output it
        (void) SQ_WriteFormat(SQ_LF);			// and a !
	continue;					// ignore
      }
      for (i = 0; i < level; i++)			// scan above
      { if ((blk[level]) && (blk[level]->block == b1))	// check for loop
	{ outc->len = sprintf((char *) &outc->buf[0], "%10d <- %10d - points at itself", b1, block); // error msg
	  icerr++;					// count it
	  (void) SQ_Write(outc);			// output it
	  (void) SQ_WriteFormat(SQ_LF);			// and a !
	  b1 = 0;					// flag error
	  break;					// quit
        }
      }							// end loop check
      if (!b1)						// check again
      { continue;
      }
      if (doing_full)
      { ic_bits(b1, 2 + (left_edge || !level), block);	// check bits
      }
      if ((lb) && (level))				// if we have a lb
      { if (brl != b1)					// if not the same
	{ outc->len = sprintf((char *) &outc->buf[0], "%10d <- %10d - right is %10d next down is %10d", lb, block, brl, b1);
                                                        // error msg
	  icerr++;					// count it
	  (void) SQ_Write(outc);				// output it
	  (void) SQ_WriteFormat(SQ_LF);			// and a !
	}
      }
      lb = b1;						// save for next
      left_edge = 0;					// clear this
      level++;						// down a level
      if (level > 1)					// from a pointer
      { brl = ic_block(b1, block, k, blk[level - 1]->mem->global); // check block
      }
      else						// from GD
      { brl = ic_block(b1, block, k, (var_u) 0ull);	// check the block (DO BETTER LATER)
      }
    }							// end block scan
  }							// end if (!isdata)
  level = Llevel;					// restore this
  blk[level] = Lgbd;					// and this
  idx = (u_short *) blk[level]->mem;			// point at the block
  iidx = (int *) blk[level]->mem;			// point at the block

  if ((blk[level]->mem->right_ptr) && (doing_full))	// if we have a RL
  { ic_bits(blk[level]->mem->right_ptr, 1, block);	// say so
  }
  if (blk[level]->dirty == (gbd *) 3)			// if we reserved it
  { blk[level]->dirty = NULL;				// clear it
  }

  if (blk[level]->mem->last_idx < IDX_START)
  { outc->len = sprintf((char *) &outc->buf[0], "%10d <- %10d - last_idx is too low", block, points_at); // error msg
    icerr++;						// count it
    (void) SQ_Write(outc);				// output it
    (void) SQ_WriteFormat(SQ_LF);			// and a !
  }

  if (((blk[level]->mem->last_free * 2 + 1 - blk[level]->mem->last_idx) * 2) < 0)
  { outc->len = sprintf((char *) &outc->buf[0], "%10d <- %10d - last_idx too high or last_free too low", block, points_at);
                                                        // error msg
    icerr++;						// count it
    (void) SQ_Write(outc);				// output it
    (void) SQ_WriteFormat(SQ_LF);			// and a !
  }
  isdata = ((blk[level]->mem->type > 64) && (level));
  isx = (u_short *) blk[level]->mem;
  iix = (u_int *) blk[level]->mem;
  k1[0] = 0;

  for (i = IDX_START; i <= blk[level]->mem->last_idx; i++)
  { c = (cstring *) &iix[isx[i]];
    if (&c->buf[c->len - 3] > eob)
    { outc->len = sprintf((char *) &outc->buf[0], "%10d <- %10d - chunk size is too big - overflows block", block, points_at);
                                                        // error msg
      icerr++;						// count it
      (void) SQ_Write(outc);				// output it
      (void) SQ_WriteFormat(SQ_LF);			// and a !
    }
    r = (cstring *) &c->buf[c->buf[1] + 2];
    if (isdata && (r->len != NODE_UNDEFINED))
    { if (&r->buf[(r->len == 0) ? 0 : (r->len - 1)] > eob)
      { outc->len = sprintf((char *) &outc->buf[0], "%10d <- %10d - dbc is too big - overflows block", block, points_at);
                                                        // error msg
        icerr++;					// count it
        (void) SQ_Write(outc);				// output it
        (void) SQ_WriteFormat(SQ_LF);			// and a !
      }
    }
    if (c->buf[0] == 255)
    { continue;
    }
    if ((i == IDX_START) && (c->buf[0]))
    { outc->len = sprintf((char *) &outc->buf[0], "%10d <- %10d - non-zero ccc in first record", block, points_at); // error msg
      icerr++;						// count it
      (void) SQ_Write(outc);				// output it
      (void) SQ_WriteFormat(SQ_LF);			// and a !
    }
    if ((i > IDX_START) && (!c->buf[1]))
    { outc->len = sprintf((char *) &outc->buf[0], "%10d <- %10d - zero ucc found", block, points_at); // error msg
      icerr++;						// count it
      (void) SQ_Write(outc);				// output it
      (void) SQ_WriteFormat(SQ_LF);			// and a !
    }
    bcopy(&c->buf[2], &k2[c->buf[0] + 1], c->buf[1]);
    k2[0] = c->buf[0] + c->buf[1];
    if ((k2[0]) || (i > IDX_START))
    { if (UTIL_Key_KeyCmp(&k1[1], &k2[1], k1[0], k2[0]) != K2_GREATER)
      { outc->len = sprintf((char *) &outc->buf[0], "%10d <- %10d - (%d) key does not follow previous", block, points_at, i);
                                                        // error msg
        icerr++;					// count it
        (void) SQ_Write(outc);				// output it
        (void) SQ_WriteFormat(SQ_LF);			// and a !
      }
    }
    bcopy(k2, k1, k2[0] + 1);
  }
  return blk[level]->mem->right_ptr;			// save for return
}

//-----------------------------------------------------------------------------
// Function: ic_map
// Descript: check map block
// Input(s): flag, -1 = Check only, -2 = Check and fix, -3 as -2 + track upto
// Return:   none
//

void ic_map(int flag)					// check the map
{ int i;						// a handy int
  u_int block;						// current block
  u_int base;						// current block base
  off_t file_off;                               	// for lseek() et al
  int lock;						// required lock
  int off;						// offset in byte
  u_char *c;						// map ptr
  u_char *e;						// end of map
  gbd *ptr;						// a handy pointer
  int status;						// block status
  u_char type_byte;					// for read

  lock = ((flag == -1) ? READ : WRITE);			// what we need
  c = (u_char *) systab->vol[volnum - 1]->map;		// point at it
  e = &c[systab->vol[volnum - 1]->vollab->max_block >> 3]; // and the end
  off = 1;						// start at 1
  while (c <= e)					// scan the map
  { base = ((u_int) (c - (u_char *) systab->vol[volnum - 1]->map)) << 3;
							// base block number
    while (SemOp(SEM_GLOBAL, lock))			// grab a lock
      continue;

    for (; off < 8; off++)				// scan the byte
    { block = base + off;				// the block#
      status = -1;					// not yet known
      if (block > systab->vol[volnum - 1]->vollab->max_block)
      { continue;
      }
      ptr = systab->vol[volnum - 1]->gbd_hash[block & (GBD_HASH - 1)];
      while (ptr != NULL)				// scan for block
      { if (ptr->block == block)			// if found
	{ type_byte = ptr->mem->type;			// save this
	  if (ptr->mem->type)				// if used
	  { status = 1;					// say used
	  }
	  else
	  { status = 0;
	  }
	  break;					// and quit loop
	}
	ptr = ptr->next;				// point at next
      }							// end memory check
      if (status == -1)					// if not found
      { file_off = (off_t) block - 1;			// block#
        file_off = (file_off * (off_t) systab->vol[volnum - 1]->vollab->block_size)
		 + (off_t) systab->vol[volnum - 1]->vollab->header_bytes;
        file_off = lseek(dbfd, file_off, SEEK_SET);	// Seek to block
        if (file_off < 1)
        { panic("ic_map: lseek failed!!");		// die on error
        }
        i = read(dbfd, &type_byte, 1);			// read one byte
	if (i < 0)
        { panic("ic_map: read failed!!");		// die on error
        }
	status = (type_byte != 0);			// check used
      }							// end disk read
      if ((*c & (1U << off)) && (status))		// used and OK
      { continue;					// go for next (in for)
      }
      if (((*c & (1U << off)) == 0) && (!status))	// free and OK
      { continue;					// go for next (in for)
      }
      icerr++;						// count error
      if (flag == -1)					// check only
      { continue;					// continue
      }
      if (status)					// used
      { *c |= (u_char) (1U << off);			// set the bit
      }
      else						// free
      { *c &= (u_char) ~(1U << off);			// clear it
      }
      systab->vol[volnum - 1]->map_dirty_flag = 1;	// map needs writing
    }							// end byte scan
    SemOp(SEM_GLOBAL, -curr_lock);			// free lock
    c++;						// point at next
    off = 0;						// now start at 0
    if (flag == -3)					// daemon?
    { systab->vol[volnum - 1]->upto = ((u_int) (e - c)) << 3;
    }
  }							// end main while
  if (flag == -3)					// daemon?
  { systab->vol[volnum - 1]->upto = 0;			// clear this
  }
  return;						// done
}