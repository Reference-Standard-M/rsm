/*
 * Package:  Reference Standard M
 * File:     rsm/util/util_routine.c
 * Summary:  module RSM util_routine - routine functions
 *
 * David Wicksell <dlw@linux.com>
 * Copyright © 2020-2021 Fourth Watch Software LC
 * https://gitlab.com/Reference-Standard-M/rsm
 *
 * Based on MUMPS V1 by Raymond Douglas Newman
 * Copyright (c) 1999-2016
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

#include <stdio.h>                              // always include
#include <stdlib.h>                             // these two
#include <errno.h>                              // error stuff
#include <string.h>				// for bcopy
#include <strings.h>
#include <time.h>				// for ctime
#include <unistd.h>				// for sleep
#include <sys/types.h>                          // for u_char def
#include "rsm.h"                                // standard includes
#include "compile.h"				// rbd structures
#include "proto.h"				// the prototypes

// The following is called ONLY from init_start.c
//
void Routine_Init(void)				// setup rdb for this vol
{ rbd *rou;					// a routine pointer
  u_int i;					// an int

  for (i = 0; i < RBD_HASH; i++)		// the hash table
    systab->vol[0]->rbd_hash[i] = NULL;		// clear it out

  rou = (rbd *) systab->vol[0]->rbd_head;	// free space entry

  systab->vol[0]->rbd_hash[RBD_HASH] = rou;	// head of free list

  i = (char *) systab->vol[0]->rbd_end - (char *) systab->vol[0]->rbd_head; // memory available

  rou->fwd_link = NULL;				// no forward link
  rou->chunk_size = i;				// size of this bit of free
  rou->attached = 0;				// nothing attached
  rou->last_access = 0;				// not used
  VAR_CLEAR(rou->rnam);				// no routine
  rou->uci = 0;					// no uci
  rou->vol = 0;					// no vol
  rou->rou_size = 0;				// no routine here

  return;					// done
}

// The following are internal only (first called from $&DEBUG())
void Dump_rbd(void)				// dump rbds
{ int i;					// an int
  short s;					// for function returns
  rbd *p;					// a pointer
  char tmp[VAR_LEN + 2];			// some space
  time_t t;					// for time

  s = SemOp(SEM_ROU, -systab->maxjob);		// write lock the rbds
  if (s < 0) return;				// exit on error
  p = (rbd *) systab->vol[0]->rbd_head;		// get the start
  t = current_time(FALSE);
  printf("Dump of all Routine Buffer Descriptors on %s\r\n", ctime(&t));
  printf("Free at %10p\r\n", systab->vol[0]->rbd_hash[RBD_HASH]);
  printf("       Address     fwd_link chunk_size attach last_access UCI VOL routine_size routine_name\r\n");
  tmp[VAR_LEN] = '\0';				// null terminate temp
  while (TRUE)					// for all
  { for (i = 0; i < VAR_LEN; i++) tmp[i] = ' ';	// space fill tmp[]
    for (i = 0; i < VAR_LEN; i++)
    { if (p->rnam.var_cu[i] == 0) break;
      tmp[i] = p->rnam.var_cu[i];
    }
    printf("%10p %12p %10u %6d %11lld %3d %3d %12d %s\r\n",
      p, p->fwd_link, p->chunk_size, p->attached, (long long) p->last_access, p->uci, p->vol, p->rou_size, tmp);
    p = (rbd *) ((u_char *) p + p->chunk_size); // point at next
    if (p >= (rbd *) systab->vol[0]->rbd_end) break; // quit when done
  }
  s = SemOp(SEM_ROU, systab->maxjob);		// release lock
  return;					// and exit
}

int Routine_Hash(var_u routine)			// return hash code
{ int hash = 0;					// for the return
  int i;					// a handy int
  int j;					// another handy int
  int p[4][8] = {{3, 5, 7, 11, 13, 17, 19, 23},
                 {29, 31, 37, 41, 43, 47, 53, 59},
                 {61, 67, 71, 73, 79, 83, 89, 97},
                 {101, 103, 107, 109, 113, 127, 131, 137}}; // odd primes
  for (i = 0; i < (VAR_LEN / 8); i++)
  { if (routine.var_qu[i] == 0) break;
    for (j = 0; j < 8; j++)                       // for each character
    { if (routine.var_qu[i] == 0) break;
      hash = (((routine.var_qu[i] & 0xFF) * p[i][j]) + hash); // add that char
      routine.var_qu[i] = (routine.var_qu[i] >> 8); // right shift one byte
    }                                             // end hash loop
  }
  return (hash % RBD_HASH);			// return the code
}

void Routine_Combine(rbd *pointer)		// combine with following block
{ rbd *ptr;					// a handy pointer
  rbd *p;					// and another

  ptr = (rbd *) ((u_char *) pointer + pointer->chunk_size); // point at next

  p = systab->vol[0]->rbd_hash[RBD_HASH];	// see where it points
  if (p == ptr)					// if that's us
    systab->vol[0]->rbd_hash[RBD_HASH] = ptr->fwd_link; // point at our fwd link
  else						// else find our entry
  { while (p->fwd_link != ptr) p = p->fwd_link; // find previous entry
    p->fwd_link = ptr->fwd_link;		// point it at our forward link
  }
  pointer->chunk_size += ptr->chunk_size;	// add the two sizes together
  return;					// and exit
}

void Routine_Free(rbd *pointer)			// call internally, rbd locked
{ rbd *ptr;					// a handy pointer
  rbd *p;					// and another
  int hash;					// hash pointer

  pointer->rou_size = 0;			// flag 'not used'

  hash = Routine_Hash(pointer->rnam);		// get the hash
  ptr = systab->vol[0]->rbd_hash[hash];		// see where it points
  if (ptr == pointer)				// if that's us
  { systab->vol[0]->rbd_hash[hash] =
      pointer->fwd_link;			// point at our forward link
  }
  else						// else find our entry
  { while (TRUE)
    { if (ptr == NULL)				// if end of list
      { ptr = systab->vol[0]->rbd_hash[RBD_HASH]; //point at first free
	if (pointer == ptr)
	{ systab->vol[0]->rbd_hash[RBD_HASH] = pointer->fwd_link;
	}
	else
        { while (ptr != NULL)			// scan free list
          { if (ptr->fwd_link == pointer)	// if in freelist
            { ptr->fwd_link = pointer->fwd_link; // take it out
	      break;				// and quit
            }
            ptr = ptr->fwd_link;		// point at next
	    // if (ptr == NULL) we should search else where!!!
          }
	}
        break;
      }
      if (ptr->fwd_link == pointer)		// if found
      { ptr->fwd_link = pointer->fwd_link;	// point it at our forward link
        break;					// and quit
      }
      ptr = ptr->fwd_link; 			// point to next
    }
  }
  pointer->fwd_link = systab->vol[0]->rbd_hash[RBD_HASH]; //point at first free
  systab->vol[0]->rbd_hash[RBD_HASH] = pointer;	// add to free list
  VAR_CLEAR(pointer->rnam);			// zot rou name
  pointer->uci = 0;				// and uci
  pointer->vol = 0;				// and vol
  pointer->last_access = (time_t) 0;		// and access

  while (TRUE)					// until end of list
  { ptr = (rbd *) ((u_char *) pointer + pointer->chunk_size); // point at next
    if (ptr >= (rbd *) systab->vol[0]->rbd_end) break;	// quit when done
    if (ptr->rou_size == 0)			// if there is no routine
      Routine_Combine(pointer);			// combine it in
    else break;					// else done
  }

  while (TRUE)					// look for previous
  { ptr = (rbd *) systab->vol[0]->rbd_head;	// start of rbds
    if (ptr == pointer) return;			// same - all done
    while (TRUE)				// scan for previous
    { p = (rbd *) ((u_char *) ptr + ptr->chunk_size); // point at next
      if (p == pointer) break;			// found it (in ptr)
      ptr = p;					// remember this one
    }
    if (ptr->rou_size == 0)			// if unused
    { pointer = ptr;				// make this one active
      Routine_Combine(pointer);			// combine it in
    }
    else return;				// else all done
  }
}

void Routine_Collect(time_t off)		// collect based on time
{ rbd *ptr;					// a pointer

  off = current_time(TRUE) - off;		// get compare time
  ptr = (rbd *) systab->vol[0]->rbd_head;	// head of rbds
  while (TRUE)					// scan whole list
  { if ((ptr->attached < 1) &&			// nothing attached
        (ptr->last_access < off) &&		// and it fits the time
	(ptr->rou_size > 0))			// and not already free
    { Routine_Free(ptr);			// free it
      ptr = (rbd *) systab->vol[0]->rbd_head;	// start from the begining
    }
    ptr = (rbd *) ((u_char *) ptr + ptr->chunk_size); // point at next
    if (ptr >= (rbd *) systab->vol[0]->rbd_end) break;	// quit when done
  }
  return;					// all done
}

rbd *Routine_Find(u_int size)			// find int bytes
{ rbd *ptr;					// a pointer
  rbd *p;					// and another
  ptr = systab->vol[0]->rbd_hash[RBD_HASH];	// get head of free list
  while (ptr != NULL)				// while we have some
  { if (ptr->chunk_size >= size) break;		// if big enough
    ptr = ptr->fwd_link;			// get next
  }
  if (ptr == NULL)				// found nothing
  { Routine_Collect(RESERVE_TIME);		// do a collect using reserve
    ptr = systab->vol[0]->rbd_hash[RBD_HASH];	// get head of free list
    while (ptr != NULL)				// while we have some
    { if (ptr->chunk_size >= size) break;	// if big enough
      ptr = ptr->fwd_link;			// get next
    }
    if (ptr == NULL)				// found nothing
    { Routine_Collect(0);			// do a collect using zero
      ptr = systab->vol[0]->rbd_hash[RBD_HASH];	// get head of free list
      while (ptr != NULL)			// while we have some
      { if (ptr->chunk_size >= size) break;	// if big enough
	ptr = ptr->fwd_link;			// get next
      }
      if (ptr == NULL) return NULL;		// found nothing - give up
    }
  }
  if ((size + SIZE_CLOSE) < ptr->chunk_size)	// if far too big
  { p = (rbd *) ((u_char *) ptr + size);	// setup for another
    p->fwd_link = ptr->fwd_link;		// and forward link
    p->chunk_size = ptr->chunk_size - size;	// its size
    p->attached = 0;				// clear this lot
    p->last_access = 0;				// no time reqd
    VAR_CLEAR(p->rnam);				// no routine name
    p->uci = 0;					// no uci
    p->vol = 0;					// no vol
    p->rou_size = 0;				// no routine (not in use)
    ptr->fwd_link = p;				// point at new chunk
    ptr->chunk_size = size;			// the new size
  }
  if (systab->vol[0]->rbd_hash[RBD_HASH] == ptr)
    systab->vol[0]->rbd_hash[RBD_HASH] = ptr->fwd_link; // new free bit
  else
  { p = systab->vol[0]->rbd_hash[RBD_HASH];	// get head of free list
    while (p->fwd_link != ptr) p = p->fwd_link;	// find our ptr
    p->fwd_link = ptr->fwd_link;		// change to new one
  }
  return ptr;					// return the pointer
}

// The following are called from the general M code
rbd *Routine_Attach(var_u routine)		// attach to routine
{ int hash = 0;					// for rbd_hash[]
  int i;					// a handy int
  u_int size;					// size required
  short s;					// a useful thing
  int t;					// a useful thing
  rbd *ptr;					// a pointer for this
  rbd *p;					// and another
  u_char tmp[VAR_LEN + 4];			// temp space
  cstring *cptr;				// for making strings
  mvar rouglob;					// mvar for $ROUTINE
  u_char uci;					// current uci
  u_char vol;					// current vol
  var_u *test;					// for testing name

  test = (var_u *) &routine;			// map as a var_u
  hash = Routine_Hash(routine);			// get the hash
  s = SemOp(SEM_ROU, -systab->maxjob);		// write lock the rbds
  if (s < 0) return NULL;			// say can't find on error
  p = systab->vol[0]->rbd_hash[hash];		// see where it points
  ptr = p;					// use it in ptr
  uci = partab.jobtab->ruci;			// get current uci
  vol = partab.jobtab->rvol;			// and vol

  if (test->var_cu[0] == '%')
  {  uci = 1;					// check for a % routine
     vol = 1;
  }
  while (ptr != NULL)				// while we have something
  { if (var_equal(ptr->rnam, routine) && (ptr->uci == uci) && (ptr->vol == vol)) // if this is the right one
    { ptr->attached++;				// count an attach
      s = SemOp(SEM_ROU, systab->maxjob);	// release the lock
      return ptr;				// and return the pointer
    }
    ptr = ptr->fwd_link;			// point at the next one
  }						// end while loop
  s = SemOp(SEM_ROU, systab->maxjob);		// release the lock

  VAR_CLEAR(rouglob.name);
  bcopy("$ROUTINE", rouglob.name.var_cu, 8);	// global name
  rouglob.volset = vol;				// volume set
  rouglob.uci = uci;				// uci
  cptr = (cstring *) tmp;			// get some temp space

  for (i = 0; i < VAR_LEN; i++)			// loop thru the name
  { if (routine.var_cu[i] == '\0') break;
    cptr->buf[i] = routine.var_cu[i]; 		// copy a byte
  }
  cptr->buf[i] = '\0';				// terminate
  cptr->len = (u_short) i;			// the count
  s = UTIL_Key_Build(cptr, rouglob.key);	// first subs
  rouglob.slen = s;				// save count so far
  cptr->buf[0] = '0';				// now the zero
  cptr->buf[1] = '\0';				// null terminate
  cptr->len = 1;				// and the length
  s = UTIL_Key_Build(cptr, &rouglob.key[s]);	// second subs
  rouglob.slen = rouglob.slen + s;		// save count so far
  t = DB_GetLen(&rouglob, 0, NULL);		// get a possible length
  if (t < 1) return NULL;			// no such
  s = SemOp(SEM_ROU, -systab->maxjob);		// write lock & try again
  if (s < 0) return NULL;			// no such

  p = systab->vol[0]->rbd_hash[hash];		// see where it points
  ptr = p;					// use it in ptr
  while (ptr != NULL)				// while we have something
  { if (var_equal(ptr->rnam, routine) && (ptr->uci == uci) && (ptr->vol == vol)) // if this is the right one
    { ptr->attached++;				// count an attach
      s = SemOp(SEM_ROU, systab->maxjob);	// release the lock
      return ptr;				// and return the pointer
    }
    p = ptr;					// save for ron
    ptr = ptr->fwd_link;			// point at the next one
  }						// end while loop

  t = DB_GetLen(&rouglob, 1, NULL);		// lock the GBD
  if (t < 1)					// if it's gone
  { t = DB_GetLen(&rouglob, -1, NULL);		// un-lock the GBD
    s = SemOp(SEM_ROU, systab->maxjob);		// release the lock
    return NULL;				// say no such
  }
  size = t + RBD_OVERHEAD + 1;			// space required
  if (size & 7) size = (size & ~7) + 8;		// round up to 8 byte boundary
  ptr = Routine_Find(size);			// find location
  if (ptr == NULL)				// no space mate!!
  { s = SemOp(SEM_ROU, systab->maxjob);		// release the lock
    t = DB_GetLen(&rouglob, -1, NULL);		// un-lock the GBD
    return (rbd *) -1;				// say no space
  }
  if (p == NULL)				// listhead for this hash
    systab->vol[0]->rbd_hash[hash] = ptr;	// save it here
  else p->fwd_link = ptr;			// or here
  ptr->fwd_link = NULL;				// ensure this is null
  ptr->attached = 1;				// count the attach
  ptr->last_access = current_time(TRUE);	// and the current time
  VAR_COPY(ptr->rnam, routine);			// the routine name
  ptr->uci = uci;				// the uci
  ptr->vol = vol;				// current volume
  ptr->rou_size = (u_short) t;			// save the size
  t = DB_GetLen(&rouglob, -1, (u_char *) &ptr->comp_ver); // get the routine

  if (t != ptr->rou_size) panic("routine load - size wrong"); // DOUBLECHECK

  ptr->tag_tbl += RBD_OVERHEAD;			// adjust for rbd junk
  ptr->var_tbl += RBD_OVERHEAD;			// adjust for rbd junk
  ptr->code += RBD_OVERHEAD;			// adjust for rbd junk
  if (ptr->comp_ver != COMP_VER) 		// check comp version
  { ptr->attached--;				// decrement the count
    Routine_Free(ptr);				// free the space
    s = SemOp(SEM_ROU, systab->maxjob);		// release the lock
    return (rbd *) -2;				// yet another *magic* number
  }
  s = SemOp(SEM_ROU, systab->maxjob);		// release the lock
  return ptr;					// success
}

void Routine_Detach(rbd *pointer)		// Detach from routine
{ short s;					// for SemOp() call
  while (SemOp(SEM_ROU, -systab->maxjob) < 0);	// lock the rbds, check error
  if (pointer->attached > 0)			// if not lost
  { pointer->attached--;			// decrement the count
  }
  if ((pointer->uci == 0) &&			// if it's invalid
      (pointer->attached == 0))			// and nothing is attached
    Routine_Free(pointer);			// free the space
  s = SemOp(SEM_ROU, systab->maxjob);		// release the lock
  if (s < 0) fprintf(stderr, "errno = %d %s\n", errno, strerror(errno));
  return;					// done
}

void Routine_Delete(var_u routine, int uci)	// mark routine deleted
// NOTE: Semaphore must be held BEFORE calling this routine
// MORE NOTE: Needs to be changed to specify the volume in the call !!!
{ int hash = 0;					// for rbd_hash[]
  rbd *ptr;					// a pointer for this

  hash = Routine_Hash(routine);			// get the hash
  ptr = systab->vol[0]->rbd_hash[hash];		// see where it points
  while (ptr != NULL)				// while we have something
  { if ((var_equal(ptr->rnam, routine)) &&
	(ptr->uci == uci)) 			// if this is the right one
    { ptr->uci = 0;				// mark as deleted
      if (ptr->attached == 0)			// if not in use
	Routine_Free(ptr);			// free the space
      break;					// and exit
    }
    ptr = ptr->fwd_link;			// point at the next one
  }						// end while loop
  return;					// done
}