/*
 * Package:  Reference Standard M
 * File:     rsm/init/init_create.c
 * Summary:  module init - create a database file
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

#include <stdio.h>                              // always include
#include <stdlib.h>                             // these two
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <errno.h>                              // error stuff
#include <fcntl.h>                              // file stuff
#include <unistd.h>                             // database access
#include <sys/types.h>
#include <sys/ipc.h>                            // shared memory
#include <sys/shm.h>                            // shared memory
#include "rsm.h"                                // standard includes
#include "database.h"				// for init MGR block

//****************************************************************************
// Create a database - switches are:
//      -v volset name          (1 to 8 chars)                          Reqd
//      -b blocksize in kb      (4 to 256)                              Reqd
//      -s db size in blocks    (100 to MAX_DATABASE_BLKS)      	Reqd
//      -m map block size in kb (dbsize/8192+1 to 512)                  Opt
//

int INIT_Create_File(int blocks,                // number of blocks
                     int bsize,                 // block size in bytes
                     int map,                   // map size in bytes may be 0
                     char *volnam,              // volume name
                     char *file)                // file name
{ int namlen;                                   // length of volume name
  int i;                                        // for loops
  union temp_tag
  { int buff[131072];                           // 512kb buffer
    char cuff[1024];                            // remap front bit
  } x;                                          // end of union stuff
  int ret;                                      // for return values
  int fid;                                      // file handle
  DB_Block *mgrblk;				// mgr block ptr
  label_block *labelblock;			// first 1024 bytes
  cstring * chunk;

  namlen = strlen(volnam);                      // get the name length
  if ((namlen < 1)||(namlen > 8))               // check name length
  { fprintf(stderr, "Volume set name must from 1 to 8 alpha characters\n\n");
    return (-1);                                // return an error
  }                                             // end name length check
  for (i = 0; i < namlen; i++)                  // check all chars in name
  { if (!isalpha((int)volnam[i]))               // must be alpha
    { fprintf(stderr, "Volume set name must from 1 to 8 alpha characters\n\n");
    return (-1);                                // return an error
    }                                           // end fail code
  }                                             // end alpha check

  if (((bsize/1024) < 4)||((bsize/1024) > 256)) // check block size
  { fprintf(stderr, "Blocksize must be from 4 to 256KB\n\n"); // complain
    return (-1);                                // return an error
  }                                             // end block size check

  blocks |= 7;					// ensure low 3 bits are set

  if ((blocks < 100)||
      (blocks > MAX_DATABASE_BLKS))		// check db size
  { fprintf(stderr,
            "Database size must be 100 to %d blocks\n\n",
            MAX_DATABASE_BLKS);       		// complain
    return (-1);                                // return an error
  }                                             // end db size check

  if (map == 0)                                 // if map not sepecified
  { map = (blocks+7)/8+1025;			// see what we need
    if (map & 1023)				// if not even K
    { map = ((map / 1024) + 1) * 1024;		// round up
    }
    if (map < bsize) map = bsize;               // at least bsize
  }
  if ((map < (blocks+7)/8+1025)||               // if less than reqd
      (map < bsize)||                           // or less than bsize
      (map > (512*1024)))                       // or too big
  { fprintf(stderr, "Invalid map block size %d\n\n", map); // complain
    return (-1);                                // return an error
  }                                             // end map size check
  printf("Creating volumeset %s in file %s\n", volnam, file);
  printf("with %d x %dkb blocks ", blocks, bsize/1024);
  printf("and a %dkb map/label block.\n", map/1024); // say what we are doing

  ret = 0;
  errno = 0;                                    // clear error flag
  fid = open(file,                              // open this new file
             O_CREAT|O_TRUNC|O_WRONLY|O_EXCL,   // create the file
             438);                              // rw everyone (for now)
  if (fid < 1)                                  // if that failed
  { fprintf(stderr, "Create of %s failed\n - %s\n", // complain
             file,                              // what we tried
             strerror(errno));                  // what was returned
    return(errno);                              // exit with error
  }                                             // end file create test
  labelblock = (label_block *) x.buff;		// point structure at it
  bzero(x.buff, map);				// clear it
  labelblock->magic = RSM_MAGIC;		// RSM magic number
  labelblock->max_block = blocks;		// maximum block number
  labelblock->header_bytes = map;		// bytes in label/map
  labelblock->block_size = bsize;		// bytes per data block
#if defined(__FreeBSD__) || defined(__arm__)
  memcpy(labelblock->volnam.var_cu, volnam, 8); // vol name (8 bytes)
#else
  strncpy((char *) labelblock->volnam.var_cu, volnam, 8); // vol name (8 bytes)
#endif
  labelblock->db_ver = DB_VER;			// database version
  labelblock->clean = 1;			// clean dismount flag

  bcopy("MGR", labelblock->uci[0].name.var_cu, 3);
  labelblock->uci[0].global = 1;		// setup MGR UCI

  x.buff[256] = 3;                              // mark blocks 0 & 1 as used

  ret = write(fid, x.buff, map);                // write out the header
  if (ret < 1)                                  // if that failed
  { i = close(fid);                             // close the file
    fprintf(stderr, "File write failed - %s\n", // complain
             strerror(errno));                  // what was returned
    return(errno);                              // and return
  }                                             // probably should delete it
  // make MGR block & $GLOBAL record

  mgrblk = (DB_Block *) x.buff;			// find block 1 (mgr)
  bzero(x.buff, bsize);				// clear it
  mgrblk->type = 65;				// type is data blk + uci
  mgrblk->last_idx = 10;			// have one rec
  mgrblk->last_free = (u_short) (bsize/4) - 7;	// minus extra 6 for rec length
  bcopy("$GLOBAL\0", &(mgrblk->global), 8);	// init the name
  i = (bsize / 4) - 6;				// point at the record
  *(u_short *) &x.cuff[20] = (u_short) i;	// save in index
  chunk = (cstring *) &x.buff[i];		// point at the chunk
  chunk->len = 24;				// size of this (incl self)
  chunk->buf[1] = 9;				// key length
  bcopy("\200$GLOBAL\0",&chunk->buf[2], 9);	// the key
  i += 4;					// point at block#
  x.buff[i] = 1;				// block 1
  ret = write(fid, x.buff, bsize);              // write MGR block

  // Now do the rest as zeroed blocks

  bzero(x.buff, bsize);				// clear it
  for (i = 0; i < blocks-1; i++)                // for each data block
  { ret = write(fid, x.buff, bsize);            // write a block
    if (ret < 1)                                // if that failed
    { i = close(fid);                           // close the file
    fprintf(stderr, "File write failed - %s\n", // complain
             strerror(errno));                  // what was returned
    return(errno);                              // and return
    }                                           // probably should delete it
  }                                             // end of write code
  i = close(fid);                               // close file
  printf("Database file created.\n");           // say we've done that
  return (0);                                   // indicate success
}
