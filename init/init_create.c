/*
 * Package:  Reference Standard M
 * File:     rsm/init/init_create.c
 * Summary:  module init - create a database file
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

#include <stdio.h>                                                              // always include
#include <stdlib.h>                                                             // these two
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <errno.h>                                                              // error stuff
#include <fcntl.h>                                                              // file stuff
#include <unistd.h>                                                             // database access
#include <sys/types.h>
#include <sys/ipc.h>                                                            // shared memory
#include <sys/shm.h>                                                            // shared memory
#include "rsm.h"                                                                // standard includes
#include "proto.h"                                                              // standard prototypes
#include "database.h"                                                           // for init manager block

/********************************************************************\
| Create a database - switches are:                                  |
|   -v volume set name           (1 to VAR_LEN chars)            Req |
|   -b block size in KiB         (4 to 256)                      Req |
|   -s database size in blocks   (100 to MAX_DATABASE_BLKS)      Req |
|   -e manager UCI name          (1 to VAR_LEN chars)            Opt |
|   -m map block size in KiB     (0 to MAX_MAP_SIZE)             Opt |
\********************************************************************/
int INIT_Create_File(u_int blocks,                                              // number of blocks
                     u_int bsize,                                               // block size in bytes
                     u_int map,                                                 // map size in bytes, may be 0
                     char  *volnam,                                             // volume name
                     char  *env,                                                // UCI name
                     char  *file)                                               // file name
{
    int namlen;                                                                 // length of volume name
    int envlen;                                                                 // length of UCI name
    int i;                                                                      // for loops

    union temp_tag {
        int  buff[512 * 1024 / 4];                                              // 512 KiB buffer
        char cuff[sizeof(label_block) + 1];                                     // remap for label block + 1 for 1st map block byte
    } x;                                                                        // end of union stuff

    int         ret;                                                            // for return values
    int         fid;                                                            // file handle
    DB_Block    *mgrblk;                                                        // manager block ptr
    label_block *labelblock;                                                    // database label block header
    cstring     *chunk;

    namlen = strlen(volnam);                                                    // get the name length

    if ((namlen < 1) || (namlen > VAR_LEN)) {                                   // check name length
        fprintf(stderr, "Volume set name must be from 1 to %d alpha characters\n", VAR_LEN);
        return -1;                                                              // return an error
    }                                                                           // end name length check

    for (i = 0; i < namlen; i++) {                                              // check all chars in name
        if (!isalpha((int) volnam[i])) {                                        // must be alpha
            fprintf(stderr, "Volume set name must be from 1 to %d alpha characters\n", VAR_LEN);
            return -1;                                                          // return an error
        }                                                                       // end fail code
    }                                                                           // end alpha check

    if (env != NULL) {                                                          // passed in UCI ?
        envlen = strlen(env);                                                   // get the name length

        if ((envlen < 1) || (envlen > VAR_LEN)) {                               // check name length
            fprintf(stderr, "Environment (UCI) name must be from 1 to %d alpha characters\n", VAR_LEN);
            return -1;                                                          // return an error
        }                                                                       // end name length check

        for (i = 0; i < envlen; i++) {                                          // check all chars in name
            if (!isalpha((int) env[i])) {                                       // must be alpha
                fprintf(stderr, "Environment (UCI) name must be from 1 to %d alpha characters\n", VAR_LEN);
                return -1;                                                      // return an error
            }                                                                   // end fail code
        }                                                                       // end alpha check
    }

    if (((bsize / 1024) < 4) || ((bsize / 1024) > 256)) {                       // check block size
        fprintf(stderr, "Block size must be from 4 to 256 KiB\n");              // complain
        return -1;                                                              // return an error
    }                                                                           // end block size check

    if ((blocks < 100) || (blocks > MAX_DATABASE_BLKS)) {                       // check DB size
        fprintf(stderr, "Database size must be from 100 to %u blocks\n", MAX_DATABASE_BLKS); // complain
        return -1;                                                              // return an error
    }                                                                           // end DB size check

    blocks |= 7;                                                                // ensure low 3 bits are set

    if (map == 0) map = (blocks + 7) / 8 + 1 + sizeof(label_block);             // if map not sepecified see what we need
    if (map & 1023) map = (map / 1024 + 1) * 1024;                              // if not even KiB round up
    if (map < bsize) map = bsize;                                               // at least bsize

    if (map < (blocks + 7) / 8 + 1 + sizeof(label_block)) {                     // if less than req
        fprintf(stderr, "Map block size of %d KiB smaller than required by database size\n", map / 1024); // complain
        return -1;                                                              // return an error
    }                                                                           // end map size check

    if (map > (MAX_MAP_SIZE * 1024)) {                                          // or too big
        fprintf(stderr, "Map block size must be from 0 to %u KiB\n", MAX_MAP_SIZE); // complain
        return -1;                                                              // return an error
    }                                                                           // end map size check

    printf("Creating volume set %s in file %s\n", volnam, file);
    if (env != NULL) printf("using %s as the name of the manager environment (UCI)\n", env);
    printf("with %u x %u KiB blocks ", blocks, bsize / 1024);
    printf("and a %d KiB label/map block.\n", map / 1024);                      // say what we are doing
    ret = 0;
    errno = 0;                                                                  // clear error flag

    fid = open(file,                                                            // open this new file
               (O_CREAT | O_TRUNC | O_WRONLY | O_EXCL),                         // create the file
               438);                                                            // rw everyone (for now)

    if (fid < 1) {                                                              // if that failed
        fprintf(stderr, "Create of %s failed\n - %s\n",                         // complain
                file,                                                           // what we tried
                strerror(errno));                                               // what was returned

        return errno;                                                           // exit with error
    }                                                                           // end file create test

    labelblock = (label_block *) x.buff;                                        // point structure at it
    bzero(x.buff, ((map > (512 * 1024)) ? (512 * 1024) : map));                   // clear it
    labelblock->magic = RSM_MAGIC;                                              // RSM magic number
    labelblock->max_block = blocks;                                             // maximum block number
    labelblock->header_bytes = map;                                             // bytes in label/map
    labelblock->block_size = bsize;                                             // bytes per data block
#if RSM_DBVER != 1
    labelblock->creation_time = (u_int64) current_time(TRUE);                   // when database file was created
#endif
    memcpy(labelblock->volnam.var_cu, volnam, namlen);                          // vol name length
    labelblock->db_ver = DB_VER;                                                // database version
    labelblock->clean = 1;                                                      // clean dismount flag

    if (env != NULL) bcopy(env, labelblock->uci[0].name.var_cu, strlen(env));   // passed in UCI ?
    else bcopy("MGR", labelblock->uci[0].name.var_cu, 3);

    labelblock->uci[0].global = 1;                                              // setup manager UCI
    x.cuff[sizeof(label_block)] = 3;                                            // mark blocks 0 & 1 as used

    if (map > (512 * 1024)) {
        ret = write(fid, x.buff, (512 * 1024));                                 // write out the first 512 KiB incl. header
                                                                                //   + first map block byte
        if (ret < 1) {                                                          // if that failed
            i = close(fid);                                                     // close the file

            fprintf(stderr, "File write failed - %s\n",                         // complain
                    strerror(errno));                                           // what was returned

            return errno;                                                       // and return
        }                                                                       // probably should delete it

        bzero(x.buff, (512 * 1024));                                            // clear it

        for (i = 0; i < (map / (512 * 1024) - 1); i++) {                        // write out large map block
            ret = write(fid, x.buff, (512 * 1024));                             // write out the header

            if (ret < 1) {                                                      // if that failed
                i = close(fid);                                                 // close the file

                fprintf(stderr, "File write failed - %s\n",                     // complain
                        strerror(errno));                                       // what was returned

                return errno;                                                   // and return
            }                                                                   // probably should delete it
        }

        if (map % (512 * 1024)) {
            ret = write(fid, x.buff, (map % (512 * 1024)));                     // write out the remainder of the large map block

            if (ret < 1) {                                                      // if that failed
                i = close(fid);                                                 // close the file

                fprintf(stderr, "File write failed - %s\n",                     // complain
                        strerror(errno));                                       // what was returned

                return errno;                                                   // and return
            }                                                                   // probably should delete it
        }
    } else {
        ret = write(fid, x.buff, map);                                          // write out the header

        if (ret < 1) {                                                          // if that failed
          i = close(fid);                                                       // close the file

          fprintf(stderr, "File write failed - %s\n",                           // complain
                  strerror(errno));                                             // what was returned

          return errno;                                                         // and return
        }                                                                       // probably should delete it
    }

    // Make manager block & $GLOBAL record
    mgrblk = (DB_Block *) x.buff;                                               // find block 1 (manager)
    bzero(x.buff, bsize);                                                       // clear it
    mgrblk->type = 65;                                                          // type is data blk + UCI
    mgrblk->last_idx = IDX_START;                                               // have one rec
    mgrblk->last_free = (u_short) (bsize / 4 - 7);                              // minus extra for rec length
    bcopy("$GLOBAL", &(mgrblk->global), 7);                                     // init the name
    i = (bsize / 4) - 6;                                                        // point at the record
    memcpy(&x.cuff[sizeof(DB_Block)], &i, sizeof(u_short));                     // save in index
    chunk = (cstring *) &x.buff[i];                                             // point at the chunk
    chunk->len = 24;                                                            // size of this (incl self)
    chunk->buf[1] = 9;                                                          // key length
    bcopy("\200$GLOBAL\0", &chunk->buf[2], 9);                                  // the key
    i += 4;                                                                     // point at block#
    x.buff[i] = 1;                                                              // block 1
    ret = write(fid, x.buff, bsize);                                            // write manager block

    // Now do the rest as zeroed blocks
    bzero(x.buff, bsize);                                                       // clear it

    for (i = 0; i < blocks - 1; i++) {                                          // for each data block
        ret = write(fid, x.buff, bsize);                                        // write a block

        if (ret < 1) {                                                          // if that failed
            i = close(fid);                                                     // close the file

            fprintf(stderr, "File write failed - %s\n",                         // complain
                    strerror(errno));                                           // what was returned

            return errno;                                                       // and return
            }                                                                   // probably should delete it
    }                                                                           // end of write code

    i = close(fid);                                                             // close file
    printf("Database file created.\n");                                         // say we've done that
    return 0;                                                                   // indicate success
}
