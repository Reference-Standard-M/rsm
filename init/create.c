/*
 * Package:  Reference Standard M
 * File:     rsm/init/create.c
 * Summary:  module init - create a database file
 *
 * David Wicksell <dlw@linux.com>
 * Copyright © 2020-2023 Fourth Watch Software LC
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
#include <ctype.h>
#include <errno.h>                                                              // error stuff
#include <fcntl.h>                                                              // file stuff
#include <unistd.h>                                                             // database access
#include <sys/types.h>
#include <sys/ipc.h>                                                            // shared memory
#include <sys/shm.h>                                                            // shared memory
#include <sys/stat.h>                                                           // file stuff
#include "rsm.h"                                                                // standard includes
#include "proto.h"                                                              // standard prototypes
#include "database.h"                                                           // for init manager block

#define BLKLEN (512 * 1024)                                                     // 512 KiB buffer length

/*******************************************************************\
* Create a database - switches are:                                 *
*   -s database size in blocks    (100 to MAX_DATABASE_BLKS)    Req *
*   -b block size in KiB          (1 to 256)                    Req *
*   -m map block size in KiB      (0 to MAX_MAP_SIZE)           Opt *
*   -v volume set name            (1 to VAR_LEN)                Req *
*   -e manager UCI name           (1 to VAR_LEN)                Opt *
*      database file name         (1 to VAR_LEN)                Req *
\*******************************************************************/
int INIT_Create_File(u_int blocks,                                              // number of blocks
                     u_int bsize,                                               // block size in bytes
                     u_int map,                                                 // map size in bytes, may be 0
                     char  *volnam,                                             // volume name
                     char  *env,                                                // UCI name
                     char  *file)                                               // file name
{
    int         namlen;                                                         // length of volume name
    int         envlen;                                                         // length of UCI name
    u_short     us;                                                             // for mgrblk index entry
    int         ret;                                                            // for return values
    int         fid;                                                            // file handle
    DB_Block    *mgrblk;                                                        // manager block ptr
    label_block *labelblock;                                                    // database label block header
    cstring     *hunk;
    char        version[120];                                                   // a string

    union temp_tag {
        int  buff[BLKLEN / 4];                                                  // 512 KiB buffer
        char cuff[sizeof(label_block) + 1];                                     // remap for label block + 1 for 1st map block byte
    } x;                                                                        // end of union stuff

    rsm_version((u_char *) version);                                            // get version into version[]
    printf("%s\n", version);                                                    // print version string
    namlen = strlen(volnam);                                                    // get the name length

    if ((namlen < 1) || (namlen > VAR_LEN)) {                                   // check name length
        fprintf(stderr, "Volume name must be from 1 to %d alpha characters\n", VAR_LEN);
        return -1;                                                              // return an error
    }                                                                           // end name length check

    for (int i = 0; i < namlen; i++) {                                          // check all chars in name
        if (!isalpha((int) volnam[i])) {                                        // must be alpha
            fprintf(stderr, "Volume name must be from 1 to %d alpha characters\n", VAR_LEN);
            return -1;                                                          // return an error
        }                                                                       // end fail code
    }                                                                           // end alpha check

    if (env != NULL) {                                                          // passed in UCI ?
        envlen = strlen(env);                                                   // get the name length

        if ((envlen < 1) || (envlen > VAR_LEN)) {                               // check name length
            fprintf(stderr, "Environment (UCI) name must be from 1 to %d alpha characters\n", VAR_LEN);
            return -1;                                                          // return an error
        }                                                                       // end name length check

        for (int i = 0; i < envlen; i++) {                                      // check all chars in name
            if (!isalpha((int) env[i])) {                                       // must be alpha
                fprintf(stderr, "Environment (UCI) name must be from 1 to %d alpha characters\n", VAR_LEN);
                return -1;                                                      // return an error
            }                                                                   // end fail code
        }                                                                       // end alpha check
    }

    if (((bsize / 1024) < 1) || ((bsize / 1024) > 256)) {                       // check block size
        fprintf(stderr, "Block size must be from 1 to 256 KiB\n");              // complain
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
        fprintf(stderr, "Map block size of %u KiB smaller than required by database size\n", map / 1024); // complain
        return -1;                                                              // return an error
    }                                                                           // end map size check

    if (map > (MAX_MAP_SIZE * 1024)) {                                          // or too big
        fprintf(stderr, "Map block size must be from 0 to %u KiB\n", MAX_MAP_SIZE); // complain
        return -1;                                                              // return an error
    }                                                                           // end map size check

    printf("Creating volume %s in file %s,\n", volnam, file);

    if (env != NULL) {
        printf("using %s as the name of the manager environment (UCI),\n", env);
    } else {
        printf("using MGR as the name of the manager environment (UCI),\n");
    }

    printf("with %u [%u KiB] blocks, and a %u KiB label/map block.\n" , blocks, bsize / 1024, map / 1024); // say what we are doing
    ret = 0;
    errno = 0;                                                                  // clear error flag
    umask(0);                                                                   // set umask to 0000

    // Create database file - with 0640 permissions
    fid = open(file, O_CREAT | O_TRUNC | O_WRONLY | O_EXCL, S_IRUSR | S_IWUSR | S_IRGRP);

    if (fid < 1) {                                                              // if that failed
        fprintf(stderr, "Create of %s failed - %s\n", file, strerror(errno));   // what was returned
        return errno;                                                           // exit with error
    }                                                                           // end file create test

    labelblock = (label_block *) x.buff;                                        // point structure at it
    memset(x.buff, 0, (map > (u_int) BLKLEN) ? (u_int) BLKLEN : map);           // clear it
    labelblock->magic = RSM_MAGIC;                                              // RSM magic number
    labelblock->max_block = blocks;                                             // maximum block number
    labelblock->header_bytes = map;                                             // bytes in label/map
    labelblock->block_size = bsize;                                             // bytes per data block
#if RSM_DBVER != 1
    labelblock->creation_time = (u_int64) current_time(TRUE);                   // when database file was created
#endif
    memcpy(labelblock->volnam.var_cu, volnam, namlen);                          // copy vol name to label block
    labelblock->db_ver = DB_VER;                                                // database version
    labelblock->clean = 1;                                                      // clean dismount flag

    if (env != NULL) {                                                          // passed in UCI ?
        memcpy(labelblock->uci[0].name.var_cu, env, strlen(env));
    } else {
        memcpy(labelblock->uci[0].name.var_cu, "MGR", 3);
    }

    labelblock->uci[0].global = 1;                                              // setup manager UCI
    x.cuff[sizeof(label_block)] = 3;                                            // mark blocks 0 & 1 as used

    if (map > (u_int) BLKLEN) {
        ret = write(fid, x.buff, BLKLEN);                                       // write out the first 512 KiB incl. header
                                                                                //   + first map block byte
        if (ret < BLKLEN) {                                                     // if that failed
            close(fid);                                                         // close the file
            fprintf(stderr, "Database file write failed - %s\n", strerror(errno)); // what was returned
            return errno;                                                       // and return
        }                                                                       // probably should delete it

        memset(x.buff, 0, BLKLEN);                                              // clear it

        for (u_int i = 0; i < (map / BLKLEN - 1); i++) {                        // write out large map block
            ret = write(fid, x.buff, BLKLEN);                                   // write out the header

            if (ret < BLKLEN) {                                                 // if that failed
                close(fid);                                                     // close the file
                fprintf(stderr, "Database file write failed - %s\n", strerror(errno)); // what was returned
                return errno;                                                   // and return
            }                                                                   // probably should delete it
        }

        if (map % BLKLEN) {
            ret = write(fid, x.buff, map % BLKLEN);                             // write out the remainder of the large map block

            if (ret < (int) (map % BLKLEN)) {                                   // if that failed
                close(fid);                                                     // close the file
                fprintf(stderr, "Database file write failed - %s\n", strerror(errno)); // what was returned
                return errno;                                                   // and return
            }                                                                   // probably should delete it
        }
    } else {
        ret = write(fid, x.buff, map);                                          // write out the header

        if (ret < (int) map) {                                                  // if that failed
            close(fid);                                                         // close the file
            fprintf(stderr, "Database file write failed - %s\n", strerror(errno)); // what was returned
            return errno;                                                       // and return
        }                                                                       // probably should delete it
    }

    // Make manager block and $GLOBAL record
    mgrblk = (DB_Block *) x.buff;                                               // find block 1 (manager)
    memset(x.buff, 0, bsize);                                                   // clear it
    mgrblk->type = 65;                                                          // type is data blk + UCI
    mgrblk->last_idx = IDX_START;                                               // have one rec
    mgrblk->last_free = (u_short) ((bsize >> 2) - 7);                           // minus extra for rec length
    memcpy(&mgrblk->global, "$GLOBAL", 7);                                      // init the name
    us = (bsize >> 2) - 6;                                                      // point at the record
    memcpy(&x.cuff[sizeof(DB_Block)], &us, sizeof(u_short));                    // save in index
    hunk = (cstring *) &x.buff[us];                                             // point at the hunk
    hunk->len = 24;                                                             // size of this (incl self)
    hunk->buf[1] = 9;                                                           // key length
    memcpy(&hunk->buf[2], "\200$GLOBAL\0", 9);                                  // the key
    us += 4;                                                                    // point at block#
    x.buff[us] = 1;                                                             // block 1
    ret = write(fid, x.buff, bsize);                                            // write manager block

    if (ret < (int) bsize) {                                                    // if that failed
        close(fid);                                                             // close the file
        fprintf(stderr, "Database file write failed - %s\n", strerror(errno));  // what was returned
        return errno;                                                           // and return
    }                                                                           // probably should delete it

    // Now do the rest as zeroed blocks
    memset(x.buff, 0, bsize);                                                   // clear it

    for (u_int i = 0; i < (blocks - 1); i++) {                                  // for each data block
        ret = write(fid, x.buff, bsize);                                        // write a block

        if (ret < 1) {                                                          // if that failed
            close(fid);                                                         // close the file
            fprintf(stderr, "Database file write failed - %s\n", strerror(errno)); // what was returned
            return errno;                                                       // and return
        }                                                                       // probably should delete it
    }                                                                           // end of write code

    close(fid);                                                                 // close file
    printf("Database file %s created.\n", file);                                // say we've done that
    return 0;                                                                   // indicate success
}
