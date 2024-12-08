/*
 * Package: Reference Standard M
 * File:    rsm/database/mount.c
 * Summary: module database - mount a database file
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

#include "database.h"                                                           // database includes
#include "error.h"                                                              // error strings
#include "proto.h"                                                              // standard includes
#include <errno.h>                                                              // error stuff
#include <fcntl.h>                                                              // file stuff
#include <stdio.h>                                                              // always include
#include <stdlib.h>                                                             // these two
#include <string.h>                                                             // string controls always handy
#include <unistd.h>                                                             // database access
#include <sys/param.h>                                                          // for realpath() function
#include <sys/shm.h>                                                            // shared memory
#include <sys/stat.h>                                                           // stat

/*
 * Mount an environment:
 *   Database file
 *   Mount as volume 2 -> MAX_VOL - vol is passed as index (1 less)
 *   MiB of global buffers
 *   MiB of routine buffers
 */
short DB_Mount(char *file, int vol, u_int gmb, u_int rmb)
{
    int             dbfd;                                                       // database file descriptor
    int             hbuf[sizeof(label_block) / 4];                              // header buffer
    int             i;                                                          // useful int
    u_int           n_gbd;                                                      // number of GBD
    int             indx;                                                       // loop control
    key_t           shar_mem_key;                                               // memory key
    int             shar_mem_id;                                                // memory id
    u_int64         volset_size;                                                // size of volset struct (bytes)
    long            pagesize;                                                   // system pagesize (bytes)
    struct shmid_ds sbuf;                                                       // for shmctl
    char            fullpathvol[MAXPATHLEN];                                    // full pathname of volume file
    int             jobs;                                                       // max number of jobs
    gbd             *gptr;                                                      // a GBD pointer
    u_char          *ptr;                                                       // and a byte one
    label_block     *labelblock;                                                // label block pointer

    if ((vol < 1) || (vol >= MAX_VOL)) return -(ERRZ71 + ERRMLAST);             // valid volume range for mount
    if (systab->vol[vol] != NULL) return -(ERRZ71 + ERRMLAST);                  // check for already mounted
    if (systab->maxjob != 1) return -(ERRZ60 + ERRMLAST);                       // if not in single-user mode then complain

    for (u_int j = 0; j < MAX_VOL; j++) {
        if (systab->vol[j] == NULL) continue;

        if (ftok(SOA(systab->vol[j])->file_name, RSM_SYSTEM) == ftok(file, RSM_SYSTEM)) { // make sure volume isn't already mounted
            return -(ERRZ71 + ERRMLAST);
        }
    }

    if (gmb > MAX_GLOBAL_BUFFERS) return -(ERRMLAST + ERRZLAST + EINVAL);       // check size of global buffers
    if (rmb > MAX_ROUTINE_BUFFERS) return -(ERRMLAST + ERRZLAST + EINVAL);      // check size of routine buffers
    jobs = systab->maxjob;
    if (gmb == 0) gmb = jobs / 2;                                               // default global buffers
    if (gmb < 1) gmb = 1;                                                       // but at least 1 MiB
    if (rmb == 0) rmb = jobs / 8;                                               // pick a default
    if (rmb < 1) rmb = 1;                                                       // but at least 1 MiB

    if (systab->addsize && (systab->addsize < (sizeof(vol_def) + (gmb * MBYTE) + (rmb * MBYTE)))) { // check for spare buffer space
        return -(ERRZ60 + ERRMLAST);
    }

    dbfd = open(file, O_RDWR);                                                  // open the database read/write
    if (dbfd == -1) return -(ERRMLAST + ERRZLAST + errno);                      // if that failed exit with error

    if (systab->addsize) {
        labelblock = (label_block *) ((u_char *) systab + systab->addoff + sizeof(vol_def)); // point at the spare memory
        i = read(dbfd, labelblock, sizeof(label_block));                        // read label block
    } else {
        i = read(dbfd, hbuf, sizeof(label_block));                              // read label block
        labelblock = (label_block *) hbuf;                                      // point label block at it
    }

    if (i == -1) return -(ERRZ73 + ERRMLAST);                                   // in case of error
    if (i < (int) sizeof(label_block)) return -(ERRZ73 + ERRMLAST);             // in case of error
    if (labelblock->magic != RSM_MAGIC) return -(ERRZ73 + ERRMLAST);
    if (labelblock->db_ver != DB_VER) return -(ERRZ73 + ERRMLAST);              // if we need to upgrade

    for (int j = 0; j < MAX_VOL; j++) {                                         // make sure no other volumes with the same name
        if ((j == vol) || (systab->vol[j] == NULL)) continue;

        if (var_equal(labelblock->volnam, SOA(SOA(systab->vol[j])->vollab)->volnam)) {
            return -(ERRZ73 + ERRMLAST);                                        // exit with error
        }
    }

    n_gbd = ((u_int64) gmb * MBYTE) / labelblock->block_size;                   // number of GBD

    while (n_gbd < MIN_GBD) {                                                   // if not enough
        gmb++;                                                                  // increase it
        n_gbd = ((u_int64) gmb * MBYTE) / labelblock->block_size;               // number of GBD
    }

    if (systab->addsize) {
        shar_mem_key = ftok(partab.vol[0]->file_name, RSM_SYSTEM);              // get key to existing share
        if (shar_mem_key == -1) return -(ERRMLAST + ERRZLAST + errno);          // if that failed exit with error
        shar_mem_id = shmget(shar_mem_key, 0, 0);                               // attach to existing share
        if (shar_mem_id == -1) return -(ERRMLAST + ERRZLAST + EEXIST);          // if that failed exit with error
    } else {
        shar_mem_key = ftok(file, RSM_SYSTEM);                                  // get key to new share
        if (shar_mem_key == -1) return -(ERRMLAST + ERRZLAST + errno);          // if that failed exit with error
        shar_mem_id = shmget(shar_mem_key, 0, 0);                               // attach to existing share
        if (shar_mem_id != -1) return -(ERRMLAST + ERRZLAST + EEXIST);          // if that didn't fail exit with error
    }

    volset_size = sizeof(vol_def)                                               // size of VOL_DEF
                + labelblock->header_bytes
                + (n_gbd * sizeof(gbd))                                         // the GBD
                + ((u_int64) gmb * MBYTE)                                       // MiB of global buffers
                + labelblock->block_size                                        // size of block (zero block)
                + ((u_long) rmb * MBYTE);                                       // MiB of routine buffers

    pagesize = sysconf(_SC_PAGESIZE);                                           // get system pagesize (bytes)
    volset_size = ((volset_size - 1) / pagesize + 1) * pagesize;                // round up
    if (systab->addsize && (systab->addsize < volset_size)) return -(ERRZ60 + ERRMLAST); // check for spare buffer space

    if (!systab->addsize) {
        shar_mem_id = shmget(shar_mem_key, volset_size, IPC_CREAT | S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP); // create share (0660)
        if (shar_mem_id == -1) return -(ERRMLAST + ERRZLAST + errno);           // if that failed exit with error
    }

    if (systab->addsize) {
        partab.vol[vol] = SOA((vol_def *) ((u_char *) systab->address + systab->addoff)); // the volume structure
        partab.vol[vol]->vollab = SBA(labelblock);                              // and point to label block
    } else {
        partab.vol[vol] = (vol_def *) shmat(shar_mem_id, SHMAT_SEED, 0);        // map it
        if ((void *) partab.vol[vol] == (void *) -1) return -(ERRMLAST + ERRZLAST + errno); // if that failed exit with error
        partab.vol[vol]->vollab = (label_block *) SBA(((u_char *) partab.vol[vol] + sizeof(vol_def))); // and point to label blk
    }

    systab->vol[vol] = SBA(partab.vol[vol]);
    partab.vol[vol]->map = (void *) ((u_char *) SBA(partab.vol[vol]->vollab + sizeof(label_block))); // and point to map
    partab.vol[vol]->first_free = partab.vol[vol]->map;                         // init first free
    partab.vol[vol]->gbd_head = (gbd *) ((u_char *) partab.vol[vol]->vollab + labelblock->header_bytes); // GBDs
    partab.vol[vol]->num_gbd = n_gbd;                                           // number of GBDs
    partab.vol[vol]->global_buf = (void *) &partab.vol[vol]->gbd_head[n_gbd];   // global buffers

    // pointer to zero block
    partab.vol[vol]->zero_block = (void *) &(((u_char *) partab.vol[vol]->global_buf)[((u_int64) gmb * MBYTE)]);
    partab.vol[vol]->rbd_head = (void *) ((u_char *) partab.vol[vol]->zero_block + labelblock->block_size); // RBDs
    partab.vol[vol]->rbd_end = (void *) ((u_char *) partab.vol[vol] + volset_size); // end of share
    partab.vol[vol]->shm_id = shar_mem_id;                                      // set up share id
    partab.vol[vol]->map_dirty_flag = 0;                                        // clear dirty map flag

    if (realpath(file, fullpathvol) != NULL) {                                  // get full path
        if (strlen(fullpathvol) <= VOL_FILENAME_MAX) {                          // if can fit in our struct
            strcpy(partab.vol[vol]->file_name, fullpathvol);                    // copy this full path into the vol_def structure
        } else {                                                                // end if path will fit - otherwise
            i = strlen(fullpathvol) - VOL_FILENAME_MAX;                         // copy as much as
            strcpy(partab.vol[vol]->file_name, &fullpathvol[i]);                // is possible, thats the best we can do
        }                                                                       // end length testing
    } else {                                                                    // end realpath worked - or there was an error
        i = errno;                                                              // save realpath error
        shmdt(systab);                                                          // detach the shared memory
        shmctl(shar_mem_id, IPC_RMID, &sbuf);                                   // remove the share
        return -(ERRMLAST + ERRZLAST + i);                                      // if that failed exit with error
    }

    if (shmctl(shar_mem_id, IPC_STAT, &sbuf) == -1) return -(ERRMLAST + ERRZLAST + errno); // get status for later
    if (lseek(dbfd, 0, SEEK_SET) == -1) return -(ERRMLAST + ERRZLAST + errno);  // re-point at start of file
    i = read(dbfd, SOA(partab.vol[vol]->vollab), labelblock->header_bytes);     // read label & map block

    if (i < (int) labelblock->header_bytes) {                                   // in case of error
        i = errno;                                                              // save realpath error
        shmdt(systab);                                                          // detach the shared memory
        shmctl(shar_mem_id, IPC_RMID, &sbuf);                                   // remove the share
        return -(ERRMLAST + ERRZLAST + i);                                      // exit with error
    }

    if (labelblock->clean == 0) {                                               // if not a clean dismount
        partab.vol[vol]->upto = 1;                                              // mark for cleaning
        return -(ERRMLAST + ERRZ6);                                             // exit with error
    } else {
        labelblock->clean = 1;                                                  // mark as mounted
        partab.vol[vol]->map_dirty_flag = 1;                                    // and map needs writing
    }

    jobs /= DAEMONS;                                                            // number of daemons
    if (jobs < MIN_DAEMONS) jobs = MIN_DAEMONS;                                 // minimum of MIN_DAEMONS
    if (jobs > MAX_DAEMONS) jobs = MAX_DAEMONS;                                 // and the max
    partab.vol[vol]->num_of_daemons = jobs;                                     // initialize this
    while (SemOp(SEM_WD, SEM_WRITE)) {}                                         // lock WD
    partab.vol_fds[vol] = dbfd;                                                 // so the daemons can close inherited FD
    fflush(stdout);                                                             // force a flush before forking

    if (systab->addsize) {
        systab->addsize -= volset_size;                                         // reset additional address size
        systab->addoff += volset_size;                                          // reset additional
    }

    for (indx = 0; indx < jobs; indx++) {                                       // for each required daemon
        i = DB_Daemon(indx, vol + 1);                                           // start each daemon (for mounted volume)

        if (i != 0) {                                                           // in case of error
            shmdt(systab);                                                      // detach the shared memory
            return -(ERRMLAST + ERRZLAST + i);                                  // exit with error
        }
    }                                                                           // all daemons started

    if (labelblock->journal_requested && labelblock->journal_file[0]) {
        struct stat sb;                                                         // File attributes
        off_t  jptr;                                                            // file pointer
        jrnrec jj;                                                              // to write with
        int    jfd;                                                             // file descriptor

        while (SemOp(SEM_GLOBAL, SEM_WRITE)) {}                                 // lock GLOBAL
        labelblock->journal_available = 0;                                      // assume fail
        i = stat(labelblock->journal_file, &sb);                                // check for file

        if ((i == -1) && (errno != ENOENT)) {                                   // if that's junk
            return -(ERRMLAST + ERRZLAST + errno);                              // exit with error
        } else {                                                                // do something
            if (i == -1) ClearJournal(vol);                                     // if doesn't exist then create it
            jfd = open(labelblock->journal_file, O_RDWR);

            if (jfd == -1) {                                                    // on fail
                return -(ERRMLAST + ERRZLAST + errno);                          // exit with error
            } else {                                                            // if open OK
                union {
                    u_int magic;
                    u_char tmp[4];
                } temp;

                lseek(jfd, 0, SEEK_SET);
                errno = 0;
                i = read(jfd, temp.tmp, 4);                                     // read the magic

                if ((i != 4) || (temp.magic != (RSM_MAGIC - 1))) {
                    i = errno;
                    close(jfd);
                    return -(ERRMLAST + ERRZLAST + i);                          // exit with error
                } else {
                    i = read(jfd, &partab.vol[vol]->jrn_next, sizeof(off_t));

                    if (i != sizeof(off_t)) {
                        i = errno;
                        close(jfd);
                        return -(ERRMLAST + ERRZLAST + i);                      // exit with error
                    } else {
                        jptr = lseek(jfd, partab.vol[vol]->jrn_next, SEEK_SET);

                        if (jptr != partab.vol[vol]->jrn_next) {
                            i = errno;
                            close(jfd);
                            return -(ERRMLAST + ERRZLAST + i);                  // exit with error
                        } else {
                            jj.action = JRN_START;
                            jj.uci = 0;
#if RSM_DBVER == 1
                            jj.size = 8;
                            jj.time = current_time(TRUE);
#else
                            jj.size = 12;
                            jj.time = (u_int64) current_time(TRUE);
#endif
                            i = write(jfd, &jj, jj.size);                       // write the create record
                            if (i == -1) return -(ERRMLAST + ERRZLAST + errno); // if that failed exit with error
                            partab.vol[vol]->jrn_next += jj.size;               // adjust pointer
                            lseek(jfd, 4, SEEK_SET);
                            i = write(jfd, &partab.vol[vol]->jrn_next, sizeof(off_t));
                            if (i == -1) return -(ERRMLAST + ERRZLAST + errno); // if that failed exit with error
                            i = close(jfd);                                     // and close it
                            if (i == -1) return -(ERRMLAST + ERRZLAST + errno); // if that failed exit with error
                            labelblock->journal_available = 1;
                        }
                    }
                }
            }
        }

        SemOp(SEM_GLOBAL, -SEM_WRITE);                                          // unlock global
    }                                                                           // end journal stuff

    SemOp(SEM_WD, -SEM_WRITE);                                                  // release WD lock
    gptr = SOA(partab.vol[vol]->gbd_head);                                      // get start of GBDs
    ptr = (u_char *) SOA(partab.vol[vol]->global_buf);                          // get start of global buffers

    for (u_int j = 0; j < partab.vol[vol]->num_gbd; j++) {                      // for each GBD
        gptr[j].mem = (DB_Block *) SBA(ptr);                                    // point at block
        ptr += labelblock->block_size;                                          // point at next

        if (j < (partab.vol[vol]->num_gbd - 1)) {                               // all but the last
            gptr[j].next = SBA(&gptr[j + 1]);                                   // link to next
        } else {                                                                // the last
            gptr[j].next = NULL;                                                // end of list
        }

        partab.vol[vol]->gbd_hash[GBD_HASH] = SBA(gptr);                        // head of free list
    }                                                                           // end setup GBDs

    Routine_Init(vol);                                                          // and the routine junk
    i = close(dbfd);                                                            // close the database for read-write
    if (i == -1) return -(ERRMLAST + ERRZLAST + errno);                         // if that failed exit with error
    dbfd = open(file, O_RDONLY);                                                // open the supplemental database for read
    if (dbfd == -1) return -(ERRMLAST + ERRZLAST + errno);                      // if that failed exit with error
    partab.vol_fds[vol] = dbfd;                                                 // make sure FD is right
    return 0;                                                                   // indicate success
}
