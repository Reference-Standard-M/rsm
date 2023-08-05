/*
 * Package:  Reference Standard M
 * File:     rsm/database/mount.c
 * Summary:  module database - mount a database file
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
#include <sys/types.h>                                                          // for u_char def
#include <string.h>                                                             // string controls always handy
#include <ctype.h>                                                              // this is handy too
#include <sys/param.h>                                                          // for realpath() function
#include <errno.h>                                                              // error stuff
#include <fcntl.h>                                                              // file stuff
#include <unistd.h>                                                             // database access
#include <sys/ipc.h>                                                            // shared memory
#include <sys/shm.h>                                                            // shared memory
#include <sys/sem.h>                                                            // semaphores
#include <sys/stat.h>                                                           // stat
#include "rsm.h"                                                                // standard includes
#include "proto.h"                                                              // standard includes
#include "database.h"                                                           // database includes
#include "error.h"                                                              // error strings

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
    int             i;                                                          // useful int
    u_int           n_gbd;                                                      // number of GBD
    long long       avbl;                                                       // available space
    int             indx;                                                       // loop control
    key_t           shar_mem_key;                                               // memory "key"
    int             shar_mem_id;                                                // memory id
    //int             map_size;                                                   // size of database map (bytes)
    long long       volset_size;                                                // size of volset struct (bytes)
    long            pagesize;                                                   // system pagesize (bytes)
    struct shmid_ds sbuf;                                                       // for shmctl
    char            fullpathvol[MAXPATHLEN];                                    // full pathname of vol file
    gbd             *gptr;                                                      // a GBD pointer
    u_char          *ptr;                                                       // and a byte one
    //u_char          *ptr2;                                                      // and another
    label_block     *labelblock;                                                // label block pointer

    if ((vol < 1) || (vol >= MAX_VOL)) return -(ERRZ71 + ERRMLAST);             // valid vol range for mount
    if (systab->vol[vol] != NULL) return -(ERRZ71 + ERRMLAST);                  // check for already mounted
    if (systab->maxjob == 1) return -(ERRZ60 + ERRMLAST);                       // if in single user mode then complain

    for (u_int j = 0; j < MAX_VOL; j++) {
        if (systab->vol[j] == NULL) continue;

        if (ftok(systab->vol[j]->file_name, RSM_SYSTEM) == ftok(file, RSM_SYSTEM)) { // make sure volume isn't already mounted
            return -(ERRZ71 + ERRMLAST);
        }
    }

    if (gmb > MAX_GLOBAL_BUFFERS) return -(ERRMLAST + ERRZLAST + EINVAL);       // check size of global buffers
    if (rmb > MAX_ROUTINE_BUFFERS) return -(ERRMLAST + ERRZLAST + EINVAL);      // check size of routine buffers
    int jobs = systab->maxjob;
    if (gmb == 0) gmb = jobs / 2;                                               // default global buffers
    if (gmb < 1) gmb = 1;                                                       // but at least 1 MiB
    if (rmb == 0) rmb = jobs / 8;                                               // pick a default
    if (rmb < 1) rmb = 1;                                                       // but at least 1 MiB

    if (systab->addsize < (sizeof(vol_def) + (gmb * MBYTE) + (rmb * MBYTE))) {  // check for spare buffer space
        return -(ERRZ60 + ERRMLAST);
    }

    dbfd = open(file, O_RDWR);                                                  // open the database read/write
    if (dbfd == -1) return -(ERRMLAST + ERRZLAST + errno);                      // if that failed exit with error
    labelblock = (label_block *) ((char *) systab->address + systab->addoff + sizeof(vol_def)); // point at the spare memory
    i = read(dbfd, labelblock, sizeof(label_block));                            // read label block
    if (i == -1) return -(ERRZ73 + ERRMLAST);                                   // in case of error
    if (i < (int) sizeof(label_block)) return -(ERRZ73 + ERRMLAST);             // in case of error
    if (labelblock->magic != RSM_MAGIC) return -(ERRZ73 + ERRMLAST);
    if (labelblock->db_ver != DB_VER) return -(ERRZ73 + ERRMLAST);              // if we need to upgrade

    for (int j = 0; j < MAX_VOL; j++) {                                         // make sure no other volumes with the same name
        if ((j == vol) || (systab->vol[j] == NULL)) continue;

        if (var_equal(labelblock->volnam, systab->vol[j]->vollab->volnam)) {
            return -(ERRZ73 + ERRMLAST);                                        // exit with error
        }
    }

    //map_size = labelblock->header_bytes - sizeof(label_block);                  // fetch map size
    //avbl = systab->addsize;                                                     // and available size
    //avbl -= map_size;                                                           // subtract map size
    //avbl -= sizeof(label_block);                                                // and label block size
    //avbl -= sizeof(vol_def);                                                    // and the vol structure
    systab->vol[vol] = (vol_def *) ((char *) systab->address + systab->addoff); // the vol structure
    systab->vol[vol]->vollab = labelblock;                                      // and point to label blk
    //ptr = (u_char *) systab->vol[vol]->vollab + sizeof(vol_def);                // up to here
    //ptr2 = ptr;                                                                 // save
    pagesize = sysconf(_SC_PAGESIZE);                                           // get system pagesize (bytes)
    //ptr += (((sizeof(vol_def) / pagesize) + 1) * pagesize);                     // round up to next page boundary
    //avbl -= (ptr - ptr2);                                                       // and adjust size
    //n_gbd = avbl / systab->vol[vol]->vollab->block_size;                        // This many
    n_gbd = ((long) gmb * MBYTE) / labelblock->block_size;                      // number of GBD

    while (n_gbd < MIN_GBD) {                                                   // if not enough
        gmb++;                                                                  // increase it
        n_gbd = ((long) gmb * MBYTE) / labelblock->block_size;                  // number of GBD
    }

    shar_mem_key = ftok(systab->vol[0]->file_name, RSM_SYSTEM);                 // get key to existing share
    if (shar_mem_key == -1) return -(ERRMLAST + ERRZLAST + errno);              // if that failed exit with error
    shar_mem_id = shmget(shar_mem_key, 0, 0);                                   // attach to existing share
    if (shar_mem_id == -1) return -(ERRMLAST + ERRZLAST + errno);               // if that failed exit with error

    volset_size = sizeof(vol_def)                                               // size of VOL_DEF (one for now)
                + labelblock->header_bytes
                + (n_gbd * sizeof(gbd))                                         // the GBD
                + ((long) gmb * MBYTE)                                          // MiB of global buffers
                + labelblock->block_size
                + ((long) rmb * MBYTE);                                         // MiB of routine buffers

    volset_size = ((volset_size - 1) / pagesize + 1) * pagesize;                // round up
    if (systab->addsize < (u_long) volset_size) return -(ERRZ60 + ERRMLAST);    // check for spare buffer space
    avbl = systab->addsize - volset_size;                                       // and available size
    systab->vol[vol]->map = (void*) ((char *) systab->vol[vol]->vollab + sizeof(label_block));

    // and point to map
    systab->vol[vol]->first_free = systab->vol[vol]->map;                       // init first free
    systab->vol[vol]->gbd_head = (gbd *) ((char *) systab->vol[vol]->vollab + labelblock->header_bytes); // GBDs
    systab->vol[vol]->num_gbd = n_gbd;                                          // number of GBDs
    systab->vol[vol]->global_buf = (void *) &systab->vol[vol]->gbd_head[n_gbd]; // global buffers
    systab->vol[vol]->zero_block = (void *) &(((u_char *) systab->vol[vol]->global_buf)[((long) gmb * MBYTE)]);

    // pointer to zero blk
    systab->vol[vol]->rbd_head = (void *) ((char *) systab->vol[vol]->zero_block + labelblock->block_size); //RBDs
    systab->vol[vol]->rbd_end = (void *) ((char *) systab->vol[vol]->vollab + volset_size); // end of share
    systab->vol[vol]->shm_id = shar_mem_id;                                     // set up share id
    systab->vol[vol]->map_dirty_flag = 0;                                       // clear dirty map flag
    systab->addoff += ((char *) systab->vol[vol]->rbd_end - ((char *) systab->address + systab->addoff)); // reset additional offset
    systab->addsize = avbl;                                                     // reset additional address size

    if (realpath(file, fullpathvol) != NULL) {                                  // get full path
        if (strlen(fullpathvol) <= VOL_FILENAME_MAX) {                          // if can fit in our struct
            strcpy(systab->vol[vol]->file_name, fullpathvol);                   // copy this full path into the vol_def structure
        } else {                                                                // end if path will fit - otherwise
            i = strlen(fullpathvol) - VOL_FILENAME_MAX;                         // copy as much as
            strcpy(systab->vol[vol]->file_name, &fullpathvol[i]);               // is possible, thats the best we can do
        }                                                                       // end length testing
    } else {                                                                    // end realpath worked - or there was an error
        i = errno;                                                              // save realpath error
        shmdt(systab);                                                          // detach the shared mem
        shmctl(shar_mem_id, IPC_RMID, &sbuf);                                   // remove the share
        return -(ERRMLAST + ERRZLAST + i);                                      // if that failed exit with error
    }

    if (shmctl(shar_mem_id, IPC_STAT, &sbuf) == -1) return -(ERRMLAST + ERRZLAST + errno); // get status for later
    if (lseek(dbfd, 0, SEEK_SET) == -1) return -(ERRMLAST + ERRZLAST + errno);  // re-point at start of file
    i = read(dbfd, systab->vol[vol]->vollab, labelblock->header_bytes);         // read label & map block

    if (i < (int) labelblock->header_bytes) {                                   // in case of error
        fprintf(stderr, "Read of label/map block failed - %s\n", strerror(errno)); // complain on error
        shmdt(systab);                                                          // detach the shared mem
        shmctl(shar_mem_id, IPC_RMID, &sbuf);                                   // remove the share
        return -(ERRMLAST + ERRZLAST + errno);                                  // exit with error
    }

    if (systab->vol[vol]->vollab->clean == 0) {                                 // if not a clean dismount
        fprintf(stderr, "WARNING: Volume was not dismounted properly!\n");
        systab->vol[vol]->upto = 1;                                             // mark for cleaning
    } else {
        systab->vol[vol]->vollab->clean = 1;                                    // mark as mounted
        systab->vol[vol]->map_dirty_flag = 1;                                   // and map needs writing
    }

    jobs /= DAEMONS;                                                            // number of daemons
    if (jobs < MIN_DAEMONS) jobs = MIN_DAEMONS;                                 // minimum of MIN_DAEMONS
    if (jobs > MAX_DAEMONS) jobs = MAX_DAEMONS;                                 // and the max
    systab->vol[vol]->num_of_daemons = jobs;                                    // initialize this
    while (SemOp(SEM_WD, WRITE)) continue;                                      // lock WD

    partab.vol_fds[vol] = dbfd;                                                 // so the daemons can close inherited FD

    for (indx = 0; indx < jobs; indx++) {                                       // for each required daemon
        i = DB_Daemon(indx, vol + 1);                                           // start each daemon (for mounted volume)

        if (i != 0) {                                                           // in case of error
            fprintf(stderr, "**** Died on error - %s ***\n\n", strerror(i));    // complain
            shmdt(systab);                                                      // detach the shared mem
            return -(ERRMLAST + ERRZLAST + i);                                  // exit with error
        }
    }                                                                           // all daemons started

    if (systab->vol[vol]->vollab->journal_requested && systab->vol[vol]->vollab->journal_file[0]) {
        struct stat sb;                                                         // File attributes
        off_t  jptr;                                                            // file ptr
        jrnrec jj;                                                              // to write with
        int    jfd;                                                             // file descriptor

        while (SemOp(SEM_GLOBAL, WRITE)) continue;                              // lock GLOBAL
        systab->vol[vol]->vollab->journal_available = 0;                        // assume fail
        i = stat(systab->vol[vol]->vollab->journal_file, &sb);                  // check for file

        if ((i == -1) && (errno != ENOENT)) {                                   // if that's junk
            fprintf(stderr, "Failed to access journal file: %s\n", systab->vol[vol]->vollab->journal_file);
        } else {                                                                // do something
            if (i == -1) ClearJournal(vol);                                     // if doesn't exist then create it
            jfd = open(systab->vol[vol]->vollab->journal_file, O_RDWR);

            if (jfd == -1) {                                                    // on fail
                fprintf(stderr, "Failed to open journal file: %s\nerrno = %d\n", systab->vol[vol]->vollab->journal_file, errno);
            } else {                                                            // if open OK
                union {
                    u_int magic;
                    u_char tmp[4];
                } temp;

                lseek(jfd, 0, SEEK_SET);
                errno = 0;
                i = read(jfd, temp.tmp, 4);                                     // read the magic

                if ((i != 4) || (temp.magic != (RSM_MAGIC - 1))) {
                    fprintf(stderr, "Failed to open journal file: %s\nWRONG MAGIC\n", systab->vol[vol]->vollab->journal_file);
                    close(jfd);
                } else {
                    i = read(jfd, &systab->vol[vol]->jrn_next, sizeof(off_t));

                    if (i != sizeof(off_t)) {
                        fprintf(stderr, "Failed to use journal file: %s\nRead failed - %d\n",
                                systab->vol[vol]->vollab->journal_file, errno);

                        close(jfd);
                    } else {
                        jptr = lseek(jfd, systab->vol[vol]->jrn_next, SEEK_SET);

                        if (jptr != systab->vol[vol]->jrn_next) {
                            fprintf(stderr, "Failed journal file: %s\nlseek failed - %d\n",
                                    systab->vol[vol]->vollab->journal_file, errno);

                            close(jfd);
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
                            systab->vol[vol]->jrn_next += jj.size;              // adjust pointer
                            lseek(jfd, 4, SEEK_SET);
                            i = write(jfd, &systab->vol[vol]->jrn_next, sizeof(off_t));
                            if (i == -1) return -(ERRMLAST + ERRZLAST + errno); // if that failed exit with error
                            i = close(jfd);                                     // and close it
                            if (i == -1) return -(ERRMLAST + ERRZLAST + errno); // if that failed exit with error
                            systab->vol[vol]->vollab->journal_available = 1;
                            printf("Journaling started to %s\n", systab->vol[vol]->vollab->journal_file); // it worked
                        }
                    }
                }
            }
        }

        SemOp(SEM_GLOBAL, -WRITE);                                              // unlock global
    }                                                                           // end journal stuff

    SemOp(SEM_WD, -WRITE);                                                      // release WD lock
    gptr = systab->vol[vol]->gbd_head;                                          // get start of GBDs
    ptr = (u_char *) systab->vol[vol]->global_buf;                              // get start of Globuff

    for (u_int j = 0; j < systab->vol[vol]->num_gbd; j++) {                     // for each GBD
        gptr[j].mem = (DB_Block *) ptr;                                         // point at block
        ptr += systab->vol[vol]->vollab->block_size;                            // point at next

        if (j < (systab->vol[vol]->num_gbd - 1)) {                              // all but the last
            gptr[j].next = &gptr[j + 1];                                        // link to next
        } else {                                                                // the last
            gptr[j].next = NULL;                                                // end of list
        }

        systab->vol[vol]->gbd_hash[GBD_HASH] = gptr;                            // head of free list
    }                                                                           // end setup GBDs

    Routine_Init(vol);                                                          // and the routine junk
    i = close(dbfd);                                                            // close the database for read-write
    if (i == -1) return -(ERRMLAST + ERRZLAST + errno);                         // if that failed exit with error
    dbfd = open(file, O_RDONLY);                                                // open the supplemental database for read
    if (dbfd == -1) return -(ERRMLAST + ERRZLAST + errno);                      // if that failed exit with error
    partab.vol_fds[vol] = dbfd;                                                 // make sure FD is right
    return 0;                                                                   // indicate success
}
