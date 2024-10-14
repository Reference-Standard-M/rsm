/*
 * Package: Reference Standard M
 * File:    rsm/init/start.c
 * Summary: module init - startup code
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

/***********************************************************************\
* Initialize an environment - switches are:                             *
*     database file name           (1 to VAR_LEN)                   Req *
*  -j max number of jobs           (1 to MAX_JOBS)                  Req *
*  -g global buffers in MiB        (0 to MAX_GLOBAL_BUFFERS)        Opt *
*  -r routine buffers in MiB       (0 to MAX_ROUTINE_BUFFERS)       Opt *
*  -a additional buffers in MiB    (1 to MAX_ADDITIONAL_BUFFERS)    Opt *
\***********************************************************************/

// database, number of jobs, MiB of global buffers, MiB of routine buffers, MiB of additional buffers (for volumes)
int INIT_Start(char *file, u_int jobs, u_int gmb, u_int rmb, u_int addmb)
{
    int               dbfd;                                                     // database file descriptor
    int               hbuf[sizeof(label_block) / 4];                            // header buffer
    int               i;                                                        // useful int
    u_int             n_gbd;                                                    // number of GBD
    long long         addoff;                                                   // offset for add buff
    key_t             shar_mem_key;                                             // memory key
    int               shar_mem_id;                                              // memory id
    int               sem_id;                                                   // semaphore id
    u_short           sem_val[SEM_MAX];                                         // to init them
    semun_t           semvals;                                                  // for a pointer to sem_val
    long long         share_size;                                               // size of share (bytes)
    int               locksize;                                                 // size of locktab
    int               size;                                                     // used to calculate real locksize
    long              sjlt_size;                                                // size of systab + jobtab + locktab
    long long         volset_size;                                              // size of volset struct (bytes)
    long              pagesize;                                                 // system pagesize (bytes)
    struct shmid_ds   sbuf;                                                     // for shmctl
    char              fullpathvol[MAXPATHLEN];                                  // full pathname of vol file
    gbd               *gptr;                                                    // a GBD pointer
    u_char            *ptr;                                                     // and a byte one
    const label_block *labelblock;                                              // label block pointer
    char              version[120];                                             // a string

    rsm_version((u_char *) version);                                            // get version into version[]
    printf("%s\n", version);                                                    // print version string

    if ((jobs < 1) || (jobs > MAX_JOBS)) {                                      // check number of jobs
        fprintf(stderr, "Invalid number of jobs %u - must be 1 to %d\n", jobs, MAX_JOBS);
        return EINVAL;                                                          // exit with error
    }

    if (gmb > MAX_GLOBAL_BUFFERS) {                                             // check size of global buffers
        fprintf(stderr, "Global buffer size must not exceed %d MiB\n", MAX_GLOBAL_BUFFERS); // complain
        return EINVAL;                                                          // return an error
    }                                                                           // end global buffers check

    if (rmb > MAX_ROUTINE_BUFFERS) {                                            // check size of routine buffers
        fprintf(stderr, "Routine buffer size must not exceed %d MiB\n", MAX_ROUTINE_BUFFERS); // complain
        return EINVAL;                                                          // return an error
    }                                                                           // end routine buffers check

    pagesize = sysconf(_SC_PAGESIZE);                                           // get system pagesize (bytes)
    if (gmb == 0) gmb = jobs / 2;                                               // default global buffers
    if (gmb < 1) gmb = 1;                                                       // but at least 1 MiB
    if (rmb == 0) rmb = jobs / 8;                                               // pick a default
    if (rmb < 1) rmb = 1;                                                       // but at least 1 MiB
    locksize = jobs * LOCKTAB_SIZE;                                             // what we need for locktab
    locksize = (((locksize - 1) / pagesize) + 1) * pagesize;                    // round up
    dbfd = open(file, O_RDWR);                                                  // open the database read/write

    if (dbfd == -1) {                                                           // if that failed
        fprintf(stderr, "Open of database %s failed - %s\n", file, strerror(errno)); // what was returned
        return errno;                                                           // exit with error
    }                                                                           // end file create test

    i = read(dbfd, hbuf, sizeof(label_block));                                  // read label block

    if (i < (int) sizeof(label_block)) {                                        // in case of error
        fprintf(stderr, "Read of label block failed - %s\n", strerror(errno));  // what was returned
        return errno;                                                           // exit with error
    }

    labelblock = (label_block *) hbuf;                                          // point label block at it

    if (labelblock->db_ver != DB_VER) {                                         // if we need to upgrade
        fprintf(stderr, "Database is version %u, image requires version %d - start failed!!\n", labelblock->db_ver, DB_VER);

        if (labelblock->db_ver < DB_VER) {
            fprintf(stderr, "Upgrade database to version %d by running the bin/upgrade script\n", DB_VER);
        }

        return -1;
    }                                                                           // end upgrade procedure

    if (labelblock->magic != RSM_MAGIC) {
        fprintf(stderr, "Invalid RSM database (wrong magic) - start failed!!\n");
        return -1;
    }

    shar_mem_key = ftok(file, RSM_SYSTEM);                                      // get a unique key

    if (shar_mem_key == -1) {                                                   // die on error
        fprintf(stderr, "Unable to access database file: %s - %s\n", file, strerror(errno)); // give an error
        return errno;                                                           // and return with error
    }

    shar_mem_id = shmget(shar_mem_key, 0, 0);                                   // attach to existing share

    if (shar_mem_id != -1) {                                                    // check to see if it's there
        fprintf(stderr, "RSM environment is already initialized.\n");
        return EEXIST;                                                          // exit with error
    }

    if (addmb > 0) {
        fprintf(stderr, "Additional buffers for mounting volumes are not currently supported - start failed!!\n");
        return -1;
    }

    n_gbd = ((long long) gmb * MBYTE) / hbuf[3];                                // number of GBD

    while (n_gbd < MIN_GBD) {                                                   // if not enough
        gmb++;                                                                  // increase it
        n_gbd = ((long long) gmb * MBYTE) / hbuf[3];                            // number of GBD
    }

    for (i = 0; i < SEM_MAX; sem_val[i++] = jobs) continue;                     // setup for sem init
    semvals.array = sem_val;

    sjlt_size = sizeof(systab_struct)                                           // size of Systab
              + ((sizeof(u_int) * (jobs - 1)) * MAX_VOL)                        // adjust for last_blk_used[MAX_VOL]
              + (sizeof(jobtab) * jobs)                                         // size of JOBTAB
              + locksize;                                                       // size of LOCKTAB

    size = sjlt_size;                                                           // used to calculate real locksize
    sjlt_size = (((sjlt_size - 1) / pagesize) + 1) * pagesize;                  // round up
    locksize += sjlt_size - size;                                               // calculate real locksize

    volset_size = sizeof(vol_def)                                               // size of VOL_DEF (one for now)
                + hbuf[2]                                                       // size of head and map block
                + (n_gbd * sizeof(gbd))                                         // the GBD
                + ((long long) gmb * MBYTE)                                     // MiB of global buffers
                + hbuf[3]                                                       // size of block (zero block)
                + ((long) rmb * MBYTE);                                         // MiB of routine buffers

    volset_size = ((volset_size - 1) / pagesize + 1) * pagesize;                // round up
    share_size = sjlt_size + volset_size;                                       // shared memory size
    addoff = share_size;                                                        // where add buff starts
    share_size += (long long) addmb * MBYTE;                                    // and the additional
    printf("Creating share for %u job%s with %u MiB routine space,\n", jobs, ((jobs > 1) ? "s" : ""), rmb);
    printf("%u MiB (%u) global buffers, %d KiB label/map space,\n", gmb, n_gbd, hbuf[2] / 1024);
    if (addmb > 0) printf("%u MiB of additional buffers for supplemental volumes,\n", addmb);
    printf("%d KiB for the lock table, with a total share size of %lld MiB.\n", locksize / 1024, share_size / MBYTE);
    shar_mem_id = shmget(shar_mem_key, share_size, IPC_CREAT | S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP); // create share memory (0660)

    if (shar_mem_id == -1) {                                                    // die on error
        fprintf(stderr, "Unable to create shared memory segment - %s\n", strerror(errno)); // give an error

        if (errno == EINVAL) {
#ifdef __APPLE__
            fprintf(stderr, "\tIncrease kern.sysv.shmmax and/or kern.sysv.shmall\n");
#elif defined(__OpenBSD__)
            fprintf(stderr, "\tIncrease kern.shminfo.shmmax and/or kern.shminfo.shmall\n");
#elif defined(__FreeBSD__)
            fprintf(stderr, "\tIncrease kern.ipc.shmmax and/or kern.ipc.shmall\n");
#elif defined(__NetBSD__)
            fprintf(stderr, "\tIncrease kern.ipc.shmmax and/or kern.ipc.shmmaxpgs\n");
#else
            fprintf(stderr, "\tIncrease kernel.shmmax and/or kernel.shmall\n");
#endif
        } else if (errno == ENOMEM) {
            fprintf(stderr, "\tInsufficient shared memory remaining\n");
        }

        return errno;                                                           // and return with error
    }

    sem_id = semget(shar_mem_key, SEM_MAX, IPC_CREAT | S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP); // create semaphores (0660)

    if (sem_id == -1) {
        fprintf(stderr, "Unable to create semaphore set - %s\n", strerror(errno)); // give an error
        shmctl(shar_mem_id, IPC_RMID, &sbuf);                                   // remove the share
        return errno;                                                           // and return with error
    }

    i = semctl(sem_id, 0, SETALL, semvals);                                     // clear the values

    if (i == -1) {                                                              // check for error
        fprintf(stderr, "Unable to clear semaphores - %s\n", strerror(errno));  // give an error
        shmctl(shar_mem_id, IPC_RMID, &sbuf);                                   // remove the share
        semctl(sem_id, 0, IPC_RMID, semvals);                                   // and the semaphores
        return errno;                                                           // and return with error
    }

    systab = shmat(shar_mem_id, SHMAT_SEED, 0);                                 // map it

    if (systab == (void *) -1) {                                                // die on error
        fprintf(stderr, "Unable to attach to the environment correctly - %s\n", strerror(errno)); // give error
        shmctl(shar_mem_id, IPC_RMID, &sbuf);                                   // remove the share
        semctl(sem_id, 0, IPC_RMID, semvals);                                   // and the semaphores
        return errno;                                                           // exit with error
    }

    printf("Environment attached at 0x%lx [semid: %d  shmid: %d]\n", (u_long) systab, sem_id, shar_mem_id);
    memset(systab, 0, share_size);                                              // zot the lot
    systab->address = systab;                                                   // store the address for ron
    systab->jobtab = (jobtab *) &systab->last_blk_used[jobs * MAX_VOL];         // setup jobtab pointer
    systab->maxjob = jobs;                                                      // save max jobs
    systab->start_user = (int) getuid();                                        // remember who started this
    systab->precision = DEFAULT_PREC;                                           // decimal precision
    systab->historic = HISTORIC_EOK | HISTORIC_OFFOK | HISTORIC_DNOK;           // default "historic" features to on
    systab->lockstart = (void *) ((char *) systab->jobtab + (sizeof(jobtab) * jobs)); // locktab
    systab->locksize = locksize;                                                // the size
    systab->lockhead = NULL;                                                    // no locks currently
    systab->lockfree = (locktab *) systab->lockstart;                           // free space
    systab->lockfree->fwd_link = NULL;                                          // only one
    systab->lockfree->size = locksize;                                          // the whole space
    systab->lockfree->job = -1;                                                 // means free
    systab->addoff = addoff;                                                    // Add buffer offset
    systab->addsize = ((long long) addmb * MBYTE);                              // and size in bytes
    systab->vol[0] = (vol_def *) ((char *) systab + sjlt_size);                 // jump to start of volume set memory
    systab->vol[0]->vollab = (label_block *) ((char *) systab->vol[0] + sizeof(vol_def)); // and point to label blk
    systab->vol[0]->map = (void *) ((char *) systab->vol[0]->vollab + sizeof(label_block)); // and point to map
    systab->vol[0]->first_free = systab->vol[0]->map;                           // init first free
    systab->vol[0]->gbd_head = (gbd *) ((char *) systab->vol[0]->vollab + hbuf[2]); // GBDs
    systab->vol[0]->num_gbd = n_gbd;                                            // number of GBDs
    systab->vol[0]->global_buf = (void *) &systab->vol[0]->gbd_head[n_gbd];     // global buffers
    systab->vol[0]->zero_block = (void *) &(((u_char *) systab->vol[0]->global_buf)[(long long) gmb * MBYTE]); // point to zero blk
    systab->vol[0]->rbd_head = (void *) ((char *) systab->vol[0]->zero_block + hbuf[3]); // RBDs
    systab->vol[0]->rbd_end = (void *) ((char *) systab + share_size - systab->addsize); // end of share
    systab->vol[0]->shm_id = shar_mem_id;                                       // set up share ID
    systab->sem_id = sem_id;                                                    // set up semaphore ID
    systab->vol[0]->map_dirty_flag = 0;                                         // clear dirty map flag

    if (realpath(file, fullpathvol) != NULL) {                                  // get full path
        if (strlen(fullpathvol) <= VOL_FILENAME_MAX) {                          // if can fit in our struct
            strcpy(systab->vol[0]->file_name, fullpathvol);                     // copy path into the vol_def structure
        } else {                                                                // end if path will fit, otherwise
            i = strlen(fullpathvol) - VOL_FILENAME_MAX;                         // copy as much as
            strcpy(systab->vol[0]->file_name, &fullpathvol[i]);                 // is possible, that's the best we can do
        }                                                                       // end length testing
    } else {                                                                    // end realpath worked, otherwise it was an error
        i = errno;                                                              // save realpath error
        fprintf(stderr, "Read of label/map block failed - %s\n", strerror(errno)); // what was returned
        shmdt(systab);                                                          // detach the shared memory
        shmctl(shar_mem_id, IPC_RMID, &sbuf);                                   // remove the share
        semctl(sem_id, 0, IPC_RMID, semvals);                                   // and the semaphores
        return i;                                                               // exit with error
    }

    if (shmctl(shar_mem_id, IPC_STAT, &sbuf) == -1) return errno;               // get status for later
    if (lseek(dbfd, 0, SEEK_SET) == (off_t) -1) return errno;                   // re-point at start of file
    i = read(dbfd, systab->vol[0]->vollab, hbuf[2]);                            // read label & map block

    if (i < hbuf[2]) {                                                          // in case of error
        i = errno;                                                              // save read error
        fprintf(stderr, "Read of label/map block failed - %s\n", strerror(errno)); // what was returned
        shmdt(systab);                                                          // detach the shared memory
        shmctl(shar_mem_id, IPC_RMID, &sbuf);                                   // remove the share
        semctl(sem_id, 0, IPC_RMID, semvals);                                   // and the semaphores
        return i;                                                               // exit with error
    }

    if (systab->vol[0]->vollab->clean == 0) {                                   // if not a clean dismount
        fprintf(stderr, "WARNING: Volume was not dismounted properly!\n");
        systab->vol[0]->upto = 1;                                               // mark for cleaning
    } else {
        systab->vol[0]->vollab->clean = 1;                                      // mark as mounted
        systab->vol[0]->map_dirty_flag = 1;                                     // and map needs writing
    }

    jobs /= DAEMONS;                                                            // number of daemons
    if (jobs < MIN_DAEMONS) jobs = MIN_DAEMONS;                                 // minimum of MIN_DAEMONS
    if (jobs > MAX_DAEMONS) jobs = MAX_DAEMONS;                                 // and the max
    systab->vol[0]->num_of_daemons = jobs;                                      // initialize this
    while (SemOp(SEM_WD, SEM_WRITE)) continue;                                  // lock WD

    partab.job_table = systab->jobtab;                                          // so the daemons can start processes
    partab.vol[0] = systab->vol[0];                                             // so the daemons can manage table slots
    partab.vol_fds[0] = dbfd;                                                   // so the daemons can close inherited FD
    fflush(stdout);                                                             // force a flush before forking

    for (u_int indx = 0; indx < jobs; indx++) {                                 // for each required daemon
        i = DB_Daemon(indx, 1);                                                 // start each daemon (volume 1)

        if (i != 0) {                                                           // in case of error
            fprintf(stderr, "*** Died on error - %s ***\n\n", strerror(i));     // what was returned
            shmdt(systab);                                                      // detach the shared memory
            return errno;                                                       // exit with error
        }
    }                                                                           // all daemons started

    if (systab->maxjob == 1) {                                                  // if in single-user mode
        printf("WARNING: Single-user, journaling not started.\n");
    } else if (systab->vol[0]->vollab->journal_requested && systab->vol[0]->vollab->journal_file[0]) {
        struct stat sb;                                                         // File attributes
        off_t       jptr;                                                       // file pointer
        jrnrec      jj;                                                         // to write with
        int         jfd;                                                        // file descriptor

        while (SemOp(SEM_GLOBAL, SEM_WRITE)) continue;                          // lock GLOBAL
        systab->vol[0]->vollab->journal_available = 0;                          // assume fail
        i = stat(systab->vol[0]->vollab->journal_file, &sb);                    // check for file

        if ((i < 0) && (errno != ENOENT)) {                                     // if that's junk
            fprintf(stderr, "Failed to access journal file: %s\n", systab->vol[0]->vollab->journal_file);
        } else {                                                                // do something
            if (i < 0) ClearJournal(0);                                         // if doesn't exist, create it
            jfd = open(systab->vol[0]->vollab->journal_file, O_RDWR);

            if (jfd < 0) {                                                      // on fail
                fprintf(stderr, "Failed to open journal file: %s\nerrno = %d\n", systab->vol[0]->vollab->journal_file, errno);
            } else {                                                            // if open OK
                union {
                    u_int magic;
                    u_char tmp[4];
                } temp;

                lseek(jfd, 0, SEEK_SET);
                errno = 0;
                i = read(jfd, temp.tmp, 4);                                     // read the magic

                if ((i != 4) || (temp.magic != (RSM_MAGIC - 1))) {
                    fprintf(stderr, "Failed to open journal file: %s\nWRONG MAGIC\n", systab->vol[0]->vollab->journal_file);
                    close(jfd);
                } else {
                    i = read(jfd, &systab->vol[0]->jrn_next, sizeof(off_t));
                    if (i != sizeof(off_t)) {
                        fprintf(stderr, "Failed to use journal file: %s\nRead failed - %d\n",
                                systab->vol[0]->vollab->journal_file, errno);

                        close(jfd);
                    } else {
                        jptr = lseek(jfd, systab->vol[0]->jrn_next, SEEK_SET);

                        if (jptr != systab->vol[0]->jrn_next) {
                            fprintf(stderr, "Failed journal file: %s\nlseek failed - %d\n",
                                    systab->vol[0]->vollab->journal_file, errno);

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

                            if (i < jj.size) {                                  // if that failed
                                close(jfd);                                     // close the file
                                fprintf(stderr, "Journal file write failed - %s\n", strerror(errno)); // what was returned
                                return errno;                                   // and return
                            }                                                   // probably should delete it

                            systab->vol[0]->jrn_next += jj.size;                // adjust pointer
                            lseek(jfd, 4, SEEK_SET);
                            i = write(jfd, &systab->vol[0]->jrn_next, sizeof(off_t));

                            if (i < (int) sizeof(off_t)) {                      // if that failed
                                close(jfd);                                     // close the file
                                fprintf(stderr, "Journal file write failed - %s\n", strerror(errno)); // what was returned
                                return errno;                                   // and return
                            }                                                   // probably should delete it

                            close(jfd);                                         // and close it
                            systab->vol[0]->vollab->journal_available = 1;
                            printf("Journaling started to %s\n", systab->vol[0]->vollab->journal_file); // say it worked
                        }
                    }
                }
            }
        }

        SemOp(SEM_GLOBAL, -SEM_WRITE);                                          // unlock global
    }                                                                           // end journal stuff

    SemOp(SEM_WD, -SEM_WRITE);                                                  // release WD lock
    gptr = systab->vol[0]->gbd_head;                                            // get start of GBDs
    ptr = (u_char *) systab->vol[0]->global_buf;                                // get start of global buffers

    for (u_int j = 0; j < systab->vol[0]->num_gbd; j++) {                       // for each GBD
        gptr[j].mem = (DB_Block *) ptr;                                         // point at block
        ptr += systab->vol[0]->vollab->block_size;                              // point at next

        if (j < (systab->vol[0]->num_gbd - 1)) {                                // all but the last
            gptr[j].next = &gptr[j + 1];                                        // link to next
        } else {                                                                // the last
            gptr[j].next = NULL;                                                // end of list
        }
    }                                                                           // end setup GBDs

    systab->vol[0]->gbd_hash[GBD_HASH] = gptr;                                  // head of free list
    Routine_Init(0);                                                            // and the routine junk
    shmdt(systab);                                                              // detach the shared memory
    close(dbfd);                                                                // close the database
    printf("RSM environment initialized.\n");                                   // say something
    return 0;                                                                   // indicate success
}
