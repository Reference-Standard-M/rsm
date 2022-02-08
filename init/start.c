/*
 * Package:  Reference Standard M
 * File:     rsm/init/start.c
 * Summary:  module RSM - startup code
 *
 * David Wicksell <dlw@linux.com>
 * Copyright © 2020-2022 Fourth Watch Software LC
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
#include <strings.h>
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

#ifdef __CYGWIN__
#    define SHM_R 0400
#    define SHM_W 0200
#endif                                                                          // Cygwin

/*********************************************************\
* Initialize an environment - switches are:               *
*      database file name           (1 to VAR_LEN)    Req *
*   -j max number of jobs           (1 to 512)        Req *
*   -g global buffers in MiB        (0 to 131072)     Opt *
*   -r routine buffers in MiB       (0 to 4095)       Opt *
*   -a additional buffers in MiB    (1 to VAR_LEN)    Opt *
\*********************************************************/
int INIT_Start(char  *file,                                                     // database
               u_int jobs,                                                      // number of jobs
               u_int gmb,                                                       // MiB of global buffers
               u_int rmb,                                                       // MiB of routine buffers
               u_int addmb)                                                     // MiB of additional buffers (for volumes)
{
    int         dbfd;                                                           // database file descriptor
    int         hbuf[sizeof(label_block) / 4];                                  // header buffer
    int         i;                                                              // useful int
    u_int       n_gbd;                                                          // number of GBD
    long long   addoff;                                                         // offset for add buff
    int         indx;                                                           // loop control
    key_t       shar_mem_key;                                                   // memory "key"
    int         shar_mem_id;                                                    // memory id
    int         sem_id;                                                         // semaphore id
    u_short     sem_val[SEM_MAX];                                               // to init them
    semun_t     semvals;                                                        // for a ptr to sem_val
    long long   share_size;                                                     // size of share (bytes)
    int         locksize;                                                       // size of locktab
    long        sjlt_size;                                                      // size of systab + jobtab + locktab
    long long   volset_size;                                                    // size of volset struct (bytes)
    int         pagesize;                                                       // system pagesize (bytes)
    struct      shmid_ds sbuf;                                                  // for shmctl
    char        fullpathvol[MAXPATHLEN];                                        // full pathname of vol file
    gbd         *gptr;                                                          // a GBD pointer
    u_char      *ptr;                                                           // and a byte one
    label_block *labelblock;                                                    // label block pointer

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

    pagesize = getpagesize();                                                   // get system pagesize (bytes)
    if (gmb == 0) gmb = jobs / 2;                                               // default global buffers
    if (gmb < 1) gmb = 1;                                                       // but at least 1 MiB
    if (rmb == 0) rmb = jobs / 8;                                               // pick a default
    if (rmb < 1) rmb = 1;                                                       // but at least 1 MiB
    locksize = jobs * LOCKTAB_SIZE;                                             // what we need for locktab
    locksize = (((locksize - 1) / pagesize) + 1) * pagesize;                    // round up
    dbfd = open(file, O_RDWR);                                                  // open the database read/write

    if (dbfd < 1) {                                                             // if that failed
        fprintf(stderr, "Open of database %s failed - %s\n", file, strerror(errno)); // what was returned
        return errno;                                                           // exit with error
    }                                                                           // end file create test

    i = read(dbfd, hbuf, sizeof(label_block));                                  // read label block

    if (i < sizeof(label_block)) {                                              // in case of error
        fprintf(stderr, "Read of label block failed - %s\n", strerror(errno));  // what was returned
        return errno;                                                           // exit with error
    }

    labelblock = (label_block *) hbuf;                                          // point label block at it

    if (labelblock->db_ver != DB_VER) {                                         // if we need to upgrade
        fprintf(stderr, "Database is version %u, image requires version %d - start failed!!\n", labelblock->db_ver, DB_VER);

        if (labelblock->db_ver == 1 && DB_VER == 2) {
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

    n_gbd = ((long) gmb * MBYTE) / hbuf[3];                                     // number of GBD

    while (n_gbd < MIN_GBD) {                                                   // if not enough
        gmb++;                                                                  // increase it
        n_gbd = ((long) gmb * MBYTE) / hbuf[3];                                 // number of GBD
    }

    printf("Creating share for %u job%s with %u MiB routine space,\n", jobs, ((jobs > 1) ? "s" : ""), rmb);
    printf("%u MiB (%u) global buffers, %d KiB label/map space,\n", gmb, n_gbd, hbuf[2] / 1024);
    printf("and %d KiB for the lock table.\n", locksize / 1024);
    if (addmb > 0) printf("With %u MiB of additional buffers.\n", addmb);
    for (i = 0; i < SEM_MAX; sem_val[i++] = jobs) continue;                     // setup for sem init
    semvals.array = sem_val;

    sjlt_size = sizeof(systab_struct)                                           // size of Systab
              + (sizeof(u_int) * (jobs - 1))                                    // adj for last_blk_used[1]
              + (sizeof(jobtab) * jobs)                                         // size of JOBTAB
              + locksize;                                                       // size of LOCKTAB

    sjlt_size = (((sjlt_size - 1) / pagesize) + 1) * pagesize;                  // round up

    volset_size = sizeof(vol_def)                                               // size of VOL_DEF (one for now)
                + hbuf[2]                                                       // size of head and map block
                + (n_gbd * sizeof(gbd))                                         // the GBD
                + ((long) gmb * MBYTE)                                          // MiB of global buffers
                + hbuf[3]                                                       // size of block (zero block)
                + ((long) rmb * MBYTE);                                         // MiB of routine buffers

    volset_size = (((volset_size - 1) / pagesize) + 1) * pagesize;              // round up
    share_size = sjlt_size + volset_size;                                       // shared memory size
    addoff = share_size;                                                        // where add buff starts
    share_size = share_size + ((long) addmb * MBYTE);                           // and the additional
    shar_mem_id = shmget(shar_mem_key, share_size, (SHM_R | SHM_W | (SHM_R >> 3) | (SHM_W >> 3) | IPC_CREAT)); // create share mem

    if (shar_mem_id == -1) {                                                    // die on error
        fprintf(stderr, "Unable to create shared memory section - %s\n", strerror(errno)); // give an error

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

    sem_id = semget(shar_mem_key, SEM_MAX,                                      // create semaphores
                    (SHM_R | SHM_W | (SHM_R >> 3) | (SHM_W >> 3) | IPC_CREAT)); // Use SMH_ not SEM_ as Linux needs that

    if (sem_id < 0) {
        fprintf(stderr, "Unable to create semaphore set - %s\n", strerror(errno)); // give an error
        i = shmctl(shar_mem_id, IPC_RMID, &sbuf);                               // remove the share
        return errno;                                                           // and return with error
    }

    i = semctl(sem_id, 0, SETALL, semvals);                                     // clear the values

    if (i < 0) {                                                                // check for error
        fprintf(stderr, "Unable to clear semaphores - %s\n", strerror(errno));  // give an error
        i = shmctl(shar_mem_id, IPC_RMID, &sbuf);                               // remove the share
        i = semctl(sem_id, 0, IPC_RMID, NULL);                                  // and the semaphores
        return errno;                                                           // and return with error
    }

    systab = shmat(shar_mem_id, SHMAT_SEED, 0);                                 // map it

    if (systab == (void *) -1) {                                                // die on error
        fprintf(stderr, "Unable to attach to systab correctly\n");              // give error
        fprintf(stderr, "error may be: %s\n", strerror(errno));                 // give error
        i = shmctl(shar_mem_id, IPC_RMID, &sbuf);                               // remove the share
        i = semctl(sem_id, 0, IPC_RMID, NULL);                                  // and the semaphores
        return errno;                                                           // exit with error
    }

    printf("Systab attached at %lx\n", (u_long) systab);
    bzero(systab, share_size);                                                  // zot the lot
    systab->address = systab;                                                   // store the address for ron
    systab->jobtab = (jobtab *) &systab->last_blk_used[jobs + 1];               // setup jobtab pointer
    systab->maxjob = jobs;                                                      // save max jobs
    systab->start_user = (int) getuid();                                        // remember who started this
    systab->precision = DEFAULT_PREC;                                           // decimal precision
    systab->lockstart = (void *) ((char *) systab->jobtab + (sizeof(jobtab) * jobs)); //locktab
    systab->locksize = locksize;                                                // the size
    systab->lockhead = NULL;                                                    // no locks currently
    systab->lockfree = (locktab *) systab->lockstart;                           // free space
    systab->lockfree->fwd_link = NULL;                                          // only one
    systab->lockfree->size = locksize;                                          // the whole space
    systab->lockfree->job = -1;                                                 // means free
    systab->addoff = addoff;                                                    // Add buffer offset
    systab->addsize = ((long) addmb * MBYTE);                                   // and size in bytes
    systab->vol[0] = (vol_def *) ((char *) systab + sjlt_size);                 // jump to start of volume set memory
    systab->vol[0]->vollab = (label_block *) ((char *) systab->vol[0] + sizeof(vol_def)); // and point to label blk
    systab->vol[0]->map = (void *) ((char *) systab->vol[0]->vollab + sizeof(label_block)); // and point to map
    systab->vol[0]->first_free = systab->vol[0]->map;                           // init first free
    systab->vol[0]->gbd_head = (gbd *) ((char *) systab->vol[0]->vollab + hbuf[2]); // GBDs
    systab->vol[0]->num_gbd = n_gbd;                                            // number of GBDs
    systab->vol[0]->global_buf = (void *) &systab->vol[0]->gbd_head[n_gbd];     // global buffers
    systab->vol[0]->zero_block = (void *) &(((u_char *) systab->vol[0]->global_buf)[(long) gmb * MBYTE]); // pointer to zero blk
    systab->vol[0]->rbd_head = (void *) ((char *) systab->vol[0]->zero_block + hbuf[3]); // RBDs
    systab->vol[0]->rbd_end = (void *) ((char *) systab + share_size - systab->addsize); // end of share
    systab->vol[0]->shm_id = shar_mem_id;                                       // set up share id
    systab->sem_id = sem_id;                                                    // set up semaphore id
    systab->vol[0]->map_dirty_flag = 0;                                         // clear dirty map flag

    if ((realpath(file, fullpathvol))) {                                        // get full path
        if (strlen(fullpathvol) < VOL_FILENAME_MAX) {                           // if can fit in our struct
            strcpy(systab->vol[0]->file_name, fullpathvol);                     // copy path into the vol_def structure
        } else {                                                                // end if path will fit, otherwise
            i = VOL_FILENAME_MAX - strlen(fullpathvol);                         // copy as much as
            strcpy(systab->vol[0]->file_name, &fullpathvol[i]);                 // is possible, that's the best we can do
        }                                                                       // end length testing
    } else {                                                                    // end realpath worked, otherwise it was an error
        i = shmdt(systab);                                                      // detach the shared mem
        i = shmctl(shar_mem_id, IPC_RMID, &sbuf);                               // remove the share
        i = semctl(sem_id, 0, IPC_RMID, NULL);                                  // and the semaphores
        return errno;                                                           // exit with error
    }

    i = shmctl(shar_mem_id, IPC_STAT, &sbuf);                                   // get status for later
    i = lseek(dbfd, 0, SEEK_SET);                                               // re-point at start of file
    i = read(dbfd, systab->vol[0]->vollab, hbuf[2]);                            // read label & map block

    if (i < hbuf[2]) {                                                          // in case of error
        fprintf(stderr, "Read of label/map block failed - %s\n", strerror(errno)); // what was returned
        i = shmdt(systab);                                                      // detach the shared mem
        i = shmctl(shar_mem_id, IPC_RMID, &sbuf);                               // remove the share
        i = semctl(sem_id, 0, IPC_RMID, NULL);                                  // and the semaphores
        return errno;                                                           // exit with error
    }

    if (systab->vol[0]->vollab->clean == 0) {                                   // if not a clean dismount
        fprintf(stderr, "WARNING: Volume was not dismounted properly!\n");
        systab->vol[0]->upto = 1;                                               // mark for cleaning
    } else {
        systab->vol[0]->vollab->clean = 1;                                      // mark as mounted
        systab->vol[0]->map_dirty_flag = 1;                                     // and map needs writing
    }

    jobs = jobs / DAEMONS;                                                      // number of daemons
    if (jobs < MIN_DAEMONS) jobs = MIN_DAEMONS;                                 // minimum of MIN_DAEMONS
    if (jobs > MAX_DAEMONS) jobs = MAX_DAEMONS;                                 // and the max
    systab->vol[0]->num_of_daemons = jobs;                                      // initialize this
    while (SemOp(SEM_WD, WRITE)) continue;                                      // lock WD

    partab.vol_fds[0] = dbfd;                                                   // so the daemons can close inherited FD

    for (indx = 0; indx < jobs; indx++) {                                       // for each required daemon
        i = DB_Daemon(indx, 1);                                                 // start each daemon (volume 1)

        if (i != 0) {                                                           // in case of error
            fprintf(stderr, "*** Died on error - %s ***\n\n", strerror(i));     // what was returned
            i = shmdt(systab);                                                  // detach the shared mem
            return errno;                                                       // exit with error
        }
    }                                                                           // all daemons started

    if (systab->maxjob == 1) {                                                  // if in single user mode
        printf("WARNING: Single user, journaling not started.\n");
    } else if (systab->vol[0]->vollab->journal_requested && systab->vol[0]->vollab->journal_file[0]) {
        struct stat sb;                                                         // File attributes
        off_t       jptr;                                                       // file ptr
        jrnrec      jj;                                                         // to write with
        int         jfd;                                                        // file descriptor

        while (SemOp(SEM_GLOBAL, WRITE)) continue;                              // lock GLOBAL
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
                            systab->vol[0]->jrn_next += jj.size;                // adjust pointer
                            lseek(jfd, 4, SEEK_SET);
                            i = write(jfd, &systab->vol[0]->jrn_next, sizeof(off_t));
                            i = close(jfd);                                     // and close it
                            systab->vol[0]->vollab->journal_available = 1;
                            printf("Journaling started to %s\n", systab->vol[0]->vollab->journal_file); // say it worked
                        }
                    }
                }
            }
        }

        SemOp(SEM_GLOBAL, -WRITE);                                              // unlock global
    }                                                                           // end journal stuff

    SemOp(SEM_WD, -WRITE);                                                      // release WD lock
    gptr = systab->vol[0]->gbd_head;                                            // get start of GBDs
    ptr = (u_char *) systab->vol[0]->global_buf;                                // get start of global buffers

    for (i = 0; i < systab->vol[0]->num_gbd; i++) {                             // for each GBD
        gptr[i].mem = (DB_Block *) ptr;                                         // point at block
        ptr += systab->vol[0]->vollab->block_size;                              // point at next

        if (i < (systab->vol[0]->num_gbd - 1)) {                                // all but the last
            gptr[i].next = &gptr[i + 1];                                        // link to next
        } else {                                                                // the last
            gptr[i].next = NULL;                                                // end of list
        }
    }                                                                           // end setup GBDs

    systab->vol[0]->gbd_hash[GBD_HASH] = gptr;                                  // head of free list
    Routine_Init();                                                             // and the routine junk
    i = shmdt(systab);                                                          // detach the shared mem
    i = close(dbfd);                                                            // close the database
    printf("RSM environment initialized.\n");                                   // say something
    return 0;                                                                   // indicate success
}
