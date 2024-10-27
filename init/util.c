/*
 * Package: Reference Standard M
 * File:    rsm/init/util.c
 * Summary: module init - command line utilities
 *
 * David Wicksell <dlw@linux.com>
 * Copyright © 2021-2024 Fourth Watch Software LC
 * https://gitlab.com/Reference-Standard-M/rsm
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
 * SPDX-FileCopyrightText:  © 2021 David Wicksell <dlw@linux.com>
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#include <stdio.h>                                                              // always include
#include <stdlib.h>                                                             // these two
#include <sys/types.h>                                                          // for u_char def
#include <sys/shm.h>                                                            // shared memory
#include <sys/ipc.h>                                                            // for semaphores
#include <sys/sem.h>                                                            // for semaphores
#include <string.h>
#include <errno.h>                                                              // error stuff
#include <signal.h>

#if RSM_DBVER != 1
#   include <time.h>
#endif

#include <fcntl.h>                                                              // file stuff
#include <unistd.h>                                                             // for getopt
#include <math.h>                                                               // math prototypes
#include "rsm.h"                                                                // standard includes
#include "compile.h"                                                            // for rbd*
#include "database.h"                                                           // database protos
#include "proto.h"                                                              // function prototypes

// *** Give help if they entered -h or need help ***
void help(void)                                                                 // give some help
{
    char version[120];                                                          // a string

    rsm_version((u_char *) version);                                            // get version into version[]
    printf("%s\n", version);                                                    // print version string
    printf("Copyright © 2020-2024 Fourth Watch Software LC\n");
    printf("https://gitlab.com/Reference-Standard-M/rsm\n\n");
    printf("Show information:\n");
    printf("  rsm -V\t\t\tOutput short version string\n");
    printf("  rsm -h\t\t\tOutput help menu\n");
    printf("  rsm -i [<database-file>]\tOutput environment info\n\n");
    printf("Create database volume:\n");
    printf("  rsm -v <volume-name>\t\tName of volume (1-%d alpha characters)\n", VAR_LEN);
    printf("      -b <block-size>\t\tSize of database blocks (1-%u KiB)\n", MAX_BLOCK_SIZE);
    printf("      -s <database-size>\tInitial size of database (100-%u blocks)\n", MAX_DATABASE_BLKS);
    printf("     [-m <map-size>]\t\tSize of map block (0-%u KiB)\n", MAX_MAP_SIZE);
    printf("     [-e <environment-name>]\tName of manager UCI (1-%d alpha characters)\n", VAR_LEN);
    printf("     [<database-file>]\t\tName of database file\n\n");
    printf("Initialize and start environment:\n");
    printf("  rsm -j <max-jobs>\t\tSize of job table in environment (1-%d jobs)\n", MAX_JOBS);
    printf("     [-g <global-buffers>]\tSize of global buffers (1-%d MiB)\n", MAX_GLOBAL_BUFFERS);
    printf("     [-r <routine-buffers>]\tSize of routine buffers (1-%d MiB)\n", MAX_ROUTINE_BUFFERS);
    printf("     [<database-file>]\t\tName of database file\n\n");
    printf("Start job and attach to environment:\n");
    printf("  rsm \t\t\t\tStarts in direct mode in manager UCI\n");
    printf("     [-e <environment-name>]\tName of initial UCI environment\n");
    printf("     [-x <M-commands>]\t\tString of M commands to execute\n");
    printf("     [-R] [<database-file>]\tStarts in restricted mode\n\n");
    printf("Stop jobs and shut down environment:\n");
    printf("  rsm -k [<database-file>]\tKill environment\n\n");
    printf("Set environment variable RSM_DBFILE to <database-file> or pass it as shown above\n");
    exit(EXIT_SUCCESS);                                                         // give help and exit
}

// *** Give database and environment info if they entered -i ***
void info(char *file)                                                           // give some info
{
    int     i = 0;                                                              // an int
    u_int   cnt = 0;                                                            // current job count
    char    version[120];                                                       // a string
    char    pidlen;                                                             // calculate length of daemon PID
    char    margin = 24;                                                        // calculate margin for daemon PID list
    locktab *lock_free;                                                         // loop through lock free space
    int     lock_size = 0;                                                      // actual size of free lock space

    rsm_version((u_char *) version);                                            // get version into version[]
    printf("%s\n", version);                                                    // print version string
    printf("Copyright © 2020-2024 Fourth Watch Software LC\n");
    printf("https://gitlab.com/Reference-Standard-M/rsm\n");
    printf("Database Version: %d\tCompiler Version: %d\n\n", DB_VER, COMP_VER);
    printf("Database Volume and Environment Configuration Information:\n");

    if (file == NULL) {
        fprintf(stderr, "\nPlease pass database file path or set RSM_DBFILE.\n");
        exit(EXIT_FAILURE);
    }

    i = UTIL_Share(file);                                                       // attach to shared memory

    if ((i != 0) || (systab == NULL) || (systab->vol[0] == NULL)) {             // if that failed
        if (i != 0) {
            fprintf(stderr, "\nCannot connect to RSM environment - %s\n", strerror(errno));
            exit(i);
        }

        fprintf(stderr, "\nCannot connect to RSM environment.\n");
        exit(EXIT_FAILURE);
    }

    printf("Job Table Size:\t\t%-12uSlot%s\n", systab->maxjob, (systab->maxjob == 1) ? "" : "s");
    partab.job_table = SOA(systab->jobtab);

    for (u_int k = 0; k < systab->maxjob; k++) {
        if (partab.job_table[k].pid && !kill(partab.job_table[k].pid, 0)) cnt++; // count active jobs
    }

    printf("Current Job Count:\t%-12uJob%s\n", cnt, (cnt == 1) ? "" : "s");
    printf("Lock Table Size:\t%-12dKiB\n", systab->locksize / 1024);
    lock_free = SOA(systab->lockfree);

    while (lock_free != NULL) {
        lock_size += lock_free->size;
        lock_free = SOA(lock_free->fwd_link);
    }

    printf("Free Lock Space:\t%-12dByte%s\n", lock_size, (lock_size == 1) ? "" : "s");
    //printf("Free Additional Space:\t%-12lldKiB\n", systab->addsize / 1024);
    printf("Semaphore Array ID:\t%d\n", systab->sem_id);

    for (i = 0; i < MAX_VOL; i++) {
#if RSM_DBVER != 1
        time_t      time;
#endif
        const gbd   *p;                                                         // a pointer
        u_int       bcnt = 0;                                                   // count free global blocks
        rbd         *rtn_free;                                                  // loop through routine free space
        u_int       rtn_size = 0;                                               // actual size of free routine space
        label_block *vol_label;                                                 // current volume label

        if (systab->vol[i] == NULL) continue;
        partab.vol[i] = SOA(systab->vol[i]);
        if (partab.vol[i]->rbd_hash[RBD_HASH] == NULL) continue;
        vol_label = SOA(partab.vol[i]->vollab);
#if RSM_DBVER != 1
        time = vol_label->creation_time;
#endif
        printf("\n*** Volume %d ***\n", i + 1);
        printf("Volume File Path:\t%s\n", partab.vol[i]->file_name);
        printf("Volume Name:\t\t%s\n", vol_label->volnam.var_cu);
        printf("Manager UCI Name:\t%s\n", vol_label->uci[0].name.var_cu);
#if RSM_DBVER != 1
        printf("Volume Creation Time:\t%s\n", strtok(ctime(&time), "\n"));
#endif

        printf("Journal File Path:\t%s [%s]\n", ((vol_label->journal_file[0] != '\0') ? vol_label->journal_file : "--"),
               (vol_label->journal_available ? "ON" : "OFF"));

        printf("Label/Map Block Size:\t%-12uKiB\n", vol_label->header_bytes / 1024);
        printf("Volume Block Size:\t%-12uKiB\n", vol_label->block_size / 1024);
        printf("Volume Size:\t\t%-12uBlocks\n", vol_label->max_block);
        printf("Volume Free:\t\t%-12dBlocks\n", DB_Free(i + 1));
        if (vol_label->journal_file[0] != '\0') printf("Journal Size:\t\t%-12lldBytes\n", (long long) partab.vol[i]->jrn_next);

        printf("Global Buffers:\t\t%-12dMiB (%u Buffers)\n", (int) (((u_char *) SOA(partab.vol[i]->zero_block) -
               (u_char *) SOA(partab.vol[i]->global_buf)) / MBYTE), partab.vol[i]->num_gbd);

        p = SOA(partab.vol[i]->gbd_head);                                       // get listhead

        for (u_int j = 0; j < partab.vol[i]->num_gbd; j++) {                    // for all
            if (p[j].block) continue;                                           // skip used buffers
            bcnt++;
        }

        printf("Free Global Buffers:\t%-12uBuffer%s\n", bcnt, (bcnt == 1) ? "" : "s");

        printf("Routine Buffer Space:\t%-12uMiB\n", (u_int) (((u_char *) SOA(partab.vol[i]->rbd_end) -
               (u_char *) SOA(partab.vol[i]->rbd_head)) / MBYTE));

        rtn_free = (rbd *) SOA(partab.vol[i]->rbd_hash[RBD_HASH]);

        while (rtn_free != NULL) {
            rtn_size += rtn_free->chunk_size;
            rtn_free = SOA(rtn_free->fwd_link);
        }

        printf("Free Routine Space:\t%-12uByte%s\n", rtn_size, (rtn_size == 1) ? "" : "s");
        printf("Shared Memory ID:\t%d\n", partab.vol[i]->shm_id);
        printf("Daemon Process IDs:\t");

        for (int j = 0; j < MAX_DAEMONS; j++) {
            if (partab.vol[i]->wd_tab[j].pid == 0) break;
#ifdef _AIX
            pidlen = 7;                                                         // AIX doesn't always have libm
#else
            pidlen = floor(log10(partab.vol[i]->wd_tab[j].pid)) + 1;
#endif

            if ((margin + pidlen + 2) > 80) {
                printf("\n\t\t\t");
                margin = 24;
            } else if (margin != 24) {
                printf("  ");
                margin += 2;
            }

            printf("%d", partab.vol[i]->wd_tab[j].pid);
            margin += pidlen;
        }

        putchar('\n');
        margin = 24;
    }

    shmdt(systab);                                                              // detach the shared mem
    exit(EXIT_SUCCESS);                                                         // give info and exit
}

// *** Shut down database and environment if they entered -k ***
void shutdown(char *file)                                                       // give some info
{
    int             i = 0;                                                      // an int
    int             no_daemon = FALSE;                                          // for daemon info
    int             user;                                                       // for user number
    char            version[120];                                               // a string
    struct shmid_ds sbuf;                                                       // for shmctl (shutdown)
#ifdef __APPLE__
    void            *semvals = NULL;
#else
    semun_t         semvals = {.val = 0};                                       // dummy for semctl IPC_RMID
#endif

    rsm_version((u_char *) version);                                            // get version into version[]
    printf("%s\n", version);                                                    // print version string

    if (file == NULL) {
        fprintf(stderr, "Please pass database file path or set RSM_DBFILE\n");
        exit(EXIT_FAILURE);
    }

    i = UTIL_Share(file);                                                       // attach to shared memory

    if (i != 0) {                                                               // quit on error
        fprintf(stderr, "RSM environment is not initialized - %s\n", strerror(errno));
        exit(i);
    }

    partab.job_table = SOA(systab->jobtab);

    if (systab->vol[0] == NULL) {
        fprintf(stderr, "Error occurred in process - Environment does not match runtime image version\n");
        exit(EXIT_FAILURE);
    }

    user = (int) getuid();                                                      // get user number

    if ((user != systab->start_user) && (user != 0)) {                          // if this user did not start it and is not root
        fprintf(stderr, "User does not have permission to shut down RSM\n");
        exit(EXIT_FAILURE);
    }

    for (i = 0; i < MAX_VOL; i++) {
        label_block *vol_label;                                                 // current volume label

        if (systab->vol[i] == NULL) continue;
        partab.vol[i] = SOA(systab->vol[i]);
        vol_label = SOA(partab.vol[i]->vollab);

        if (vol_label->journal_available && vol_label->journal_requested) {     // if journaling
            // open file to add last entry and dismount
            partab.jnl_fds[i] = open(vol_label->journal_file, O_RDWR);

            if (partab.jnl_fds[i] == -1) {
                fprintf(stderr, "Failed to open journal file: %s\nerrno = %d\n", vol_label->journal_file, errno);
                vol_label->journal_available = 0;                               // turn off journaling
            }
        }
    }

    printf("Shutting down RSM environment at 0x%lx.\n", (u_long) systab->address);
    systab->start_user = -1;                                                    // Say 'shutting down'

    for (i = (MAX_VOL - 1); i >= 0; i--) {
        if (systab->vol[i] == NULL) continue;
        printf("Sending the daemons the signal to sync dirty queues.\n");
        partab.vol[i]->writelock = -(MAX_JOBS + 1);                             // write lock the database (system job)

        if (i == 0) {                                                           // only in volume 1
            no_daemon = TRUE;                                                   // assume no daemon for volume 1

            while (partab.vol[i]->writelock < 0) {
                sleep(1);

                if (!kill(partab.vol[i]->wd_tab[0].pid, 0)) {                   // if the main one exists
                    no_daemon = FALSE;
                    break;
                }

                if (no_daemon) break;                                           // if the daemon is gone, don't wait forever
            }
        }

        printf("Marking the shared memory segment for destruction [shmid: %d].\n", partab.vol[i]->shm_id);

        if (shmctl(partab.vol[i]->shm_id, IPC_RMID, &sbuf) == -1) {             // remove the shares
            fprintf(stderr, "errno = %d %s\n", errno, strerror(errno));
        }

        if (i == 0) {                                                           // only in volume 1
            printf("Sending the shutdown signal to all running RSM jobs.\n");

            for (u_int j = 0; j < systab->maxjob; j++) {                        // for each job
                int cnt;

                cnt = partab.job_table[j].pid;                                  // get PID

                if (cnt) {
                    if (kill(cnt, SIGTERM) == -1) {                             // kill this one
                        partab.job_table[j].trap = 1U << SIGTERM;               // or say go away
                        partab.job_table[j].attention = 1;                      // and look at it
                    }
                }
            }
        }

        printf("Stopping journaling and dismounting the database.\n");

        if (i == 0) {
            printf("Sending the signal to the daemons to remove the semaphore set [semid: %d].\n", systab->sem_id);
        }

        DB_Dismount(i + 1);                                                     // dismount the volume

        if (no_daemon) {
            printf("Removing the semaphore set more forcefully [semid: %d].\n", systab->sem_id);

            if (semctl(systab->sem_id, 0, IPC_RMID, semvals) == -1) {           // remove the semaphores
                fprintf(stderr, "errno = %d %s\n", errno, strerror(errno));
            }
        }
    }

    printf("RSM environment shut down.\n");                                     // success
    exit(EXIT_SUCCESS);                                                         // and exit
}
