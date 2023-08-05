/*
 * Package:  Reference Standard M
 * File:     rsm/init/util.c
 * Summary:  module init - command line utilities
 *
 * David Wicksell <dlw@linux.com>
 * Copyright © 2021-2023 Fourth Watch Software LC
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
 * along with this program. If not, see http://www.gnu.org/licenses/.
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
#include <time.h>
#include <fcntl.h>                                                              // file stuff
#include <unistd.h>                                                             // for getopt
#include <math.h>                                                               // math prototypes
#include "rsm.h"                                                                // standard includes
#include "compile.h"                                                            // for rbd*
#include "proto.h"                                                              // function prototypes

// *** Give help if they entered -h or need help ***
void help(void)                                                                 // give some help
{
    char version[120];                                                          // a string

    rsm_version((u_char *) version);                                            // get version into version[]
    printf("%s\n", version);                                                    // print version string
    printf("Copyright (c) 2020-2023 Fourth Watch Software LC\n");
    printf("https://gitlab.com/Reference-Standard-M/rsm\n\n");
    printf("Show information:\n");
    printf("  rsm -V\t\t\tOutput short version string\n");
    printf("  rsm -h\t\t\tOutput help menu\n");
    printf("  rsm -i [<database-file>]\tOutput environment info\n\n");
    printf("Create database:\n");
    printf("  rsm -v <volume-name>\t\tName of volume (1-%d alpha characters)\n", VAR_LEN);
    printf("      -b <block-size>\t\tSize of database blocks (1-256 KiB)\n");
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
    printf("     [-R]\t\t\tStarts in restricted mode\n");
    printf("     [<database-file>]\t\tName of database file\n\n");
    printf("Stop and shut down environment:\n");
    printf("  rsm -k [<database-file>]\tKill environment\n\n");
    printf("Set environment variable RSM_DBFILE=<database-file> or pass it to each command\n");
    exit(EXIT_SUCCESS);                                                         // give help and exit
}

// *** Give database and environment info if they entered -i ***
void info(char *file)                                                           // give some info
{
    int   i = 0;                                                                // an int
    int   j = 0;                                                                // another int
    u_int cnt = 0;                                                              // current job count
    char  pidlen;                                                               // calculate length of daemon PID
    char  margin = 24;                                                          // calculate margin for daemon PID list
    char  version[120];                                                         // a string

    rsm_version((u_char *) version);                                            // get version into version[]
    printf("%s\n", version);                                                    // print version string
    printf("Copyright (c) 2020-2023 Fourth Watch Software LC\n");
    printf("https://gitlab.com/Reference-Standard-M/rsm\n");
    printf("Database Version: %d\tCompiler Version: %d\n\n", DB_VER, COMP_VER);
    printf("Database Volume and Environment Configuration Information:\n\n");

    if (file == NULL) {
        fprintf(stderr, "Please pass database file path or set RSM_DBFILE.\n");
        exit(EXIT_FAILURE);
    }

    i = UTIL_Share(file);                                                       // attach to shared memory

    if ((i != 0) || (systab == NULL) || (systab->vol[0] == NULL)) {             // if that failed
        if (i != 0) {
            fprintf(stderr, "Cannot connect to RSM environment - %s\n", strerror(errno));
            exit(i);
        }

        fprintf(stderr, "Cannot connect to RSM environment.\n");
        exit(EXIT_FAILURE);
    }

    printf("Job Table Size:\t\t%-12uSlot%s\n", systab->maxjob, (systab->maxjob == 1) ? "" : "s");

    for (u_int k = 0; k < systab->maxjob; k++) {
        if (systab->jobtab[k].pid) cnt++;                                       // count active jobs
    }

    printf("Current Job Count:\t%-12uJob%s\n", cnt, (cnt == 1) ? "" : "s");
    printf("Lock Table Size:\t%-12dKiB\n", systab->locksize / 1024);
    printf("Free Lock Space:\t%-12dBytes\n", (systab->lockfree == NULL) ? 0 : systab->lockfree->size);
    printf("Semaphore Array ID:\t%d\n", systab->sem_id);

    for (i = 0; i < MAX_VOL; i++) {
        time_t time;

        if (systab->vol[i] == NULL) continue;
        time = systab->vol[i]->vollab->creation_time;
        printf("\n*** Volume %d ***\n", i + 1);
        printf("DB File Path:\t\t%s\n", systab->vol[i]->file_name);
        printf("DB Volume Name:\t\t%s \n", systab->vol[i]->vollab->volnam.var_cu);
        printf("DB Manager UCI Name:\t%s \n", systab->vol[i]->vollab->uci[0].name.var_cu);
        printf("DB Creation Time:\t%s \n", strtok(asctime(gmtime(&time)), "\n"));

        printf("DB Journal File Path:\t%s [%s]\n",
               ((systab->vol[i]->vollab->journal_file[0] != '\0') ? systab->vol[i]->vollab->journal_file : "--"),
               (systab->vol[i]->vollab->journal_available ? "ON" : "OFF"));

        printf("DB HDR/Map Block Size:\t%-12uKiB\n", systab->vol[i]->vollab->header_bytes / 1024);
        printf("DB Block Size:\t\t%-12uKiB\n", systab->vol[i]->vollab->block_size / 1024);
        printf("DB Volume Size:\t\t%-12uBlocks\n", systab->vol[i]->vollab->max_block);
        printf("DB Volume Free:\t\t%-12dBlocks\n", DB_Free(i + 1));

        printf("Global Buffers:\t\t%-12dMiB (%u Buffers)\n",
               (int) ((systab->vol[i]->zero_block - systab->vol[i]->global_buf) / MBYTE), systab->vol[i]->num_gbd);

        printf("Routine Buffer Space:\t%-12uMiB\n", (u_int) ((systab->vol[i]->rbd_end - systab->vol[i]->rbd_head) / MBYTE));
        printf("Free Routine Space:\t%-12uBytes\n", ((rbd *) systab->vol[i]->rbd_hash[RBD_HASH])->chunk_size);
        printf("Shared Memory ID:\t%d\n", systab->vol[i]->shm_id);
        printf("Daemon Process IDs:\t");

        for (j = 0; j < MAX_DAEMONS; j++) {
            if (systab->vol[i]->wd_tab[j].pid == 0) break;
#ifdef _AIX
            pidlen = 10;                                                        // AIX doesn't always have libm (PID_MAX is 10)
#else
            pidlen = floor(log10(systab->vol[i]->wd_tab[j].pid)) + 1;
#endif

            if ((margin + pidlen) > 80) {
                margin = pidlen + 26;
                printf("\n\t\t\t");
            } else {
                margin += (pidlen + 2);
            }

            printf("%d  ", systab->vol[i]->wd_tab[j].pid);
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
    int             j = 0;                                                      // another int
    int             no_daemons;                                                 // for daemon info
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
        if (systab->vol[i] == NULL) continue;

        if (systab->vol[i]->vollab->journal_available && systab->vol[i]->vollab->journal_requested) { // if journaling
            partab.jnl_fds[i] = open(systab->vol[i]->vollab->journal_file, O_RDWR); // open file to add last entry and dismount

            if (partab.jnl_fds[i] == -1) {
                fprintf(stderr, "Failed to open journal file: %s\nerrno = %d\n", systab->vol[i]->vollab->journal_file, errno);
                systab->vol[i]->vollab->journal_available = 0;                  // turn off journaling
            }
        }
    }

    printf("Shutting down RSM environment at 0x%lx.\n", (u_long) systab);
    systab->start_user = -1;                                                    // Say 'shutting down'
    printf("Sending the daemons the signal to sync dirty queues.\n");

    for (i = (MAX_VOL - 1); i >= 0; i--) {
        no_daemons = TRUE;                                                      // assume no daemons
        if (systab->vol[i] == NULL) continue;
        systab->vol[i]->writelock = -(MAX_JOBS + 1);                            // write lock the database (system job)

        while (systab->vol[i]->writelock < 0) {
            sleep(1);

            for (j = 0; j < systab->vol[i]->num_of_daemons; j++) {              // each one
                if (!kill(systab->vol[i]->wd_tab[j].pid, 0)) {                  // if one exists
                    no_daemons = FALSE;
                    break;
                }
            }

            if (no_daemons) break;                                              // if all the daemons have gone, don't wait forever
        }

        systab->vol[i]->writelock = MAX_JOBS + 1;                               // release system write lock on database
        printf("Marking the shared memory segment for destruction [shmid: %d].\n", systab->vol[i]->shm_id);

        if (shmctl(systab->vol[i]->shm_id, IPC_RMID, &sbuf) == -1) {            // remove the share
            fprintf(stderr, "errno = %d %s\n", errno, strerror(errno));
        }
    }

    printf("Sending the shutdown signal to all running RSM jobs.\n");

    for (u_int k = 0; k < systab->maxjob; k++) {                                // for each job
        int cnt = systab->jobtab[k].pid;                                        // get PID

        if (cnt) {
            if (kill(cnt, SIGTERM) == -1) {                                     // kill this one
                systab->jobtab[k].trap = 1U << SIGTERM;                         // or say go away
                systab->jobtab[k].attention = 1;                                // and look at it
            }
        }
    }

    printf("Turning off journaling and dismounting database volumes.\n");
    printf("Sending the signal to the daemons to remove the semaphore set [semid: %d].\n", systab->sem_id);

    for (i = (MAX_VOL - 1); i >= 0; i--) {
        if (systab->vol[i] == NULL) continue;
        DB_Dismount(i + 1);                                                     // dismount all volumes

        if ((i == 0) && no_daemons) {
            printf("Removing the semaphore set more forcefully [semid: %d].\n", systab->sem_id);

            if (semctl(systab->sem_id, 0, IPC_RMID, semvals) == -1) {           // remove the semaphores
                fprintf(stderr, "errno = %d %s\n", errno, strerror(errno));
            }
        }
    }

    printf("RSM environment shut down.\n");                                     // success
    exit(EXIT_SUCCESS);                                                         // and exit
}
