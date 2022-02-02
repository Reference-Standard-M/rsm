/*
 * Package:  Reference Standard M
 * File:     rsm/init/util.c
 * Summary:  module RSM - command line utilities
 *
 * David Wicksell <dlw@linux.com>
 * Copyright © 2021-2022 Fourth Watch Software LC
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
#include <errno.h>                                                              // error stuff
#include <signal.h>
#include <fcntl.h>                                                              // file stuff
#include <unistd.h>                                                             // for getopt
#include "rsm.h"                                                                // standard includes
#include "proto.h"                                                              // function prototypes
#include "math.h"                                                               // math prototypes

// *** Give help if they entered -h or need help ***
void help(void)                                                                 // give some help
{
    char str[100];                                                              // a string
    (void) rsm_version((u_char *) str);                                         // get version into str[]
    printf("%s\n", str);                                                        // print version string
    printf("David Wicksell <dlw@linux.com>\n");
    printf("Copyright (c) 2020-2022 Fourth Watch Software LC\n");
    printf("https://gitlab.com/Reference-Standard-M/rsm\n\n");
    printf("Show information:\n");
    printf("  rsm -V\t\t\tOutput the short version string\n");
    printf("  rsm -h\t\t\tOutput the help menu\n");
    printf("  rsm -i [<database-file>]\tOutput the system info menu\n\n");
    printf("Create database:\n");
    printf("  rsm -v <volume-name>\t\tName of volume (1-%d alpha characters)\n", VAR_LEN);
    printf("      -b <block-size>\t\tSize of database blocks (4-256 KiB)\n");
    printf("      -s <database-size>\tInitial size of database (100-%u blocks)\n", MAX_DATABASE_BLKS);
    printf("     [-m <map-size>]\t\tSize of map block (0-%u KiB)\n", MAX_MAP_SIZE);
    printf("     [-e <environment-name>]\tName of manager UCI (1-%d alpha characters)\n", VAR_LEN);
    printf("     [<database-file>]\t\tName of the database file\n\n");
    printf("Initialize and start environment:\n");
    printf("  rsm -j <max-jobs>\t\tMaximum jobs allowed in environment (1-%d jobs)\n", MAX_JOBS);
    printf("     [-g <global-buffers>]\tSize of global buffers (1-%d MiB)\n", MAX_GLOBAL_BUFFERS);
    printf("     [-r <routine-buffers>]\tSize of routine buffers (1-%d MiB)\n", MAX_ROUTINE_BUFFERS);
    printf("     [<database-file>]\t\tName of the database file\n\n");
    printf("Start job and attach to environment:\n");
    printf("  rsm \t\t\t\tStarts in direct mode in manager UCI\n");
    printf("     [-e <environment-name>]\tName of initial UCI environment\n");
    printf("     [-x <M-command(s)>]\tString of M commands to execute\n");
    printf("     [<database-file>]\t\tName of the database file\n\n");
    printf("Stop and shut down environment:\n");
    printf("  rsm -k\t\t\tKill the environment\n");
    printf("     [<database-file>]\t\tName of the database file\n\n");
    printf("Set environment variable RSM_DBFILE=<database-file> or pass it to each command\n");
    exit(0);                                                                    // give help and exit
}

// *** Give database and environment info if they entered -i ***
void info(char *file)                                                           // give some info
{
    int  i = 0;                                                                 // an int
    int  dbfd = 0;                                                              // database file descriptor
    char str[100];                                                              // a string
    char pidlen;                                                                // calculate length of daemon PID
    char margin = 24;                                                           // calculate margin for daemon PID list

    (void) rsm_version((u_char *) str);                                         // get version into str[]
    printf("%s\n", str);                                                        // print version string
    printf("Database Version: %d\tCompiler Version: %d\n\n", DB_VER, COMP_VER);
    printf("David Wicksell <dlw@linux.com>\n");
    printf("Copyright (c) 2020-2022 Fourth Watch Software LC\n");
    printf("https://gitlab.com/Reference-Standard-M/rsm\n\n");
    printf("Database and Environment Configuration Information:\n\n");

    if (file == NULL) {
        fprintf(stderr, "Please pass database file path or set RSM_DBFILE.\n");
        exit(1);
    }

    dbfd = open(file, O_RDONLY);                                                // open the database for read

    if (dbfd < 0) {                                                             // if that failed
        fprintf(stderr, "Cannot open database file: %s\n", file);
        exit(1);
    }

    i = UTIL_Share(file);                                                       // attach to shared mem

    if (i != 0) {                                                               // if that failed
        fprintf(stderr, "Cannot connect to environment.\n");
        exit(1);
    }

    if (systab->vol[0] == NULL) {
        fprintf(stderr, "Cannot connect to environment.\n");
        exit(1);
    }

    printf("DB File Path:\t\t%s\n", file);
    printf("DB Volume Name:\t\t%s \n", systab->vol[0]->vollab->volnam.var_cu);
    printf("DB Manager UCI Name:\t%s \n", systab->vol[0]->vollab->uci[0].name.var_cu);

    printf("DB Journal File Path:\t%s [%s]\n",
           ((systab->vol[0]->vollab->journal_file[0] != '\0') ? systab->vol[0]->vollab->journal_file : "--"),
           (systab->vol[0]->vollab->journal_available ? "ON" : "OFF"));

    printf("DB Size in Blocks:\t%u\n", systab->vol[0]->vollab->max_block);
    printf("DB Map Block Size:\t%u\tKiB\n", systab->vol[0]->vollab->header_bytes / 1024);
    printf("DB Block Size:\t\t%u\tKiB\n", systab->vol[0]->vollab->block_size / 1024);

    printf("Global Buffers:\t\t%d\tMiB (%u Buffers)\n",
           (int) ((systab->vol[0]->zero_block - systab->vol[0]->global_buf) / 1048576),
           systab->vol[0]->num_gbd);

    printf("Routine Buffer Space:\t%d\tMiB\n", (int) ((systab->vol[0]->rbd_end - systab->vol[0]->rbd_head) / 1048576));
    printf("Lock Table Size:\t%d\tKiB\n", systab->locksize / 1024);
    printf("Job Table Slots:\t%u\tJob%s\n", systab->maxjob, ((systab->maxjob > 1) ? "s" : ""));
    printf("Daemon Process IDs:\t");

    for (i = 0; i < MAX_DAEMONS; i++) {
        if (systab->vol[0]->wd_tab[i].pid == 0) break;
#ifdef _AIX
        pidlen = 10;                                                            // AIX doesn't always have libm (PID_MAX is 10)
#else
        pidlen = floor(log10(systab->vol[0]->wd_tab[i].pid)) + 1;
#endif

        if ((margin + pidlen) > 80) {
            margin = pidlen + 25;
            printf("\n\t\t\t");
        } else {
            margin += (pidlen + 1);
        }

        printf("%d ", systab->vol[0]->wd_tab[i].pid);
    }

    putchar('\n');
    i = shmdt(systab);                                                          // detach the shared mem
    i = close(dbfd);                                                            // close the database
    exit(0);                                                                    // give info and exit
}

// *** Shut down database and environment if they entered -k ***
void shutdown(char *file)                                                       // give some info
{
    int             i = 0;                                                      // an int
    int             cnt;                                                        // count of bytes used
    int             user;                                                       // for user number
    int             pid;                                                        // for PID number
    int             dbfd = 0;                                                   // database file descriptor
    struct shmid_ds sbuf;                                                       // for shmctl (shutdown)

    if (file == NULL) {
        fprintf(stderr, "Please pass database file path or set RSM_DBFILE.\n");
        exit(1);
    }

    dbfd = open(file, O_RDONLY);                                                // open the database for read

    if (dbfd < 0) {                                                             // if that failed
        fprintf(stderr, "Cannot open database file: %s\n", file);
        exit(1);
    }

    i = UTIL_Share(file);                                                       // attach to shared mem

    if (i != 0) {                                                               // quit on error
        fprintf(stderr, "RSM environment is not initialized.\n");
        exit(1);
    }

    if (systab->vol[0] == NULL) {
        fprintf(stderr, "Error occurred in process - Environment does not match runtime image version\n");
        exit(1);
    }

    if (systab->vol[0]->vollab->journal_available && systab->vol[0]->vollab->journal_requested) { // if journaling
        partab.jnl_fds[0] = open(systab->vol[0]->vollab->journal_file, O_RDWR);

        if (partab.jnl_fds[0] < 0) {
            fprintf(stderr, "Failed to open journal file %s\nerrno = %d\n", systab->vol[0]->vollab->journal_file, errno);
            exit(1);
        }
    }

    user = (int) getuid();                                                      // get user number

    if ((user != systab->start_user) && (user != 0)) {                          // if this user started it or this user is root
        fprintf(stderr, "User does not have permission to shut down RSM.\n");
        exit(1);
    }

    systab->vol[i]->writelock = -(MAX_JOBS + 1);                                // write lock the database (system job)
    while (systab->vol[i]->writelock < 0) sleep(1);                             // wait for writelock to finish queue flush
    systab->start_user = -1;                                                    // Say 'shutting down'
    i = shmctl(systab->vol[0]->shm_id, IPC_RMID, &sbuf);                        // remove the share
    pid = (int) getpid();                                                       // get current PID

    for (i = 0; i < systab->maxjob; i++) {                                      // for each job
        cnt = systab->jobtab[i].pid;                                            // get PID

        if ((cnt != pid) && cnt) {
            if (!kill(cnt, SIGTERM)) {                                          // kill this one
                systab->jobtab[i].trap = 1U << SIGTERM;                         // say go away
                systab->jobtab[i].attention = 1;                                // look at it
            }
        }
    }

    DB_Dismount(1);                                                             // dismount main vol
    printf("RSM environment shut down.\n");                                     // success
    exit(0);                                                                    // and exit
}
