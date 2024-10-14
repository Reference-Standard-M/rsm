/*
 * Package: Reference Standard M
 * File:    rsm/init/rsm.c
 * Summary: module init - startup (main) code
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
#include <sys/shm.h>                                                            // shared memory
#include <sys/types.h>                                                          // for u_char def
#include <string.h>
#include <ctype.h>
#include <fcntl.h>                                                              // file stuff
#include <unistd.h>                                                             // for getopt
#include <errno.h>                                                              // error stuff
#include "rsm.h"                                                                // standard includes
#include "proto.h"                                                              // function prototypes
#include "init.h"                                                               // init prototypes

int restricted = FALSE;                                                         // whether RSM is in restricted mode or not

// *** Main entry for create, init, run, help, info, and shutdown ***
int main(int argc, char **argv)                                                 // main entry point
{
    int        c;                                                               // for case
    int        bsize = 0;                                                       // block size
    char       i = FALSE;                                                       // for info()
    char       k = FALSE;                                                       // for shutdown()
    char       *env = NULL;                                                     // start environment name
    int        jobs = 0;                                                        // max jobs
    int        map = 0;                                                         // header/map block bytes
    u_int      gmb = 0;                                                         // global buf MiB
    u_int      rmb = 0;                                                         // routine buf MiB
    u_int      addmb = 0;                                                       // additional buffer in MiB
    int        blocks = 0;                                                      // number of data blocks
    char       *volnam = NULL;                                                  // volume name
    char       *cmd = NULL;                                                     // startup command
    const char *dbfile;                                                         // pass volume in environment
    char       file[VOL_FILENAME_MAX];
    char       version[40];                                                     // for the version string (-V)

    dbfile = getenv("RSM_DBFILE");
    if ((argc < 2) && (dbfile == NULL)) help();                                 // they need help

    while ((c = getopt(argc, argv, "a:b:e:g:hij:km:r:s:v:x:RV")) != EOF) {
        switch (c) {
        case 'a':                                                               // switch -a
            addmb = atoi(optarg);                                               // additional buffer (init - not ready yet)
            break;

        case 'b':                                                               // switch -b
            bsize = atoi(optarg);                                               // database block size (create)
            break;

        case 'e':                                                               // switch -e
            env = optarg;                                                       // environment name (init or run)
            break;

        case 'g':                                                               // switch -g
            gmb = atoi(optarg);                                                 // global buffer MiB (init)
            break;

        case 'h':                                                               // switch -h
            if (!i && !k) help();                                               // exit via help()
            break;

        case 'i':                                                               // switch -i
            i = TRUE;                                                           // call info() below
            break;

        case 'j':                                                               // switch -j
            jobs = atoi(optarg);                                                // max number of jobs (init)
            break;

        case 'k':                                                               // switch -k
            k = TRUE;                                                           // call shutdown() below
            break;

        case 'm':                                                               // switch -m
            map = atoi(optarg);                                                 // size of map block (create)
            break;

        case 'r':                                                               // switch -r
            rmb = atoi(optarg);                                                 // routine buffer MiB (init)
            break;

        case 's':                                                               // switch -s
            blocks = atoi(optarg);                                              // number of data blocks (create)
            break;

        case 'v':                                                               // switch -v
            volnam = optarg;                                                    // volume name (create)
            break;

        case 'x':                                                               // switch -x
            if (cmd == NULL) cmd = optarg;                                      // initial command (run)
            break;

        case 'R':
            restricted = TRUE;                                                  // turn on restricted mode
            break;

        case 'V':                                                               // switch -V
            if (i || k) break;
            short_version((u_char *) version, sprintf((char *) &version[0], "V")); // get version string
            printf("%s\n", version);                                            // print version string
            exit(EXIT_SUCCESS);                                                 // give version and exit
            break;

        default:                                                                // some sort of error
            putchar('\n');
            help();                                                             // just give help
            break;
        }
    }

    argc -= optind;                                                             // adjust for used args
    argv += optind;                                                             // should point at parameter

    if (argc == 1) {
        strcpy(file, *argv);
    } else if (dbfile != NULL) {
        strcpy(file, dbfile);
    } else {
        if (i) info(NULL);                                                      // exit via info()
        if (k) shutdown(NULL);                                                  // exit via shutdown()
        help();                                                                 // must have database name
    }

    if (i) info(file);                                                          // exit via info()
    if (k) shutdown(file);                                                      // exit via shutdown()

    if (volnam != NULL) {                                                       // do a create
        // number of blocks, block size in bytes, map size in bytes, volume name, UCI name, file name
        exit(INIT_Create_File(blocks, bsize * 1024, map * 1024, volnam, env, file));
    }

    if (jobs > 0) {                                                             // do an init
        // database, number of jobs, MiB of global buf, MiB of routine buf, MiB of additional buf
        exit(INIT_Start(file, jobs, gmb, rmb, addmb));
    }

    c = INIT_Run(file, env, cmd);                                               // run a job

    if (c != 0) {
        fprintf(stderr, "Error occurred in process - %s\n", strerror(c));       // what was returned
    }

#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__NetBSD__)
    if (c == ENOENT) {
        fprintf(stderr, "\tRSM database not loaded\n");
    } else if (c == ENOMEM) {
        fprintf(stderr, "\tRSM job table is full\n");
    }
#endif

    fclose(stdin);                                                              // close standard I/O
    fclose(stdout);                                                             // close standard I/O
    fclose(stderr);                                                             // close standard I/O
    exit(c);                                                                    // exit with value
}
