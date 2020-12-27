/*
 * Package:  Reference Standard M
 * File:     rsm/init/rsm.c
 * Summary:  module RSM - startup (main) code
 *
 * David Wicksell <dlw@linux.com>
 * Copyright © 2020 Fourth Watch Software LC
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

#include <stdio.h>                              // always include
#include <stdlib.h>                             // these two
#include <sys/types.h>                          // for u_char def
#include <string.h>
#include <ctype.h>
#include <unistd.h>				// for getopt
#include <errno.h>                              // error stuff
#include "rsm.h"                                // standard includes
#include "proto.h"                              // function prototypes
#include "init.h"                               // init prototypes

//****************************************************************************
// Give help if they entered -h or similar
//
void help(void)                                 // give some help
{ char str[100];                                // a string
  (void) rsm_version((u_char *) str);           // get version into str[]
  printf("--------------------------------------------------------------------------------\n");
  printf("%s\n", str);                          // print version string
  printf("Database version: %d   Compiler version: %d\n\n", DB_VER, COMP_VER);
  printf("David Wicksell <dlw@linux.com>\n");
  printf("Copyright (c) 2020 Fourth Watch Software LC\n");
  printf("https://gitlab.com/Reference-Standard-M/rsm\n\n");
  printf("Set the RSM_DBFILE environment variable to <database-file-path> to avoid having\n");
  printf("to pass it to the rsm executable. Arguments in square brackets are optional.\n\n");
  printf("Create a database:\n\n");
  printf("rsm -v <volume name>\t\tName of volume (1-%d alpha characters)\n", VAR_LEN);
  printf("    -b <block size>\t\tSize of database blocks (4-256 KiB)\n");
  printf("    -s <database size>\t\tInitial size of database (100-%u blocks)\n", MAX_DATABASE_BLKS);
  printf("    [-e <environment name>]\tName of manager UCI (1-%d alpha characters)\n", VAR_LEN);
  printf("    [-m <map size>]\t\tSize of the map block (0-%u KiB)\n", MAX_MAP_SIZE);
  printf("    <database-file-path>\tName of the database file\n\n");
  printf("Initialize an environment:\n\n");
  printf("rsm -j <max job>\t\tMaximum attached jobs for volume (1-256 jobs)\n");
  printf("    [-g <global buffers>]\tSize of global buffers in MiB\n");
  printf("    [-r <routine buffers>]\tSize of routine buffers in MiB\n");
  printf("    [-a <additional buffers>]\tSize of additional buffers in MiB\n");
  printf("    <database-file-path>\tName of the database file\n\n");
  printf("Start a job and attach to an environment:\n\n");
  printf("rsm \t\t\t\tStarts in direct mode in manager UCI\n");
  printf("    [-e <environment name>]\tName of initial UCI environment\n");
  printf("    [-x <M command(s)>]\t\tA string of M commands to execute\n");
  printf("    <database-file-path>\tName of the database file\n");
  printf("--------------------------------------------------------------------------------\n");
  exit(0);                                      // give help and exit
}

//****************************************************************************
// Main entry for create, init and run

int main(int argc, char **argv)                 // main entry point
{ int c;                                        // for case
  int bsize = 0;                                // block size
  char *env = NULL;                             // start environment name
  int gmb = 0;                                  // global buf MiB
  int jobs = 0;                                 // max jobs
  int map = 0;                                  // header/map block bytes
  int rmb = 0;                                  // routine buf MiB
  int addmb = 0;                                // additional buffer in MiB
  int blocks = 0;                               // number of data blocks
  char *volnam = NULL;                          // volume name
  char *cmd = NULL;                             // startup command
  char *dbfile = getenv("RSM_DBFILE");          // pass volume in environment
  char file[VOL_FILENAME_MAX];

  if (argc < 2 && dbfile == NULL) help();       // they need help
  while ((c = getopt(argc, argv, "a:b:e:g:hj:m:r:s:v:x:")) != EOF)
  { switch(c)
    { case 'a':                                 // switch -a
        addmb = atoi(optarg);                   // additional buffer
        break;
      case 'b':                                 // switch -b
        bsize = atoi(optarg);                   // database block size (creDB)
        break;
      case 'e':                                 // switch -e
        env = optarg;                           // environment name    (run)
        break;
      case 'g':                                 // switch -g
        gmb = atoi(optarg);                     // global buffer MiB   (init)
        break;
      case 'h':                                 // switch -h
        help();                                 // exit via help()
        break;
      case 'j':                                 // switch -j
        jobs = atoi(optarg);                    // max number of jobs  (init)
        break;
      case 'm':                                 // switch -m
        map = atoi(optarg);                     // size of map block   (creDB)
        break;
      case 'r':                                 // switch -r
        rmb = atoi(optarg);                     // routine buffer MiB  (init)
        break;
      case 's':                                 // switch -s
        blocks = atoi(optarg);                  // number of data blks (creDB)
        break;
      case 'v':                                 // switch -v
        volnam = optarg;                        // volume name         (creDB)
        break;
      case 'x':                                 // switch -x
        cmd = optarg;                           // initial command     (run)
        break;
      default:                                  // some sort of error
        help();                                 // just give help
        break;
    }
  }
  argc -= optind;                               // adjust for used args
  argv += optind;                               // should point at parameter

  if (argc == 1)
  { strcpy(file, *argv);
  }
  else if (dbfile != NULL)
  { strcpy(file, dbfile);
  }
  else
  { help();                                     // must have database name
  }

  if (volnam != NULL) exit(                     // do a create
          INIT_Create_File(blocks,              // number of blocks
                           bsize * 1024,        // block size in bytes
                           map * 1024,          // map size in bytes
                           volnam,              // volume name
                           env,                 // UCI name
                           file));              // file name

  if (jobs > 0) exit(                           // do an init
          INIT_Start(file,                      // database
                     jobs,                      // number of jobs
                     gmb,                       // MiB of global buf
                     rmb,                       // MiB of routine buf
                     addmb));                   // MiB of additional buf

  c = INIT_Run(file, env, cmd);                 // run a job
  if (c != 0) fprintf(stderr,
                      "Error occurred in process - %s\n", // complain
                      strerror(c));             // what was returned
#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__NetBSD__)
	if (c == ENOENT)
		fprintf(stderr, "\tRSM database not loaded\n");
	else if (c == ENOMEM)
		fprintf(stderr, "\tRSM job table is full\n");
#endif
  exit (c);                                     // exit with value
}
