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
{ char str[80];                                 // a string
  (void) rsm_version((u_char *) str);           // get version into str[]
  printf("----------------------------------------------------------------------------\n");
  printf("%s\n\n", str);                        // print version string
  printf("David Wicksell <dlw@linux.com>\n");
  printf("Copyright (c) 2020 Fourth Watch Software LC\n");
  printf("https://gitlab.com/Reference-Standard-M/rsm\n\n");
  printf("To create a database:\n");
  printf("> rsm -v volnam -b blocksize(kb) -s dbsize(Blocks) filename\n");
  printf("                   and optionally -m mapblocksize(kb)\n");
  printf("  volnam is 1 to 8 alpha characters\n\n");
  printf("To initialize an environment:\n");
  printf("> rsm -j maxjobs -r routinemb -g globalmb -a addmb database\n");
  printf("                 routinemb, globalmg and addmb are optional\n\n");
  printf("To attach to an environment:\n");
  printf("> rsm -x command -e environment(uci) database\n");
  printf("               where both switches are optional\n");
  printf("----------------------------------------------------------------------------\n");
  exit(0);                                      // give help and exit
}

//****************************************************************************
// Main entry for create, init and run

int main(int argc,char **argv)                  // main entry point
{ int c;                                        // for case
  int bsize = 0;                                // block size
  char *env = NULL;                             // start environment name
  int gmb = 0;                                  // global buf MB
  int jobs = 0;                                 // max jobs
  int map = 0;                                  // map/header block bytes
  int rmb = 0;                                  // routine buf MB
  int addmb = 0;                                // additional buffer in MB
  int blocks = 0;                               // number of data blocks
  char *volnam = NULL;                          // volume name
  char *cmd = NULL;                             // startup command
  char *cmd1 = "D ^%1MV1LGI\0";                 // cmd for one
  char *db1 = "/one/onedb\0";                   // db for one
//  printf ("argc = %d\nargv[0] = %s\n", argc, argv[0]);
  if (argc == 1)
  { if (strcmp(argv[0], "one\0") == 0)          // allow for a name of 'one'
    { cmd = cmd1;                               // use this command
      argv[0] = db1;                            // and this as a database
      goto runit;                               // and go do it
    }
  }
  if (argc < 2) help();                         // they need help
  while ((c = getopt(argc, argv, "a:b:e:g:hj:m:r:s:v:x:")) != EOF)
  { switch(c)
    { case 'a':                                 // switch -a
        addmb = atoi(optarg);                   // additional buffer
        break;
      case 'b':                                 // switch -b
        bsize = atoi(optarg);                   // database block size  (creDB)
        break;
      case 'e':                                 // switch -e
        env = optarg;                           // environment name     (run)
        break;
      case 'g':                                 // switch -g
        gmb = atoi(optarg);                     // global buffer MB     (init)
        break;
      case 'h':                                 // switch -h
        help();                                 // exit via help()
        break;
      case 'j':                                 // switch -j
        jobs = atoi(optarg);                    // max number of jobs   (init)
        break;
      case 'm':                                 // switch -m
        map = atoi(optarg);                     // size of map block    (creDB)
        break;
      case 'r':                                 // switch -r
        rmb = atoi(optarg);                     // routine buffer MB    (init)
        break;
      case 's':                                 // switch -s
        blocks = atoi(optarg);                  // number of data blks  (creDB)
        break;
      case 'v':                                 // switch -v
        volnam = optarg;                        // volume name          (creDB)
        break;
      case 'x':                                 // switch -x
        cmd = optarg;                           // initial command      (run)
        break;
      default:                                  // some sort of error
        help();                                 // just give help
        break;
    }
  }
  argc -= optind;                               // adjust for used args
  argv += optind;                               // should point at parameter
  if (argc != 1) help();                        // must have database name

  if (volnam != NULL) exit(                     // do a create
          INIT_Create_File(blocks,              // number of blocks
                           bsize*1024,          // block size in bytes
                           map*1024,            // map size in bytes
                           volnam,              // volume name
                           *argv));             // file name

  if (jobs > 0) exit(                           // do an init
          INIT_Start(*argv,                     // database
                     jobs,                      // number of jobs
                     gmb,                       // mb of global buf
                     rmb,                       // mb of routine buf
                     addmb));                   // mb of additional buf

runit:
  c = INIT_Run(*argv, env, cmd);                // run a job
  if (c != 0) fprintf(stderr,
                       "Error occured in process - %s\n", // complain
                       strerror(c));            // what was returned
#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__NetBSD__)
	if (c == ENOENT)
		fprintf(stderr, "\tRSM database not loaded\n");
	else if (c == ENOMEM)
		fprintf(stderr, "\tRSM job table is full\n");
#endif
  exit (c);                                     // exit with value
}
