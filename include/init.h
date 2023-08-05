/*
 * Package:  Reference Standard M
 * File:     rsm/include/init.h
 * Summary:  module RSM header file - prototypes (module init)
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

#ifndef _RSM_INIT_H_                                                            // only do this once
#define _RSM_INIT_H_

#define MAX_GROUPS  32                                                          // max number of UNIX groups

// Create a database file volume
int INIT_Create_File(u_int blocks,                                              // number of blocks
                     u_int bsize,                                               // block size in bytes
                     u_int map,                                                 // map size in bytes may be 0
                     char  *volnam,                                             // volume name
                     char  *env,                                                // manager UCI name
                     char  *file);                                              // file name

// Create and start an environment
int INIT_Start(char  *file,                                                     // database file
               u_int jobs,                                                      // number of jobs
               u_int gmb,                                                       // MiB of global buffers
               u_int rmb,                                                       // MiB of routine buffers
               u_int addmb);                                                    // MiB of additional buffers

// Start a job
int INIT_Run(char *file,                                                        // database file
             char *env,                                                         // environment (UCI)
             char *cmd);                                                        // command

// Initialize the symbol table
void ST_Init(void);                                                             // empty symbol table

void help(void);                                                                // give some help
void info(char *file);                                                          // give some info
void shutdown(char *file);                                                      // defined in rsm/init/shutdown.c

#endif                                                                          // !_RSM_INIT_H_
