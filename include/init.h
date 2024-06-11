/*
 * Package: Reference Standard M
 * File:    rsm/include/init.h
 * Summary: module RSM header file - prototypes (module init)
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

#ifndef RSM_INIT_H
#define RSM_INIT_H

#define MAX_GROUPS 32                                                           // max number of UNIX groups

/*
 * @function {public} INIT_Create_File
 * @summary Create a database volume
 * @param {u_int} blocks (100-MAX_DATABASE_BLKS) - The initial size of the database in blocks
 * @param {u_int} bsize (1-256) - The block size for the database
 * @param {u_int} map (0-MAX_MAP_SIZE) - The size of the map block (database label header + block bitmap)
 * @param {char *} volnam (1-32 alpha) - The name of the volume
 * @param {char *} env (1-32 alpha) - The name of the manager UCI (default is MGR)
 * @param {char *} file (1-VOL_FILENAME_MAX) - The database file path
 * @returns {int} (0|-1|errno) - The return code representing success or failure
 */
int INIT_Create_File(u_int blocks, u_int bsize, u_int map, char *volnam, char *env, char *file);

/*
 * @function {public} INIT_Start
 * @summary Create and start an environment
 * @param {char *} file (1-VOL_FILENAME_MAX) - The database file path
 * @param {u_int} jobs (1-1024) - The number of jobs allocated for the environment
 * @param {u_int} gmb (0-131072) - The size of the global buffers
 * @param {u_int} rmb (0-4095) - The size of the routine buffers
 * @param {u_int} addmb - The size of the additional buffers
 * @returns {int} (0|-1|errno) - The return code representing success or failure
 */
int INIT_Start(char *file, u_int jobs, u_int gmb, u_int rmb, u_int addmb);

/*
 * @function {public} INIT_Run
 * @summary Start a job
 * @param {char *} file (1-VOL_FILENAME_MAX) - The database file path
 * @param {char *} env (1-32 alpha) - The name of job's initial UCI (default is UCI 1)
 * @param {char *} cmd (1-ARG_MAX) - A string of M commands to be Xecuted
 * @returns {int} (0|-1|errno) - The return code representing success or failure
 */
int INIT_Run(char *file, char *env, char *cmd);

// Initialize the symbol table
void ST_Init(void);                                                             // empty symbol table

void help(void);                                                                // give some help
void info(char *file);                                                          // give some info
void shutdown(char *file);                                                      // shutdown environment

#endif
