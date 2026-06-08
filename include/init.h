/*
 * Package: Reference Standard M
 * File:    include/init.h
 * Summary: RSM Module Header File - prototypes (module init)
 *
 * SPDX-FileCopyrightText:  © 2020-2026 Fourth Watch Software LC
 * SPDX-FileContributor:    David Wicksell <dlw@linux.com>
 * SPDX-FileComment:        https://gitlab.com/Reference-Standard-M/rsm
 * SPDX-FileComment:        Derived from MUMPS V1 (BSD-3-Clause)
 * SPDX-FileComment:        Original work by Raymond Douglas Newman (1999-2018)
 * SPDX-License-Identifier: AGPL-3.0-or-later
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
 */

#ifndef RSM_INIT_H
#define RSM_INIT_H

#include "rsm.h"                                                                // standard RSM includes

#define MAX_GROUPS  32                                                          // max number of UNIX groups

/*
 * @function {public} init_create
 * @summary Create a database volume
 * @param {u_int} blocks (100-MAX_DATABASE_BLKS) - The initial size of the database in blocks
 * @param {u_int} bsize (1-256) - The block size for the database
 * @param {u_int} map (0-MAX_MAP_SIZE) - The size of the map block (database label header + block bitmap)
 * @param {const char *} volnam (1-32 alpha) - The name of the volume
 * @param {const char *} env (1-32 alpha) - The name of the manager UCI (default is MGR)
 * @param {char *} file (1-VOL_FILENAME_MAX) - The database file path
 * @param {int} unsecured - Whether to create the database volume with unsecure permissions
 * @returns {int} (0|-1|errno) - The return code representing success or failure
 */
int init_create(u_int blocks, u_int bsize, u_int map, const char *volnam, const char *env, char *file, int unsecured);

/*
 * @function {public} init_start
 * @summary Create and start an environment
 * @param {char *} file (1-VOL_FILENAME_MAX) - The database file path
 * @param {u_int} jobs (1-4096) - The number of jobs allocated for the environment
 * @param {u_int} gmb (0-131072) - The size of the global buffers
 * @param {u_int} rmb (0-4095) - The size of the routine buffers
 * @param {u_int} addmb - The size of the additional buffers
 * @param {int} unsecured - Whether unsecured mode should be enabled or not
 * @returns {int} (0|-1|errno) - The return code representing success or failure
 */
int init_start(char *file, u_int jobs, u_int gmb, u_int rmb, u_int addmb, int unsecured);

/*
 * @function {public} init_run
 * @summary Start a job
 * @param {char *} file (1-VOL_FILENAME_MAX) - The database file path
 * @param {const char *} env (1-32 alpha) - The name of job's initial UCI (default is UCI 1)
 * @param {char *} cmd (1-ARG_MAX) - A string of M commands to be Xecuted
 * @returns {int} (0|-1|errno) - The return code representing success or failure
 */
int init_run(char *file, const char *env, char *cmd);

// Initialize the symbol table
void ST_Init(void);                                                             // empty symbol table

void help(void);                                                                // give some help
void info(char *file);                                                          // give some info
void shutdown(char *file);                                                      // shutdown environment

#endif
