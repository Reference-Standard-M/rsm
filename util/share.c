/*
 * Package: Reference Standard M
 * File:    rsm/util/share.c
 * Summary: module RSM share - shared memory
 *
 * David Wicksell <dlw@linux.com>
 * Copyright © 2020-2024 Fourth Watch Software LC
 * https://gitlab.com/Reference-Standard-M/rsm
 *
 * Based on MUMPS V1 by Raymond Douglas Newman
 * Copyright © 1999-2016
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

#include "error.h"                                                              // standard includes
#include "proto.h"                                                              // standard includes
#include <errno.h>                                                              // error stuff
#include <stdlib.h>                                                             // always include
#include <sys/sem.h>                                                            // semaphores
#include <sys/shm.h>                                                            // shared memory

extern int curr_lock;                                                           // for tracking SEM_GLOBAL

/*
 * Function: UTIL_Share - attach shared memory section
 * Returns 0 on success or errno on error
 */
int UTIL_Share(char *dbfile, int flags)                                         // pointer to dbfile name
{
    key_t shar_mem_key;                                                         // memory "key"
    int   shar_mem_id;                                                          // memory id
    int   sem_id;                                                               // semaphore id

    shar_mem_key = ftok(dbfile, RSM_SYSTEM);                                    // get a unique key
    if (shar_mem_key == -1) return errno;                                       // die on error
    shar_mem_id = shmget(shar_mem_key, 0, 0);                                   // attach to existing share
    if (shar_mem_id == -1) return errno;                                        // die on error
    systab = (systab_struct *) shmat(shar_mem_id, SHMAT_SEED, flags);           // map it
    if (systab == (void *) -1) return errno;                                    // die on error
    sem_id = semget(shar_mem_key, 0, 0);                                        // attach to semaphores
    if (sem_id == -1) return errno;                                             // die on error
    return 0;                                                                   // return 0 for OK
}

/*
 *  struct sembuf {
 *      u_short sem_num;                                                        // semaphore #
 *      short   sem_op;                                                         // semaphore operation
 *      short   sem_flg;                                                        // operation flags
 *  };
 */
short SemOp(int sem_num, int numb)                                              // Add/Remove semaphore
{
    struct sembuf buf[2] = {{0, 0, SEM_UNDO}, {SEM_ATOMIC, SEM_WRITE, SEM_UNDO}}; // for semop()
    short         semops = 1;
    static int    atomic = FALSE;                                               // flag whether atomic lock is held by current job

    if (numb == 0) return 0;                                                    // check for junk? then just return
    buf[0].sem_num = (u_short) sem_num;                                         // get the one we want
    buf[0].sem_op = (short) numb;                                               // and the number of them

    for (int i = 0; i < 5; i++) {                                               // try this many times
        int t;

        if ((atomic == FALSE) && (numb < 0) && (sem_num == SEM_GLOBAL) && (partab.jobtab != NULL)) {
            semops = 2;                                                         // need to add a check for the atomic
        }

        t = semop(systab->sem_id, buf, semops);                                 // do it

        if (t == 0) {                                                           // if that worked
            if (sem_num == SEM_GLOBAL) {
                curr_lock += numb;                                              // adjust curr_lock

                if (semops == 2) {                                              // if the atomic was acquired
                    buf[1].sem_op = -SEM_WRITE;
                    semop(systab->sem_id, &buf[1], 1);                          // release the atomic
                }
            } else if (sem_num == SEM_ATOMIC) {
                if (numb == SEM_WRITE) {
                    atomic = TRUE;
                } else {
                    atomic = FALSE;
                }
            }

            return 0;                                                           // exit success
        }

        if (numb < 0) {                                                         // if it was an add
            if (partab.jobtab == NULL) panic("SemOp: Error in write daemon");   // from a daemon, yes - die
        }

        if (partab.jobtab->trap) return -(ERRZ51 + ERRMLAST);                   // and we got a <Control-C> then return an error
    }

    if (systab->start_user == -1) exit(EXIT_SUCCESS);                           // If shutting down then just quit
    if ((sem_num != SEM_LOCK) || (numb != 1)) panic("SemOp: Failed");           // die... unless a lock release
    return 0;                                                                   // shouldn't get here except lock
}
