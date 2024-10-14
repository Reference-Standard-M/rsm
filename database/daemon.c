/*
 * Package: Reference Standard M
 * File:    rsm/database/daemon.c
 * Summary: module database - database daemon functions
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
#include <string.h>                                                             // for memcpy
#include <unistd.h>                                                             // for file reading
#include <time.h>                                                               // for ctime
#include <ctype.h>                                                              // for GBD stuff
#include <errno.h>                                                              // for errors
#include <fcntl.h>                                                              // for file stuff
#include <signal.h>                                                             // for kill()
#include <sys/shm.h>
#include <sys/stat.h>                                                           // for mkdir()
#include <sys/types.h>                                                          // for semaphores
#include <sys/ipc.h>                                                            // for semaphores
#include <sys/sem.h>                                                            // for semaphores
#include "rsm.h"                                                                // standard includes
#include "database.h"                                                           // database protos
#include "proto.h"                                                              // standard prototypes
#include "error.h"                                                              // error strings

int  dbfd;                                                                      // global db file desc
int  myslot;                                                                    // my slot in WD table
void ic_map(int flag);                                                          // check the map

/*
 * Function: daemon_check
 * Summary:  Ensure all daemons are currently running
 * Input(s): None
 * Return:   None
 */
void daemon_check(void)                                                         // ensure all running
{
    int i;                                                                      // a handy int

    while (SemOp(SEM_WD, SEM_WRITE)) continue;                                  // lock WD

    for (i = 0; i < partab.vol[volnum - 1]->num_of_daemons; i++) {
        if (i != myslot) {                                                      // don't check self
            if (kill(partab.vol[volnum - 1]->wd_tab[i].pid, 0) == -1) {         // if gone
                if (errno == ESRCH) DB_Daemon(i, volnum);                       // restart the daemon
                // SHOULD LOG THIS SUCCESS OR FAIL
            }
        }
    }                                                                           // end daemon check

    SemOp(SEM_WD, -SEM_WRITE);                                                  // release lock
    return;
}

/*
 * Function: do_write
 * Summary:  Write out dirty GBDs
 * Input(s): None
 * Return:   None
 */
void do_write(void)                                                             // write GBDs
{
    off_t file_off;                                                             // for lseek() et al
    int   i;                                                                    // a handy int
    gbd   *gbdptr;                                                              // for the GBD
    gbd   *lastptr = NULL;                                                      // for the GBD

    gbdptr = partab.vol[volnum - 1]->wd_tab[myslot].currmsg.gbddata;            // get the gbdptr from daemon table
    if (!gbdptr) panic("do_write: write message GBD is NULL");                  // check for null
    if (curr_lock == 0) SemOp(SEM_GLOBAL, SEM_READ);                            // if we need a lock then take a read lock

    while (TRUE) {                                                              // until we break
        if (gbdptr->last_accessed == (time_t) 0) {                              // if garbaged
            gbdptr->block = 0;                                                  // just zot the block
        } else {                                                                // do a write
            file_off = (off_t) gbdptr->block - 1;                               // block#

            file_off = (file_off * (off_t) partab.vol[volnum - 1]->vollab->block_size)
                     + (off_t) partab.vol[volnum - 1]->vollab->header_bytes;

            file_off = lseek(dbfd, file_off, SEEK_SET);                         // seek to block

            if (file_off < 1) {
                partab.vol[volnum - 1]->stats.diskerrors++;                     // count an error
                panic("lseek failed in do_write()!!");                          // die on error
            }

            i = write(dbfd, gbdptr->mem, partab.vol[volnum - 1]->vollab->block_size); // write it

            if (i < 0) {
                partab.vol[volnum - 1]->stats.diskerrors++;                     // count an error
                panic("write failed in do_write()!!");                          // die on error
            }

            partab.vol[volnum - 1]->stats.phywt++;                              // count a write
        }                                                                       // end write code

        if (gbdptr->dirty == NULL) {
            partab.vol[volnum - 1]->wd_tab[myslot].currmsg.gbddata = NULL;      // update the daemon table JIC I vanish
            partab.vol[volnum - 1]->wd_tab[myslot].doing = DOING_NOTHING;       // and here
            break;                                                              // break from while
        }

        lastptr = gbdptr;                                                       // remember this pointer
        gbdptr = gbdptr->dirty;                                                 // get next in list

        if (lastptr != gbdptr) {                                                // if not at end
            partab.vol[volnum - 1]->wd_tab[myslot].currmsg.gbddata = gbdptr;    // update the daemon table JIC I vanish
        } else {
            partab.vol[volnum - 1]->wd_tab[myslot].currmsg.gbddata = NULL;      // update the daemon table JIC I vanish
            partab.vol[volnum - 1]->wd_tab[myslot].doing = DOING_NOTHING;       // and here
        }

        lastptr->dirty = NULL;                                                  // clear old dirty ptr
        if (lastptr == gbdptr) break;                                           // if reached end then break from while
    }                                                                           // end dirty write

    SemOp(SEM_GLOBAL, -curr_lock);                                              // release lock
    return;                                                                     // done
}

/*
 * Function: do_free
 * Summary:  Free a block in the map and GBDs (if required)
 * Input(s): Block number to free
 * Return:   None
 */
void do_free(u_int gb)                                                          // free from map et al
{
    gbd *ptr;                                                                   // GBD ptr

    while (TRUE) {                                                              // a few times
        daemon_check();                                                         // ensure all running
        if (!SemOp(SEM_GLOBAL, SEM_WRITE)) break;                               // gain write lock stop if it worked
        sleep(1);                                                               // wait a bit
    }

    Free_block(gb);                                                             // free the block
    ptr = partab.vol[volnum - 1]->gbd_hash[gb & (GBD_HASH - 1)];                // get listhead

    while (ptr != NULL) {                                                       // for each in list
        if (ptr->block == gb) {                                                 // found it
            if (ptr->dirty < (gbd *) 3) {                                       // not in use
                Free_GBD(ptr);                                                  // free it
            } else {                                                            // in use or not locked
                ptr->last_accessed = (time_t) 0;                                // mark as zotted
            }

            break;                                                              // and exit the loop
        }

        ptr = ptr->next;                                                        // get next
    }                                                                           // end GBD stuff

    SemOp(SEM_GLOBAL, -curr_lock);                                              // release lock
    return;                                                                     // exit
}

/*
 * Function: do_zot
 * Summary:  Zot block(s) - at this stage there is NO recovery and there is no integrity check
 * Input(s): Block number to zot
 * Return:   Negative error number or type byte of block zotted
 */
int do_zot(u_int gb)                                                            // zot block
{
    u_int    u;                                                                 // a handy int
    int      ret;                                                               // for returns
    int      Idx;                                                               // the index
    DB_Block *bptr;                                                             // block pointer
    off_t    file_off;                                                          // for lseek() et al
    int      type;                                                              // block type
    int      zot_data = FALSE;                                                  // bottom level flag
    gbd      *ptr;                                                              // a handy pointer

    if ((bptr = malloc(partab.vol[volnum - 1]->vollab->block_size)) == NULL) {  // get some memory
        fprintf(stderr, "do_zot: malloc() for block %u failed\n", gb);
        fflush(stderr);                                                         // flush to the file
        return errno;                                                           // return fail
    }

    // file offset of block to zot
    file_off = (off_t) (gb - 1) * partab.vol[volnum - 1]->vollab->block_size + partab.vol[volnum - 1]->vollab->header_bytes;

    while(SemOp(SEM_GLOBAL, SEM_READ)) continue;                                // take a global lock
    ptr = partab.vol[volnum - 1]->gbd_hash[gb & (GBD_HASH - 1)];                // get the head

    while (ptr != NULL) {                                                       // for entire list
        if (ptr->block == gb) {                                                 // found it?
            memcpy(bptr, ptr->mem, partab.vol[volnum - 1]->vollab->block_size);
            ptr->last_accessed = (time_t) 0;                                    // mark as zotted
            break;                                                              // exit
        }

        ptr = ptr->next;                                                        // point at next
    }                                                                           // end memory search

    SemOp(SEM_GLOBAL, -curr_lock);                                              // release the lock

    if (ptr == NULL) {                                                          // if not found
        if (lseek(dbfd, file_off, SEEK_SET) == (off_t) -1) {                    // seek to block
            free(bptr);                                                         // free memory
            fprintf(stderr, "do_zot: lseek() to block %u failed\n", gb);
            fflush(stderr);                                                     // flush to the file
            return errno;                                                       // return error
        }

        ret = read(dbfd, bptr, partab.vol[volnum - 1]->vollab->block_size);     // read it

        if (ret < (int) partab.vol[volnum - 1]->vollab->block_size) {
            free(bptr);                                                         // free memory
            fprintf(stderr, "do_zot: read() of block %u failed\n", gb);
            fflush(stderr);                                                     // flush to the file
            if (ret == -1) return errno;                                        // return error
            return -1;                                                          // return generic error
        }
    }                                                                           // end read from disk

    type = bptr->type;                                                          // save type
    if (type > 64) goto zotit;                                                  // data type? if so, just zot

    for (Idx = IDX_START; Idx <= bptr->last_idx; Idx++) {                       // for each entry
        idx = (u_short *) bptr;                                                 // point at the block
        iidx = (int *) bptr;                                                    // point at the block
        chunk = (cstring *) &iidx[idx[Idx]];                                    // point at the chunk
        record = (cstring *) &chunk->buf[chunk->buf[1] + 2];                    // point at the dbc
        Align_record();                                                         // ensure aligned
        u = *(u_int *) record;                                                  // get block#

        if (zot_data) {                                                         // if we are zotting
            off_t off;                                                          // for lseek() for each entry

            // block#
            off = (off_t) (u - 1) * partab.vol[volnum - 1]->vollab->block_size + partab.vol[volnum - 1]->vollab->header_bytes;

            if (lseek(dbfd, off, SEEK_SET) == (off_t) -1) {                     // seek to block
                fprintf(stderr, "do_zot: seek() to block %u failed\n", u);
                fflush(stderr);                                                 // flush to the file
            } else {                                                            // looks ok
                // write zeroes
                if (write(dbfd, partab.vol[volnum - 1]->zero_block, partab.vol[volnum - 1]->vollab->block_size) == -1) {
                    fprintf(stderr, "do_zot: write() zero of block %u failed\n", u);
                    fflush(stderr);                                             // flush to the file
                }

                partab.vol[volnum - 1]->stats.phywt++;                          // count a write
                partab.vol[volnum - 1]->stats.logwt++;                          // and a logical
                do_free(u);                                                     // free the block
            }
        } else {                                                                // end zotting - give to lower level
            if (do_zot(u) > 64) zot_data = TRUE;                                // re-call - data block? then do the rest here
        }
    }                                                                           // end of indexes

zotit:
    if (lseek(dbfd, file_off, SEEK_SET) == (off_t) -1) {                        // seek to block
        free(bptr);                                                             // free memory
        fprintf(stderr, "do_zot: lseek() to block %u failed\n", gb);
        fflush(stderr);                                                         // flush to the file
        return errno;                                                           // return error
    }

    if (write(dbfd, partab.vol[volnum - 1]->zero_block, partab.vol[volnum - 1]->vollab->block_size) == -1) { // write zeroes
        type = -1;                                                              // flag fail
        fprintf(stderr, "do_zot: write() zero of block %u failed\n", gb);
        fflush(stderr);                                                         // flush to the file
    }

    partab.vol[volnum - 1]->stats.phywt++;                                      // count a write
    partab.vol[volnum - 1]->stats.logwt++;                                      // and a logical
    free(bptr);                                                                 // free memory
    do_free(gb);                                                                // and the block
    return type;                                                                // return the type
}

/*
 * Function: do_garb
 * Summary:  Garbage collect some block(s)
 *           At this stage there is NO recovery and there is no integrity check
 * Input(s): None
 * Return:   None
 */
void do_garb(void)                                                              // garbage collect
{
    u_int gb;                                                                   // block being garbed

    if (partab.vol[volnum - 1]->wd_tab[myslot].currmsg.intdata == 0) {          // done?
        partab.vol[volnum - 1]->wd_tab[myslot].doing = DOING_NOTHING;           // yes
        return;                                                                 // and exit
    }

    gb = partab.vol[volnum - 1]->wd_tab[myslot].currmsg.intdata;                // get block
    partab.vol[volnum - 1]->wd_tab[myslot].currmsg.intdata = 0;                 // clear slot
    do_zot(gb);                                                                 // do it
    partab.vol[volnum - 1]->wd_tab[myslot].doing = DOING_NOTHING;               // flag done
    return;                                                                     // and exit
}

/*
 * Function: do_dismount
 * Summary:  Dismount current volnum
 * Input(s): None
 * Return:   None
 */
void do_dismount(void)                                                          // dismount volnum
{
    int     cnt;                                                                // and another
    int     pid;                                                                // for jobs
    time_t  t;                                                                  // for ctime()
    struct  shmid_ds sbuf;                                                      // for shmctl
#ifdef __APPLE__
    void    *semvals = NULL;
#else
    semun_t semvals = {.val = 0};                                               // dummy for semctl IPC_RMID
#endif

    if (shmctl(partab.vol[volnum - 1]->shm_id, IPC_RMID, &sbuf) == -1) {        // remove share
        fprintf(stderr, "do_dismount: shmctl() error: %d - %s\n", errno, strerror(errno));
        fflush(stderr);                                                         // flush to the file
    }

    if (volnum == 1) {
        for (u_int u = 0; u < systab->maxjob; u++) {                            // for each job
            pid = partab.job_table[u].pid;                                      // get pid

            if (pid) {                                                          // if pid != 0
                if (kill(pid, SIGTERM) == -1) {                                 // kill this one
                    partab.job_table[u].trap = 1U << SIGTERM;                   // or say go away
                    partab.job_table[u].attention = 1;                          // and look at it
                }
            }
        }
    }

    for (u_int u = 0; u < partab.vol[volnum - 1]->num_gbd; u++) {               // look for unwritten
        if ((partab.vol[volnum - 1]->gbd_head[u].block) &&                      // if there is a block
          (partab.vol[volnum - 1]->gbd_head[u].last_accessed != (time_t) 0) &&
          (partab.vol[volnum - 1]->gbd_head[u].dirty)) {
            partab.vol[volnum - 1]->gbd_head[u].dirty = &partab.vol[volnum - 1]->gbd_head[u]; // point at self
            partab.vol[volnum - 1]->wd_tab[0].currmsg.gbddata = &partab.vol[volnum - 1]->gbd_head[u]; // add to our struct
            do_write();                                                         // write it
        }                                                                       // end GBD has block
    }                                                                           // end block search

    cnt = TRUE;

    while (cnt) {                                                               // while there are pids
        cnt = FALSE;                                                            // reset pid counter
        SemOp(SEM_WD, SEM_WRITE);                                               // lock daemon table

        for (int j = 1; j < partab.vol[volnum - 1]->num_of_daemons; j++) {      // search
            if (partab.vol[volnum - 1]->wd_tab[j].pid) {
                if (kill(partab.vol[volnum - 1]->wd_tab[j].pid, 0) == -1) {
                    if (errno == ESRCH) {                                       // if no such
                        partab.vol[volnum - 1]->wd_tab[j].pid = 0;              // clear it
                    } else {
                        cnt = TRUE;                                             // remember still there
                    }
                } else {
                    cnt = TRUE;
                }
            }
        }

        SemOp(SEM_WD, -SEM_WRITE);                                              // unlock daemon table
        if (cnt) sleep(1);                                                      // if pids still around then wait a second...
    }                                                                           // end wait for daemons

    pid = partab.vol[volnum - 1]->wd_tab[myslot].pid;
    t = current_time(FALSE);                                                    // for ctime()

    fprintf(stderr, "%s [%7d]: Daemon %2d writing out clean flag as clean\n", strtok(ctime(&t), "\n"), pid, myslot); // operation

    fflush(stderr);                                                             // flush to the file
    partab.vol[volnum - 1]->vollab->clean = 1;                                  // set database as clean

    if (lseek(dbfd, 0, SEEK_SET) == (off_t) -1) {                               // seek to start of file
        fprintf(stderr, "do_dismount lseek() error: %d - %s\n", errno, strerror(errno));
        fflush(stderr);                                                         // flush to the file
    }

    if (write(dbfd, partab.vol[volnum - 1]->vollab, partab.vol[volnum - 1]->vollab->header_bytes) == -1) { // write label/clean flag
        fprintf(stderr, "do_dismount write() error: %d - %s\n", errno, strerror(errno));
        fflush(stderr);                                                         // flush to the file
    }

    if (volnum == 1) {
        if (semctl(systab->sem_id, 0, IPC_RMID, semvals) == -1) {               // remove the semaphores
            fprintf(stderr, "do_dismount semctl() error: %d - %s\n", errno, strerror(errno));
            fflush(stderr);                                                     // flush to the file
        }
    }

    t = current_time(FALSE);                                                    // for ctime()

    fprintf(stderr,"%s [%7d]: Daemon %2d stopped and detached from %s\n",
            strtok(ctime(&t), "\n"), pid, myslot, partab.vol[volnum - 1]->file_name); // stopping

    fflush(stderr);                                                             // flush to the file
    return;                                                                     // done
}

/*
 * Function: do_daemon
 * Summary:  Do daemon type things
 * Input(s): None
 * Return:   None
 */
void do_daemon(void)                                                            // do something
{
    int    i;                                                                   // handy int
    int    j;                                                                   // and another
    int    pid;
    time_t t;                                                                   // for ctime()

start:
    daemon_check();                                                             // ensure all running

    if (partab.vol[volnum - 1]->wd_tab[myslot].doing == DOING_NOTHING) {
        if (!myslot && partab.vol[volnum - 1]->map_dirty_flag) {                // first daemon
            if (lseek(dbfd, 0, SEEK_SET) == (off_t) -1) {                       // move to start of file
                partab.vol[volnum - 1]->stats.diskerrors++;                     // count an error
                panic("do_daemon: lseek() to start of file failed");
            }

            // label/map
            if (write(dbfd, partab.vol[volnum - 1]->vollab, partab.vol[volnum - 1]->vollab->header_bytes) == -1) {
                partab.vol[volnum - 1]->stats.diskerrors++;                     // count an error
                panic("do_daemon: write() map block failed");
            }

            partab.vol[volnum - 1]->stats.phywt++;                              // count a write
            partab.vol[volnum - 1]->map_dirty_flag = 0;                         // unset dirty flag
        }                                                                       // end map write

        if (!myslot && (partab.vol[volnum - 1]->writelock < 0)) {               // check writelock
            while (TRUE) {                                                      // loop
                i = (partab.vol[volnum - 1]->dirtyQ[partab.vol[volnum - 1]->dirtyQr] != NULL); // check dirty queue
                i = (i || (partab.vol[volnum - 1]->garbQ[partab.vol[volnum - 1]->garbQr] != 0)); // and garbQ

                for (j = 1; j < partab.vol[volnum - 1]->num_of_daemons; j++) {  // each one
                    i = (i || (partab.vol[volnum - 1]->wd_tab[myslot].doing != DOING_NOTHING));
                }

                if (!i) break;                                                  // if all clear then leave loop
                daemon_check();                                                 // ensure all running
                sleep(1);                                                       // wait a bit
            }                                                                   // end while (TRUE)

            sleep(1);                                                           // just a bit more

            // Set the writelock to a positive value when all quiet
            partab.vol[volnum - 1]->writelock = abs(partab.vol[volnum - 1]->writelock);
        }                                                                       // end wrtlock

        while (SemOp(SEM_WD, SEM_WRITE)) continue;                              // lock WD

        if (partab.vol[volnum - 1]->dirtyQ[partab.vol[volnum - 1]->dirtyQr] != NULL) { // any writes?
            partab.vol[volnum - 1]->wd_tab[myslot].currmsg.gbddata =
              partab.vol[volnum - 1]->dirtyQ[partab.vol[volnum - 1]->dirtyQr];  // get

            partab.vol[volnum - 1]->wd_tab[myslot].doing = DOING_WRITE;
            partab.vol[volnum - 1]->dirtyQ[partab.vol[volnum - 1]->dirtyQr] = NULL;
            partab.vol[volnum - 1]->dirtyQr++;                                  // increment index
            partab.vol[volnum - 1]->dirtyQr &= (NUM_DIRTY - 1);                 // do wrap
        } else if (partab.vol[volnum - 1]->garbQ[partab.vol[volnum - 1]->garbQr]) { // any garbage?
            // get
            partab.vol[volnum - 1]->wd_tab[myslot].currmsg.intdata = partab.vol[volnum - 1]->garbQ[partab.vol[volnum - 1]->garbQr];
            partab.vol[volnum - 1]->wd_tab[myslot].doing = DOING_GARB;
            partab.vol[volnum - 1]->garbQ[partab.vol[volnum - 1]->garbQr] = 0;
            partab.vol[volnum - 1]->garbQr++;                                   // increment index
            partab.vol[volnum - 1]->garbQr &= (NUM_GARB - 1);                   // do wrap
        }

        SemOp(SEM_WD, -SEM_WRITE);                                              // release WD lock
    }                                                                           // end looking for work

    pid = partab.vol[volnum - 1]->wd_tab[myslot].pid;

    if (partab.vol[volnum - 1]->wd_tab[myslot].doing == DOING_NOTHING) {
        if (partab.vol[volnum - 1]->dismount_flag) {                            // dismounting?
            if (myslot) {                                                       // first?
                partab.vol[volnum - 1]->wd_tab[myslot].pid = 0;                 // say gone
                t = current_time(FALSE);                                        // for ctime()

                fprintf(stderr,"%s [%7d]: Daemon %2d stopped and detached from %s\n",
                        strtok(ctime(&t), "\n"), pid, myslot, partab.vol[volnum - 1]->file_name); // stopping

                fflush(stderr);                                                 // flush to the file
                exit(EXIT_SUCCESS);                                             // and exit
            }

            do_dismount();                                                      // dismount it
            partab.vol[volnum - 1]->wd_tab[myslot].pid = 0;                     // say gone
            exit(EXIT_SUCCESS);                                                 // and exit
        } else {                                                                // end dismount code
            return;                                                             // nothing to do
        }
    }

    if (partab.vol[volnum - 1]->wd_tab[myslot].doing == DOING_WRITE) {
        do_write();                                                             // do it
        goto start;                                                             // try again
    }
    if (partab.vol[volnum - 1]->wd_tab[myslot].doing == DOING_GARB) {
        do_garb();                                                              // or this
        goto start;                                                             // try again
    }

    return;                                                                     // can't get here
}

/*
 * Function: DB_Daemon
 * Summary:  Start daemon for passed in slot and volume numbers
 * Input(s): Slot# and Vol#
 * Return:   0 -> Ok, any non-zero = error
 */
int DB_Daemon(int slot, int vol)                                                // start a daemon
{
    int    i;                                                                   // a handy int
    int    k;                                                                   // and another
    int    fit;                                                                 // for fork ret
    int    pid;                                                                 // for child PID
    char   logfile[VOL_FILENAME_MAX + 22];                                      // daemon log file name
    time_t t;                                                                   // for ctime()

    volnum = vol;                                                               // save vol# here
    fit = ForkIt(-1);                                                           // start a daemon

    if (fit > 0) {                                                              // check for ok (parent)
        partab.vol[volnum - 1]->wd_tab[slot].pid = fit;                         // put in childs pid
        return 0;                                                               // return (am parent)
    }                                                                           // end parent code

    if (fit < 0) return errno;                                                  // die on error
    curr_lock = 0;                                                              // clear lock flag

    // -- Create log file name --
    k = strlen(partab.vol[volnum - 1]->file_name);                              // get len of filename
    for (i = (k - 1); (partab.vol[volnum - 1]->file_name[i] != '/') && (i > -1); i--) continue; // find last '/'
    strncpy(logfile, partab.vol[volnum - 1]->file_name, i + 1);                 // copy to log filename
    logfile[i + 1] = (char) '\0';                                               // terminate for strlen
    sprintf(&logfile[strlen(logfile)], "log/");                                 // add the log directory to the file path
    umask(0);                                                                   // set umask to 0000

    // --- Create the log directory, with 0755 permissions, ignore if it already exists
    if (mkdir(logfile, S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH) == -1) {
        if (errno != EEXIST) return errno;
    }

    umask(S_IWGRP | S_IROTH | S_IWOTH);                                         // set umask to 0026
    sprintf(&logfile[strlen(logfile)], "rsm-daemon-%d.log", volnum);            // create daemon log file path
    myslot = slot;                                                              // remember my slot

#ifndef __FreeBSD__
    // --- Close original FD to database file inherited from parent ---
    if (close(partab.vol_fds[volnum - 1]) == -1) return errno;
#endif

    // --- Reopen stdin, stdout, and stderr (logfile) ---
    if (freopen("/dev/null", "r", stdin) == NULL) return errno;                 // stdin to bitbucket
    if (freopen("/dev/null", "w", stdout) == NULL) return errno;                // stdout to bitbucket
    if (freopen(logfile, "a", stderr) == NULL) return errno;                    // stderr to logfile
    pid = partab.vol[volnum - 1]->wd_tab[slot].pid;                             // get current PID
    dbfd = open(partab.vol[volnum - 1]->file_name, O_RDWR);                     // open database RW
    t = current_time(FALSE);                                                    // for ctime()

    if (dbfd < 0) {
        fprintf(stderr, "%s [%7d]: Daemon %2d failed to attach to %s - exiting \n",
                strtok(ctime(&t), "\n"), pid, myslot, partab.vol[volnum - 1]->file_name); // failure

        fflush(stderr);                                                         // flush to the file
        return errno;                                                           // check for error
    }

    fprintf(stderr, "%s [%7d]: Daemon %2d started and attached to %s\n",
            strtok(ctime(&t), "\n"), pid, myslot, partab.vol[volnum - 1]->file_name); // success

    fflush(stderr);                                                             // flush to the file
    if (partab.vol[volnum - 1]->upto && !myslot) ic_map(-3);                    // if map needs check then do it
    i = sleep(2);                                                               // wait a bit
    if (i != 0) sleep(i);                                                       // if interrupted, sleep the rest

    while (TRUE) {                                                              // forever
        do_daemon();                                                            // do something
        sleep(1);                                                               // rest
        //sleep(partab.vol[volnum - 1]->num_of_daemons);                        // rest
    }

    return 0;                                                                   // never gets here
}
