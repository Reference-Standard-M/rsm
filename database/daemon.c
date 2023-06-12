/*
 * Package:  Reference Standard M
 * File:     rsm/database/daemon.c
 * Summary:  module database - database daemon functions
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
 * Descript: Ensure all daemons are currently running
 * Input(s): none
 * Return:   none
 */
void daemon_check(void)                                                         // ensure all running
{
    int i;                                                                      // a handy int

    while (SemOp(SEM_WD, WRITE)) continue;                                      // lock WD

    for (i = 0; i < systab->vol[volnum - 1]->num_of_daemons; i++) {
        if (i != myslot) {                                                      // don't check self
            if (kill(systab->vol[volnum - 1]->wd_tab[i].pid, 0) == -1) {        // if gone
                if (errno == ESRCH) DB_Daemon(i, volnum);                       // restart the daemon
                // SHOULD LOG THIS SUCCESS OR FAIL
            }
        }
    }                                                                           // end daemon check

    SemOp(SEM_WD, -WRITE);                                                      // release lock
    return;
}

/*
 * Function: do_write
 * Descript: Write out dirty GBDs
 * Input(s): none
 * Return:   none
 */
void do_write(void)                                                             // write GBDs
{
    off_t file_off;                                                             // for lseek() et al
    int   i;                                                                    // a handy int
    gbd   *gbdptr;                                                              // for the GBD
    gbd   *lastptr = NULL;                                                      // for the GBD

    gbdptr = systab->vol[volnum - 1]->wd_tab[myslot].currmsg.gbddata;           // get the gbdptr from daemon table
    if (!gbdptr) panic("Daemon: write message GBD is NULL");                    // check for null
    if (curr_lock == 0) SemOp(SEM_GLOBAL, READ);                                // if we need a lock then take a read lock

    while (TRUE) {                                                              // until we break
        if (gbdptr->last_accessed == (time_t) 0) {                              // if garbaged
            gbdptr->block = 0;                                                  // just zot the block
        } else {                                                                // do a write
            file_off = (off_t) gbdptr->block - 1;                               // block#

            file_off = (file_off * (off_t) systab->vol[volnum - 1]->vollab->block_size)
                     + (off_t) systab->vol[volnum - 1]->vollab->header_bytes;

            file_off = lseek(dbfd, file_off, SEEK_SET);                         // Seek to block

            if (file_off < 1) {
                systab->vol[volnum - 1]->stats.diskerrors++;                    // count an error
                panic("lseek failed in Write_Chain()!!");                       // die on error
            }

            i = write(dbfd, gbdptr->mem, systab->vol[volnum - 1]->vollab->block_size); // write it

            if (i < 0) {
                systab->vol[volnum - 1]->stats.diskerrors++;                    // count an error
                panic("write failed in Write_Chain()!!");
            }

            systab->vol[volnum - 1]->stats.phywt++;                             // count a write
        }                                                                       // end write code

        if (!gbdptr->dirty) {
            systab->vol[volnum - 1]->wd_tab[myslot].currmsg.gbddata = NULL;     // update the daemon table JIC I vanish
            systab->vol[volnum - 1]->wd_tab[myslot].doing = DOING_NOTHING;      // and here
            break;                                                              // break from while
        }

        lastptr = gbdptr;                                                       // remember this ptr
        gbdptr = gbdptr->dirty;                                                 // get next in list

        if (lastptr != gbdptr) {                                                // if not at end
            systab->vol[volnum - 1]->wd_tab[myslot].currmsg.gbddata = gbdptr;   // update the daemon table JIC I vanish
        } else {
            systab->vol[volnum - 1]->wd_tab[myslot].currmsg.gbddata = NULL;     // update the daemon table JIC I vanish
            systab->vol[volnum - 1]->wd_tab[myslot].doing = DOING_NOTHING;      // and here
        }

        lastptr->dirty = NULL;                                                  // clear old dirtyptr
        if (lastptr == gbdptr) break;                                           // if reached end then break from while
    }                                                                           // end dirty write

    SemOp(SEM_GLOBAL, -curr_lock);                                              // release lock
    return;                                                                     // done
}

/*
 * Function: do_free
 * Descript: Free a block in the map and GBDs (if required)
 * Input(s): block number to free
 * Return:   none
 */
void do_free(u_int gb)                                                          // free from map et al
{
    gbd *ptr;                                                                   // GBD ptr

    while (TRUE) {                                                              // a few times
        daemon_check();                                                         // ensure all running
        if (!SemOp(SEM_GLOBAL, WRITE)) break;                                   // gain write lock stop if it worked
        sleep(1);                                                               // wait a bit
    }

    Free_block(gb);                                                             // free the block
    ptr = systab->vol[volnum - 1]->gbd_hash[gb & (GBD_HASH - 1)];               // get listhead

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
 * Descript: Zot block(s)
 *           At this stage:     there is NO recovery.
 *                              there is no integrity check.
 * Input(s): block number to zot
 * Return:   negative error number or type byte of block zotted.
 */
int do_zot(u_int gb)                                                            // zot block
{
    u_int    i;                                                                 // a handy int
    int      ret;                                                               // for returns
    int      Idx;                                                               // the index
    DB_Block *bptr;                                                             // block pointer
    off_t    file_off;                                                          // for lseek() et al
    off_t    file_ret;                                                          // for lseek() et al
    int      typ;                                                               // block type
    int      zot_data = 0;                                                      // bottom level flag
    gbd      *ptr;                                                              // a handy pointer

    bptr = malloc(systab->vol[volnum - 1]->vollab->block_size);                 // get some memory

    if (bptr == NULL) {                                                         // if failed
        fprintf(stderr, "do_zot: malloc for block %u failed\n", gb);
        fflush(stderr);                                                         // flush to the file
        return -1;                                                              // return fail
    }

    file_off = (off_t) gb - 1;                                                  // the block#

    file_off = (file_off * (off_t) systab->vol[volnum - 1]->vollab->block_size)
             + (off_t) systab->vol[volnum - 1]->vollab->header_bytes;

    while(SemOp(SEM_GLOBAL, READ)) continue;                                    // take a global lock
    ptr = systab->vol[volnum - 1]->gbd_hash[gb & (GBD_HASH - 1)];               // get head

    while (ptr != NULL) {                                                       // for entire list
        if (ptr->block == gb) {                                                 // found it?
            memcpy(bptr, ptr->mem, systab->vol[volnum - 1]->vollab->block_size);
            ptr->last_accessed = (time_t) 0;                                    // mark as zotted
            break;                                                              // exit
        }

        ptr = ptr->next;                                                        // point at next
    }                                                                           // end memory search

    SemOp(SEM_GLOBAL, -curr_lock);                                              // release the lock

    if (ptr == NULL) {                                                          // if not found
        file_ret = lseek(dbfd, file_off, SEEK_SET);                             // Seek to block

        if (file_ret < 1) {
            fprintf(stderr, "do_zot: seek to block %u failed\n", gb);
            fflush(stderr);                                                     // flush to the file
            free(bptr);                                                         // free memory
            return -1;                                                          // return error
        }

        ret = read(dbfd, bptr, systab->vol[volnum - 1]->vollab->block_size);    // read it

        if (ret < 0) {                                                          // if it failed
            fprintf(stderr, "do_zot: read of block %u failed\n", gb);
            fflush(stderr);                                                     // flush to the file
            free(bptr);                                                         // free memory
            return -1;                                                          // return error
        }
    }                                                                           // end read from disk

    typ = bptr->type;                                                           // save type
    if (typ > 64) goto zotit;                                                   // data type? if so, just zot

    for (Idx = IDX_START; Idx <= bptr->last_idx; Idx++) {                       // for each entry
        idx = (u_short *) bptr;                                                 // point at the block
        iidx = (int *) bptr;                                                    // point at the block
        chunk = (cstring *) &iidx[idx[Idx]];                                    // point at the chunk
        record = (cstring *) &chunk->buf[chunk->buf[1] + 2];                    // point at the dbc
        Align_record();                                                         // ensure aligned
        i = *(u_int *) record;                                                  // get block#

        if (zot_data) {                                                         // if we are zotting
            file_ret = (off_t) i - 1;                                           // block#

            file_ret = (file_ret * (off_t) systab->vol[volnum - 1]->vollab->block_size)
                     + (off_t) systab->vol[volnum - 1]->vollab->header_bytes;

            file_ret = lseek(dbfd, file_ret, SEEK_SET);                         // Seek to block

            if (file_ret < 1) {                                                 // check for fail
                fprintf(stderr, "do_zot: seek to block %u failed\n", i);
                fflush(stderr);                                                 // flush to the file
            } else {                                                            // looks ok
                ret = write(dbfd, systab->vol[volnum - 1]->zero_block, systab->vol[volnum - 1]->vollab->block_size); // write zeroes

                if (ret == -1) {                                                // fail ?
                    fprintf(stderr, "do_zot: zero of block %u failed\n", i);
                    fflush(stderr);                                             // flush to the file
                }

                systab->vol[volnum - 1]->stats.phywt++;                         // count a write
                systab->vol[volnum - 1]->stats.logwt++;                         // and a logical
                do_free(i);                                                     // free the block
            }
        } else {                                                                // end zotting - give to lower level
            ret = do_zot(i);                                                    // re-call
            if (ret > 64) zot_data = TRUE;                                      // data block? then do the rest here
        }
    }                                                                           // end of indexes

zotit:
    file_ret = lseek(dbfd, file_off, SEEK_SET);                                 // Seek to block

    if (file_ret < 1) {
        fprintf(stderr, "do_zot: zeroing seek to block %u failed\n", gb);
        fflush(stderr);                                                         // flush to the file
        free(bptr);                                                             // free memory
        return -1;                                                              // return error
    }

    ret = write(dbfd, systab->vol[volnum - 1]->zero_block, systab->vol[volnum - 1]->vollab->block_size); // write zeroes

    if (ret == -1) {                                                            // if it failed
        fprintf(stderr, "do_zot: zero of block %u failed\n", gb);
        fflush(stderr);                                                         // flush to the file
        typ = -1;                                                               // flag fail
    }

    systab->vol[volnum - 1]->stats.phywt++;                                     // count a write
    systab->vol[volnum - 1]->stats.logwt++;                                     // and a logical
    free(bptr);                                                                 // free memory
    do_free(gb);                                                                // and the block
    return typ;                                                                 // return the type
}

/*
 * Function: do_garb
 * Descript: Garbage collect some block(s)
 *           At this stage:     there is NO recovery.
 *                              there is no integrity check.
 * Input(s): none
 * Return:   none
 */
void do_garb(void)                                                              // garbage collect
{
    u_int gb;                                                                   // block being garbed

    if (systab->vol[volnum - 1]->wd_tab[myslot].currmsg.intdata == 0) {         // done?
        systab->vol[volnum - 1]->wd_tab[myslot].doing = DOING_NOTHING;          // yes
        return;                                                                 // and exit
    }

    gb = systab->vol[volnum - 1]->wd_tab[myslot].currmsg.intdata;               // get block
    systab->vol[volnum - 1]->wd_tab[myslot].currmsg.intdata = 0;                // clear slot
    do_zot(gb);                                                                 // do it
    systab->vol[volnum - 1]->wd_tab[myslot].doing = DOING_NOTHING;              // flag done
    return;                                                                     // and exit
}

/*
 * Function: do_dismount
 * Descript: Dismount current volnum
 * Input(s): none
 * Return:   none
 */
void do_dismount(void)                                                          // dismount volnum
{
    int     ret;                                                                // for function returns
    int     cnt;                                                                // and another
    int     pid;                                                                // for jobs
    struct  shmid_ds sbuf;                                                      // for shmctl
#ifdef __APPLE__
    void    *semvals = NULL;
#else
    semun_t semvals;                                                            // dummy for semctl IPC_RMID
#endif
    time_t  t;                                                                  // for ctime()

    ret = shmctl(systab->vol[volnum - 1]->shm_id, IPC_RMID, &sbuf);             // remove share
    if (ret == -1) fprintf(stderr, "errno = %d %s\n", errno, strerror(errno));

    if (volnum == 1) {
        for (u_int i = 0; i < systab->maxjob; i++) {                            // for each job
            pid = systab->jobtab[i].pid;                                        // get pid

            if (pid) {                                                          // if pid != 0
                if (kill(pid, SIGTERM)) {                                       // kill this one
                    systab->jobtab[i].trap = 1U << SIGTERM;                     // say go away
                    systab->jobtab[i].attention = 1;                            // look at it
                }
            }
        }
    }

    for (u_int i = 0; i < systab->vol[volnum - 1]->num_gbd; i++) {              // look for unwritten
        if ((systab->vol[volnum - 1]->gbd_head[i].block) &&                     // if there is a blk
          (systab->vol[volnum - 1]->gbd_head[i].last_accessed != (time_t) 0) &&
          (systab->vol[volnum - 1]->gbd_head[i].dirty)) {
            systab->vol[volnum - 1]->gbd_head[i].dirty = &systab->vol[volnum - 1]->gbd_head[i]; // point at self
            systab->vol[volnum - 1]->wd_tab[0].currmsg.gbddata = &systab->vol[volnum - 1]->gbd_head[i]; // add to our struct
            do_write();                                                         // write it
        }                                                                       // end GBD has blk
    }                                                                           // end blk search

    cnt = 1;

    while (cnt) {                                                               // while theres pids
        cnt = 0;                                                                // reset pid counter
        SemOp(SEM_WD, WRITE);                                                   // lock daemon table

        for (int j = 1; j < systab->vol[volnum - 1]->num_of_daemons; j++) {     // search
            if (systab->vol[volnum - 1]->wd_tab[j].pid) {
                if (kill(systab->vol[volnum - 1]->wd_tab[j].pid, 0) == -1) {
                    if (errno == ESRCH) {                                       // if no such
                        systab->vol[volnum - 1]->wd_tab[j].pid = 0;             // clear it
                    } else {
                        cnt = 1;                                                // remember still there
                    }
                } else {
                    cnt = 1;
                }
            }
        }

        SemOp(SEM_WD, -WRITE);                                                  // unlock daemon table
        if (cnt) sleep(1);                                                      // if pids still around then wait a second...
    }                                                                           // end wait for daemons

    pid = systab->vol[volnum - 1]->wd_tab[myslot].pid;
    t = current_time(FALSE);                                                    // for ctime()
    fprintf(stderr, "%s [%6d]: Daemon %2d writing out clean flag as clean\n", strtok(ctime(&t), "\n"), pid, myslot); // operation
    systab->vol[volnum - 1]->vollab->clean = 1;                                 // set database as clean
    lseek(dbfd, 0, SEEK_SET);                                                   // seek to start of file
    ret = write(dbfd, systab->vol[volnum - 1]->vollab, systab->vol[volnum - 1]->vollab->header_bytes); // write the label/clean flag
    if (ret == -1) fprintf(stderr, "errno = %d %s\n", errno, strerror(errno));

    if (volnum == 1) {
        ret = semctl(systab->sem_id, 0, IPC_RMID, semvals);                     // remove the semaphores
        if (ret == -1) fprintf(stderr, "errno = %d %s\n", errno, strerror(errno));
    }

    t = current_time(FALSE);                                                    // for ctime()

    fprintf(stderr,"%s [%6d]: Daemon %2d stopped and detached from %s\n",
            strtok(ctime(&t), "\n"), pid, myslot, systab->vol[volnum - 1]->file_name); // stopping

    fflush(stderr);                                                             // flush to the file
    return;                                                                     // done
}

/*
 * Function: do_daemon
 * Descript: do daemon type things
 * Input(s): none
 * Return:   none
 */
void do_daemon(void)                                                            // do something
{
    int    i;                                                                   // handy int
    int    j;                                                                   // and another
    int    pid;
    off_t  file_off;                                                            // for lseek()
    time_t t;                                                                   // for ctime()

start:
    daemon_check();                                                             // ensure all running

    if (systab->vol[volnum - 1]->wd_tab[myslot].doing == DOING_NOTHING) {
        if (!myslot && systab->vol[volnum - 1]->map_dirty_flag) {               // first daemon
            file_off = lseek(dbfd, 0, SEEK_SET);                                // move to start of file

            if (file_off == -1) {
                systab->vol[volnum - 1]->stats.diskerrors++;                    // count an error
                panic("do_daemon: lseek() to start of file failed");
            }

            i = write(dbfd, systab->vol[volnum - 1]->vollab, systab->vol[volnum - 1]->vollab->header_bytes); // label/map

            if (i == -1) {
                systab->vol[volnum - 1]->stats.diskerrors++;                    // count an error
                panic("do_daemon: write() map block failed");
            }

            systab->vol[volnum - 1]->stats.phywt++;                             // count a write
            systab->vol[volnum - 1]->map_dirty_flag = 0;                        // unset dirty flag
        }                                                                       // end map write

        if (!myslot && (systab->vol[volnum - 1]->writelock < 0)) {              // check writelock
            while (TRUE) {                                                      // loop
                i = (systab->vol[volnum - 1]->dirtyQ[systab->vol[volnum - 1]->dirtyQr] != NULL); // check dirty queue
                i = (i || (systab->vol[volnum - 1]->garbQ[systab->vol[volnum - 1]->garbQr] != 0)); // and garbQ

                for (j = 1; j < systab->vol[volnum - 1]->num_of_daemons; j++) { // each one
                    i = (i || (systab->vol[volnum - 1]->wd_tab[myslot].doing != 0));
                }

                if (!i) break;                                                  // if all clear then leave loop
                daemon_check();                                                 // ensure all running
                sleep(1);                                                       // wait a bit
            }                                                                   // end while (TRUE)

            sleep(1);                                                           // just a bit more

            // Set the writelock to a positive value when all quiet
            systab->vol[volnum - 1]->writelock = abs(systab->vol[volnum - 1]->writelock);
        }                                                                       // end wrtlock

        while (SemOp(SEM_WD, WRITE)) continue;                                  // lock WD

        if (systab->vol[volnum - 1]->dirtyQ[systab->vol[volnum - 1]->dirtyQr] != NULL) { // any writes?
            systab->vol[volnum - 1]->wd_tab[myslot].currmsg.gbddata =
              systab->vol[volnum - 1]->dirtyQ[systab->vol[volnum - 1]->dirtyQr]; // get

            systab->vol[volnum - 1]->wd_tab[myslot].doing = DOING_WRITE;
            systab->vol[volnum - 1]->dirtyQ[systab->vol[volnum - 1]->dirtyQr] = NULL;
            systab->vol[volnum - 1]->dirtyQr++;                                 // increment ptr
            systab->vol[volnum - 1]->dirtyQr &= (NUM_DIRTY - 1);                // do wrap
        } else if (systab->vol[volnum - 1]->garbQ[systab->vol[volnum - 1]->garbQr]) { // any garbage?
            systab->vol[volnum - 1]->wd_tab[myslot].currmsg.intdata =
              systab->vol[volnum - 1]->garbQ[systab->vol[volnum - 1]->garbQr]; // get

            systab->vol[volnum - 1]->wd_tab[myslot].doing = DOING_GARB;
            systab->vol[volnum - 1]->garbQ[systab->vol[volnum - 1]->garbQr] = 0;
            systab->vol[volnum - 1]->garbQr++;                                  // increment ptr
            systab->vol[volnum - 1]->garbQr &= (NUM_GARB - 1);                  // do wrap
        }

        SemOp(SEM_WD, -WRITE);                                                  // release WD lock
    }                                                                           // end looking for work

    pid = systab->vol[volnum - 1]->wd_tab[myslot].pid;

    if (systab->vol[volnum - 1]->wd_tab[myslot].doing == DOING_NOTHING) {
        if (systab->vol[volnum - 1]->dismount_flag) {                           // dismounting?
            if (myslot) {                                                       // first?
                systab->vol[volnum - 1]->wd_tab[myslot].pid = 0;                // say gone
                t = current_time(FALSE);                                        // for ctime()

                fprintf(stderr,"%s [%6d]: Daemon %2d stopped and detached from %s\n",
                        strtok(ctime(&t), "\n"), pid, myslot, systab->vol[volnum - 1]->file_name); // stopping

                fflush(stderr);                                                 // flush to the file
                exit(EXIT_SUCCESS);                                             // and exit
            }

            do_dismount();                                                      // dismount it
            exit(EXIT_SUCCESS);                                                 // and exit
        } else {                                                                // end dismount code
            return;                                                             // nothing to do
        }
    }

    if (systab->vol[volnum - 1]->wd_tab[myslot].doing == DOING_WRITE) {
        do_write();                                                             // do it
        goto start;                                                             // try again
    }
    if (systab->vol[volnum - 1]->wd_tab[myslot].doing == DOING_GARB) {
        do_garb();                                                              // or this
        goto start;                                                             // try again
    }

    return;                                                                     // can't get here
}

/*
 * Function: DB_Daemon
 * Descript: Start daemon for passed in slot and vol#
 * Input(s): slot# and Vol#
 * Return:   0 -> Ok, any non-zero = error
 */
int DB_Daemon(int slot, int vol)                                                // start a daemon
{
    int    i;                                                                   // a handy int
    int    k;                                                                   // and another
    int    fit;                                                                 // for fork ret
    int    pid;                                                                 // for child PID
    char   logfile[VOL_FILENAME_MAX + 18];                                      // daemon log file name
    time_t t;                                                                   // for ctime()

    volnum = vol;                                                               // save vol# here
    fit = ForkIt(-1);                                                           // start a daemon

    if (fit > 0) {                                                              // check for ok (parent)
        systab->vol[volnum - 1]->wd_tab[slot].pid = fit;                        // put in childs pid
        return 0;                                                               // return (am parent)
    }                                                                           // end parent code

    if (fit < 0) return errno;                                                  // die on error
    curr_lock = 0;                                                              // clear lock flag

    // -- Create log file name --
    k = strlen(systab->vol[volnum - 1]->file_name);                             // get len of filename
    for (i = (k - 1); (systab->vol[volnum - 1]->file_name[i] != '/') && (i > -1); i--) continue; // find last '/'
    strncpy(logfile, systab->vol[volnum - 1]->file_name, i + 1);                // copy to log filename
    logfile[i + 1] = (char) '\0';                                               // terminate for strlen
    sprintf(&logfile[strlen(logfile)], "log/");                                 // add the log directory to the file path
    umask(0);                                                                   // set umask to 0000

    // --- Create the log directory, with 0755 permissions, ignore if it already exists
    if (mkdir(logfile, S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH) == -1) {
        if (errno != EEXIST) return errno;
    }

    umask(S_IWGRP | S_IROTH | S_IWOTH);                                         // set umask to 0026
    sprintf(&logfile[strlen(logfile)], "daemon-%d.log", volnum);                // create daemon log file path
    myslot = slot;                                                              // remember my slot

#ifndef __FreeBSD__
    // --- Close original FD to database file inherited from parent ---
    if (close(partab.vol_fds[volnum - 1]) == -1) return errno;
#endif

    // --- Reopen stdin, stdout, and stderr (logfile) ---
    if (freopen("/dev/null", "r", stdin) == NULL) return errno;                 // stdin to bitbucket
    if (freopen("/dev/null", "w", stdout) == NULL) return errno;                // stdout to bitbucket
    if (freopen(logfile, "a", stderr) == NULL) return errno;                    // stderr to logfile
    pid = systab->vol[volnum - 1]->wd_tab[slot].pid;                            // get current PID
    dbfd = open(systab->vol[volnum - 1]->file_name, O_RDWR);                    // open database RW
    t = current_time(FALSE);                                                    // for ctime()

    if (dbfd < 0) {
        fprintf(stderr, "%s [%6d]: Daemon %2d failed to attach to %s - exiting \n",
                strtok(ctime(&t), "\n"), pid, myslot, systab->vol[volnum - 1]->file_name); // failure

        fflush(stderr);                                                         // flush to the file
        return errno;                                                           // check for error
    }

    fprintf(stderr, "%s [%6d]: Daemon %2d started and attached to %s\n",
            strtok(ctime(&t), "\n"), pid, myslot, systab->vol[volnum - 1]->file_name); // success

    fflush(stderr);                                                             // flush to the file
    if (systab->vol[volnum - 1]->upto && !myslot) ic_map(-3);                   // if map needs check then do it
    i = sleep(2);                                                               // wait a bit
    if (i != 0) sleep(i);                                                       // if interrupted, sleep the rest

    while (TRUE) {                                                              // forever
        do_daemon();                                                            // do something
        sleep(1);                                                               // rest
        //sleep(systab->vol[volnum - 1]->num_of_daemons);                         // rest
    }

    return 0;                                                                   // never gets here
}
