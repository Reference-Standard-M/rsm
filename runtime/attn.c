/*
 * Package: Reference Standard M
 * File:    rsm/runtime/attn.c
 * Summary: module runtime - look after attention conditions
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

#include "compile.h"                                                            // for XECUTE
#include "error.h"                                                              // standard errors
#include "opcode.h"                                                             // the op codes
#include "proto.h"                                                              // standard prototypes
#include "seqio.h"                                                              // for signal stuff
#include <errno.h>                                                              // error stuff
#include <sched.h>                                                              // for sched_yield
#include <signal.h>
#include <stdio.h>                                                              // always include
#include <string.h>
#include <termios.h>                                                            // for ioctl
#include <time.h>                                                               // for nanosleep
#include <unistd.h>                                                             // for sleep
#include <sys/ioctl.h>                                                          // for ioctl

extern int failed_tty;                                                          // tty reset flag

short attention(void)                                                           // process attention
{
    short s = 0;                                                                // return value

    if (partab.jobtab->trap & SIG_CC) {                                         // <Control-C>
        partab.jobtab->trap &= ~SIG_CC;                                         // clear it
        partab.jobtab->async_error = -(ERRZ51 + ERRMLAST);                      // store the error
    }

    /*
    if (partab.jobtab->trap & SIG_WS) {                                         // window size change SIGWINCH
        partab.jobtab->trap = partab.jobtab->trap & ~SIG_WS;                    // clear it
        // Note: This is ignored in seqio/signal.c currently
    }
    */

    if (partab.jobtab->trap & SIG_HUP) {                                        // SIGHUP
        partab.jobtab->trap &= ~SIG_HUP;                                        // clear it
        partab.jobtab->async_error = -(ERRZ66 + ERRMLAST);                      // store the error
    }

    if (partab.jobtab->trap & SIG_U1) {                                         // user defined signal 1
        partab.jobtab->trap &= ~SIG_U1;                                         // clear it
        partab.jobtab->async_error = -(ERRZ67 + ERRMLAST);                      // store the error
    }

    if (partab.jobtab->trap & SIG_U2) {                                         // user defined signal 2
        partab.jobtab->trap &= ~SIG_U2;                                         // clear it
        partab.jobtab->async_error = -(ERRZ68 + ERRMLAST);                      // store the error
    }

    if (partab.jobtab->trap & (SIG_QUIT | SIG_TERM | SIG_STOP)) {               // stop type
        partab.jobtab->trap = 0;                                                // clear it
        partab.jobtab->async_error = 0;                                         // clear the error
        partab.jobtab->attention = 0;                                           // clear attention
        return OPHALT;                                                          // and just halt
    }

    partab.jobtab->trap = 0;                                                    // clear signals
    partab.jobtab->attention = 0;                                               // clear attention
    s = partab.jobtab->async_error;                                             // do we have an error
    partab.jobtab->async_error = 0;                                             // clear it

    if ((s == 0) && (partab.debug > BREAK_OFF)) {                               // check the debug junk
        if (partab.debug <= (int) partab.jobtab->commands) {                    // there yet?
            s = BREAK_NOW;                                                      // time to break again
        } else {
            partab.jobtab->attention = 1;                                       // reset attention
        }
    }

    return s;                                                                   // return whatever
}

// DoInfo - look after a <Control-T>
void DoInfo(void)
{
    int         i;                                                              // a handy int
    int         j;                                                              // and another
    char        ct[400];                                                        // some space for control t
    const char *p;                                                              // a handy pointer
    mvar        *var;                                                           // and another
    struct      winsize w;                                                      // for ioctl

    memcpy(ct, "\033\067\033[99;1H", 9);                                        // start off
    i = 9;                                                                      // next char
    i += sprintf(&ct[i],"%d", (int) (partab.jobtab - partab.job_table) + 1);
    i += sprintf(&ct[i]," (%d) ", partab.jobtab->pid);
    p = (char *) &partab.jobtab->dostk[partab.jobtab->cur_do].rounam;           // point at routine name
    for (j = 0; (j < VAR_LEN) && p[j]; ct[i++] = p[j++]) {}                     // copy it
    i += sprintf(&ct[i]," Cmds: %u ", partab.jobtab->commands);
    i += sprintf(&ct[i],"Grefs: %u ", partab.jobtab->grefs);
    var = &partab.jobtab->last_ref;                                             // point at $R

    if (var->name.var_cu[0] != '\0') {                                          // something there?
        i += UTIL_String_Mvar(var, (u_char *) &ct[i], MAX_NUM_SUBS);            // decode it
    }

    if ((ioctl(STDOUT_FILENO, TIOCGWINSZ, &w) != -1) && (i > (w.ws_col + 9))) {
        i = w.ws_col + 9;                                                       // fit on terminal
    } else if (i > 89) {
        i = 89;                                                                 // fit on terminal if ioctl failed
    }

    memcpy(&ct[i], "\033\133\113\033\070\0", 6);                                // and the trailing bit
    fprintf(stderr, "%s", ct);                                                  // output it
    return;                                                                     // all done
}

/*
 * The ForkIt subroutine, forks another RSM process
 *
 * Returns:  Success (parent) M job number of child
 *           Success (child) -M job number of parent
 *           Failure 0 (zot)
 *
 *    cft = -1 Daemons (do not copy file table - FreeBSD)
 *           0 JOB
 *           1 TCP FORK
 *           2 $&%FORK
 */
int ForkIt(int cft)                                                             // Copy File Table
{
    int          i;                                                             // a handy int
    int          ret;                                                           // and another
    volatile int mid = -1;                                                      // for the M id
    const void   *j;                                                            // a handy pointer

    for (u_int k = 0; k < systab->maxjob; k++) {                                // scan the slots
        ret = partab.job_table[k].pid;                                          // get pid

        if (ret) {                                                              // if one there
            if (kill(ret, 0) == -1) {                                           // check the job
                if (errno == ESRCH) {                                           // doesn't exist
                    CleanJob(k + 1);                                            // zot if not there
                    break;                                                      // have at least one
                }
            }
        } else {                                                                // it's free or ours
            break;                                                              // quit
        }
    }

    if (cft != -1) {                                                            // not a daemon
        i = SemOp(SEM_SYS, SEM_WRITE);                                          // lock systab
        if (i < 0) return 0;                                                    // quit on error

        for (u_int k = 0; k < systab->maxjob; k++) {                            // look for a free slot
            if (partab.job_table[k].pid == 0) {                                 // this one ?
                mid = k;                                                        // yes - save int job num
                break;                                                          // and exit
            }
        }

        if (mid == -1) {                                                        // if no slots
            SemOp(SEM_SYS, -SEM_WRITE);                                         // unlock
            return 0;                                                           // return fail
        }
    }

    if (cft != 2) setSignal(SIGCHLD, IGNORE);                                   // ignore child termination (except for $&%FORK)
#ifdef __FreeBSD__                                                              // for FreeBSD
    ret = RFPROC | RFNOWAIT;

    if (cft == -1) {
        ret |= RFCFDG;                                                          // default - don't copy file table
    } else {
        ret |= RFFDG;                                                           // if it is a fork or JOB then copy the file table
    }

    i = rfork(ret);                                                             // create new process
#else                                                                           // Linux, macOS, et al.
    i = fork();
#endif

    if (!i) {                                                                   // child
        failed_tty = -1;                                                        // don't restore term settings on exit
        setSignal(SIGINT, IGNORE);                                              // disable <Control-C>
    }

    if (cft == -1) {                                                            // daemons
        if (!i) {                                                               // child
            j = freopen("/dev/null", "r", stdin);                               // redirect stdin
            if (j == NULL) fprintf(stderr, "ForkIt: freopen() errno = %d - %s\n", errno, strerror(errno));
            j = freopen("/dev/null", "w", stdout);                              // redirect stdout
            if (j == NULL) fprintf(stderr, "ForkIt: freopen() errno = %d - %s\n", errno, strerror(errno));
            j = freopen("/dev/null", "w", stderr);                              // redirect stderr
            if (j == NULL) fprintf(stderr, "ForkIt: freopen() errno = %d - %s\n", errno, strerror(errno));
        }

        return i;
    }

    if (i == -1) {                                                              // fail ?
        fprintf(stderr, "ForkIt: fork() errno = %d - %s\n", errno, strerror(errno));
        SemOp(SEM_SYS, -SEM_WRITE);                                             // unlock
        return 0;                                                               // return fail
    } else if (i > 0) {                                                         // the parent ?
        memcpy(&partab.job_table[mid], partab.jobtab, sizeof(jobtab));          // copy job info
        partab.job_table[mid].pid = i;                                          // save the pid
        SemOp(SEM_SYS, -SEM_WRITE);                                             // unlock
        return mid + 1;                                                         // return child job number
    }

    ret = -(partab.jobtab - partab.job_table + 1);                              // save minus parent job#
    partab.jobtab = &partab.job_table[mid];                                     // and save our jobtab address

    for (i = 0; i < 1000; i++) {                                                // wait for the above to happen
        if (getpid() == partab.jobtab->pid) break;                              // done yet? if yes - exit
        SchedYield(TRUE);                                                       // give up slice (or sleep)
    }

    if (i > 999) {                                                              // if that didn't work
        for (i = 0; ; i++) {                                                    // try the long way
            if (systab->start_user == -1) return ret;                           // if parent gone and shutting down (JIC)
            if (getpid() == partab.jobtab->pid) break;                          // done yet? if yes - exit
            if (i > 120) panic("ForkIt: Child job never got setup");            // two minutes is enough
            sleep(1);                                                           // wait for a second
        }
    }

    if (cft) {                                                                  // fork type?
        i = SemOp(SEM_ROU, SEM_WRITE);                                          // grab the routine semaphore
        if (i < 0) panic("ForkIt: Can't get SEM_ROU semaphore)");               // die on fail

        for (i = partab.jobtab->cur_do; i > 0; i--) {                           // scan all do frames
            if (partab.jobtab->dostk[i].flags & DO_FLAG_ATT) {
                ((rbd *) SOA(partab.jobtab->dostk[i].routine))->attached++;     // count attached
            }
        }

        SemOp(SEM_ROU, -SEM_WRITE);                                             // release the routine buffers
        return ret;                                                             // return -parent job#
    }

    for (i = 1; i < MAX_SEQ_IO; SQ_Close(i++)) {}                               // close all open files (job type)
    j = freopen("/dev/null", "r", stdin);                                       // redirect stdin
    if (j == NULL) fprintf(stderr, "ForkIt: freopen() errno = %d - %s\n", errno, strerror(errno));
    j = freopen("/dev/null", "w", stdout);                                      // redirect stdout
    if (j == NULL) fprintf(stderr, "ForkIt: freopen() errno = %d - %s\n", errno, strerror(errno));
    j = freopen("/dev/null", "w", stderr);                                      // redirect stderr
    if (j == NULL) fprintf(stderr, "ForkIt: freopen() errno = %d - %s\n", errno, strerror(errno));
    return ret;                                                                 // return -parent job#
}

// SchedYield
void SchedYield(u_char sleep)                                                   // do a sched_yield or a nanosleep or nothing
{
    struct timespec time = {                                                    // 10ms sleep
        .tv_sec = 0,
        .tv_nsec = 10000000
    };

#if !defined(__APPLE__) && !defined(__OpenBSD__)                                // macOS/OpenBSD doesn't support the scheduler API
    int policy;

    policy = sched_getscheduler(0);                                             // get current scheduler policy

    if ((policy == SCHED_FIFO) || (policy == SCHED_RR)) {                       // real-time schedules
        sched_yield();                                                          // do it
        return;                                                                 // and exit
    }
#endif

    if (sleep) nanosleep(&time, NULL);                                          // sleep
    return;                                                                     // and exit if not a real-time policy
}
