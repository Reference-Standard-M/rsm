/*
 * Package:  Reference Standard M
 * File:     rsm/runtime/attn.c
 * Summary:  module runtime - look after attention conditions
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
#include <sys/types.h>                                                          // for u_char def
#include <sys/ioctl.h>                                                          // for ioctl
#include <termios.h>                                                            // for ioctl
#include <string.h>
#include <ctype.h>
#include <signal.h>
#include <unistd.h>                                                             // for sleep
#include <sched.h>                                                              // for sched_yield()
#include <sysexits.h>                                                           // for exit() condition
#include <errno.h>                                                              // error stuff
#include "rsm.h"                                                                // standard includes
#include "proto.h"                                                              // standard prototypes
#include "error.h"                                                              // standard errors
#include "opcode.h"                                                             // the op codes
#include "compile.h"                                                            // for XECUTE
#include "seqio.h"                                                              // for signal stuff

extern int failed_tty;                                                          // tty reset flag

short attention(void)                                                           // process attention
{
    short s = 0;                                                                // return value

    if (partab.jobtab->trap & SIG_CC) {                                         // Control-C
        partab.jobtab->trap = partab.jobtab->trap & ~SIG_CC;                    // clear it
        partab.jobtab->async_error = -(ERRZ51 + ERRMLAST);                      // store the error
    }

    /*
    if (partab.jobtab->trap & SIG_WS) {                                         // window size change SIGWINCH
        partab.jobtab->trap = partab.jobtab->trap & ~SIG_WS;                    // clear it
        // THIS IS IGNORED IN SQ_Signal.c CURRENTLY
    }
    */

    if (partab.jobtab->trap & SIG_HUP) {                                        // SIGHUP
        partab.jobtab->trap = partab.jobtab->trap & ~SIG_HUP;                   // clear it
        partab.jobtab->async_error = -(ERRZ66 + ERRMLAST);                      // store the error
    }

    if (partab.jobtab->trap & SIG_U1) {                                         // user defined signal 1
        partab.jobtab->trap = partab.jobtab->trap & ~SIG_U1;                    // clear it
        partab.jobtab->async_error = -(ERRZ67 + ERRMLAST);                      // store the error
    }

    if (partab.jobtab->trap & SIG_U2) {                                         // user defined signal 2
        partab.jobtab->trap = partab.jobtab->trap & ~SIG_U2;                    // clear it
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

    if ((s == 0) && (partab.debug > 0)) {                                       // check the debug junk
        if (partab.debug <= (int) partab.jobtab->commands) {                    // there yet?
            s = BREAK_NOW;                                                      // time to break again
        } else {
            partab.jobtab->attention = 1;                                       // reset attention
        }
    }

    return s;                                                                   // return whatever
}

// DoInfo() - look after a Control-T
void DoInfo(void)
{
    int    i;                                                                   // a handy int
    int    j;                                                                   // and another
    char   ct[400];                                                             // some space for control t
    char   *p;                                                                  // a handy pointer
    mvar   *var;                                                                // and another
    struct winsize w;                                                           // for ioctl

    memcpy(ct, "\033\067\033[99;1H", 9);                                        // start off
    i = 9;                                                                      // next char
    i += sprintf(&ct[i],"%d", (int) (partab.jobtab - systab->jobtab) + 1);
    i += sprintf(&ct[i]," (%d) ", partab.jobtab->pid);
    p = (char *) &partab.jobtab->dostk[partab.jobtab->cur_do].rounam;           // point at routine name
    for (j = 0; (j < VAR_LEN) && p[j]; ct[i++] = p[j++]) continue;              // copy it
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
 * Returns:  Success (parent) M job number of child
 *           Success (child) -M job number of parent
 *           Failure 0 (zot)
 *
 *     cft = 0 JOB
 *           1 FORK
 *          -1 Just do a fork()/rfork() for the daemons, no file table
 */
int ForkIt(int cft)                                                             // Copy File Table True/False
{
    int  i;                                                                     // a handy int
    int  ret;                                                                   // and another
    int  mid = -1;                                                              // for the M id
    void *j;                                                                    // a handy pointer

    for (u_int k = 0; k < systab->maxjob; k++) {                                // scan the slots
        ret = systab->jobtab[k].pid;                                            // get pid

        if (ret) {                                                              // if one there
            if (kill(ret, 0)) {                                                 // check the job
                if (errno == ESRCH) {                                           // doesn't exist
                    CleanJob(k + 1);                                            // zot if not there
                    break;                                                      // have at least one
                }
            }
        } else {                                                                // it's free or ours
            break;                                                              // quit
        }
    }

#ifdef __FreeBSD__
    if (cft > -1) {
        ret = RFPROC | RFNOWAIT | RFFDG;                                        // if it is a fork or JOB then copy the file table
    } else {
        ret = RFPROC | RFNOWAIT | RFCFDG;                                       // default - no copy FT
    }
#endif

    if (cft > -1) {                                                             // not a daemon
        i = SemOp(SEM_SYS, -systab->maxjob);                                    // lock systab
        if (i < 0) return 0;                                                    // quit on error


        for (u_int k = 0; k < systab->maxjob; k++) {                            // look for a free slot
            if (systab->jobtab[k].pid == 0) {                                   // this one ?
                mid = k;                                                        // yes - save int job num
                break;                                                          // and exit
            }
        }

        if (mid == -1) {                                                        // if no slots
            SemOp(SEM_SYS, systab->maxjob);                                     // unlock
            return 0;                                                           // return fail
        }
    }

#ifdef __FreeBSD__                                                              // for FreeBSD
    i = rfork(ret);                                                             // create new process
#else                                                                           // Linux, MacOS X et al
    signal(SIGCHLD, SIG_IGN);                                                   // try this
    i = fork();
#endif

    if (!i) {                                                                   // child
        failed_tty = -1;                                                        // don't restore term settings on exit
        setSignal(SIGINT, IGNORE);                                              // disable Control-C
    }

    if (cft == -1) {                                                            // daemons
        if (!i) {                                                               // child
            j = freopen("/dev/null", "r", stdin);                               // redirect stdin
            if (j == NULL) fprintf(stderr, "freopen() errno = %d - %s\n", errno, strerror(errno));
            j = freopen("/dev/null", "w", stdout);                              // redirect stdout
            if (j == NULL) fprintf(stderr, "freopen() errno = %d - %s\n", errno, strerror(errno));
            j = freopen("/dev/null", "w", stderr);                              // redirect stderr
            if (j == NULL) fprintf(stderr, "freopen() errno = %d - %s\n", errno, strerror(errno));
        }

        return i;
    }

    if (i == -1) {                                                              // fail ?
        fprintf(stderr, "fork() errno = %d - %s\n", errno, strerror(errno));
        SemOp(SEM_SYS, systab->maxjob);                                         // unlock
        return 0;                                                               // return fail
    } else if (i > 0) {                                                         // the parent ?
        memcpy(&systab->jobtab[mid], partab.jobtab, sizeof(jobtab));            // copy job info
        systab->jobtab[mid].pid = i;                                            // save the pid
        SemOp(SEM_SYS, systab->maxjob);                                         // unlock
        return mid + 1;                                                         // return child job number
    }

    ret = -(partab.jobtab - systab->jobtab + 1);                                // save minus parent job#
    partab.jobtab = &systab->jobtab[mid];                                       // and save our jobtab address

    for (i = 0; i < 10000; i++) {                                               // wait for the above to happen
        if (getpid() == partab.jobtab->pid) break;                              // done yet? if yes - exit
        SchedYield();                                                           // give up slice
    }

    if (i > 9999) {                                                             // if that didn't work
        for (i = 0; ; i++) {                                                    // try the long way
            if (getpid() == partab.jobtab->pid) break;                          // done yet? if yes - exit
            if (i > 120) panic("ForkIt: Child job never got setup");            // two minutes is enough
            sleep(1);                                                           // wait for a second
        }
    }

    if (cft) {                                                                  // fork type?
        i = SemOp(SEM_ROU, -systab->maxjob);                                    // grab the routine semaphore
        if (i < 0) panic("Can't get SEM_ROU in ForkIt()");                      // die on fail

        for (i = partab.jobtab->cur_do; i > 0; i--) {                           // scan all do frames
            if (partab.jobtab->dostk[i].flags & DO_FLAG_ATT) {
                ((rbd *) partab.jobtab->dostk[i].routine)->attached++;          // count attached
            }
        }

        SemOp(SEM_ROU, systab->maxjob);                                         // release the routine buffers
        return ret;                                                             // return -parent job#
    }

    for (i = 1; i < MAX_SEQ_IO; SQ_Close(i++)) continue;                        // close all open files (job type)
    j = freopen("/dev/null", "r", stdin);                                       // redirect stdin
    if (j == NULL) fprintf(stderr, "freopen() errno = %d - %s\n", errno, strerror(errno));
    j = freopen("/dev/null", "w", stdout);                                      // redirect stdout
    if (j == NULL) fprintf(stderr, "freopen() errno = %d - %s\n", errno, strerror(errno));
    j = freopen("/dev/null", "w", stderr);                                      // redirect stderr
    if (j == NULL) fprintf(stderr, "freopen() errno = %d - %s\n", errno, strerror(errno));
    return ret;                                                                 // return -parent job#
}

// SchedYield()
void SchedYield(void)                                                           // do a sched_yield()
{
    sched_yield();                                                              // do it
    return;                                                                     // and exit
}
