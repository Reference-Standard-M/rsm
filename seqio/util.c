/*
 * Package:  Reference Standard M
 * File:     rsm/seqio/util.c
 * Summary:  module IO - sequential misc. IO operations
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
 *
 * Extended Summary:
 *
 * This module implements the following miscellaneous sequential
 * input/output (i.e., IO) operations:
 *
 *     printBytestr     - Prints the integer value of each element in a character
 *                        array
 *
 *     printSQChan      - Prints out each field in the structure SQ_Chan, and
 *                        selected fields in the structure jobtab
 *
 *     getError         - Returns a negative integer value (which corresponds to
 *                        a particular error message)
 *
 *     setSignalBitMask - Sets the bits of those signals that have been caught
 *     seqioSelect      - Waits until an object is ready for reading or writing
 */

#include <stdio.h>                                                              // always include
#include <stdlib.h>                                                             // these two
#include <errno.h>
#include <sys/types.h>                                                          // for u_char def
#include <sys/time.h>
//#include <sys/wait.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>
#include "rsm.h"
#include "error.h"
#include "proto.h"
#include "seqio.h"

/*
 * This function works as follows:
 *     Type Description    Return
 *
 *     SYS  System error   -(ERRMLAST + ERRZLAST + errnum);
 *     INT  Internal error -(ERRMLAST + errnum);
 */
int getError(int type, int errnum)
{
    int err;

    switch (type) {
    case SYS:
        err = (ERRMLAST + ERRZLAST + errnum);
        break;

    case INT:
        err = (ERRMLAST + errnum);
        break;

    default:
        err = (ERRZ20 + ERRMLAST);
        break;
    }

    errno = 0;                                                                  // clear this
    return -err;
}

/*
 * This function notifies the process that a signal has been caught by setting
 * "partab.jobtab->attention" to 1. The process can then determine which
 * signal(s) have been caught by the bits in the mask "partab.jobtab->trap";
 * hence, if a bit has been set, then the signal corresponding to that bit has
 * been caught.
 */
void setSignalBitMask(int sig)
{
    if (sig == SIGQUIT) {                                                       // TODO: decide whether to keep hijacking SIGQUIT
        DoInfo();
    /*
    } else if (sig == SIGCHLD) {                                                // TODO: look at this more
        int mask;

        wait(&mask);
    */
    } else {
        int mask;

        partab.jobtab->attention = 1;
        mask = 1U << sig;
        partab.jobtab->trap |= mask;
    }

    return;
}

/*
 * This function will return 0 when the object referenced by the descriptor
 * "sid" is ready for reading or writing (as determined by "type").
 * Otherwise, a negative integer is returned to indicate the error that has
 * occurred.
 */
int seqioSelect(int sid, int type, int tout)
{
    int            nfds;
    fd_set         fds;
    int            ret;
    struct timeval timeout;

    nfds = sid + 1;
    FD_ZERO(&fds);
    FD_SET(sid, &fds);

    if (tout == 0) {
        timeout.tv_sec = 0;
        timeout.tv_usec = 0;

        if (type == FDRD) {
            ret = select(nfds, &fds, NULL, NULL, &timeout);
        } else {
            ret = select(nfds, NULL, &fds, NULL, &timeout);
        }
    } else {
        // Time out handled by alarm(), where no alarm() would be set if "tout" was -1
        if (type == FDRD) {
            ret = select(nfds, &fds, NULL, NULL, NULL);
        } else {
            ret = select(nfds, NULL, &fds, NULL, NULL);
        }
    }

    // An error has occurred (possibly timed out by alarm())
    if (ret == -1) {
        return getError(SYS, errno);
    } else if (ret == 0) {
        if (raise(SIGALRM)) return getError(SYS, errno);                        // Force select to time out
        return -1;
    } else {
        return 0;                                                               // Success
    }
}
