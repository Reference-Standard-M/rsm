/*
 * Package:  Reference Standard M
 * File:     rsm/seqio/pipe.c
 * Summary:  module IO - sequential named pipe IO
 *
 * David Wicksell <dlw@linux.com>
 * Copyright © 2020-2021 Fourth Watch Software LC
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
 * This module implements the following sequential input/output (i.e., IO)
 * operations for named pipes (or FIFO):
 *
 *     SQ_Pipe_Open  - Opens FIFO for reading or writing
 *     SQ_Pipe_Write - Writes to FIFO
 *     SQ_Pipe_Read  - Reads from FIFO
 *     SQ_Pipe_Close - Closes (and/or removes) FIFO
 */

#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <fcntl.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>
#include "error.h"
#include "seqio.h"

int createPipe(char *pipe);

// Pipe functions

/*
 * This function opens the FIFO "pipe" for the specified operation "op". A
 * FIFO can be opened for one of two operations:
 *
 *    Operation - Description
 *    NEWPIPE   - Create and open FIFO for reading (does not block on open)
 *    PIPE      - Open pipe for writing
 *
 * If successful, this function returns a non-negative integer, termed a
 * descriptor. Otherwise, a negative integer value is returned to indicate the
 * error that has occurred.
 */
int SQ_Pipe_Open (char *pipe, int op)
{
    int ret;
    int flag;
    int pid;

    switch (op) {
    case NEWPIPE:
        ret = createPipe(pipe);
        if (ret < 0) return ret;
        flag = (O_RDONLY | O_NONBLOCK);
        break;

    case PIPE:
        flag = O_WRONLY;
        break;

    default:
        return getError(INT, ERRZ21);
    }

    pid = open(pipe, flag, 0);
    if (pid == -1) return getError(SYS, errno);
    return pid;
}

/*
 * This function closes the FIFO "pipe"; hence:
 *
 *   - deletes the descriptor "pid" from the per-process object reference table;
 *   - and removes the FIFO "pipe" itself.
 *
 * Upon successful completion, a value of 0 is returned. Otherwise, a negative
 * integer value is returned to indicate the error that has occurred.
 */
int SQ_Pipe_Close (int pid, char *pipe)
{
    int ret;
    int oid;

    ret = close(pid);
    if (ret == -1) return getError(SYS, errno);

    /*
     * Determine if there are any other readers on the pipe
     *
     * By closing the reader, and opening a writer on the pipe, if there does not
     * exist another reader on the pipe, an ENXIO error will occur; that is, the
     * named file is a character special or block special file, and the device
     * associated with this special file does not exist.
     */

    oid = open(pipe, (O_WRONLY | O_NONBLOCK), 0);

    if (oid == -1) {
        ret = unlink(pipe);
        if (ret == -1) return getError(SYS, errno);
    }

    return 0;
}

/*
 * This function writes "nbytes" bytes from the buffer "writebuf" to the FIFO
 * associated with the descriptor "pid". Upon successful completion, the number
 * of bytes actually written is returned. Otherwise, a negative integer value
 * is returned to indicate the error that has occurred.
 *
 * Note, if one tries to write to a pipe with no reader, a SIGPIPE signal is
 * generated. This signal is caught, where write will return -1 with errno set
 * to EPIPE (i.e., broken pipe).
 */
int SQ_Pipe_Write (int pid, u_char *writebuf, int nbytes)
{
    int ret;

    ret = write(pid, writebuf, nbytes);

    if (ret == -1) {
        if (errno == EPIPE) {
            return getError(INT, ERRZ46);
        } else {
            return getError(SYS, errno);
        }
    } else {
        return ret;
    }
}

/*
 * This function reads "nbytes" bytes into the buffer "readbuf" from the FIFO
 * associated with the descriptor "pid". Upon successful completion, the number
 * of bytes actually read is returned. Otherwise, a negative integer value is
 * returned to indicate the error that has occurred.
 */
int SQ_Pipe_Read (int pid, u_char *readbuf, int tout)
{
    int ret;
    int bytesread;

start:
    // Wait for input
    ret = seqioSelect(pid, FDRD, tout);
    if (ret < 0) return ret;

    // Read byte
    bytesread = read(pid, readbuf, 1);

    if ((bytesread == -1) && (errno == EAGAIN)) {                               // Resource temporarily unavailable
        sleep(1);
        goto start;
    }

    // An error has occurred
    if (bytesread == -1) {
        if (errno == EAGAIN) {                                                  // Resource temporarily unavailable
            sleep(1);                                                           // wait a bit
            errno = 0;                                                          // clear this
            goto start;                                                         // and try again
        }

        return getError(SYS, errno);                                            // EOF received
    } else if (bytesread == 0) {                                                // Force read to time out
        if (tout == 0) {
            ret = raise(SIGALRM);
            if (ret == -1) return getError(SYS, errno);
            return -1;
        }

        // Wait, then check for any data on the pipe
        sleep(1);
        return 0;
    } else {                                                                    // Return bytes read (i.e., 1)
        return 1;
    }
}

// Local functions

/*
 * This function creates a FIFO special file with the name "pipe". Upon
 * successful completion, a value of 0 is returned. Otherwise, a negative
 * integer value is returned to indicate the error that has occurred.
 */
int createPipe (char *pipe)
{
    int ret;

    ret = mkfifo(pipe, MODE);

    if (ret == -1) {
        return getError(SYS, errno);
    } else {
        return ret;
    }
}
