/*
 * Package: Reference Standard M
 * File:    rsm/seqio/file.c
 * Summary: module IO - sequential file IO
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
 *
 *
 * Extended Summary:
 *
 * This module implements the following sequential input/output (i.e., IO)
 * operations for files:
 *
 *     SQ_File_Open  - Opens a file for read, write or append mode
 *     SQ_File_Write - Writes to file
 *     SQ_File_Read  - Reads from file
 */

#include "seqio.h"
#include "error.h"
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>

// File functions

/*
 * This function opens a sequential file "file" for the specified operation
 * "op" (i.e., writing, reading or appending). If successful, it returns a
 * non-negative integer, termed a file descriptor. Otherwise, a negative
 * integer is returned to indicate the error that has occurred.
 */
int SQ_File_Open(char *file, int op)
{
    int flag;
    int fid;

    switch (op) {
    case WRITE:
        flag = O_WRONLY | O_TRUNC | O_CREAT;
        break;

    case READ:
        flag = O_RDONLY;
        break;

    case APPEND:
        flag = O_WRONLY | O_APPEND | O_CREAT;
        break;

    case IO:
        flag = O_RDWR | O_CREAT;
        break;

    default:
        return getError(INT, ERRZ21);
    }

    /*
     * I am assuming that MODE will always be ignored, except when the file does
     * not exist, and "op" is either WRITE, APPEND, or IO.
     */

    fid = open(file, flag, MODE);
    if (fid == -1) return getError(SYS, errno);
    return fid;
}

/*
 * This function writes "nbytes" bytes from the buffer "writebuf" to the file
 * associated with the descriptor "fid". Upon successful completion, the number
 * of bytes actually written is returned. Otherwise, a negative integer is
 * returned to indicate the error that has occurred.
 */
int SQ_File_Write(int fid, u_char *writebuf, int nbytes)
{
    int ret;

    ret = write(fid, writebuf, nbytes);
    if (ret == -1) return getError(SYS, errno);
    return ret;
}

/*
 * This function reads "nbytes" bytes into the buffer "readbuf" from the file
 * associated with the descriptor "fid". If successful, the number of bytes
 * actually read is returned. Otherwise, a negative integer is returned to
 * indicate the error that has occurred.
 */
int SQ_File_Read(int fid, u_char *readbuf)
{
    int ret;

    ret = read(fid, readbuf, 1);

    if (ret == -1) {
        return getError(SYS, errno);
    } else if (ret == 0) {                                                      // EOF received
        if (strcmp((char *) partab.jobtab->seqio[fid].name, "Not a tty") == 0) { // stdin was probably redirected from a file
            partab.jobtab->trap |= SIG_QUIT;                                    // don't set partab.jobtab->attention
        }
    }

    return ret;
}
