/*
 * Package:  Reference Standard M
 * File:     rsm/seqio/device.c
 * Summary:  module IO - sequential device IO
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
 * operations for devices:
 *
 *     SQ_Device_Open  - Opens a file for read, write or read/write mode
 *     SQ_Device_Write - Writes to a device
 *     SQ_Device_Read  - Determines the type of device to read from
 */

#include <errno.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <fcntl.h>
#include <signal.h>
#include <termios.h>
#include <unistd.h>
#include "error.h"
#include "seqio.h"

int SQ_Device_Read_TTY(int fid, u_char *buf, int tout);

// Device functions

/*
 * This function opens a device "device" for the specified operation "op" (ie
 * writing, reading or reading and/or writing). If successful, it returns a
 * non-negative integer, termed a descriptor. Otherwise, it returns a negative
 * integer to indicate the error that has occurred.
 */
int SQ_Device_Open (char *device, int op)
{
    int flag;
    int did;

    switch (op) {
    case WRITE:
        flag = O_WRONLY;
        break;

    case READ:
        flag = O_RDONLY;
        break;

    case IO:
        flag = O_RDWR;
        break;

    default:
        return getError(INT, ERRZ21);
    }

    // If device is busy, keep trying until a timeout (i.e., alarm signal) has been received
    while (TRUE) {
        did = open(device, flag, 0);

        if (did == -1) {
            if (errno != EBUSY) {
                return getError(SYS, errno);
            } else if (partab.jobtab->trap & 16384) {                           // MASK[SIGALRM]
                return -1;
            }
        } else {
            return did;
        }
    }                                                                           // end while (TRUE)

    return getError(INT, ERRZ20);
}

/*
 * This function writes "nbytes" bytes from the buffer "writebuf" to the device
 * associated with the descriptor "did". Upon successful completion, the number
 * of bytes actually written is returned. Otherwise, it returns a negative
 * integer to indicate the error that has occurred.
 */
int SQ_Device_Write(int did, u_char *writebuf, int nbytes)
{
    int ret;

    ret = write(did, writebuf, nbytes);
    if (ret == -1) return getError(SYS, errno);
    return ret;
}

/*
 * This function determines the type of device to read from. If it can not
 * determine the type of device, a negative integer value is returned to
 * indicate the error that has occurred.
 *
 * Note, support is only implemented for terminal type devices.
 */
int SQ_Device_Read(int did, u_char *readbuf, int tout)
{
    int ret;

    ret = isatty(did);
    if (ret == 1) {
        return SQ_Device_Read_TTY(did, readbuf, tout);
    } else {
        return getError(INT, ERRZ24);
    }
}

// Local functions

/*
 * This function reads at most one character from the device associated with
 * the descriptor "did" into the buffer "readbuf". A pending read is not
 * satisfied until one byte or a signal has been received. Upon successful
 * completion, the number of bytes actually read is returned. Otherwise, a
 * negative integer is returned to indicate the error that has occurred.
 */
int SQ_Device_Read_TTY(int did, u_char *readbuf, int tout)
{
    struct termios settings;
    int            ret;
    int            rret;

    if (tout == 0) {
        ret = tcgetattr(did, &settings);
        if (ret == -1) return getError(SYS, errno);
        settings.c_cc[VMIN] = 0;
        ret = tcsetattr(did, TCSANOW, &settings);
        if (ret == -1) return getError(SYS, errno);
    }

    rret = read(did, readbuf, 1);

    if (tout == 0) {
        ret = tcgetattr(did, &settings);
        if (ret == -1) return getError(SYS, errno);
        settings.c_cc[VMIN] = 1;
        ret = tcsetattr(did, TCSANOW, &settings);
        if (ret == -1) return getError(SYS, errno);

        if (rret == 0) {                                                        // zero timeout and no chars
            partab.jobtab->trap |= 16384;                                       // MASK[SIGALRM]
            return -1;
        }
    }

    if (rret == -1) {
        if (errno == EAGAIN) {
            ret = raise(SIGALRM);
            if (ret == -1) return getError(SYS, errno);
        }

        return getError(SYS, errno);
    } else {
        return rret;
    }
}
