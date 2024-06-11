/*
 * Package: Reference Standard M
 * File:    rsm/seqio/tcpip.c
 * Summary: module IO - sequential TCP/IP socket IO
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

/*
 * Extended Summary:
 *
 * This module implements the following sequential I/O operations for TCP/IP
 * sockets:
 *
 *     SQ_Tcpip_Open   - Determines the type of socket to open
 *     SQ_Tcpip_Accept - Accepts a connection on a server socket
 *     SQ_Tcpip_Write  - Writes to a socket
 *     SQ_Tcpip_Read   - Reads from a socket
 */

#include <sys/types.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "error.h"
#include "seqio.h"

// Local functions

/*
 * This function opens a server socket end-point and binds it to the port
 * "bind". If successful, it returns a non-negative integer, termed a
 * descriptor. Otherwise, it returns a negative integer to indicate the error
 * that has occurred.
 */
int SQ_Tcpip_Open_Server(char *bind)
{
    int     sid;
    int     ret;
    u_short port;

    sid = SQ_Socket_Create(1);
    if (sid < 0) return sid;
    port = atoi(bind);
    ret = SQ_Socket_Bind(sid, port);

    if (ret < 0) {
        close(sid);
        return ret;
    }

    ret = SQ_Socket_Listen(sid);

    if (ret < 0) {
        close(sid);
        return ret;
    }

    return sid;
}

/*
 * This function opens a client socket end-point and connects it to the
 * port "conn". If successful, it returns a non-negative integer, termed a
 * descriptor. Otherwise, it returns a negative integer to indicate the error
 * that has occurred.
 */
int SQ_Tcpip_Open_Client(char *conn)
{
    int     sid;
    char    *portptr;
    char    *addrptr;
    u_short port;
    int     ret;
    char    xxxx[100];

    strcpy(xxxx, conn);
    sid = SQ_Socket_Create(0);
    if (sid < 0) return sid;
    portptr = strpbrk(xxxx, " ");

    if (portptr == NULL) {
        close(sid);
        return getError(INT, ERRZ28);
    }

    *portptr = '\0';
    addrptr = xxxx;
    portptr++;
    port = atoi(portptr);
    ret = SQ_Socket_Connect(sid, addrptr, port);

    if (ret < 0) {
        close(sid);
        return ret;
    }

    return sid;
}

// TCP/IP functions

/*
 * This function determines the type of socket to open. If it can not determine
 * the type of socket, a negative integer value is returned to indicate the
 * error that has occurred.
 */
int SQ_Tcpip_Open(char *bind, int op)
{
    switch (op) {
    case SERVER:
        return SQ_Tcpip_Open_Server(bind);

    case TCPIP:
        return SQ_Tcpip_Open_Client(bind);

    default:
        return getError(INT, ERRZ21);
    }
}

// Refer to function SQ_Socket_Accept in the file rsm/seqio/socket.c.
int SQ_Tcpip_Accept(int sid, int tout)
{
    return SQ_Socket_Accept(sid, tout);
}

// Refer to function SQ_Socket_Write in the file rsm/seqio/socket.c.
int SQ_Tcpip_Write(int sid, u_char *writebuf, int nbytes)
{
    return SQ_Socket_Write(sid, writebuf, nbytes);
}

// Refer to function SQ_Socket_Read in the file rsm/seqio/socket.c.
int SQ_Tcpip_Read(int sid, u_char *readbuf, int tout)
{
    return SQ_Socket_Read(sid, readbuf, tout);
}
