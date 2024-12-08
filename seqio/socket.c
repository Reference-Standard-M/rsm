/*
 * Package: Reference Standard M
 * File:    rsm/seqio/socket.c
 * Summary: module IO - sequential socket IO
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
 * operations for sockets:
 *
 *     SQ_Socket_Create  - Creates a socket
 *     SQ_Socket_Bind    - Binds server socket to a port
 *     SQ_Socket_Listen  - Listens for a connection(s) on server socket
 *     SQ_Socket_Accept  - Accepts a connection on server socket
 *     SQ_Socket_Connect - Connects client socket to a remote port
 *     SQ_Socket_Write   - Writes to socket
 *     SQ_Socket_Read    - Reads from socket
 */

#include "seqio.h"
#include "error.h"
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>

#define BACKLOG 20                                                              // Connections to queue (max is SOMAXCONN)

extern short proto_family;                                                      // PF_INET or PF_INET6
extern short addr_family;                                                       // AF_INET or AF_INET6
extern short sock_type;                                                         // SOCK_STREAM or SOCK_DGRAM
extern short sock_proto;                                                        // IPPROTO_TCP or IPPROTO_UDP

// Socket functions

/*
 * This function creates an endpoint for communication. If successful, it
 * returns a descriptor referencing the socket. Otherwise, a negative integer
 * value is returned to indicate the error that has occurred.
 *
 * NOTE: The socket is marked as non-blocking
 */
int SQ_Socket_Create(int nonblock)
{
    int sid;

    sid = socket(proto_family, sock_type, sock_proto);
    if (sid == -1) return getError(SYS, errno);

    if (nonblock && (sock_type == SOCK_STREAM)) {
        int flag = fcntl(sid, F_GETFL, 0);
        int ret;

        if (flag == -1) {
            close(sid);
            return getError(SYS, errno);
        }

        flag |= O_NONBLOCK;
        ret = fcntl(sid, F_SETFL, flag);

        if (ret == -1) {
            close(sid);
            return getError(SYS, errno);
        }
    }

    return sid;
}

/*
 * This function binds the socket "sid" to the port "port". If the bind is
 * successful, 0 is returned. Otherwise, a negative integer value is returned
 * to indicate the error that has occurred.
 */
int SQ_Socket_Bind(int sid, u_short port)
{
    int ret;
    int opt;
    int soid;

    opt = 1;
    soid = setsockopt(sid, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));
    if (soid == -1) return getError(SYS, errno);

    if (addr_family == AF_INET6) {
        struct sockaddr_in6 sin6;

        sin6.sin6_family = addr_family;
        sin6.sin6_port = htons(port);
        sin6.sin6_addr = in6addr_any;
        ret = bind(sid, (struct sockaddr *) &sin6, sizeof(sin6));
    } else {
        struct sockaddr_in sin;

        sin.sin_family = addr_family;
        sin.sin_port = htons(port);
        sin.sin_addr.s_addr = INADDR_ANY;
        ret = bind(sid, (struct sockaddr *) &sin, sizeof(sin));
    }

    if (ret == -1) return getError(SYS, errno);
    return 0;
}

/*
 * This function listens for a connection(s) on the socket "sid". If
 * successful, it returns 0. Otherwise, it returns a negative integer value to
 * indicate the error that has occurred.
 */
int SQ_Socket_Listen(int sid)
{
    int ret;

    if (sock_type == SOCK_DGRAM) return 0;                                      // No listen for connectionless UDP
    ret = listen(sid, BACKLOG);
    if (ret == -1) return getError(SYS, errno);
    return 0;
}

/*
 * This function accepts a connection on the socket "sid". If successful, it
 * returns a non-negative integer that is a descriptor for the accepted socket.
 * Otherwise, it returns a negative integer value to indicate the error that has
 * occurred.
 */
int SQ_Socket_Accept(int sid, int tout)
{
    int ret;
    int len;

    if (sock_type == SOCK_DGRAM) return 0;                                      // No accept for connectionless UDP
    ret = seqioSelect(sid, FDRD, tout);
    if (ret < 0) return ret;

    if (addr_family == AF_INET6) {
        struct sockaddr_in6 addr6;

        len = sizeof(struct sockaddr_in6);
        ret = accept(sid, (struct sockaddr *) &addr6, (socklen_t *) &len);
    } else {
        struct sockaddr_in addr;

        len = sizeof(struct sockaddr_in);
        ret = accept(sid, (struct sockaddr *) &addr, (socklen_t *) &len);
    }

    if (ret == -1) return getError(SYS, errno);
    return ret;
}

/*
 * This function attempts to make a connection from the socket end-point "sid",
 * to another end-point specified by the IP address "addr" and port "port". If
 * the connection succeeds, 0 is returned. Otherwise, a negative integer value
 * is returned to indicate the error that has occurred.
 */
int SQ_Socket_Connect(int sid, char *addr, u_short port)
{
    int ret;

    if (addr_family == AF_INET6) {
        struct sockaddr_in6 sin6;

        sin6.sin6_family = addr_family;
        sin6.sin6_port = htons(port);
        ret = inet_pton(addr_family, addr, &sin6.sin6_addr);
        if (ret == 0) return getError(INT, ERRZ48);
        ret = connect(sid, (struct sockaddr *) &sin6, sizeof(sin6));
    } else {
        struct sockaddr_in sin;

        sin.sin_family = addr_family;
        sin.sin_port = htons(port);
        ret = inet_pton(addr_family, addr, &sin.sin_addr);
        if (ret == 0) return getError(INT, ERRZ48);
        ret = connect(sid, (struct sockaddr *) &sin, sizeof(sin));
    }

    if (ret == -1) {
        if (errno == EINPROGRESS) {
            ret = seqioSelect(sid, FDWR, -1);
            if (ret < 0) return ret;
            return sid;
        }

        return getError(SYS, errno);
    }

    return sid;
}

/*
 * This function writes "nbytes" bytes from the buffer "writebuf" to the file
 * associated with the descriptor "sid". Upon successful completion, the number
 * of bytes actually written is returned. If the peer has disconnected, then
 * send will return -1 with errno set to EPIPE. Otherwise, a negative integer
 * is returned to indicate the error that has occurred.
 */
int SQ_Socket_Write(int sid, u_char *writebuf, int nbytes)
{
    int     ret;
    SQ_Chan *c = &partab.jobtab->seqio[partab.jobtab->io];

    if ((sock_type == SOCK_DGRAM) && (c->mode == NOFORK)) {
        char       *addrptr;
        char       *portptr;
        u_short    port;
        char       xxxx[100];
        const char *addr = (char *) c->s.name;

        strcpy(xxxx, addr);
        portptr = strpbrk(xxxx, " ");
        *portptr = '\0';
        addrptr = xxxx;
        portptr++;
        port = atoi(portptr);

        if (addr_family == AF_INET6) {
            struct sockaddr_in6 sin6;

            sin6.sin6_family = addr_family;
            sin6.sin6_port = htons(port);
            ret = inet_pton(addr_family, addrptr, &sin6.sin6_addr);
            if (ret == 0) return getError(INT, ERRZ48);
            ret = sendto(sid, writebuf, nbytes, 0, (struct sockaddr *) &sin6, sizeof(sin6));
        } else {
            struct sockaddr_in sin;

            sin.sin_family = addr_family;
            sin.sin_port = htons(port);
            ret = inet_pton(addr_family, addrptr, &sin.sin_addr);
            if (ret == 0) return getError(INT, ERRZ48);
            ret = sendto(sid, writebuf, nbytes, 0, (struct sockaddr *) &sin, sizeof(sin));
        }
    } else {
        ret = send(sid, writebuf, nbytes, 0);
    }

    if (ret == -1) {
        if (errno == EPIPE) {
            return getError(INT, ERRZ46);
        } else if (errno == EAGAIN) {
            return 0;
        } else {
            return getError(SYS, errno);
        }
    } else {
        return ret;
    }
}

int SQ_Socket_Read(int sid, u_char *readbuf, int tout)
{
    int ret;
    int len;

    if (tout != 0) {
        ret = seqioSelect(sid, FDRD, tout);
        if (ret < 0) return ret;
    }

    if (sock_type == SOCK_DGRAM) {
        if (addr_family == AF_INET6) {
            struct sockaddr_in6 sin6;

            ret = recvfrom(sid, readbuf, MAX_STR_LEN, 0, (struct sockaddr *) &sin6, (socklen_t *) &len);
        } else {
            struct sockaddr_in sin;

            ret = recvfrom(sid, readbuf, MAX_STR_LEN, 0, (struct sockaddr *) &sin, (socklen_t *) &len);
        }
    } else {
        ret = recv(sid, readbuf, 1, 0);
    }

    if (ret == -1) {
        if (errno == EAGAIN) {
            if (raise(SIGALRM)) return getError(SYS, errno);
        }

        return getError(SYS, errno);
    }

    return ret;
}
