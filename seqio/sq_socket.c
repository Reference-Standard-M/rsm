/*
 * Package:  Reference Standard M
 * File:     rsm/seqio/sq_socket.c
 * Summary:  module IO - sequential socket I/O
 *
 * David Wicksell <dlw@linux.com>
 * Copyright © 2020 Fourth Watch Software LC
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
 * This module implements the following sequential input/output ( ie IO )
 * operations for sockets:
 *
 *      - SQ_Socket_Create	Creates a socket
 *      - SQ_Socket_Bind	Binds server socket to a port
 *      - SQ_Socket_Listen	Listens for a connection(s) on server socket
 *      - SQ_Socket_Accept	Accepts a connection on server socket
 *      - SQ_Socket_Connect	Connects client socket to a remote port
 *	- SQ_Socket_Write	Writes to socket
 *	- SQ_Socket_Read	Reads from socket
 */

#include	<errno.h>
#include	<sys/time.h>
#include	<sys/types.h>
#include	<sys/socket.h>
#include	<netinet/in.h>
#include	<arpa/inet.h>
#include	<fcntl.h>
#include	<signal.h>
#include	<string.h>
#include	<unistd.h>
#include	"error.h"
#include	"seqio.h"

#define		BACKLOG		3		// Connections to queue

int SQ_Socket_Create(int nonblock);
int SQ_Socket_Bind(int sid, u_short port);
int SQ_Socket_Listen(int sid);
int SQ_Socket_Accept(int sid, int tout);
int SQ_Socket_Connect(int sid, char *addr, u_short port);
int SQ_Socket_Write(int sid, u_char *writebuf, int nbytes);
int SQ_Socket_Read(int sid, u_char *readbuf, int tout);

// ************************************************************************* //
//									     //
// Socket functions							     //
//									     //
// ************************************************************************* //

// ************************************************************************* //
// This function creates an endpoint for communication. If successful, it
// returns a descriptor referencing the socket. Otherwise, a negative integer
// value is returned to indicate the error that has occurred.
//
// NOTE, that the socket is marked as non-blocking.

int SQ_Socket_Create(int nonblock)
{ int	sid;
  int	flag;
  int	ret;

  sid = socket(PF_INET, SOCK_STREAM, 0);
  if (sid == -1) return (getError(SYS, errno));

  if (nonblock)
  { flag = fcntl(sid, F_GETFL, 0);
    if (flag == -1)
    { (void) close(sid);
      return (getError(SYS, errno));
    }
    flag |= O_NONBLOCK;
    ret = fcntl(sid, F_SETFL, flag);
    if (ret == -1)
    { (void) close(sid);
      return (getError(SYS, errno));
    }
  }
  return (sid);
}

// ************************************************************************* //
// This function binds the socket "sid" to the port "port". If the bind is
// successful, 0 is returned. Otherwise, a negative integer value is returned
// to indicate the error that has occurred.

int SQ_Socket_Bind(int sid, u_short port)
{ int			ret;
  struct sockaddr_in	sin;

  sin.sin_family = AF_INET;
  sin.sin_port = htons(port);
  sin.sin_addr.s_addr = INADDR_ANY;
  ret = bind(sid, (struct sockaddr *) &sin, sizeof(sin));
  if (ret == -1) return (getError(SYS, errno));
  return (0);
}

// ************************************************************************* //
// This function listens for a connection(s) on the socket "sid". If
// successful, it returns 0. Otherwise, it returns a negative integer value to
// indicate the error that has occurred.

int SQ_Socket_Listen(int sid)
{ int	ret;

  ret = listen(sid, BACKLOG);
  if (ret == -1) return (getError(SYS, errno));
  return (0);
}

// ************************************************************************* //
// This function accepts a connection on the socket "sid". If successful, it
// returns a non-negative integer that is a descriptor for the accepted socket.
// Otherwise, it returns a negative integer value to indicate the error that has
// occurred.

int SQ_Socket_Accept(int sid, int tout)
{ int			ret;
  int			len;
  struct sockaddr_in	addr;

  ret = seqioSelect(sid, FDRD, tout);
  if (ret < 0) return (ret);
  len = sizeof(struct sockaddr_in);
  ret = accept(sid, (struct sockaddr *) &addr, (socklen_t *) &len);
  if (ret == -1) return (getError(SYS, errno));
  return (ret);
}

// ************************************************************************* //
// This function attempts to make a connection from the socket end-point "sid",
// to another end-point specified by the IP address "addr" and port "port". If
// the connection succeeds, 0 is returned. Otherwise, a negative integer value
// is returned to indicate the error that has occurred.

int SQ_Socket_Connect(int sid, char *addr, u_short port)
{ int			ret;
  struct in_addr	inaddr;
  struct sockaddr_in	sin;

  sin.sin_family = AF_INET;
  sin.sin_port = htons(port);
  ret = inet_aton(addr, &inaddr);
  if (ret == 0) return (getError(INT, ERRZ48));
  sin.sin_addr.s_addr = inaddr.s_addr;
  ret = connect(sid, (struct sockaddr *) &sin, sizeof(sin));
  if (ret == -1)
  { if (errno == EINPROGRESS)
    { ret = seqioSelect(sid, FDWR, -1);
      if (ret < 0) return (ret);
      return (sid);
    }
    return (getError(SYS, errno));
  }
  return (sid);
}

// ************************************************************************* //
// This function writes "nbytes" bytes from the buffer "writebuf" to the file
// associated with the descriptor "sid". Upon successful completion, the number
// of bytes actually written is returned. If the peer has disconnected, then
// send will return -1 with errno set to EPIPE. Otherwise, a negative integer
// is returned to indicate the error that has occurred.

int SQ_Socket_Write(int sid, u_char *writebuf, int nbytes)
{ int	ret;

  ret = send(sid, writebuf, nbytes, 0);
  if (ret == -1)
  { if (errno == EPIPE) return (getError(INT, ERRZ46));
    else if (errno == EAGAIN) return (0);
    else return (getError(SYS, errno));
  }
  else return (ret);
}

// ************************************************************************* //

int SQ_Socket_Read(int sid, u_char *readbuf, int tout)
{ int	ret;

  if (tout != 0)
  { ret = seqioSelect(sid, FDRD, tout);
    if (ret < 0) return (ret);
  }
  ret = recv(sid, readbuf, 1, 0);
  if (ret == -1)
  { if (errno == EAGAIN)
    { ret = raise(SIGALRM);
      if (ret == -1) return (getError(SYS, errno));
    }
    return (getError(SYS, errno));
  }
  else if (ret == 0)
    return (ret);
  else
    return (ret);
}
