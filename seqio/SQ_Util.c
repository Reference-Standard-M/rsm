/*
 * Package:  Reference Standard M
 * File:     rsm/seqio/SQ_Util.c
 * Summary:  module IO - sequential misc. I/O operations
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
 * This module implements the following miscellaneous sequential
 * input/output ( ie IO ) operations:
 *
 *	printBytestr	Prints the integer value of each element in a character
 *			array
 *	printSQChan	Prints out each field in the structure SQ_Chan, and
 *			selected fields in the structure jobtab
 *	getError	Returns a negative integer value ( which corresponds to
 *			a particular error message )
 *	setSignalBitMask
 *			Sets the bits of those signals that have been caught
 *	seqioSelect	Waits until an object is ready for reading or writing
 */

#include	<stdio.h>				// always include
#include 	<stdlib.h>                         	// these two
#include	<errno.h>
#include 	<sys/types.h>                      	// for u_char def
#include	<sys/time.h>
#include	<signal.h>
#include	<string.h>
#include	<unistd.h>
#include	"rsm.h"
#include	"error.h"
#include	"proto.h"
#include	"seqio.h"

// ************************************************************************* //
// This function works as follows:
//
//	"type"	Description	Return
//
//	SYS	System error	-( "errnum" + ERRMLAST + ERRZLAST );
//	INT	Internal error	-( "errnum" + ERRMLAST );

int getError(int type, int errnum)
{ int	err;

  switch (type)
  { case SYS:
      err = errnum + ERRMLAST + ERRZLAST;
      break;
    case INT:
      err = errnum + ERRMLAST;
      break;
    default:
      err = ERRZ20 + ERRMLAST;
      break;
  }
  return (-err);
}

// ************************************************************************* //
// This function notifies the process that a signal has been caught by setting
// "partab.jobtab->attention" to 1. The process can then determine which
// signal(s) have been caught by the bits in the mask "partab.jobtab->trap";
// hence, if a bit has been set, then the signal corresponding to that bit has
// been caught.

void setSignalBitMask(int sig)
{ int	mask;
  if (sig == SIGQUIT)
    DoInfo();
//  else if ( sig == SIGCHLD )
//    wait(&mask);
  else
  { partab.jobtab->attention = 1;
    mask = 1U << sig;
    partab.jobtab->trap |= mask;
  }
}

// ************************************************************************* //
// This function will return 0 when the object referenced by the descriptor
// "sid" is ready for reading or writing ( as determined by "type" ).
// Otherwise, a negative integer is returned to indicate the error that has
// occured.

int seqioSelect(int sid, int type, int tout)
{ int           	nfds;
  fd_set        	fds;
  int           	ret;
  struct timeval	timeout;

  nfds = sid + 1;
  FD_ZERO(&fds);
  FD_SET(sid, &fds);
  if (tout == 0)
  { timeout.tv_sec = 0;
    timeout.tv_usec = 0;
    if (type == FDRD) ret = select(nfds, &fds, NULL, NULL, &timeout);
    else ret = select(nfds, NULL, &fds, NULL, &timeout);
  }

  // Time out handled by alarm(), where no alarm() would be set if "tout"
  // was -1

  else
  { if (type == FDRD) ret = select(nfds, &fds, NULL, NULL, NULL);
    else ret = select(nfds, NULL, &fds, NULL, NULL);
  }

  // An error has occured ( possibly timed out by alarm() )

  if (ret == -1) return (getError(SYS, errno));
  else if (ret == 0)
  { ret = raise(SIGALRM);			// Force select to time out
    if (ret == -1) return (getError(SYS, errno));
    return (-1);
  }
  else return (0);				// Success
}
