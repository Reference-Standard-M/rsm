/*
 * Package:  Reference Standard M
 * File:     rsm/include/seqio.h
 * Summary:  This module declares all the global constants and functions
 *           used only by sequential Input/Output (IO) operations.
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
 */

#ifndef _RSM_SEQIO_H_				// only do this once
#define _RSM_SEQIO_H_

#include	<sys/types.h>
#include	"rsm.h"

// ************************************************************************* //
//									     //
// Global constants							     //
//									     //
// ************************************************************************* //

// IO operations

#define		WRITE		1		// Write only
#define		READ		2		// Read only
#define		APPEND		3		// Append
#define		IO		4		// Write/Read
#define		TCPIP		5		// Client socket
#define		SERVER		6		// Forking server socket
#define		NOFORK		7		// Non-forking server socket
#define		FORKED		8		// Forked server socket client
#define		PIPE		9		// Named pipe (writing)
#define		NEWPIPE		10		// Named pipe (reading)

// Signal operations

#define		CATCH		0		// Catch signal
#define		IGNORE		1		// Ignore signal

// Types of errors

#define		SYS		0		// System error
#define		INT		1		// Internal error

// File/Pipe permissions

#define		MODE		S_IRUSR|S_IWUSR|S_IRGRP|S_IROTH

// Select operations

#define		FDRD		0		// Ready to read on object
#define		FDWR		1		// Ready to write on object

// ************************************************************************* //
//									     //
// Global function declarations						     //
//									     //
// ************************************************************************* //

void setSignalBitMask(int sig);
int setSignal(int sig, int flag);
int setSignals(void);
void printBytestr(char *bytestr, int nbytes);
void printSQChan(jobtab *jobptr, SQ_Chan *chanptr);
int seqioSelect(int sid, int type, int tout);
int getError(int type, int errnum);
int SQ_File_Open(char *file, int op);
int SQ_File_Write(int fid, u_char *writebuf, int nbytes);
int SQ_File_Read(int fid, u_char *readbuf);
int SQ_Device_Open(char *device, int op);
int SQ_Device_Write(int did, u_char *writebuf, int nbytes);
int SQ_Device_Read(int did, u_char *readbuf, int tout);
int SQ_Pipe_Open(char *pipe, int op);
int SQ_Pipe_Close(int pid, char *pipe);
int SQ_Pipe_Read(int pid, u_char *readbuf, int tout);
int SQ_Pipe_Write(int pid, u_char *writebuf, int nbytes);
int SQ_Tcpip_Open(char *bind, int op);
int SQ_Tcpip_Write(int sid, u_char *writebuf, int nbytes);
int SQ_Tcpip_Accept(int sid, int tout);
int SQ_Tcpip_Read(int sid, u_char *readbuf, int tout);

#endif						// _RSM_SEQIO_H_
