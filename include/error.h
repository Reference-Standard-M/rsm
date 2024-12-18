/*
 * Package: Reference Standard M
 * File:    rsm/include/error.h
 * Summary: module RSM header file - error definitions
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
 * Errors returned by functions internally are minus one of the following:
 *   Specifically -1 = ERRM1
 *   Functions return  -ERRMn
 *                     -(ERRZn + ERRMLAST)
 *                     -(ERRMLAST + ERRZLAST + errno)
 *
 * USE: short UTIL_strerror(int err, u_char *buf) to return the error string
 *
 * Add ERRZn definitions to the end of this file and the text form to rsm/util/error.c
 */

#ifndef RSM_ERROR_H
#define RSM_ERROR_H

// The following are the MDC specific errors
#define ERRM1       1                                                           // Naked indicator undefined
#define ERRM2       2                                                           // Invalid $FNUMBER P code string combination
#define ERRM3       3                                                           // $RANDOM argument less than 1
#define ERRM4       4                                                           // No true condition in $SELECT
#define ERRM5       5                                                           // Line reference less than 0
#define ERRM6       6                                                           // Undefined local variable
#define ERRM7       7                                                           // Undefined global variable
#define ERRM8       8                                                           // Undefined special variable
#define ERRM9       9                                                           // Divide by zero
#define ERRM10      10                                                          // Invalid pattern match range
#define ERRM11      11                                                          // No parameters passed
#define ERRM12      12                                                          // Invalid line reference (negative offset)
#define ERRM13      13                                                          // Invalid line reference (line not found)
#define ERRM14      14                                                          // Line level not one
#define ERRM15      15                                                          // Undefined index variable
#define ERRM16      16                                                          // QUIT with an argument not allowed
#define ERRM17      17                                                          // QUIT with an argument required
#define ERRM18      18                                                          // Fixed length READ not greater than 0
#define ERRM19      19                                                          // Cannot merge a tree or subtree into itself
#define ERRM20      20                                                          // Line must have a formal list
#define ERRM21      21                                                          // Formal list name duplication
#define ERRM22      22                                                          // SET or KILL to ^$GLOBAL when data in global
#define ERRM23      23                                                          // SET or KILL to ^$JOB for non-existent job
#define ERRM25      25                                                          // Attempt to modify currently executing routine
#define ERRM26      26                                                          // Non-existent environment
#define ERRM28      28                                                          // Mathematical function, parameter out of range
#define ERRM29      29                                                          // SET or KILL on SSVN not allowed by implementation
#define ERRM33      33                                                          // SET or KILL to ^$ROUTINE when routine exists
#define ERRM35      35                                                          // Device does not support mnemonicspace
#define ERRM36      36                                                          // Incompatible mnemonicspaces
#define ERRM37      37                                                          // READ from device identified by the empty string
#define ERRM38      38                                                          // Invalid SSVN subscript
#define ERRM39      39                                                          // Invalid $NAME argument
#define ERRM40      40                                                          // Call-by-reference in JOB actual
#define ERRM43      43                                                          // Invalid range ($X, $Y)
#define ERRM45      45                                                          // Invalid GOTO reference
#define ERRM46      46                                                          // Invalid attribute name
#define ERRM47      47                                                          // Invalid attribute value
#define ERRM56      56                                                          // Name length exceeds implementation's limit
#define ERRM57      57                                                          // More than one defining occurrence
                                                                                //   of label in routine
#define ERRM58      58                                                          // Too few formal parameters
#define ERRM59      59                                                          // Environment reference not permitted for this SSVN
#define ERRM60      60                                                          // Undefined SSVN
#define ERRM75      75                                                          // String length exceeds implementation's limit
#define ERRM92      92                                                          // Mathematical overflow
#define ERRM93      93                                                          // Mathematical underflow
#define ERRM99      99                                                          // Invalid operation for context
#define ERRM101     101                                                         // Attempt to assign incorrect value to $ECODE
#define ERRMLAST    200                                                         // Must equal last MDC assigned error
// The following are the implementation specific errors
#define ERRZ1       1                                                           // Subscript too long (max 127)
#define ERRZ2       2                                                           // Key too long (max 255)
#define ERRZ3       3                                                           // Error in key
#define ERRZ4       4                                                           // Error in database create
#define ERRZ5       5                                                           // Null character not permitted in key
#define ERRZ6       6                                                           // Error when reading from database file
#define ERRZ7       7                                                           // DO stack overflow
#define ERRZ8       8                                                           // String stack overflow
#define ERRZ9       9                                                           // Invalid BREAK parameter
#define ERRZ10      10                                                          // String stack underflow
#define ERRZ11      11                                                          // Database file is full cannot SET
#define ERRZ12      12                                                          // Expression syntax error
#define ERRZ13      13                                                          // Command syntax error
#define ERRZ14      14                                                          // Unknown opcode encountered
#define ERRZ15      15                                                          // Too many subscripts
#define ERRZ16      16                                                          // Null subscript
#define ERRZ17      17                                                          // Too many IF commands in one line (max 256)
#define ERRZ18      18                                                          // Unknown external routine
#define ERRZ19      19                                                          // Too many nested FOR commands (max 256)
// The following are the sequential IO implementation specific errors
#define ERRZ20      20                                                          // IO: Unknown internal error
#define ERRZ21      21                                                          // IO: Unrecognised operation
#define ERRZ22      22                                                          // IO: Timeout < -1
#define ERRZ23      23                                                          // IO: Operation timed out
#define ERRZ24      24                                                          // IO: Device not supported
#define ERRZ25      25                                                          // IO: Channel out of range
#define ERRZ26      26                                                          // IO: Channel not free
#define ERRZ27      27                                                          // IO: Channel free
#define ERRZ28      28                                                          // IO: Unexpected NULL value
#define ERRZ29      29                                                          // IO: Can not determine object from operation
#define ERRZ30      30                                                          // IO: Unrecognised object
#define ERRZ31      31                                                          // IO: Set bit flag out of range
#define ERRZ33      33                                                          // IO: Number of bytes for buffer out of range
#define ERRZ34      34                                                          // IO: ASCII character expected
#define ERRZ35      35                                                          // IO: Unrecognised mode
#define ERRZ36      36                                                          // IO: Maximum bytes to read < -1
#define ERRZ37      37                                                          // IO: Read buffer size exceeded
#define ERRZ38      38                                                          // IO: End of file has been reached
#define ERRZ39      39                                                          // IO: $KEY too long
#define ERRZ40      40                                                          // IO: Bytes to write < 0
#define ERRZ41      41                                                          // IO: Write format specifier < -2
#define ERRZ42      42                                                          // IO: Maximum number of jobs could be exceeded
#define ERRZ43      43                                                          // IO: Device not found or a character special dev
#define ERRZ44      44                                                          // IO: Printf failed
#define ERRZ45      45                                                          // IO: Unsigned integer value expected
#define ERRZ46      46                                                          // IO: Peer has disconnected
#define ERRZ47      47                                                          // IO: Peer not connected
#define ERRZ48      48                                                          // IO: Invalid Internet address
// More implementation specific errors
#define ERRZ49      49                                                          // Job table is full
#define ERRZ50      50                                                          // Invalid argument to $STACK()
#define ERRZ51      51                                                          // Interrupt - Control-C Received
#define ERRZ52      52                                                          // Insufficient space to load routine
#define ERRZ53      53                                                          // Too many tags (max 256)
#define ERRZ54      54                                                          // Too many lines in routine (max 65534)
#define ERRZ55      55                                                          // End of linked data reached
#define ERRZ56      56                                                          // Symbol table full
#define ERRZ57      57                                                          // Invalid name indirection
#define ERRZ58      58                                                          // Too many levels of indirection
#define ERRZ59      59                                                          // Routine version mismatch - please recompile
#define ERRZ60      60                                                          // Insufficient global buffer space
#define ERRZ61      61                                                          // Database integrity violation found
#define ERRZ62      62                                                          // Cannot create global - global directory full
#define ERRZ63      63                                                          // Error in VIEW arguments
#define ERRZ64      64                                                          // Parameter out of range
#define ERRZ65      65                                                          // Duplicate tag in routine
#define ERRZ66      66                                                          // HUP signal received
#define ERRZ67      67                                                          // USR1 signal received
#define ERRZ68      68                                                          // USR2 signal received
#define ERRZ69      69                                                          // Unknown signal received
#define ERRZ70      70                                                          // Offset not permitted in entryref
#define ERRZ71      71                                                          // No such host is known
#define ERRZ72      72                                                          // Type h_errno error has occurred
#define ERRZ73      73                                                          // Invalid database file specified
#define ERRZ74      74                                                          // Too many variables (max 255)
#define ERRZ75      75                                                          // Too many arguments (max 127)
#define ERRZ77      77                                                          // RSM is in restricted mode
#define ERRZ78      78                                                          // Lock count exceeds implementation's limit
#define ERRZLAST    200                                                         // Must equal last implementation error
// Database dummy errors
#define USRERR      -9999                                                       // they SET $ECODE="U..."

#endif
