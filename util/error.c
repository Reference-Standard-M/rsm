/*
 * Package: Reference Standard M
 * File:    rsm/util/error.c
 * Summary: module RSM error - return full name of error
 *
 * David Wicksell <dlw@linux.com>
 * Copyright © 2020-2024 Fourth Watch Software LC
 * https://gitlab.com/Reference-Standard-M/rsm
 *
 * Based on MUMPS V1 by Raymond Douglas Newman
 * Copyright © 1999-2016
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
 * Errors returned by functions internally are minus one of the following
 * Specifically -1 = ERRM1
 * Functions return: -ERRMn
 *                   -(ERRZn + ERRMLAST)
 *                   -(ERRMLAST + ERRZLAST + errno)
 *
 * Use:  u_short UTIL_strerror(int err, u_char *buf)  to return the error string
 */

#include <stdio.h>                                                              // always include
#include <stdlib.h>                                                             // these two
#include <sys/types.h>                                                          // for u_char def
#include <string.h>                                                             // for strerror
#include <unistd.h>
#include <time.h>                                                               // for ctime
#include <errno.h>                                                              // error stuff
#include "rsm.h"                                                                // standard includes
#include "proto.h"                                                              // standard includes
#include "error.h"                                                              // M errors

static struct {
    int                   err;
    char                  *msg;
} merrtab[] = {
    // MDC standard errors
    {0,                   "No error"},
    {ERRM1,               "Naked indicator undefined"},
    {ERRM2,               "Invalid $FNUMBER P code string combination"},
    {ERRM3,               "$RANDOM argument less than 1"},
    {ERRM4,               "No true condition in $SELECT"},
    {ERRM5,               "Line reference less than 0"},
    {ERRM6,               "Undefined local variable"},
    {ERRM7,               "Undefined global variable"},
    {ERRM8,               "Undefined special variable"},
    {ERRM9,               "Divide by zero"},
    {ERRM10,              "Invalid pattern match range"},
    {ERRM11,              "No parameters passed"},
    {ERRM12,              "Invalid line reference (negative offset)"},
    {ERRM13,              "Invalid line reference (line not found)"},
    {ERRM14,              "Line level not one"},
    {ERRM15,              "Undefined index variable"},
    {ERRM16,              "QUIT with an argument not allowed"},
    {ERRM17,              "QUIT with an argument required"},
    {ERRM18,              "Fixed length READ not greater than 0"},
    {ERRM19,              "Cannot merge a tree or subtree into itself"},
    {ERRM20,              "Line must have a formal list"},
    {ERRM21,              "Formal list name duplication"},
    {ERRM22,              "SET or KILL to ^$GLOBAL when data in global"},
    {ERRM23,              "SET or KILL to ^$JOB for non-existent job"},
    {ERRM25,              "Attempt to modify currently executing routine"},
    {ERRM26,              "Non-existent environment"},
    {ERRM28,              "Mathematical function, parameter out of range"},
    {ERRM29,              "SET or KILL on SSVN not allowed by implementation"},
    {ERRM33,              "SET or KILL to ^$ROUTINE when routine exists"},
    {ERRM35,              "Device does not support mnemonicspace"},
    {ERRM36,              "Incompatible mnemonicspaces"},
    {ERRM37,              "READ from device identified by the empty string"},
    {ERRM38,              "Invalid SSVN subscript"},
    {ERRM39,              "Invalid $NAME argument"},
    {ERRM40,              "Call-by-reference in JOB actual"},
    {ERRM43,              "Invalid range ($X, $Y)"},
    {ERRM45,              "Invalid GOTO reference"},
    {ERRM46,              "Invalid attribute name"},
    {ERRM47,              "Invalid attribute value"},
    {ERRM56,              "Name length exceeds implementation's limit"},
    {ERRM57,              "More than one defining occurrence of label in routine"},
    {ERRM58,              "Too few formal parameters"},
    {ERRM59,              "Environment reference not permitted for this SSVN"},
    {ERRM60,              "Undefined SSVN"},
    {ERRM75,              "String length exceeds implementation's limit"},
    {ERRM92,              "Mathematical overflow"},
    {ERRM93,              "Mathematical underflow"},
    {ERRM99,              "Invalid operation for context"},
    {ERRM101,             "Attempt to assign incorrect value to $ECODE"},
    // The following are the implementation specific errors
    {(ERRZ1 + ERRMLAST),  "Subscript too long (max " RSM_STRING(MAX_SUB_LEN) ")"},
    {(ERRZ2 + ERRMLAST),  "Key too long (max " RSM_STRING(MAX_KEY_SIZE) ")"},
    {(ERRZ3 + ERRMLAST),  "Error in key"},
    {(ERRZ4 + ERRMLAST),  "Error in database create "},
    {(ERRZ5 + ERRMLAST),  "Null character not permitted in key"},
    {(ERRZ6 + ERRMLAST),  "Error when reading from database file"},
    {(ERRZ7 + ERRMLAST),  "DO stack overflow (max " RSM_STRING(MAX_DO_FRAMES) ")"},
    {(ERRZ8 + ERRMLAST),  "String stack overflow"},
    {(ERRZ9 + ERRMLAST),  "Invalid BREAK parameter"},
    {(ERRZ10 + ERRMLAST), "String stack underflow"},
    {(ERRZ11 + ERRMLAST), "Database file is full cannot SET"},
    {(ERRZ12 + ERRMLAST), "Expression syntax error"},
    {(ERRZ13 + ERRMLAST), "Command syntax error"},
    {(ERRZ14 + ERRMLAST), "Unknown opcode encountered"},
    {(ERRZ15 + ERRMLAST), "Too many subscripts (max " RSM_STRING(MAX_NUM_SUBS) ")"},
    {(ERRZ16 + ERRMLAST), "Null subscript"},
    {(ERRZ17 + ERRMLAST), "Too many IF commands in one line (max 256)"},
    {(ERRZ18 + ERRMLAST), "Unknown external routine"},
    {(ERRZ19 + ERRMLAST), "Too many nested FOR commands (max 256)"},
    // The following are the sequential IO implementation specific errors
    {(ERRZ20 + ERRMLAST), "IO: Unknown internal error"},
    {(ERRZ21 + ERRMLAST), "IO: Unrecognised operation"},
    {(ERRZ22 + ERRMLAST), "IO: Timeout < -1"},
    {(ERRZ23 + ERRMLAST), "IO: Operation timed out"},
    {(ERRZ24 + ERRMLAST), "IO: Device not supported"},
    {(ERRZ25 + ERRMLAST), "IO: Channel out of range"},
    {(ERRZ26 + ERRMLAST), "IO: Channel not free"},
    {(ERRZ27 + ERRMLAST), "IO: Channel free"},
    {(ERRZ28 + ERRMLAST), "IO: Unexpected NULL value"},
    {(ERRZ29 + ERRMLAST), "IO: Can not determine object from operation"},
    {(ERRZ30 + ERRMLAST), "IO: Unrecognised object"},
    {(ERRZ31 + ERRMLAST), "IO: Set bit flag out of range"},
    {(ERRZ33 + ERRMLAST), "IO: Number of bytes for buffer out of range"},
    {(ERRZ34 + ERRMLAST), "IO: ASCII character expected"},
    {(ERRZ35 + ERRMLAST), "IO: Unrecognised mode"},
    {(ERRZ36 + ERRMLAST), "IO: Maximum bytes to read < -1"},
    {(ERRZ37 + ERRMLAST), "IO: Read buffer size exceeded"},
    {(ERRZ38 + ERRMLAST), "IO: End of file has been reached"},
    {(ERRZ39 + ERRMLAST), "IO: $KEY too long"},
    {(ERRZ40 + ERRMLAST), "IO: Bytes to write < 0"},
    {(ERRZ41 + ERRMLAST), "IO: Write format specifier < -2"},
    {(ERRZ42 + ERRMLAST), "IO: Maximum number of jobs could be exceeded"},
    {(ERRZ43 + ERRMLAST), "IO: Device not found or a character special device"},
    {(ERRZ44 + ERRMLAST), "IO: Printf failed"},
    {(ERRZ45 + ERRMLAST), "IO: Unsigned integer value expected"},
    {(ERRZ46 + ERRMLAST), "IO: Peer has disconnected"},
    {(ERRZ47 + ERRMLAST), "IO: No peer connected"},
    {(ERRZ48 + ERRMLAST), "IO: Invalid internet address"},
    // The following are more implementation specific errors
    {(ERRZ49 + ERRMLAST), "Job table is full"},
    {(ERRZ50 + ERRMLAST), "Invalid argument to $STACK()"},
    {(ERRZ51 + ERRMLAST), "Interrupt - Control-C Received"},
    {(ERRZ52 + ERRMLAST), "Insufficient space to load routine"},
    {(ERRZ53 + ERRMLAST), "Too many tags (max " RSM_STRING(MAX_NUM_TAGS) ")"},
    {(ERRZ54 + ERRMLAST), "Too many lines in routine (max " RSM_STRING(MAXROULINE) ")"},
    {(ERRZ55 + ERRMLAST), "End of linked data reached"},
    {(ERRZ56 + ERRMLAST), "Symbol table full"},
    {(ERRZ57 + ERRMLAST), "Invalid name indirection"},
    {(ERRZ58 + ERRMLAST), "Too many levels of indirection"},
    {(ERRZ59 + ERRMLAST), "Routine version mismatch - please recompile"},
    {(ERRZ60 + ERRMLAST), "Insufficient global buffer space"},
    {(ERRZ61 + ERRMLAST), "Database integrity violation found"},
    {(ERRZ62 + ERRMLAST), "Cannot create global - global directory full"},
    {(ERRZ63 + ERRMLAST), "Error in VIEW arguments"},
    {(ERRZ64 + ERRMLAST), "Parameter out of range"},
    {(ERRZ65 + ERRMLAST), "Duplicate tag in routine"},
    {(ERRZ66 + ERRMLAST), "HUP signal received"},
    {(ERRZ67 + ERRMLAST), "USR1 signal received"},
    {(ERRZ68 + ERRMLAST), "USR2 signal received"},
    {(ERRZ69 + ERRMLAST), "Unknown signal received"},
    {(ERRZ70 + ERRMLAST), "Offset not permitted in entryref"},
    {(ERRZ71 + ERRMLAST), "No such host is known"},
    {(ERRZ72 + ERRMLAST), "Type h_errno error has occurred"},
    {(ERRZ73 + ERRMLAST), "Invalid database file specified"},
    {(ERRZ74 + ERRMLAST), "Too many variables (max " RSM_STRING(MAX_NUM_VARS) ")"},
    {(ERRZ75 + ERRMLAST), "Too many arguments (max " RSM_STRING(MAX_NUM_ARGS) ")"},
    {(ERRZ77 + ERRMLAST), "RSM is in restricted mode"},
    {(ERRZ78 + ERRMLAST), "Lock count exceeds implementation's limit"},
    {0,                   NULL}
};                                                                              // merrtab[]

u_short UTIL_strerror(int err, u_char *buf)                                     // return string form
{
    u_char       none[] = {"No such error number"};                             // invalid
    const u_char *ptr;                                                          // pointer to msg

    ptr = none;                                                                 // default to none
    if (err < 0) err = -err;                                                    // ensure err +ve

    if (err > (ERRMLAST + ERRZLAST)) {                                          // check for errno
        ptr = (u_char *) strerror(err - (ERRMLAST + ERRZLAST));                 // get error msg
    } else {
        for (int i = 0; merrtab[i].msg != NULL; i++) {                          // search our table
            if (merrtab[i].err == err) {                                        // if found
                ptr = (u_char *) merrtab[i].msg;                                // set ptr
                break;                                                          // and exit
            }
        }
    }                                                                           // end our error

    strcpy((char *) buf, (char *) ptr);                                         // copy the message
    return (u_short) strlen((char *) ptr);                                      // and return length
}

void panic(const char *msg)                                                     // print msg and exit
{
    const FILE *a;                                                              // for freopen
    char       tmp[512];                                                        // some string space
    time_t     t;                                                               // for time

    fprintf(stderr, "\r\nFATAL RSM ERROR occurred!!\r\n%s\r\n", msg);           // print
    if (errno) fprintf(stderr, "errno = %d - %s\r\n", errno, strerror(errno));
    fflush(stderr);
    a = freopen("RSM_CRASH", "a", stderr);                                      // redirect stderr

    if (a != NULL) {                                                            // if that worked
        t = current_time(FALSE);                                                // current time
        fprintf(stderr, "RSM CRASH OCCURRED on %s", ctime(&t));                 // output the time
        rsm_version((u_char *) tmp);
        fprintf(stderr, "%s", tmp);
        fprintf(stderr, "\nFATAL RSM ERROR occurred - PID %ld!!\n%s\n", (long) getpid(), msg); // print
        if (errno) fprintf(stderr, "errno = %d - %s\n", errno, strerror(errno));

        if (partab.jobtab != NULL) {                                            // if not a daemon
            int j = partab.jobtab->cur_do;                                      // get current do

            fprintf(stderr, "Job Number: %d\n", ((int) (partab.jobtab - partab.job_table) + 1));

            for (int i = 0; i < VAR_LEN; i++) {
                tmp[i] = partab.jobtab->dostk[j].rounam.var_cu[i];              // copy it
            }

            tmp[VAR_LEN] = '\0';                                                // and terminate
            fprintf(stderr, "UCI: %d  Routine: %s  Line: %d\n", partab.jobtab->dostk[j].uci, tmp, partab.jobtab->dostk[j].line_num);

            if (partab.jobtab->last_ref.name.var_cu[0]) {                       // if there is a $REF
                UTIL_String_Mvar(&partab.jobtab->last_ref, (u_char *) &tmp[0], MAX_NUM_SUBS); // get in string form
                fprintf(stderr, "Last Global: %s\n", tmp);                      // print it
            }

            // more status stuff to go here
        }                                                                       // end not a daemon

        fprintf(stderr, "----------------------------------------------------------------------------\n");
        fflush(stderr);
    }

    abort();                                                                    // and exit, generating a core dump - status 134
}
