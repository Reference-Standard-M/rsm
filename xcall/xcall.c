/*
 * Package:  Reference Standard M
 * File:     rsm/xcall/xcall.c
 * Summary:  module xcall - supplied external calls
 *
 * David Wicksell <dlw@linux.com>
 * Copyright © 2020-2024 Fourth Watch Software LC
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

// SYSTEM INCLUDE FILES
#include <stdio.h>                                                              // always include
#include <stdlib.h>                                                             // these two
#include <sys/types.h>                                                          // for u_char def
#include <sys/stat.h>                                                           // $&%FILE
#include <string.h>
#include <netdb.h>                                                              // $&%HOST
#include <netinet/in.h>                                                         // $&%HOST
#include <arpa/inet.h>                                                          // $&%HOST
#include <sys/socket.h>                                                         // $&%HOST
#include <signal.h>                                                             // $&%SIGNAL
#include <ctype.h>
#include <errno.h>                                                              // error stuff
#include <syslog.h>                                                             // for syslog()
#include <termios.h>                                                            // terminal settings
#include <dirent.h>
#include <sys/ipc.h>                                                            // for semctl
#include <sys/sem.h>                                                            // for semctl
#include <sys/wait.h>                                                           // $&%WAIT

#ifdef __sun__
#   include <crypt.h>
#endif

#include "rsm.h"                                                                // standard includes
#include "error.h"                                                              // errors
#include "proto.h"                                                              // standard prototypes
#include "compile.h"
#include "database.h"
#include "symbol.h"

#ifdef __APPLE__
#   include <DirectoryService/DirectoryService.h>
#   include <assert.h>
#endif

#ifdef linux
#   define _XOPEN_SOURCE                                                        // define this
#   define __USE_XOPEN                                                          // and this
#   define _USE_BSD
#endif

#include <unistd.h>                                                             // crypt et al

#define TRIM_PARITY      0x1                                                    // 'P'
#define DISCARD_SPACES   0x2                                                    // 'D'
#define DISCARD_CONTROLS 0x4                                                    // 'C'
#define DISCARD_LEADING  0x8                                                    // 'B'
#define COMPRESS_SPACES  0x10                                                   // 'R'
#define CONVERT_TO_UPPER 0x20                                                   // 'U'
#define CONVERT_TO_LOWER 0x40                                                   // 'L'
#define DISCARD_TRAILING 0x80                                                   // 'T'
#define PRESERVE_QUOTED  0x100                                                  // 'Q'

static u_long crcTable[256];                                                    // used by CRC functions

void crcgen(void)                                                               // build the crcTable
{
    static int blnitDone = FALSE;
    u_long     poly;
    int        i;
    int        j;

    if (blnitDone == TRUE) return;                                              // only do it once

    blnitDone = TRUE;
    poly = 0xEDB88320L;

    for (i = 0; i < 256; i++) {
        u_long crc = i;

        for (j = 8; j > 0; j--) {
            if (crc & 1) {
                crc = (crc >> 1) ^ poly;
            } else {
                crc >>= 1;
            }
        }

        crcTable[i] = crc;
    }
}

/*
 *  CALLING RULES
 *  -------------
 *  1. All XCALL functions are of type short or int.
 *  2. The first argument is of type *char and is the destination
 *     for the value returned by the function (max size is MAX_STR_LEN).
 *  3. The subsequent two arguments are read only *cstring and are
 *     the passed in values.
 *  4. The function returns a count of characters in the return string
 *     or a negative error number.
 *  5. The function name is Xcall_name where the call is $&name().
 *
 *  E.g., short Xcall_name(char *ret_buffer, cstring *arg1, cstring *arg2)
 *    or   int Xcall_name(char *ret_buffer, cstring *arg1, cstring *arg2)
 */

// FUNCTION DEFINITIONS

// $&DEBUG() - Dump info on console
short Xcall_debug(char *ret_buffer, cstring *arg, __attribute__((unused)) cstring *dummy)
{
    if (strcasecmp((char *) arg->buf, "gbd") == 0) {                            // Global Buffer Descriptors
        Dump_gbd();
    } else if (strcasecmp((char *) arg->buf, "rbd") == 0) {                     // Routine Buffer Descriptors
        Dump_rbd();
    } else if (strcasecmp((char *) arg->buf, "ltd") == 0) {                     // Lock Table Descriptors
        Dump_ltd();
    } else if (strcasecmp((char *) arg->buf, "sems") == 0) {                    // Semaphores
        for (int i = 0; i < SEM_MAX; i++) {
            int val = semctl(systab->sem_id, i, GETVAL, 0);
            int sempid = semctl(systab->sem_id, i, GETPID, 0);

            printf("%d) %s", i,
                   ((i == SEM_SYS) ? "SEM_SYS" :
                   ((i == SEM_ROU) ? "SEM_ROU" :
                   ((i == SEM_LOCK) ?  "SEM_LOCK" :
                   ((i == SEM_GLOBAL) ? "SEM_GLOBAL" :
                   ((i == SEM_WD) ? "SEM_WD" :
                   ((i == SEM_ATOMIC) ? "SEM_ATOMIC" : "?")))))));

            printf("\t= %d \t(last PID %d)\r\n", val, sempid);
        }

        printf("(maxjobs = %u)\r\n", systab->maxjob);
    } else if (strcasecmp((char *) arg->buf, "struct") == 0) {                  // sanity check structs
        printf("FOR_STACK size %zu\r\n", sizeof(struct FOR_STACK));
        printf("TAGS size %zu\r\n", sizeof(struct TAGS));
        printf("RBD size %zu\r\n", sizeof(struct RBD));
        printf("DB_BLOCK size %zu\r\n", sizeof(struct DB_BLOCK));
        printf("GBD size %zu\r\n", sizeof(struct GBD));
        printf("JRNREC size %zu\r\n", sizeof(struct JRNREC));
        printf("CSTRING size %zu\r\n", sizeof(struct CSTRING));
        printf("MVAR size %zu\r\n", sizeof(struct MVAR));
        printf("UCI_TAB size %zu\r\n", sizeof(struct UCI_TAB));
        printf("WD_TAB size %zu\r\n", sizeof(struct WD_TAB));
        printf("LABEL_BLOCK size %zu\r\n", sizeof(struct LABEL_BLOCK));
        printf("DB_STAT size %zu\r\n", sizeof(struct DB_STAT));
        printf("VOL_DEF size %zu\r\n", sizeof(struct VOL_DEF));
        printf("DO_FRAME size %zu\r\n", sizeof(struct DO_FRAME));
        printf("FORKTAB size %zu\r\n", sizeof(struct FORKTAB));
        printf("SERVERTAB size %zu\r\n", sizeof(struct SERVERTAB));
        printf("SQ_CHAN size %zu\r\n", sizeof(struct SQ_CHAN));
        printf("JOBTAB size %zu\r\n", sizeof(struct JOBTAB));
        printf("LOCKTAB size %zu\r\n", sizeof(struct LOCKTAB));
        printf("TRANTAB size %zu\r\n", sizeof(struct TRANTAB));
        printf("SYSTAB size %zu\r\n", sizeof(struct SYSTAB));
        printf("PARTAB size %zu\r\n", sizeof(struct PARTAB));
        //printf("NEW_STACK size %zu\r\n", sizeof(struct NEW_STACK));
        printf("ST_DEPEND size %zu\r\n", sizeof(struct ST_DEPEND));
        printf("ST_DATA size %zu\r\n", sizeof(struct ST_DATA));
        printf("SYMTAB size %zu\r\n", sizeof(struct SYMTAB));
        printf("ST_LOCDATA size %zu\r\n", sizeof(struct ST_LOCDATA));
        printf("ST_NEWTAB size %zu\r\n", sizeof(struct ST_NEWTAB));
        printf("KEY_STRUCT size %zu\r\n", sizeof(struct KEY_STRUCT));
    } else {
        return -(ERRZ18 + ERRMLAST);                                            // no such
    }

    ret_buffer[0] = '\0';
    return 0;
}

// %DIRECTORY() - Perform host directory list
#define MAXFILENAME 4096                                                        // Maximum filename length (PATH_MAX?)
#define PATHSEP     '/'                                                         // Path separator

static DIR  *dirp = NULL;                                                       // Directory pointer
static char pattern[MAXFILENAME];                                               // Pattern

// Get current directory by looking in the environment variable...

/*
 * This function finds the first occurrence of the character "ch" in the
 * array "filename". If "ch" is found, a pointer to "ch" in "filename"
 * is returned. Otherwise, a NULL pointer is returned.
 */
char *findNextChar(char ch, char *filename)
{
    while (*filename != '\0') {
        if (*filename == ch) {
            return filename;
        } else {
            filename += 1;
        }
    }

    return filename;                                                            // "ch" not found
}

/*
 * This function determines if the file "filename" matches the pattern
 * "pattern". A pattern can contain the following wildcards:
 *     * - 0 or more times
 *     ? - exactly once
 *
 * It returns 1 (true) if it does match, otherwise 0.
 */
int isPatternMatch(char *pattern, char *filename)
{
    int flag;                                                                   // Flag

    flag = 0;

    while (*pattern != '\0') {
        if (*pattern == '*')  {                                                 // Get next character to match
            flag = 1;
            pattern += 1;
        } else if (*pattern == '?') {                                           // Wildcard
            if (*filename == '\0') {
                return 0;
            } else {
                filename += 1;
                pattern += 1;
            }
        } else if (flag == 1) {                                                 // Find first occurrence
            filename = findNextChar(*pattern, filename);

            if (*filename == '\0') {
                return 0;
            } else {
                filename += 1;
                pattern += 1;
                flag = 0;
            }
        } else {                                                                // Match character
            if (*filename == '\0') {
                return 0;
            } else if (*filename != *pattern) {
                return 0;
            } else {
                filename += 1;
                pattern += 1;
            }
        }
    }

    return 1;                                                                   // Pattern match
}

// $&%DIRECTORY - Directory lookup
short Xcall_directory(char *ret_buffer, cstring *file, __attribute__((unused)) cstring *dummy)
{
    struct dirent *dent;                                                        // Directory entry
    char          path[MAXFILENAME];                                            // Directory
    int           ret;                                                          // Return value
    cstring       env;                                                          // Current directory

    // If file is empty, then return the next matching directory entry or NULL
    if (((file == NULL) || (file->buf[0] == '\0')) && (dirp == NULL)) {
        return -ERRM11;
    }

    if (file->buf[0] == '\0') {
        while (TRUE) {
            dent = readdir(dirp);

            if (dent == NULL) {                                                 // No more matching directory entries
                ret_buffer = NULL;
                return 0;
            }

            ret = isPatternMatch(pattern, dent->d_name);

            if (ret == 1) {                                                     // Found next matching directory entry
                sprintf(ret_buffer, "%s", dent->d_name);
                return (short) strlen(ret_buffer);
            }
        }
    } else {
        int  len;                                                               // Filename length
        char *patptr;                                                           // Pointer to pattern

        // Otherwise, open the directory and return the first matching directory entry or NULL
        int  i = snprintf(path, MAX_SEQ_NAME, "%s", file->buf);                 // Make a local copy of the

        if (i < 0) fprintf(stderr, "errno = %d - %s\r\n", errno, strerror(errno));
        len = strlen(path);                                                     //  filename
        patptr = path;                                                          // Move pointer to the
        patptr += len;                                                          //  filenames NULL terminator

        /*
         * Moving back to front, find the first occurrence of PATHSEP in "path",
         * and replace it with a NULL terminator; and hence, giving us the
         * directory to open and the pattern of desired filenames.
         */

        while (len >= 0) {
            if (*patptr == PATHSEP) {
                *patptr = '\0';

                // If the first occurrence of PATHSEP is the first character in "path",
                // then we want to search the root directory.
                if (len == 0) sprintf(path, "%c", PATHSEP);
                patptr += 1;
                len = -2;
            } else {
                if (len != 0) patptr -= 1;
                len--;
            }
        }

        sprintf(pattern, "%s", patptr);                                         // Record pattern for subsequent calls

        // If path is empty, then assume current directory
        if (len == -1) {
            sprintf((char *) env.buf, "%s", "PWD");
            env.len = strlen((char *) env.buf);
            ret = Xcall_getenv(path, &env, NULL);
            if (ret < 0) return (short) ret;
            strcat(path, "/");
        }

        dirp = opendir(path);                                                   // Open the directory

        if (dirp == NULL) {                                                     // Failed to open directory
            ret_buffer = NULL;
            return 0;
        }

        while (TRUE) {                                                          // Get first matching entry
            dent = readdir(dirp);

            if (dent == NULL) {                                                 // No more matching directory entries
                ret_buffer = NULL;
                return 0;
            }

            ret = isPatternMatch(pattern, dent->d_name);

            if (ret == 1) {                                                     // Found next matching directory entry
                sprintf(ret_buffer, "%s", dent->d_name);
                return (short) strlen(ret_buffer);
            }
        }
    }
}

// $&%ERRMSG - Return error text
short Xcall_errmsg(char *ret_buffer, cstring *err, __attribute__((unused)) cstring *dummy)
{
    int errnum = 0;                                                             // init error number

    if (err->len < 1) return -ERRM11;                                           // they gotta pass something

    if (err->buf[0] == 'U') {                                                   // user error ?
        memcpy(ret_buffer, "User error: ", 12);
        memmove(&ret_buffer[12], err->buf, err->len);
        return err->len + 12;
    }

    if (err->len < 2) return -ERRM11;                                           // they gotta pass something

    if (err->buf[0] == 'Z') {
        errnum += ERRMLAST;                                                     // point at Z errors
    } else if (err->buf[0] != 'M') {
        return -ERRM99;                                                         // that's junk
    }

    errnum += atoi((char *) &err->buf[1]);                                      // add the number to it
    return UTIL_strerror(errnum, (u_char *) ret_buffer);                        // do it
}

// $&%OPCOM - Message control
short Xcall_opcom(char *ret_buffer, cstring *msg, const cstring *device)
{
    ret_buffer[0] = '\0';                                                       // null terminate nothing

    if (device->len == 0) {                                                     // if no device specified
        syslog(LOG_INFO, "%s", &msg->buf[0]);                                   // give it to the system
        return 0;                                                               // all OK
    }

    return SQ_Force(device, msg);                                               // do it in seqio
}

// $&%SIGNAL - Send signal to PID
short Xcall_signal(char *ret_buffer, cstring *pid, cstring *sig)
{
    int i;                                                                      // PID
    int j;                                                                      // Signal

    i = cstringtoi(pid);                                                        // get pid
    if (i < 1) return -ERRM99;                                                  // must be positive or error
    j = cstringtoi(sig);                                                        // get signal#
    if ((j > 31) || (j < 0)) return -ERRM99;                                    // if out of range then error
    i = kill(i, j);                                                             // do it

    if (!i || (!j && (errno != ESRCH))) {                                       // if ok
        ret_buffer[0] = '1';                                                    // say ok
    } else {
        ret_buffer[0] = '0';                                                    // say failed
    }

    ret_buffer[1] = '\0';                                                       // null terminated
    return 1;                                                                   // and return
}

// $&%SPAWN - Create subprocess
short Xcall_spawn(char *ret_buffer, cstring *cmd, const cstring *type)
{
    int ret;

    if (restricted) return -(ERRZ77 + ERRMLAST);                                // -R was passed when job started

    if (type->len) {
        FILE *fp = NULL;
        int  echk, err;

        // Do command
        fp = popen((char *) cmd->buf, "r");

        // Return error if necessary
        if (fp == NULL) return -(ERRMLAST + ERRZLAST + errno);

        // Read output
        ret = fread(ret_buffer, 1, MAXFILENAME, fp);

        // Check for error
        echk = ferror(fp);
        err = errno;

        // Close pipe
        pclose(fp);

        // Return error if necessary
        if (echk != 0) {
            return -(ERRMLAST + ERRZLAST + err);
        } else {
            return (short) ret;
        }
    } else {
        struct termios term, save;

        if (tcgetattr(STDIN_FILENO, &term) == -1) return -(ERRMLAST + ERRZLAST + errno);
        if (tcgetattr(STDIN_FILENO, &save) == -1) return -(ERRMLAST + ERRZLAST + errno); // Store current settings

        // Change to sane settings for shell out
        term.c_iflag |= ICRNL;
        term.c_oflag |= ONLCR;
        term.c_lflag |= (ICANON | ECHO);
        if (tcsetattr(STDIN_FILENO, TCSANOW, &term)  == -1) return -(ERRMLAST + ERRZLAST + errno);

        // ret_buffer unused
        ret_buffer[0] = '\0';

        // Do command
        ret = system((char *) cmd->buf);

        // allow ECHILD
        if ((ret == -1) && (errno == ECHILD)) ret = 0;

        // Restore original settings
        if (tcsetattr(STDIN_FILENO, TCSANOW, &save) == -1) return -(ERRMLAST + ERRZLAST + errno);

        // Return 0 or error
        if (ret == -1) {
            return -(ERRMLAST + ERRZLAST + errno);
        } else {
            return 0;
        }
    }
}

/*
 * $&%VERSION - Supply version information - arg1 must be "NAME"
 * returns "Reference Standard M V<major.minor.patch[-pre]> [T<test>] for <platform> <datetime> ..."
 */
short Xcall_version(char *ret_buffer, __attribute__((unused)) cstring *dummy1, __attribute__((unused)) cstring *dummy2)
{
    return (short) rsm_version((u_char *) ret_buffer);                          // do it elsewhere
}

/*
 * $&%ZWRITE - Dump local symbol table (arg1/tmp can be a global reference)
 * returns 0 on success, or negative for errors - sets variables to empty string
 */
short Xcall_zwrite(char *ret_buffer, cstring *tmp, __attribute__((unused)) cstring *dummy)
{
    mvar  var;                                                                  // JIC
    short s;                                                                    // for functions

    ret_buffer[0] = '\0';                                                       // for returns
    if (tmp->len < 2) return ST_Dump();                                         // 'normal' call? then do it in symbol
    s = UTIL_MvarFromCStr(tmp, &var);                                           // make an mvar from it
    if (s < 0) return s;                                                        // complain on error

    if ((var.uci == UCI_IS_LOCALVAR) || (var.name.var_cu[0] == '$')) {          // must be global and not SSVN
        return -(ERRZ12 + ERRMLAST);                                            // junk
    }

    return ST_DumpV(&var);                                                      // do it
}

// $&E - Perform string editing functions (two arguments)
short Xcall_e(char *ret_buffer, cstring *istr, cstring *STR_mask)
{
    int  i;
    int  in_quotes = FALSE;
    char *ostr = &ret_buffer[0];
    char c;
    int  mask = 445;                                                            // bit mask for edit

    if (STR_mask->len != 0) {                                                   // if we have a mask
        if (STR_mask->buf[0] < 'A') {                                           // If it's a numeric arg
            mask = 0;                                                           // clear mask

            for (i = 0; i != (int) STR_mask->len; i++) {                        // for all characters
                mask = (mask * 10) + ((int) STR_mask->buf[i] - 48);             // convert to int
            }
        } else {                                                                // It's the string type
            mask = 0;                                                           // clear the mask

            for (i = 0; i != (int) STR_mask->len; i++) {                        // For all characters
                switch (toupper(STR_mask->buf[i])) {
                case 'P':
                    mask |= TRIM_PARITY;
                    break;

                case 'D':
                    mask |= DISCARD_SPACES;
                    break;

                case 'C':
                    mask |= DISCARD_CONTROLS;
                    break;

                case 'B':
                    mask |= DISCARD_LEADING;
                    break;

                case 'R':
                    mask |= COMPRESS_SPACES;
                    break;

                case 'U':
                    mask |= CONVERT_TO_UPPER;
                    break;

                case 'L':
                    mask |= CONVERT_TO_LOWER;
                    break;

                case 'T':
                    mask |= DISCARD_TRAILING;
                    break;

                case 'Q':
                    mask |= PRESERVE_QUOTED;
                    break;
                }
            }
        }
    }                                                                           // end mask decode

    for (i = 0; i != (int) istr->len; i++) {
        c = istr->buf[i];                                                       // get next char
        if (mask & TRIM_PARITY) c &= 0x7f;                                      // NO parity!
        if ((c == '"') && (mask & PRESERVE_QUOTED)) in_quotes = !in_quotes;     // toggle flag if quote

        if (in_quotes) {
            *ostr++ = c;                                                        // store output char
            mask &= ~DISCARD_LEADING;                                           // clear leading flag
        } else {
            if (islower((int) c) && (mask & CONVERT_TO_UPPER)) c &= ~0x20;      // lower to upper
            if (isupper((int) c) && (mask & CONVERT_TO_LOWER)) c |= 0x20;       // upper to lower

            if (c > ' ') {
                *ostr++ = c;                                                    // store output char
                mask &= ~DISCARD_LEADING;                                       // clear leading flag
            } else {
                while (TRUE) {
                    if (((mask & DISCARD_LEADING)  || (mask & DISCARD_SPACES)) && ((c == ' ') || (c == '\011'))) {
                        break;
                    }

                    if ((mask & DISCARD_CONTROLS) && (c < ' ')  && (c != '\011')) {
                        break;
                    }

                    if (mask & COMPRESS_SPACES) {
                        if (c == '\011') c = ' ';
                        if ((ostr != &ret_buffer[0]) && (ostr[-1] == ' ')) break;
                    }

                    *ostr++ = c;                                                // store output char
                    mask &= ~DISCARD_LEADING;                                   // clear leading flag
                    break;                                                      // remember the WHILE()
                }
            }
        }
    }

    // now handle Trailing spaces and tabs...
    if (mask & DISCARD_TRAILING) {
        while (ostr != &ret_buffer[0]) {
            if (((c = ostr[-1]) != ' ') && (c != '\011')) break;
            ostr--;
        }
    }

    // return the result string (if any!)
    i = (short) (ostr - &ret_buffer[0]);                                        // set length
    ret_buffer[i] = '\0';                                                       // ensure nulll terminated
    return (short) i;                                                           // return the length
}

// PASCHK - Perform username, password check
#ifdef __APPLE__

enum {
    kDefaultDSBufferSize = 1024
};

#if defined(__clang__) && !defined(NDEBUG)
/*
 * Open Directory Utility Routines
 * Appends a value to a data buffer. dataPtr and dataLen describe
 * the value to append. buf is the data buffer to which it's added.
 */
static tDirStatus dsDataBufferAppendData(tDataBufferPtr buf, const void *dataPtr, size_t dataLen)
{
    tDirStatus err;

    assert(buf != NULL);
    assert(dataPtr != NULL);
    assert(buf->fBufferLength <= buf->fBufferSize);

    if ((buf->fBufferLength + dataLen) > buf->fBufferSize) {
        err = eDSBufferTooSmall;
    } else {
        memcpy(&buf->fBufferData[buf->fBufferLength], dataPtr, dataLen);
        buf->fBufferLength += dataLen;
        err = eDSNoErr;
    }

    return err;
}

/*
 * dsDataListDeallocate deallocates the list contents but /not/ the
 * list header (because the list header could be allocated on the
 * stack). This routine is a wrapper that deallocates both. It's
 * the logical opposite of the various list allocation routines
 * (for example, dsBuildFromPath).
 */
static tDirStatus dsDataListAndHeaderDeallocate(tDirReference inDirReference, tDataListPtr inDataList)
{
    tDirStatus err;

    assert(inDirReference != 0);
    assert(inDataList != NULL);
    err = dsDataListDeallocate(inDirReference, inDataList);
    if (err == eDSNoErr) free(inDataList);
    return err;
}
#endif

/*
 * This routine is designed to handle the case where a
 * Open Directory routine returns eDSBufferTooSmall.
 * If so, it doubles the size of the buffer, allowing the
 * caller to retry the Open Directory routine with the
 * large buffer.
 *
 * errPtr is a pointer to a Open Directory error.
 * This routine does nothing unless that error is
 * eDSBufferTooSmall. In that case it frees the buffer
 * referenced by *bufPtrPtr, replacing it with a buffer
 * of twice the size. It then leaves *errPtr set to
 * eDSBufferTooSmall so that the caller retries the
 * call with the larger buffer.
 */
static void DoubleTheBufferSizeIfItsTooSmall(tDirStatus *errPtr, tDirNodeReference dirRef, tDataBufferPtr *bufPtrPtr)
{
    tDirStatus     err;
    tDataBufferPtr tmpBuf;

    assert(errPtr != NULL);
    assert(dirRef != 0);
    assert(bufPtrPtr != NULL);
    assert(*bufPtrPtr != NULL);

    if (*errPtr == eDSBufferTooSmall) {
        err = eDSNoErr;

        // If the buffer size is already bigger than 16 MiB, don't try to
        // double it again; something has gone horribly wrong.
        if ((*bufPtrPtr)->fBufferSize >= (16 * 1024 * 1024)) {
            err = eDSAllocationFailed;
        }

        if (err == eDSNoErr) {
            tmpBuf = dsDataBufferAllocate(dirRef, (*bufPtrPtr)->fBufferSize * 2);

            if (tmpBuf == NULL) {
                err = eDSAllocationFailed;
            } else {
#if TEST_BUFFER_DOUBLING
                fprintf(stderr, "Doubled buffer size to %lu.\r\n", tmpBuf->fBufferSize);
#endif
                assert(dsDataBufferDeAllocate(dirRef, *bufPtrPtr) == eDSNoErr);
                *bufPtrPtr = tmpBuf;
            }
        }

        /*
         * If err is eDSNoErr, the buffer expansion was successful
         * so we leave *errPtr set to eDSBufferTooSmall. If err
         * is any other value, the expansion failed and we set
         * *errPtr to that error.
         */
        if (err != eDSNoErr) {
            *errPtr = err;
        }
    }
}

/*
 * A wrapper for dsFindDirNodes that handles two special cases:
 *
 * o If the routine returns eDSBufferTooSmall, it doubles the
 *   size of the buffer referenced by *inOutDataBufferPtrPtr
 *   and retries.
 *
 *   Note that this change requires a change of the function
 *   prototype; the second parameter is a pointer to a pointer
 *   to the buffer, rather than just a pointer to the buffer.
 *   This is so that I can modify the client's buffer pointer.
 *
 * o If the routine returns no nodes but there's valid continue data,
 *   it retries.
 *
 * In other respects this works just like dsFindDirNodes.
 */
static tDirStatus dsFindDirNodesQ(tDirReference inDirReference, tDataBufferPtr *inOutDataBufferPtrPtr,
                                  tDataListPtr inNodeNamePattern, tDirPatternMatch inPatternMatchType,
                                  UInt32 *outDirNodeCount, tContextData *inOutContinueData)
{
    tDirStatus err;

    // I only supply pre-conditions for the parameters that I touch.
    assert(inOutDataBufferPtrPtr != NULL);
    assert(*inOutDataBufferPtrPtr != NULL);
    assert(outDirNodeCount != NULL);
    assert(inOutContinueData != NULL);

    do {
        do {
            err = dsFindDirNodes(inDirReference, *inOutDataBufferPtrPtr, inNodeNamePattern,
                                 inPatternMatchType, outDirNodeCount, inOutContinueData);

            DoubleTheBufferSizeIfItsTooSmall(&err, inDirReference, inOutDataBufferPtrPtr);
        } while (err == eDSBufferTooSmall);
    } while ((err == eDSNoErr) && (*outDirNodeCount == 0) && (inOutContinueData != NULL));

    return err;
}

/*
 * A wrapper for dsGetRecordList that handles two special cases:
 *
 * o If the routine returns eDSBufferTooSmall, it doubles the
 *   size of the buffer referenced by *inOutDataBufferPtr
 *   and retries.
 *
 *   Note that this change requires a change of the function
 *   prototype; the second parameter is a pointer to a pointer
 *   to the buffer, rather than just a pointer to the buffer.
 *   This is so that I can modify the client's buffer pointer.
 *
 * o If the routine returns no records but there's valid continue data,
 *   it retries.
 *
 * In other respects this works just like dsGetRecordList.
 */
static tDirStatus dsGetRecordListQ(tDirNodeReference inDirReference, tDirNodeReference inDirNodeReference,
                                   tDataBufferPtr *inOutDataBufferPtr, tDataListPtr inRecordNameList,
                                   tDirPatternMatch inPatternMatchType, tDataListPtr inRecordTypeList,
                                   tDataListPtr inAttributeTypeList, dsBool inAttributeInfoOnly,
                                   UInt32 *inOutRecordEntryCount, tContextData *inOutContinueData)
{
    tDirStatus err;
    u_long     originalRecordCount;

    // I only supply pre-conditions for the parameters that I touch.
    assert(inOutDataBufferPtr != NULL);
    assert(*inOutDataBufferPtr != NULL);
    assert(inOutRecordEntryCount != NULL);
    assert(inOutContinueData != NULL);
    originalRecordCount = *inOutRecordEntryCount;

    do {
        *inOutRecordEntryCount = originalRecordCount;

        do {
            err = dsGetRecordList(inDirNodeReference, *inOutDataBufferPtr, inRecordNameList,
                                  inPatternMatchType, inRecordTypeList, inAttributeTypeList,
                                  inAttributeInfoOnly, inOutRecordEntryCount, inOutContinueData);

            DoubleTheBufferSizeIfItsTooSmall(&err, inDirReference, inOutDataBufferPtr);
        } while (err == eDSBufferTooSmall);
    } while ((err == eDSNoErr) && (*inOutRecordEntryCount == 0) && (inOutContinueData != NULL));

    return err;
}

/*
 * A wrapper for dsDoDirNodeAuth that handles a special cases,
 * to wit, if dsDoDirNodeAuth returns eDSBufferTooSmall, it doubles
 * the size of the buffer referenced by *outAuthStepDataResponsePtr
 * and retries.
 *
 * Note that this change requires a change of the function
 * prototype; the second parameter is a pointer to a pointer
 * to the buffer, rather than just a pointer to the buffer.
 * This is so that I can modify the client's buffer pointer.
 *
 * In other respects this works just like dsDoDirNodeAuth.
 */
static tDirStatus dsDoDirNodeAuthQ(tDirNodeReference inDirReference, tDirNodeReference inDirNodeReference,
                                   tDataNodePtr inDirNodeAuthName, dsBool inDirNodeAuthOnlyFlag, tDataBufferPtr inAuthStepData,
                                   tDataBufferPtr *outAuthStepDataResponsePtr, tContextData *inOutContinueData)
{
    tDirStatus err;

    // I only supply pre-conditions for the parameters that I touch.
    assert(outAuthStepDataResponsePtr != NULL);
    assert(*outAuthStepDataResponsePtr != NULL);

    do {
        err = dsDoDirNodeAuth(inDirNodeReference, inDirNodeAuthName, inDirNodeAuthOnlyFlag,
                              inAuthStepData, *outAuthStepDataResponsePtr, inOutContinueData);

        DoubleTheBufferSizeIfItsTooSmall(&err, inDirReference, outAuthStepDataResponsePtr);
    } while (err == eDSBufferTooSmall);

    return err;
}

/*
 * Authentication Code
 *
 * Returns the path to the Open Directory search node.
 * dirRef is the connection to Open Directory.
 * On success, *searchNodePathListPtr is a data list that
 * contains the search node's path components.
 */
static tDirStatus GetSearchNodePathList(tDirReference dirRef, tDataListPtr * searchNodePathListPtr)
{
    tDirStatus       err;
    tDataBufferPtr   buf;
    tDirPatternMatch patternToFind;
    UInt32           nodeCount = 0;
    tContextData     context;

    assert(dirRef != 0);
    assert(searchNodePathListPtr != NULL);
    assert(*searchNodePathListPtr == NULL);
    patternToFind = eDSLocalNodeNames;
    context = 0;

    // Allocate a buffer for the node find results. We'll grow
    // this buffer if it proves to be to small.
    buf = dsDataBufferAllocate(dirRef, kDefaultDSBufferSize);
    err = eDSNoErr;
    if (buf == NULL) err = eDSAllocationFailed;

    /*
     * Find the node. Note that this is a degenerate case because
     * we're only looking for a single node, the search node, so
     * we don't need to loop calling dsFindDirNodes, which is the
     * standard way of using dsFindDirNodes.
     */
    if (err == eDSNoErr) {
        // place results in buf and no pattern (NULL), rather... patternToFind... hardwired search type
        err = dsFindDirNodesQ(dirRef, &buf, NULL, patternToFind, &nodeCount, &context);
    }

    // If we didn't find any nodes, that's bad.
    if ((err == eDSNoErr) && (nodeCount < 1)) err = eDSNodeNotFound;

    /*
     * Grab the first node from the buffer. Note that the inDirNodeIndex
     * parameter to dsGetDirNodeName is one-based, so we pass in the constant 1.
     *
     * Also, if we found more than one, that's unusual, but not enough to
     * cause us to error.
     */
    if (err == eDSNoErr) {
        if (nodeCount > 1) {
            fprintf(stderr, "GetSearchNodePathList: nodeCount = %u, weird.\r\n", (u_int) nodeCount);
        }

        err = dsGetDirNodeName(dirRef, buf, 1, searchNodePathListPtr);
    }

    // Clean up.
    if (context != 0) {
        assert(dsReleaseContinueData(dirRef, context) == eDSNoErr);
    }

    if (buf != NULL) {
        assert(dsDataBufferDeAllocate(dirRef, buf) == eDSNoErr);
    }

    assert((err == eDSNoErr) == (*searchNodePathListPtr != NULL));
    return err;
}

/*
 * Finds the authentication information for a given user.
 * dirRef is the connection to Open Directory.
 * nodeRef is the node to use to do the searching. Typically
 * this is the authentication search node, whose path is found
 * using GetSearchNodePathList. username is the user whose
 * information we're looking for.
 *
 * On success, *pathListToAuthNodePtr is a data list that
 * contain's the path components of the authentication node
 * for the specified user.
 * On success, *userNameForAuthPtr contains a pointer to C string
 * that is the user's name for authentication. This can be
 * different from username. For example, if user's long name is
 * "Mr Gumby" and their short name is "mrgumby", username can be
 * either the long name or the short name, but *userNameForAuthPtr
 * will always be the short name. The caller is responsible for
 * freeing this string using free.
 */
static tDirStatus FindUsersAuthInfo(tDirReference dirRef, tDirNodeReference nodeRef, const char *username,
                                    tDataListPtr *pathListToAuthNodePtr, char **userNameForAuthPtr)
{
    tDirStatus        err;
    tDataBufferPtr    buf;
    tDataListPtr      recordType;
    tDataListPtr      recordName;
    tDataListPtr      requestedAttributes;
    UInt32            recordCount = 0;
    tAttributeListRef foundRecAttrList;
    tContextData      context;
    tRecordEntryPtr   foundRecEntry;
    tDataListPtr      pathListToAuthNode;
    char              *userNameForAuth;

    assert(dirRef != 0);
    assert(nodeRef != 0);
    assert(username != NULL);
    assert(pathListToAuthNodePtr != NULL);
    assert(*pathListToAuthNodePtr == NULL);
    assert(userNameForAuthPtr != NULL);
    assert(*userNameForAuthPtr == NULL);
    recordType = NULL;
    recordName = NULL;
    requestedAttributes = NULL;
    foundRecAttrList = 0;
    context = 0;
    foundRecEntry = NULL;
    pathListToAuthNode = NULL;
    userNameForAuth = NULL;

    // Allocate a buffer for the record results. We'll grow this
    // buffer if it proves to be too small.
    err = eDSNoErr;
    buf = dsDataBufferAllocate(dirRef, kDefaultDSBufferSize);
    if (buf == NULL) err = eDSAllocationFailed;

    /*
     * Create the information needed for the search. We're searching for
     * a record of type kDSStdRecordTypeUsers whose name is "username".
     * We want to get back the kDSNAttrMetaNodeLocation and kDSNAttrRecordName
     * attributes.
     */
    if (err == eDSNoErr) {
        recordType = dsBuildListFromStrings(dirRef, kDSStdRecordTypeUsers, NULL);
        recordName = dsBuildListFromStrings(dirRef, username, NULL);
        requestedAttributes = dsBuildListFromStrings(dirRef, kDSNAttrMetaNodeLocation, kDSNAttrRecordName, NULL);

        if ((recordType == NULL) || (recordName == NULL) || (requestedAttributes == NULL)) {
            err = eDSAllocationFailed;
        }
    }

    // Search for a matching record.
    if (err == eDSNoErr) {
        recordCount = 1;                                                        // we only want one match (the first)

        err = dsGetRecordListQ(dirRef, nodeRef, &buf, recordName, eDSExact, recordType,
                               requestedAttributes, false, &recordCount, &context);
    }

    if ((err == eDSNoErr) && (recordCount < 1)) err = eDSRecordNotFound;

    /*
     * Get the first record from the search. Then enumerate the attributes for
     * that record. For each attribute, extract the first value (remember that
     * attributes can by multi-value). Then see if the attribute is one that
     * we care about. If it is, remember the value for later processing.
     */
    if (err == eDSNoErr) {
        assert(recordCount == 1);                                               // we only asked for one record, shouldn't get more
        err = dsGetRecordEntry(nodeRef, buf, 1, &foundRecAttrList, &foundRecEntry);
    }

    if (err == eDSNoErr) {
        u_long attrIndex;

        // Iterate over the attributes.
        for (attrIndex = 1; attrIndex <= foundRecEntry->fRecordAttributeCount; attrIndex++) {
            tAttributeValueListRef  thisValue;
            tAttributeEntryPtr      thisAttrEntry;
            tAttributeValueEntryPtr thisValueEntry;

            thisValue = 0;
            thisAttrEntry = NULL;
            thisValueEntry = NULL;

            // Get the information for this attribute.
            err = dsGetAttributeEntry(nodeRef, buf, foundRecAttrList, attrIndex, &thisValue, &thisAttrEntry);

            if (err == eDSNoErr) {
                const char *thisAttrName = thisAttrEntry->fAttributeSignature.fBufferData;

                // We only care about attributes that have values.
                if (thisAttrEntry->fAttributeValueCount > 0) {

                    // Get the first value for this attribute. This is common code for
                    // the two potential attribute values listed below, so we do it first.
                    err = dsGetAttributeValue(nodeRef, buf, 1, thisValue, &thisValueEntry);

                    if (err == eDSNoErr) {
                        const char *thisValueDataPtr;
                        u_long     thisValueDataLen;

                        thisValueDataPtr = thisValueEntry->fAttributeValueData.fBufferData;
                        thisValueDataLen = thisValueEntry->fAttributeValueData.fBufferLength;

                        // Handle each of the two attributes we care about; ignore any others.
                        if (strcmp(thisAttrName, kDSNAttrMetaNodeLocation) == 0) {
                            assert(pathListToAuthNode == NULL);                 // same attribute twice

                            /*
                             * This is the kDSNAttrMetaNodeLocation attribute, which contains
                             * a path to the node used for authenticating this record; convert
                             * its value into a path list.
                             */
                            pathListToAuthNode = dsBuildFromPath(dirRef, thisValueDataPtr, "/");
                            if (pathListToAuthNode == NULL) err = eDSAllocationFailed;
                        } else if (strcmp(thisAttrName, kDSNAttrRecordName) == 0) {
                            assert(userNameForAuth == NULL);                    // same attribute twice

                            /*
                             * This is the kDSNAttrRecordName attribute, which contains the
                             * user name used for authentication; remember its value in a
                             * freshly allocated string.
                             */
                            userNameForAuth = malloc(thisValueDataLen + 1);

                            if (userNameForAuth == NULL) {
                                err = eDSAllocationFailed;
                            } else {
                                memcpy(userNameForAuth, thisValueDataPtr, thisValueDataLen);
                                userNameForAuth[thisValueDataLen] = 0;          // terminating null
                            }
                        } else {
                            fprintf(stderr, "FindUsersAuthInfo: Unexpected attribute '%s'.", thisAttrName);
                        }
                    }
                } else {
                    fprintf(stderr, "FindUsersAuthInfo: Unexpected no-value attribute '%s'.", thisAttrName);
                }
            }

            // Clean up.
            if (thisValueEntry != NULL) {
                assert(dsDeallocAttributeValueEntry(dirRef, thisValueEntry) == eDSNoErr);
            }

            if (thisValue != 0) {
                assert(dsCloseAttributeValueList(thisValue) == eDSNoErr);
            }

            if (thisAttrEntry != NULL) {
                assert(dsDeallocAttributeEntry(dirRef, thisAttrEntry) == eDSNoErr);
            }

            if (err != eDSNoErr) break;
        }
    }

    // Copy results out to caller.
    if (err == eDSNoErr) {
        if ((pathListToAuthNode != NULL) && (userNameForAuth != NULL)) {
            // Copy out results.
            *pathListToAuthNodePtr = pathListToAuthNode;
            *userNameForAuthPtr = userNameForAuth;

            // NULL out locals so that we don't dispose them.
            pathListToAuthNode = NULL;
            userNameForAuth = NULL;
        } else {
            err = eDSAttributeNotFound;
        }
    }

    // Clean up.
    if (pathListToAuthNode != NULL) {
        assert(dsDataListAndHeaderDeallocate(dirRef, pathListToAuthNode) == eDSNoErr);
    }

    if (userNameForAuth != NULL) free(userNameForAuth);

    if (foundRecAttrList != 0) {
        assert(dsCloseAttributeList(foundRecAttrList) == eDSNoErr);
    }

    if (context != 0) {
        assert(dsReleaseContinueData(dirRef, context) == eDSNoErr);
    }

    if (foundRecAttrList != 0) {
        assert(dsDeallocRecordEntry(dirRef, foundRecEntry) == eDSNoErr);
    }

    if (requestedAttributes != NULL) {
        assert(dsDataListAndHeaderDeallocate(dirRef, requestedAttributes) == eDSNoErr);
    }

    if (recordName != NULL) {
        assert(dsDataListAndHeaderDeallocate(dirRef, recordName) == eDSNoErr);
    }

    if (recordType != NULL) {
        assert(dsDataListAndHeaderDeallocate(dirRef, recordType) == eDSNoErr);
    }

    if (buf != NULL) {
        assert(dsDataBufferDeAllocate(dirRef, buf) == eDSNoErr);
    }

    assert((err == eDSNoErr) == ((*pathListToAuthNodePtr != NULL) && (*userNameForAuthPtr != NULL)));
    return err;
}

/*
 * Authenticate a user with their authentication node.
 * dirRef is the connection to Open Directory.
 * pathListToAuthNode is a data list that contain's the
 * path components of the authentication node for the
 * specified user. userNameForAuth and password are the
 * user name and password to authenticate.
 */
static tDirStatus AuthenticateWithNode(tDirReference dirRef, tDataListPtr pathListToAuthNode,
                                       const char *userNameForAuth, const char *password)
{
    tDirStatus        err;
    size_t            userNameLen;
    size_t            passwordLen;
    tDirNodeReference authNodeRef;
    tDataNodePtr      authMethod;
    tDataBufferPtr    authOutBuf;
    tDataBufferPtr    authInBuf;
    unsigned long     length;

    assert(dirRef != 0);
    assert(pathListToAuthNode != NULL);
    assert(userNameForAuth != NULL);
    assert(password != NULL);
    authNodeRef = 0;
    authMethod = NULL;
    authOutBuf = NULL;
    authInBuf = NULL;
    userNameLen = strlen(userNameForAuth);
    passwordLen = strlen(password);

    // Open the authentication node.
    err = dsOpenDirNode(dirRef, pathListToAuthNode, &authNodeRef);

    /*
     * Create the input parameters to dsDoDirNodeAuth and then call it. The most
     * complex input parameter to dsDoDirNodeAuth is authentication data itself,
     * held in authInBuf. This holds the following items:
     *
     * 4 byte length of user name (includes trailing null)
     * user name, including trailing null
     * 4 byte length of password (includes trailing null)
     * password, including trailing null
     */
    if (err == eDSNoErr) {
        authMethod = dsDataNodeAllocateString(dirRef, kDSStdAuthNodeNativeClearTextOK);
        if (authMethod == NULL) err = eDSAllocationFailed;
    }

    if (err == eDSNoErr) {
        /*
         * Allocate some arbitrary amount of space for the authOutBuf. This
         * buffer comes back containing a credential generated by the
         * authentication (apparently a kDS1AttrAuthCredential). However,
         * we never need this information, so we basically just create the
         * buffer, pass it in to dsDoDirNodeAuth, and then throw it away.
         * Unfortunately dsDoDirNodeAuth won't let us pass in NULL.
         */
        authOutBuf = dsDataBufferAllocate(dirRef, kDefaultDSBufferSize);
        if (authOutBuf == NULL) err = eDSAllocationFailed;
    }

    if (err == eDSNoErr) {
        authInBuf = dsDataBufferAllocate(dirRef, sizeof(length) + userNameLen + 1 + sizeof(length) + passwordLen + 1);
        if (authInBuf == NULL) err = eDSAllocationFailed;
    }

    if (err == eDSNoErr) {
        length = userNameLen + 1;                                               // + 1 to include trailing null
#ifdef __clang__
        assert(dsDataBufferAppendData(authInBuf, &length, sizeof(length)) == noErr);
        assert(dsDataBufferAppendData(authInBuf, userNameForAuth, userNameLen + 1) == noErr);
#endif
        length = passwordLen + 1;                                               // + 1 to include trailing null
#ifdef __clang__
        assert(dsDataBufferAppendData(authInBuf, &length, sizeof(length)) == noErr);
        assert(dsDataBufferAppendData(authInBuf, password, passwordLen + 1) == noErr);
#endif

        // Call dsDoDirNodeAuth to do the authentication.
        err = dsDoDirNodeAuthQ(dirRef, authNodeRef, authMethod, true, authInBuf, &authOutBuf, NULL);
    }

    // Clean up.
    if (authInBuf != NULL) {
        assert(dsDataBufferDeAllocate(dirRef, authInBuf) == eDSNoErr);
    }

    if (authOutBuf != NULL) {
        assert(dsDataBufferDeAllocate(dirRef, authOutBuf) == eDSNoErr);
    }

    if (authMethod != NULL) {
        assert(dsDataNodeDeAllocate(dirRef, authMethod) == eDSNoErr);
    }

    if (authNodeRef != 0) {
        assert(dsCloseDirNode(authNodeRef) == eDSNoErr);
    }

    return err;
}

// Check a user name and password using Open Directory.
static tDirStatus CheckPasswordUsingOpenDirectory(const char *username, const char *password)
{
    tDirStatus        err;
    tDirReference     dirRef;
    tDataListPtr      pathListToSearchNode;
    tDirNodeReference searchNodeRef;
    tDataListPtr      pathListToAuthNode;
    char              *userNameForAuth;

    assert(username != NULL);
    assert(password != NULL);
    dirRef = 0;
    pathListToSearchNode = NULL;
    searchNodeRef = 0;
    pathListToAuthNode = NULL;
    userNameForAuth = NULL;

    // Connect to Open Directory.
    err = dsOpenDirService(&dirRef);

    // Open the search node.
    if (err == eDSNoErr) err = GetSearchNodePathList(dirRef, &pathListToSearchNode);
    if (err == eDSNoErr) err = dsOpenDirNode(dirRef, pathListToSearchNode, &searchNodeRef);

    // Search for the user's record and extract the user's authentication
    // node and authentication user name..
    if (err == eDSNoErr) {
        err = FindUsersAuthInfo(dirRef, searchNodeRef, username, &pathListToAuthNode, &userNameForAuth);
    }

    // Open the authentication node and do the authentication.
    if (err == eDSNoErr) err = AuthenticateWithNode(dirRef, pathListToAuthNode, userNameForAuth, password);

    // Clean up.
    if (userNameForAuth != NULL) free(userNameForAuth);

    if (pathListToAuthNode != NULL) {
        assert(dsDataListAndHeaderDeallocate(dirRef, pathListToAuthNode) == eDSNoErr);
    }

    if (searchNodeRef != 0) {
        assert(dsCloseDirNode(searchNodeRef) == eDSNoErr);
    }

    if (pathListToSearchNode != NULL) {
        assert(dsDataListAndHeaderDeallocate(dirRef, pathListToSearchNode) == eDSNoErr);
    }

    if (dirRef != 0) {
        assert(dsCloseDirService(dirRef) == eDSNoErr);
    }

    return err;
}

// $&PASCHK - Check user/password
short Xcall_paschk(char *ret_buffer, const cstring *user, cstring *pwd)
{
    char       *username;
    char       *password;
    tDirStatus err;

    username = (char *) user->buf;
    password = (char *) pwd->buf;
    ret_buffer[0] = '0';                                                        // Assume fail
    ret_buffer[1] = '\0';                                                       // and terminate it
    err = CheckPasswordUsingOpenDirectory(username, password);

    switch (err) {
    case eDSNoErr:
        ret_buffer[0] = '1';                                                    // indicate OK
        break;

    case eDSAuthNewPasswordRequired:
        ret_buffer[0] = '2';                                                    // sort of OK
        break;

    case eDSAuthPasswordExpired:
        ret_buffer[0] = '2';                                                    // ditto
        break;

    default:
        ret_buffer[0] = '-';                                                    // server error
        ret_buffer[1] = '1';                                                    // server error
        ret_buffer[2] = '\0';                                                   // and terminate it
        return 2;
    }

    return 1;                                                                   // return with char count
}
#else

// $&PASCHK - Check user/password
#ifndef __CYGWIN__
short Xcall_paschk(char *ret_buffer, const cstring *user, cstring *pwd)
#else
short Xcall_paschk(char *ret_buffer, const cstring *user, __attribute__((unused)) cstring *pwd)
#endif
{
    FILE *fd;                                                                   // secure user database
    char line[256];                                                             // line
    char *err;                                                                  // fgets error
    char *preptr;                                                               // ':' (i.e., username:)
    char *postptr;                                                              // ':' (i.e., username:password:)
    char password[256] = {0};                                                   // encrypted password

#if defined(__FreeBSD__) || defined(__NetBSD__)
    fd = fopen("/etc/master.passwd", "r");
#else
    fd = fopen("/etc/shadow", "r");
#endif

    if (fd == NULL) {                                                           // fopen() failed
        ret_buffer[0] = '-';
        ret_buffer[1] = '1';
        ret_buffer[2] = '\0';
        return 2;
    }

    while (feof(fd) == 0) {
        err = fgets(line, 256, fd);                                             // fgets() failed

        if (err == NULL) {
            fclose(fd);
            ret_buffer[0] = '-';
            ret_buffer[1] = '1';
            ret_buffer[2] = '\0';
            return 2;
        }

        preptr = strchr(line, ':');

        if (preptr != NULL) {
            *preptr = '\0';

            if (strcmp((char *) user->buf, line) == 0) {
                preptr++;
                postptr = strchr(preptr, ':');
                *postptr = '\0';
#ifndef __CYGWIN__
                strcpy(password, crypt((char *) pwd->buf, preptr));             // WON'T WORK ON CYGWIN
#endif

                if (strcmp(password, preptr) == 0) {
                    fclose(fd);
                    ret_buffer[0] = '1';
                    ret_buffer[1] = '\0';
                    return 1;
                } else {
                    fclose(fd);
                    ret_buffer[0] = '0';
                    ret_buffer[1] = '\0';
                    return 1;
                }
            }
        }
    }

    fclose(fd);
    ret_buffer[0] = '-';                                                        // indicate fail
    ret_buffer[1] = '1';                                                        // indicate fail
    ret_buffer[2] = '\0';                                                       // null terminate
    return 2;                                                                   // return string length
}
#endif

// $&V - Generate an absolute Escape sequence for (Y,X) positioning
int Xcall_v(char *ret_buffer, cstring *lin, cstring *col)
{
    int i;
    int len = 0;                                                                // length of it

    ret_buffer[len++] = 27;                                                     // Store the ESC
    ret_buffer[len++] = '[';                                                    // Store the '['
    for (i = 0; i != (int) lin->len; i++) ret_buffer[len++] = lin->buf[i];      // for all char in lin, copy one char
    ret_buffer[len++] = ';';                                                    // Then the ';'
    for (i = 0; i != (int) col->len; i++) ret_buffer[len++] = col->buf[i];      // for all char in col, copy one char
    ret_buffer[len++] = 'H';                                                    // Finally the 'H'
    ret_buffer[len] = '\0';                                                     // NUL terminate
    return len;                                                                 // and return the length
}

// $&X - Checksum a string of characters (normal)
int Xcall_x(char *ret_buffer, cstring *str, const cstring *flag)
{
    u_long crc;
    u_long ulldx;

    if (flag->len == 0) {                                                       // check for the old type
        int tmp;
        int xx = 0;                                                             // for the result

        for (tmp = 0; tmp != (int) str->len; tmp++) xx += (int) str->buf[tmp];
        tmp = sprintf(ret_buffer, "%d", xx);                                    // convert to ascii
        return tmp;                                                             // and return length
    }                                                                           // end standard $&X()

    crcgen();                                                                   // ensure table built
    crc = 0xFFFFFFFF;

    for (ulldx = 0; ulldx < str->len; ulldx++) {
        int c = *(str->buf + ulldx);

        crc = ((crc >> 8) & 0x00FFFFFF) ^ crcTable[(crc ^ c) & 0xFF];
    }

    crc ^= 0xFFFFFFFF;
    memcpy(ret_buffer, &crc, 4);
    return sizeof(u_long);
}

/*
 * $&XRSM - Checksum a string of characters (RSM type)
 *        Three-byte checksum. chk(3).
 *        This code uses the global variable "crc" to accumulate a 16 bit
 *        CCITT Cyclic Redundancy Check on the string pointed to by "str"
 *        of length "len". Note the magic number 0x1081 (or 010201 octal)
 *        which is required for the algorithm.
 */
short Xcall_xrsm(char *ret_buffer, cstring *str, __attribute__((unused)) cstring *dummy)
{
    int     tmp;
    u_char  *chp = &str->buf[0];
    u_short crc = 0;                                                            // CRC result number

    for (tmp = (int) str->len; tmp > 0; tmp--) {
        u_short c = *chp++ & 0xFF;                                              // (or unsigned char)
        u_short q = (crc ^ c) & 0x0F;                                           // (or unsigned char) - low nibble

        crc = (crc >> 4) ^ (q * 0x1081);
        q = (crc ^ (c >> 4)) & 0x0F;                                            // high nibble
        crc = (crc >> 4) ^ (q * 0x1081);
    }

    // store into ASCII string, high byte first
    ret_buffer[0] = ((crc & 0xF000) >> 12) + 0x20;
    ret_buffer[1] = ((crc & 0x0FC0) >> 6) + 0x20;
    ret_buffer[2] = (crc & 0x003F) + 0x20;
    return 3;                                                                   // return length (always 3)
}

// $&%GETENV - Returns the value of an environment variable
int Xcall_getenv(char *ret_buffer, const cstring *env, __attribute__((unused)) cstring *dummy)
{
    char *p;                                                                    // ptr for getenv

    p = getenv((char *) env->buf);                                              // get the variable
    ret_buffer[0] = '\0';                                                       // null terminate return
    if (p == NULL) return 0;                                                    // nothing there
    return mcopy((u_char *) p, (u_char *) ret_buffer, strlen(p));               // return it
}

/*
 * $&%SETENV - Sets an environment variable (where it overwrites an
 *           existing environment variable), or unsets an existing
 *           environment variable if "value" == NULL
 */
short Xcall_setenv(char *ret_buffer, cstring *env, cstring *value)
{
    int ret;                                                                    // Return value

    ret_buffer[0] = '\0';                                                       // Always return NULL

    if (value == NULL) {                                                        // Unset environment variable
#ifndef _AIX
        unsetenv ((char *) env->buf);                                           // TODO: AIX doesn't have this, use a different API
#endif
        return 0;
    } else {                                                                    // Set environment variable
        ret = setenv((char *) env->buf, (char *) value->buf, 1);

        if (ret == -1) {                                                        // Error has occurred
            return -(ERRMLAST + ERRZLAST + errno);
        } else {
            return 0;
        }
    }
}

/*
 * $&%FORK - Create a copy of this process
 * Returns: Fail: 0
 *          Parent: Child M job number
 *          Child:  Minus Parent M job number
 */
short Xcall_fork(char *ret_buffer, __attribute__((unused)) cstring *dummy1, __attribute__((unused)) cstring *dummy2)
{
    int s = ForkIt(1);                                                          // fork it, copy file table

    return itocstring((u_char *) ret_buffer, s);                                // return result
}

/*
 * $&%FILE - Obtains information about a file.
 *
 * Arguments:
 *     Filename  - File to obtain information about
 *     Attribute - File attribute. The following information can
 *                 be obtained for a file:
 *
 *                 EXISTS - 1:true ; 0:false
 *                 SIZE   - File size, in bytes
 *                 ATIME  - Last access time
 *                 CTIME  - Last status change time
 *                 MTIME  - Last modification time
 *                 UID    - User ID
 *                 GID    - Group ID
 *
 * Returns:
 *     Fail    -  < 0
 *     Success - Number of bytes in ret_buffer (which contains
 *               the newly acquired information)
 */
short Xcall_file(char *ret_buffer, cstring *file, cstring *attr)
{
    struct stat sb;                                                             // File attributes
    int         ret;                                                            // Return value
    int         exists;                                                         // 1:true ; 0:false

    // Get all the file's attributes
    ret = stat((char *) file->buf, &sb);

    /*
     * Check if stat() failed. Ignore (at this stage), if the attribute is
     * EXISTS, and stat() fails with either:
     *   ENOENT  - The named file does not exist
     *   ENOTDIR - A component of the path prefix is not a directory
     */
    if (ret == -1) {                                                            // stat() failed
        if ((strcasecmp("exists", (char *) attr->buf) == 0) && ((errno == ENOENT) || (errno == ENOTDIR))) {
            exists = 0;
        } else {
            ret_buffer[0] = '\0';
            return -(ERRMLAST + ERRZLAST + errno);
        }
    } else {
        exists = 1;
    }

    // Get desired attribute
    if (strcasecmp("exists", (char *) attr->buf) == 0) {                        // File exists
        ret = sprintf(ret_buffer, "%d", exists);
        return (short) ret;                                                     // Size of ret_buffer (EXISTS)
    } else if (strcasecmp("size", (char *) attr->buf) == 0) {
        ret = sprintf(ret_buffer, "%lld", (long long) sb.st_size);
        return (short) ret;                                                     // Size of ret_buffer (SIZE)
    } else if (strcasecmp("atime", (char *) attr->buf) == 0) {                  // Last access time
        ret = sprintf(ret_buffer, "%lld", (long long) sb.st_atime);
        return (short) ret;                                                     // Size of ret_buffer (ATIME)
    } else if (strcasecmp("ctime", (char *) attr->buf) == 0) {                  // Last status change (inode)
        ret = sprintf(ret_buffer, "%lld", (long long) sb.st_ctime);
        return (short) ret;                                                     // Size of ret_buffer (CTIME)
    } else if (strcasecmp("mtime", (char *) attr->buf) == 0) {                  // Last modification time
        ret = sprintf(ret_buffer, "%lld", (long long) sb.st_mtime);
        return (short) ret;                                                     // Size of ret_buffer (MTIME)
    } else if (strcasecmp("uid", (char *) attr->buf) == 0) {                    // UID of file
        ret = sprintf(ret_buffer, "%u", sb.st_uid);
        return (short) ret;                                                     // Size of ret_buffer (UID)
    } else if (strcasecmp("gid", (char *) attr->buf) == 0) {                    // GID of file
        ret = sprintf(ret_buffer, "%u", sb.st_gid);
        return (short) ret;                                                     // Size of ret_buffer (GID)
    } else {                                                                    // Invalid attribute name
        ret_buffer[0] = '\0';
        return -ERRM46;
    }

    // Unreachable
}

/*
* $&%HOST - Resolves a host's IP address
*
* Arguments:
*     Name  - Name of host to resolve.
*             "IP" or "IP6" return TCP/IP in IPv4 or IPv6 format
*         or  "UIP" or "UIP6" return UDP/IP in IPv4 or IPv6 format
*         or  "NAME" or "NAME6" return name of host in IPv4 or IPv6 format
*
* Returns:
*     Fail    -  < 0
*     Success - Number of bytes in ret_buffer (which contains
*               the resolved host's IP address)
*/
short Xcall_host(char *ret_buffer, cstring *name, cstring *arg)
{
    int  i;
    char host[1024];
    char service[20];

    if ((strcasecmp((char *) arg->buf, "ip") == 0) || (strcasecmp((char *) arg->buf, "ip6") == 0) ||
     (strcasecmp((char *) arg->buf, "uip") == 0) || (strcasecmp((char *) arg->buf, "uip6") == 0)) {
        struct addrinfo info;                                                   // info for address match
        struct addrinfo *addr;                                                  // list of addresses returned

        if (name->len == 0) return 0;                                           // have to have a hostname
        memset(&info, 0, sizeof(info));                                         // zero out structure

        if ((strcasecmp((char *) arg->buf, "ip6") == 0) || (strcasecmp((char *) arg->buf, "uip6") == 0)) {
            info.ai_family = AF_INET6;                                          // IPv6
        } else {
            info.ai_family = AF_INET;                                           // IPv4
        }

        if ((strcasecmp((char *) arg->buf, "uip") == 0) || (strcasecmp((char *) arg->buf, "uip6") == 0)) {
            info.ai_socktype = SOCK_DGRAM;                                      // only datagram sockets
            info.ai_protocol = IPPROTO_UDP;                                     // only UDP protocol
        } else {
            info.ai_socktype = SOCK_STREAM;                                     // only stream sockets
            info.ai_protocol = IPPROTO_TCP;                                     // only TCP protocol
        }

        i = getaddrinfo((char *) name->buf, NULL, &info, &addr);

        if (i == 0) {
            if ((strcasecmp((char *) arg->buf, "ip6") == 0) || (strcasecmp((char *) arg->buf, "uip6") == 0)) {
                char ipstr6[INET6_ADDRSTRLEN];

                snprintf((char *) ret_buffer, MAX_SEQ_NAME, "%s", inet_ntop(addr->ai_addr->sa_family,
                         &((struct sockaddr_in6 *) addr->ai_addr)->sin6_addr, ipstr6, INET6_ADDRSTRLEN));
            } else {
                char ipstr[INET_ADDRSTRLEN];

                snprintf((char *) ret_buffer, MAX_SEQ_NAME, "%s", inet_ntop(addr->ai_addr->sa_family,
                         &((struct sockaddr_in *) addr->ai_addr)->sin_addr, ipstr, INET_ADDRSTRLEN));
            }

            freeaddrinfo(addr);                                                 // free addrinfo allocated in getaddrinfo()
        } else {
            strcpy(ret_buffer, gai_strerror(i));
        }

        return (short) strlen(ret_buffer);
    } else if (strcasecmp((char *) arg->buf, "name") == 0) {
        struct sockaddr_in sin;

        if (name->len == 0) {
            i = gethostname(ret_buffer, 1023);                                  // get it
            if (i == -1) return -(ERRMLAST + ERRZLAST + errno);                 // die on error
            ret_buffer[1023] = '\0';                                            // JIC
            return (short) strlen(ret_buffer);
        }

        sin.sin_family = AF_INET;
        i = inet_pton(AF_INET, (char *) name->buf, &sin.sin_addr);
        if (i == 0) return -(ERRZ48 + ERRMLAST);                                // die on error
        i = getnameinfo((struct sockaddr *) &sin, sizeof(sin), host, sizeof(host), service, sizeof(service), 0);

        if (i == 0) {
            strcpy(ret_buffer, host);
        } else {
            strcpy(ret_buffer, gai_strerror(i));
        }

        return (short) strlen(ret_buffer);
    } else if (strcasecmp((char *) arg->buf, "name6") == 0) {
        struct sockaddr_in6 sin6;

        if (name->len == 0) {
            i = gethostname(ret_buffer, 1023);                                  // get it
            if (i == -1) return -(ERRMLAST + ERRZLAST + errno);                 // die on error
            ret_buffer[1023] = '\0';                                            // JIC
            return (short) strlen(ret_buffer);
        }

        sin6.sin6_family = AF_INET6;
        i = inet_pton(AF_INET6, (char *) name->buf, &sin6.sin6_addr);
        if (i == 0) return -(ERRZ48 + ERRMLAST);                                // die on error
        i = getnameinfo((struct sockaddr *) &sin6, sizeof(sin6), host, sizeof(host), service, sizeof(service), 0);

        if (i == 0) {
            strcpy(ret_buffer, host);
        } else {
            strcpy(ret_buffer, gai_strerror(i));
        }

        return (short) strlen(ret_buffer);
    }

    ret_buffer[0] = '\0';
    return -(ERRZ18 + ERRMLAST);                                                // error
}

/*
 * $&%WAIT() - Wait on a child
 *
 * Original author: Martin Kula <mkula@users.sourceforge.net>
 *
 * Arguments:
 *   First:
 *       none - wait's on a pid
 *       PID  - wait on the PID
 *   Second:
 *       "BLOCK" - blocked waiting
 *       none    - non blocked waiting
 *
 * Returns:
 *     <0 - failed
 *     0  - none pid exit
 *
 * num_bytes in ret_buffer which contains "pid number#error_code#terminate signal number"
 */
short Xcall_wait(char *ret_buffer, cstring *arg1, const cstring *arg2)
{
    int   pid;                                                                  // PID number
    int   status;                                                               // Exit status
    short s;                                                                    // length of the returned string
    int   blocked = WNOHANG;                                                    // blocked flag

    ret_buffer[0] = '\0';

    if (arg2->len) {                                                            // blocked waiting
        if (!strncmp((char *) arg2->buf, "BLOCK", 5)) {
            blocked = 0;
        } else if (strncmp((char *) arg2->buf, "NOBLOCK", 7)) {
            return -ERRM99;
        }
    }

    if (arg1->len) {                                                            // call with an arguments (PID)
        pid = cstringtoi(arg1);                                                 // get pid number
        if (pid < 1) return -ERRM99;
        pid = wait4(pid, &status, blocked, NULL);
    } else {                                                                    // call without an arguments
        pid = wait3(&status, blocked, NULL);
        if ((pid < 0) && blocked && (errno == ECHILD)) pid = 0;                 // no error if non blocked
    }

    if (pid < 0) return -(ERRMLAST + ERRZLAST + errno);
    if (!pid) return 0;                                                         // none pid exit
    s = itocstring((u_char *) ret_buffer, pid);
    ret_buffer[s++] = '#';
    ret_buffer[s++] = '\0';
    if (WIFEXITED(status)) s += itocstring((u_char *) &ret_buffer[s], WEXITSTATUS(status));
    ret_buffer[s++] = '#';
    ret_buffer[s++] = '\0';
    if (WIFSIGNALED(status)) s += itocstring((u_char *) &ret_buffer[s], WTERMSIG(status));
    return s;
}
