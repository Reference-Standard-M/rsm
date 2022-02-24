/*
 * Package:  Reference Standard M
 * File:     rsm/runtime/ssvn.c
 * Summary:  module runtime - Runtime Variables
 *
 * David Wicksell <dlw@linux.com>
 * Copyright © 2020-2022 Fourth Watch Software LC
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

#include <stdio.h>                                                              // always include
#include <stdlib.h>                                                             // these two
#include <unistd.h>                                                             // usually this too
#include <sys/types.h>                                                          // for u_char def
#include <sys/ipc.h>                                                            // shared memory
#include <sys/shm.h>                                                            // shared memory
#include <sys/sem.h>                                                            // semaphores
#include <signal.h>                                                             // for kill()
#include <pwd.h>                                                                // for user name
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <errno.h>                                                              // error stuff
#include <sys/time.h>                                                           // for priority
#include <sys/resource.h>                                                       // ditto
#include <termios.h>                                                            // for tcgetattr
#include "rsm.h"                                                                // standard includes
#include "proto.h"                                                              // standard prototypes
#include "database.h"                                                           // for GBD def
#include "error.h"                                                              // standard errors

extern struct termios tty_settings;                                             // man 3 termios

/*
 * SSVNs use the same structures as
 * ST_* and DB_ *functions (as SS_ *functions)
 *
 * Note valid SSVNs are:  $CHARACTER
 *                        $DEVICE
 *                        $GLOBAL
 *                        $JOB
 *                        $LOCK
 *                        $ROUTINE
 *                        $SYSTEM
 */

int priv(void)                                                                  // return TRUE if job has priv
{
    return (partab.jobtab->priv || (partab.jobtab->dostk[partab.jobtab->cur_do].rounam.var_cu[0] == '%')); // is it privileged?
}

short SS_Norm(mvar *var)                                                        // "normalize" SSVN
{
    int i;                                                                      // for loops

    for (i = 0; i < VAR_LEN; i++) var->name.var_cu[i] = toupper(var->name.var_cu[i]); // scan the supplied name & copy to upper case

    switch (var->name.var_cu[1]) {                                              // check initial of name
    case 'C':                                                                   // $CHARACTER
        if ((var->name.var_cu[2] == '\0') || (bcmp("CHARACTER\0", &var->name.var_cu[1], 10) == 0)) { // short form of name or if OK
            VAR_CLEAR(var->name);
            bcopy("$CHARACTER", &var->name.var_cu[0], 10);                      // copy in full name
            if (var->uci == 0) var->uci = 1;                                    // ensure UCI is set
            if (var->volset == 0) var->volset = 1;                              // ensure volset is set
            if (var->uci != 1) return -ERRM59;                                  // Environment reference not OK
            if (var->volset != 1) return -ERRM59;                               // Environment reference not OK
            return 0;                                                           // and return saying OK
        }

        return -ERRM60;                                                         // Undefined SSVN

    case 'D':                                                                   // $DEVICE
        if ((var->name.var_cu[2] == '\0') || (bcmp("DEVICE\0", &var->name.var_cu[1], 7) == 0)) { // short form of name or if OK
            VAR_CLEAR(var->name);
            bcopy("$DEVICE", &var->name.var_cu[0], 7);                          // copy in full name
            if (var->uci == 0) var->uci = 1;                                    // ensure UCI is set
            if (var->volset == 0) var->volset = 1;                              // ensure volset is set
            if (var->uci != 1) return -ERRM59;                                  // Environment reference not OK
            if (var->volset != 1) return -ERRM59;                               // Environment reference not OK
            return 0;                                                           // and return saying OK
        }

        return -ERRM60;                                                         // Undefined SSVN

    case 'G':                                                                   // $GLOBAL
        if ((var->name.var_cu[2] == '\0') || (bcmp("GLOBAL\0", &var->name.var_cu[1], 7) == 0)) { // short form of name or if OK
            VAR_CLEAR(var->name);
            bcopy("$GLOBAL", &var->name.var_cu[0], 7);                          // copy in full name
            if (var->uci == 0) var->uci = partab.jobtab->uci;                   // ensure UCI is set
            if (var->volset == 0) var->volset = partab.jobtab->vol;             // ensure volset is set
            return 0;                                                           // and return saying OK
        }

        return -ERRM60;                                                         // Undefined SSVN

    case 'J':                                                                   // $JOB
        if ((var->name.var_cu[2] == '\0') || (bcmp("JOB\0", &var->name.var_cu[1], 4) == 0)) { // short form of name or if OK
            VAR_CLEAR(var->name);
            bcopy("$JOB", &var->name.var_cu[0], 4);                             // copy in full name
            if (var->uci == 0) var->uci = 1;                                    // ensure UCI is set
            if (var->volset == 0) var->volset = 1;                              // ensure volset is set
            if (var->uci != 1) return -ERRM59;                                  // Environment reference not OK
            if (var->volset != 1) return -ERRM59;                               // Environment reference not OK
            return 0;                                                           // and return saying OK
        }

        return -ERRM60;                                                         // Undefined SSVN

    case 'L':                                                                   // $LOCK
        if ((var->name.var_cu[2] == '\0') || (bcmp("LOCK\0", &var->name.var_cu[1], 5) == 0)) { // short form of name or if OK
            VAR_CLEAR(var->name);
            bcopy("$LOCK", &var->name.var_cu[0], 5);                            // copy in full name
            if (var->uci == 0) var->uci = 1;                                    // ensure UCI is set
            if (var->volset == 0) var->volset = partab.jobtab->lvol;            // ensure volset is set
            if (var->uci != 1) return -ERRM59;                                  // Environment reference out
            return 0;                                                           // and return saying OK
        }

        return -ERRM60;                                                         // Undefined SSVN

    case 'R':                                                                   // $ROUTINE
        if ((var->name.var_cu[2] == '\0') || (bcmp("ROUTINE\0", &var->name.var_cu[1], 8) == 0)) { // short form of name or if OK
            VAR_CLEAR(var->name);
            bcopy("$ROUTINE", &var->name.var_cu[0], 8);                         // copy in full name
            if (var->volset == 0) var->volset = partab.jobtab->rvol;            // check volset, ensure non-zero

            if (var->uci == 0) {                                                // check UCI
                if (var->key[1] == '%') {
                    var->uci = 1;                                               // manager
                } else {
                    var->uci = partab.jobtab->ruci;                             // or here
                }
            }

            return 0;                                                           // and return saying OK
        }

        return -ERRM60;                                                         // Undefined SSVN

    case 'S':                                                                   // $SYSTEM
        if ((var->name.var_cu[2] == '\0') || (bcmp("SYSTEM\0", &var->name.var_cu[1], 7) == 0)) { // short form of name or if OK
            VAR_CLEAR(var->name);
            bcopy("$SYSTEM", &var->name.var_cu[0], 7);                          // copy in full name
            if (var->uci == 0) var->uci = 1;                                    // ensure UCI is set
            if (var->volset == 0) var->volset = 1;                              // ensure volset is set
            if (var->uci != 1) return -ERRM59;                                  // Environment reference out
            if (var->volset != 1) return -ERRM59;                               // Environment reference out
            return 0;                                                           // and return saying OK
        }

        return -ERRM60;                                                         // Undefined SSVN

    default:                                                                    // error
        return -ERRM60;                                                         // Undefined SSVN
    }
}

int SS_Get(mvar *var, u_char *buf)                                              // get SSVN data
{
    int           i = 0;                                                        // useful int
    int           j;                                                            // and another
    int           s;                                                            // for functions
    int           cnt;                                                          // count of bytes used
    var_u         *rounam;                                                      // to extract rou name
    u_char        tmp[1024];                                                    // temp string space
    int           ptmp = 0;                                                     // pointer into this
    int           nsubs = 0;                                                    // count subscripts
    struct passwd *pp;                                                          // for getpwuid()
    cstring       *subs[4];                                                     // where to put them
    mvar          *vp;                                                          // variable ptr

    while (i < var->slen) {                                                     // for all subs
        cnt = 0;                                                                // flag no rabbit ears quotes
        if (nsubs > 3) return -ERRM38;                                          // junk
        subs[nsubs] = (cstring *) &tmp[ptmp];                                   // point at the buffer

        s = UTIL_Key_Extract(&var->key[i],                                      // key from here
                             subs[nsubs]->buf,                                  // where to put it
                             &cnt);                                             // the count

        if (s < 0) return s;                                                    // die on error
        subs[nsubs++]->len = s;                                                 // save the size (incr count)
        ptmp += s + sizeof(u_short) + 1;                                        // move up temp area
        i = i + cnt;                                                            // count used bytes
    }

    i = SS_Norm(var);                                                           // normalize the name
    if (i < 0) return i;                                                        // return on error

    switch (var->name.var_cu[1]) {                                              // check initial of name
    case 'C':                                                                   // $CHARACTER
        if (strncasecmp((char *) subs[0]->buf, "m\0", 2) != 0) return -ERRM38;  // only the M character set is supported

        if (nsubs == 2) {                                                       // two sub case
            if (strncasecmp((char *) subs[1]->buf, "collate\0", 8) == 0) return 0; // empty string refers to M collation
            if (strncasecmp((char *) subs[1]->buf, "ident\0", 6) == 0) return 0; // empty string refers to default ident algorithm
        } else if (nsubs == 3) {                                                // end of two sub case - three sub case
            // patcode algoref is not yet implemented
            if (strncasecmp((char *) subs[1]->buf, "patcode\0", 8) == 0) return -ERRM38; // subs[2] = CLEANUP
            if (strncasecmp((char *) subs[2]->buf, "m\0", 2) != 0) return -ERRM38; // only the M character set is supported
            if (strncasecmp((char *) subs[1]->buf, "input\0", 6) == 0) return 0; // empty string refers to M collation
            if (strncasecmp((char *) subs[1]->buf, "output\0", 7) == 0) return 0; // empty string refers to M collation
        }                                                                       // end 3 subs

        return -ERRM38;                                                         // junk

    case 'D':                                                                   // $DEVICE
        if (nsubs == 2) {                                                       // two sub case
            i = cstringtoi(subs[0]);                                            // make an int of I/O channel#
            if ((i < 0) || (i > (MAX_SEQ_IO - 1))) return -ERRM38;              // out of I/O channel range
            if (partab.jobtab->seqio[i].type == 0) return -ERRM38;              // no currently opened device

            if (strncasecmp((char *) subs[1]->buf, "$x\0", 3) == 0) {
                return itocstring(buf, partab.jobtab->seqio[i].dx);
            }

            if (strncasecmp((char *) subs[1]->buf, "$y\0", 3) == 0) {
                return itocstring(buf, partab.jobtab->seqio[i].dy);
            }

            if (strncasecmp((char *) subs[1]->buf, "character\0", 10) == 0) {
                return mcopy((u_char *) "M", buf, 1);                           // just an M
            }

            if (strncasecmp((char *) subs[1]->buf, "fd\0", 4) == 0) {
                return itocstring(buf, partab.jobtab->seqio[i].fid);
            }

            if (strncasecmp((char *) subs[1]->buf, "mode\0", 5) == 0) {
                if (partab.jobtab->seqio[i].mode == 0) {
                    return mcopy((u_char *) "PRINCIPAL", buf, 9);
                } else if (partab.jobtab->seqio[i].mode == 1) {
                    return mcopy((u_char *) "WRITE", buf, 5);
                } else if (partab.jobtab->seqio[i].mode == 2) {
                    return mcopy((u_char *) "READ", buf, 4);
                } else if (partab.jobtab->seqio[i].mode == 3) {
                    return mcopy((u_char *) "APPEND", buf, 6);
                } else if (partab.jobtab->seqio[i].mode == 4) {
                    return mcopy((u_char *) "IO", buf, 2);
                } else if (partab.jobtab->seqio[i].mode == 5) {
                    return mcopy((u_char *) "TCPIP", buf, 5);
                } else if (partab.jobtab->seqio[i].mode == 6) {
                    return mcopy((u_char *) "SERVER", buf, 6);
                } else if (partab.jobtab->seqio[i].mode == 7) {
                    return mcopy((u_char *) "NOFORK", buf, 6);
                } else if (partab.jobtab->seqio[i].mode == 8) {
                    return mcopy((u_char *) "FORKED", buf, 6);
                } else if (partab.jobtab->seqio[i].mode == 9) {
                    return mcopy((u_char *) "PIPE", buf, 4);
                } else if (partab.jobtab->seqio[i].mode == 10) {
                    return mcopy((u_char *) "NEWPIPE", buf, 7);
                } else {
                    return mcopy((u_char *) "UNKNOWN", buf, 7);
                }
            }

            if (strncasecmp((char *) subs[1]->buf, "name\0", 5) == 0) {
                return mcopy((u_char *) partab.jobtab->seqio[i].name, buf, MAX_SEQ_NAME);
            }

            if (strncasecmp((char *) subs[1]->buf, "namespace\0", 10) == 0) {
                for (j = 0; j < VAR_LEN; j++) {
                    if (partab.jobtab->seqio[i].namespace.var_cu[j] == '\0') break;
                }

                return mcopy((u_char *) partab.jobtab->seqio[i].namespace.var_cu, buf, j);
            }

            if (strncasecmp((char *) subs[1]->buf, "type\0", 5) == 0) {
                if (partab.jobtab->seqio[i].type == 1) {
                    return mcopy((u_char *) "1,FILE", buf, 6);
                } else if (partab.jobtab->seqio[i].type == 2) {
                    return mcopy((u_char *) "2,SOCKET", buf, 8);
                } else if (partab.jobtab->seqio[i].type == 3) {
                    return mcopy((u_char *) "3,PIPE", buf, 6);
                } else if (partab.jobtab->seqio[i].type == 4) {
                    return mcopy((u_char *) "4,TERMINAL", buf, 10);
                }
            }
        } else if (nsubs == 3) {                                                // end of two sub case - three sub case
            if (strncasecmp((char *) subs[1]->buf, "options\0", 8) == 0) {
                if (strncasecmp((char *) subs[2]->buf, "delete\0", 7) == 0) {
                    if ((partab.jobtab->seqio[i].options & 32) && (partab.jobtab->seqio[i].options & 16)) {
                        return mcopy((u_char *) "BOTH", buf, 4);
                    } else if (partab.jobtab->seqio[i].options & 32) {
                        return mcopy((u_char *) "DELETE", buf, 6);
                    } else if (partab.jobtab->seqio[i].options & 16) {
                        return mcopy((u_char *) "BACK", buf, 4);
                    } else {
                        return mcopy((u_char *) "NONE", buf, 4);
                    }
                }

                if (strncasecmp((char *) subs[2]->buf, "echo\0", 5) == 0) {
                    if (partab.jobtab->seqio[i].options & 8) {
                        return mcopy((u_char *) "1", buf, 1);
                    } else {
                        return mcopy((u_char *) "0", buf, 1);
                    }
                }

                if (strncasecmp((char *) subs[2]->buf, "escape\0", 7) == 0) {
                    if (partab.jobtab->seqio[i].options & 4) {
                        return mcopy((u_char *) "1", buf, 1);
                    } else {
                        return mcopy((u_char *) "0", buf, 1);
                    }
                }

                if (strncasecmp((char *) subs[2]->buf, "output\0", 7) == 0) {
                    if (partab.jobtab->seqio[i].options & 2) {
                        char temp_buf[24];
                        s = 0;

                        for (j = 0; j < partab.jobtab->seqio[i].out_len; j++) {
                            if (iscntrl(partab.jobtab->seqio[i].out_term[j])) temp_buf[s++] = '\\';

                            switch (partab.jobtab->seqio[i].out_term[j]) {
                            case '\a':
                                temp_buf[s++] = 'a';
                                break;

                            case '\b':
                                temp_buf[s++] = 'b';
                                break;

                            case '\f':
                                temp_buf[s++] = 'f';
                                break;

                            case '\n':
                                temp_buf[s++] = 'n';
                                break;

                            case '\r':
                                temp_buf[s++] = 'r';
                                break;

                            case '\t':
                                temp_buf[s++] = 't';
                                break;

                            case '\v':
                                temp_buf[s++] = 'v';
                                break;

                            default:
                                if (iscntrl(partab.jobtab->seqio[i].out_term[j])) {
                                    sprintf(&temp_buf[s], "%03o", partab.jobtab->seqio[i].out_term[j]);
                                    s += 3;
                                } else {
                                    temp_buf[s++] = partab.jobtab->seqio[i].out_term[j];
                                }
                            }
                        }

                        return mcopy((u_char *) temp_buf, buf, s);
                    } else {
                        return 0;
                    }
                }

                if (strncasecmp((char *) subs[2]->buf, "terminator\0", 11) == 0) {
                    if (partab.jobtab->seqio[i].options & 1) {
                        u_int64 in_term = partab.jobtab->seqio[i].in_term.interm[0];
                        int cnt = 0;
                        u_char temp_buf[402];                                   // enough to hold all ASCII characters with ,
                        s = 0;

                        for (j = 0; cnt < in_term; j++) {
                            cnt = 1U << j;

                            if (in_term & cnt) {
                                s += itocstring(&temp_buf[s], j);
                                temp_buf[s++] = ',';
                            }
                        }

                        in_term = partab.jobtab->seqio[i].in_term.interm[1];
                        cnt = 0;

                        for (j = 0; cnt < in_term; j++) {
                            cnt = 1U << j;

                            if (in_term & cnt) {
                                s += itocstring(&temp_buf[s], j + 64);
                                temp_buf[s++] = ',';
                            }
                        }

                        return mcopy((u_char *) temp_buf, buf, --s);
                    } else {
                        return 0;
                    }
                }
            }
        }                                                                       // end 3 subs

        return -ERRM38;                                                         // junk

    case 'G':                                                                   // $GLOBAL
        if (nsubs == 2) {                                                       // two sub case
            if (strncasecmp((char *) subs[1]->buf, "character\0", 10) == 0) {
                return mcopy((u_char *) "M", buf, 1);                           // just an M
            }

            if (strncasecmp((char *) subs[1]->buf, "collate\0", 8) == 0) return 0; // empty string refers to M collation

            if (strncasecmp((char *) subs[1]->buf, "journal\0", 8) == 0) {
                var->slen = strlen((char *) var->key) + 1;                      // first subscript only
                i = DB_GetFlags(var);                                           // get flags
                if (i < 0) return i;                                            // if error then return it
                return itocstring(buf, i & GL_JOURNAL);                         // return 1/0
            }                                                                   // end journal
        }                                                                       // end 2 subs

        if (nsubs > 1) return -ERRM38;                                          // junk
        return DB_Get(var, buf);                                                // let the database module do it

    case 'J':                                                                   // $JOB
        buf[0] = '\0';                                                          // JIC
        if (nsubs == 0) return itocstring(buf, systab->maxjob);                 // max permitted jobs
        if (nsubs < 2) return -ERRM38;                                          // junk
        i = cstringtoi(subs[0]) - 1;                                            // make an int of job#
        if ((i < 0) || (i >= systab->maxjob)) return -ERRM23;                   // in range? no - complain
        if (systab->jobtab[i].pid == 0) return -ERRM23;                         // process id? complain if no such

        if (kill(systab->jobtab[i].pid, 0)) {                                   // check the job
            if (errno == ESRCH) {                                               // doesn't exist
                CleanJob(i + 1);                                                // zot if not there
                return -ERRM23;                                                 // no - complain
            }
        }

        if (nsubs == 2) {                                                       // two sub case
            if (strncasecmp((char *) subs[1]->buf, "$io\0", 4) == 0) {
                return itocstring(buf, systab->jobtab[i].io);                   // ^$JOB(n,"$IO")
            }

            if (strncasecmp((char *) subs[1]->buf, "$reference\0", 11) == 0) {
                vp = &systab->jobtab[i].last_ref;                               // addr of $REFERENCE
                if (vp->name.var_cu[0] == '\0') return 0;                       // return null string
                bcopy(vp, tmp, vp->slen + sizeof(var_u) + 4);                   // copy to tmp
                vp = (mvar *) tmp;                                              // point at tmp
                if (vp->uci == 0) vp->uci = systab->jobtab[i].uci;
                if (vp->volset == 0) vp->volset = systab->jobtab[i].vol;
                return UTIL_String_Mvar(vp, buf, MAX_NUM_SUBS);                 // ^$JOB(n,"$REFERENCE")
            }

            if (strncasecmp((char *) subs[1]->buf, "$stack\0", 7) == 0) {
                return itocstring(buf, systab->jobtab[i].cur_do);
            }

            if (strncasecmp((char *) subs[1]->buf, "character\0", 10) == 0) {
                return mcopy((u_char *) "M", buf, 1);                           // just an M
            }

            if (strncasecmp((char *) subs[1]->buf, "commands\0", 9) == 0) {
                return uitocstring(buf, systab->jobtab[i].commands);
            }

            if (strncasecmp((char *) subs[1]->buf, "global\0", 7) == 0) {
                return itocstring(buf, systab->jobtab[i].uci);
            }

            if (strncasecmp((char *) subs[1]->buf, "global_vol\0", 11) == 0) {
                return itocstring(buf, systab->jobtab[i].vol);
            }

            if (strncasecmp((char *) subs[1]->buf, "grefs\0", 6) == 0) {
                return uitocstring(buf, systab->jobtab[i].grefs);
            }

            if (strncasecmp((char *) subs[1]->buf, "lock\0", 5) == 0) {
                return itocstring(buf, systab->jobtab[i].luci);
            }

            if (strncasecmp((char *) subs[1]->buf, "lock_vol\0", 9) == 0) {
                return itocstring(buf, systab->jobtab[i].lvol);
            }

            if (strncasecmp((char *) subs[1]->buf, "owner\0", 6) == 0) {
                pp = getpwuid((uid_t) systab->jobtab[i].user);                  // get pw
                if (pp == NULL) return itocstring(buf, systab->jobtab[i].user); // on fail, return numb
                strcpy((char *) buf, pp->pw_name);                              // copy it
                return (int) strlen((char *) buf);                              // return len
            }

            if (strncasecmp((char *) subs[1]->buf, "owner_id\0", 9) == 0) {
                return itocstring(buf, systab->jobtab[i].user);
            }

            if (strncasecmp((char *) subs[1]->buf, "pid\0", 4) == 0) {
                return itocstring(buf, systab->jobtab[i].pid);
            }

            if (strncasecmp((char *) subs[1]->buf, "precision\0", 10) == 0) {
                return itocstring(buf, systab->jobtab[i].precision);
            }

            if (strncasecmp((char *) subs[1]->buf, "priority\0", 9) == 0) {
                errno = 0;
                j = getpriority(PRIO_PROCESS, systab->jobtab[i].pid);
                if (errno != 0) return -(ERRMLAST + ERRZLAST + errno);
                return itocstring(buf, j);
            }

            if (strncasecmp((char *) subs[1]->buf, "priv\0", 5) == 0) {
                return itocstring(buf, systab->jobtab[i].priv);
            }

            if (strncasecmp((char *) subs[1]->buf, "process_start\0", 14) == 0) {
                return mcopy(systab->jobtab[i].start_dh,                        // the data
                             buf,                                               // the destination
                             systab->jobtab[i].start_len);                      // and length
            }

            if (strncasecmp((char *) subs[1]->buf, "routine\0", 8) == 0) {
                return itocstring(buf, systab->jobtab[i].ruci);
            }

            if (strncasecmp((char *) subs[1]->buf, "routine_name\0", 13) == 0) {
                j = systab->jobtab[i].cur_do;                                   // get current do
                rounam = &systab->jobtab[i].dostk[j].rounam;                    // point at routine name

                for (s = 0; s < VAR_LEN; s++) {                                 // for each character
                    if ((buf[s] = rounam->var_cu[s]) == 0) break;               // copy it
                }

                buf[s] = '\0';                                                  // null terminate
                return s;                                                       // return length
            }

            if (strncasecmp((char *) subs[1]->buf, "routine_vol\0", 12) == 0) {
                return itocstring(buf, systab->jobtab[i].rvol);
            }
        } else if (nsubs == 3) {                                                // end of two sub case - three sub case
            if (strncasecmp((char *) subs[1]->buf, "$io\0", 4) == 0) {
                j = cstringtoi(subs[2]);                                        // get chan as int
                if ((j < 0) || (j >= MAX_SEQ_IO)) return -ERRM38;               // out of range
                if (systab->jobtab[i].seqio[j].type == SQ_FREE) return 0;       // not in use
                cnt = strlen((char *) systab->jobtab[i].seqio[j].name);         // get length
                return mcopy(systab->jobtab[i].seqio[j].name, buf, cnt);        // copy and exit
            }                                                                   // end $io,n)

            if (strncasecmp((char *) subs[1]->buf, "$stack\0", 7) == 0) {
                return Dstack1x(buf, cstringtoi(subs[2]), i);                   // do it elsewhere
            }
        } else if (nsubs == 4) {                                                // end 3 sub case - four sub case
            if (strncasecmp((char *) subs[1]->buf, "$stack\0", 7) == 0)
            return Dstack2x(buf, cstringtoi(subs[2]), subs[3], i);              // do it elsewhere
        }                                                                       // end 4 sub case

        return -ERRM38;                                                         // junk

    case 'L':                                                                   // $LOCK
        if (nsubs != 1) return -ERRM38;                                         // junk
        if (subs[0]->len > 511) return -(ERRZ12 + ERRMLAST);                    // junk
        vp = (mvar *) &tmp[512];                                                // some temp space
        s = UTIL_MvarFromCStr(subs[0], vp);                                     // convert to mvar
        if (s < 0) return s;                                                    // quit on error
        s = UTIL_mvartolock(vp, subs[0]->buf);                                  // cvt to locktab style
        if (s < 0) return s;                                                    // quit on error
        subs[0]->len = s;                                                       // save the length
        return LCK_Get(subs[0], buf);                                           // do it and exit

    case 'R':                                                                   // $ROUTINE
        if (nsubs > 2) return -ERRM38;                                          // junk

        if (nsubs == 2) {
            if (strncasecmp((char *) subs[1]->buf, "character\0", 10) == 0) {
                return mcopy((u_char *) "M", buf, 1);                           // just an M
            }
        }

        return DB_Get(var, buf);                                                // let the database module do it

    case 'S':                                                                   // $SYSTEM
        if (nsubs == 0) return -ERRM38;                                         // junk

        if ((nsubs == 1) && (strncasecmp((char *) subs[0]->buf, "$nextok\0", 8) == 0)) {
            return itocstring(buf, (systab->historic & HISTORIC_DNOK) / HISTORIC_DNOK); // return the value
        }

        if ((nsubs == 1) && (strncasecmp((char *) subs[0]->buf, "eok\0", 4) == 0)) {
            return itocstring(buf, (systab->historic & HISTORIC_EOK));          // return the value
        }

        if ((nsubs == 1) && (strncasecmp((char *) subs[0]->buf, "offok\0", 6) == 0)) {
            return itocstring(buf, (systab->historic & HISTORIC_OFFOK) / HISTORIC_OFFOK); // return the value
        }

        if ((nsubs == 1) && (strncasecmp((char *) subs[0]->buf, "big_endian\0", 11) == 0)) {
            u_int end = 0x1;
            return itocstring(buf, (*(u_char *) &end == 0x1) ? 0 : 1);          // little-endian is 0, big-endian is 1
        }

        if ((nsubs == 1) && (strncasecmp((char *) subs[0]->buf, "name_length\0", 12) == 0)) {
            return itocstring(buf, VAR_LEN);
        }

        if ((nsubs == 1) && (strncasecmp((char *) subs[0]->buf, "precision\0", 10) == 0)) {
            return itocstring(buf, systab->precision);                          // return the value
        }

        if ((nsubs == 1) && (strncasecmp((char *) subs[0]->buf, "string_max\0", 11) == 0)) {
            return itocstring(buf, MAX_STR_LEN);
        }

        if (strncasecmp((char *) subs[0]->buf, "trantab\0", 8) == 0) {
            i = cstringtoi(subs[1]) - 1;                                        // make an int of entry#
            if (!(i < MAX_TRANTAB) || (i < 0)) return -ERRM38;                  // validate it, junk
            if (nsubs != 2) return -ERRM38;                                     // must be 2 subs

            if (!systab->tt[i].from_vol) {                                      // if nothing there
                buf[0] = '\0';                                                  // null terminate
                return 0;                                                       // and return nothing
            }

            s = UTIL_String_Mvar((mvar *) &systab->tt[i].to_global, buf, 0);
            buf[s++] = '=';
            s += UTIL_String_Mvar((mvar *) &systab->tt[i].from_global, &buf[s], 0);
            return s;
        }                                                                       // end trantab stuff

        if (strncasecmp((char *) subs[0]->buf, "vol\0", 4) == 0) {
            i = cstringtoi(subs[1]) - 1;                                        // make an int of vol#

            if (!(i < MAX_VOL) || (i < 0) || (systab->vol[i] == NULL)) {        // validate it
                return -ERRM38;                                                 // junk
            }

            if (nsubs < 3) return -ERRM38;                                      // must be 3 subs

            if (strncasecmp((char *) subs[2]->buf, "block\0", 6) == 0) {
                return uitocstring(buf, systab->vol[i]->vollab->block_size);
            }

            if (strncasecmp((char *) subs[2]->buf, "file\0", 5) == 0) {
                strcpy((char *) buf, systab->vol[i]->file_name);                // copy it
                return (int) strlen((char *) buf);                              // return the length
            }

            if (strncasecmp((char *) subs[2]->buf, "free\0", 5) == 0) {
                return uitocstring(buf, DB_Free(i + 1));                        // return free blocks
            }

            if (strncasecmp((char *) subs[2]->buf, "header\0", 7) == 0) {
                return itocstring(buf, systab->vol[i]->vollab->header_bytes);
            }

            if (strncasecmp((char *) subs[2]->buf, "journal_available\0", 18) == 0) {
                return itocstring(buf, systab->vol[i]->vollab->journal_available);
            }

            if (strncasecmp((char *) subs[2]->buf, "journal_file\0", 13) == 0) {
                (void) strcpy((char *) buf, systab->vol[i]->vollab->journal_file);
                return (int) strlen((char *) buf);
            }

            if (strncasecmp((char *) subs[2]->buf, "journal_requested\0", 18) == 0) {
                return itocstring(buf, systab->vol[i]->vollab->journal_requested);
            }

            if (strncasecmp((char *) subs[2]->buf, "journal_size\0", 13) == 0) {
                return itocstring(buf, systab->vol[i]->jrn_next);
            }

            if (strncasecmp((char *) subs[2]->buf, "name\0", 5) == 0) {
                for (j = 0; j < VAR_LEN; j++) {
                    if ((buf[j] = systab->vol[i]->vollab->volnam.var_cu[j]) == 0) break;
                }

                buf[j] = '\0';
                return j;
            }

            if (strncasecmp((char *) subs[2]->buf, "size\0", 5) == 0) {
                return itocstring(buf, systab->vol[i]->vollab->max_block);
            }

            if (strncasecmp((char *) subs[2]->buf, "uci\0", 4) == 0) {
                if (nsubs != 4) return -ERRM38;                                 // must be 4 subs
                j = cstringtoi(subs[3]) - 1;                                    // make an int of UCI#
                if (!(j < UCIS) || (j < 0)) return -ERRM38;                     // validate it, junk

                for (s = 0; s < VAR_LEN; s++) {
                    if ((buf[s] = systab->vol[i]->vollab->uci[j].name.var_cu[s]) == 0) break;
                }

                buf[s] = '\0';
                return s;
            }

            if (strncasecmp((char *) subs[2]->buf, "writelock\0", 10) == 0) {
                return itocstring(buf, systab->vol[i]->writelock);
            }

            if (strncasecmp((char *) subs[2]->buf, "blkalloc\0", 9) == 0) {
                return itocstring(buf, systab->vol[i]->stats.blkalloc);
            }

            if (strncasecmp((char *) subs[2]->buf, "blkdeall\0", 9) == 0) {
                return itocstring(buf, systab->vol[i]->stats.blkdeall);
            }

            if (strncasecmp((char *) subs[2]->buf, "blkreorg\0", 9) == 0) {
                return itocstring(buf, systab->vol[i]->stats.blkreorg);
            }

            if (strncasecmp((char *) subs[2]->buf, "dbdat\0", 6) == 0) {
                return itocstring(buf, systab->vol[i]->stats.dbdat);
            }

            if (strncasecmp((char *) subs[2]->buf, "dbget\0", 6) == 0) {
                return itocstring(buf, systab->vol[i]->stats.dbget);
            }

            if (strncasecmp((char *) subs[2]->buf, "dbkil\0", 6) == 0) {
                return itocstring(buf, systab->vol[i]->stats.dbkil);
            }

            if (strncasecmp((char *) subs[2]->buf, "dbord\0", 6) == 0) {
                return itocstring(buf, systab->vol[i]->stats.dbord);
            }

            if (strncasecmp((char *) subs[2]->buf, "dbqry\0", 6) == 0) {
                return itocstring(buf, systab->vol[i]->stats.dbqry);
            }

            if (strncasecmp((char *) subs[2]->buf, "dbset\0", 6) == 0) {
                return itocstring(buf, systab->vol[i]->stats.dbset);
            }

            if (strncasecmp((char *) subs[2]->buf, "lastok\0", 7) == 0) {
                return itocstring(buf, systab->vol[i]->stats.lastok);
            }

            if (strncasecmp((char *) subs[2]->buf, "lasttry\0", 8) == 0) {
                return itocstring(buf, systab->vol[i]->stats.lasttry);
            }

            if (strncasecmp((char *) subs[2]->buf, "logrd\0", 6) == 0) {
                return itocstring(buf, systab->vol[i]->stats.logrd);
            }

            if (strncasecmp((char *) subs[2]->buf, "logwt\0", 6) == 0) {
                return itocstring(buf, systab->vol[i]->stats.logwt);
            }

            if (strncasecmp((char *) subs[2]->buf, "phyrd\0", 6) == 0) {
                return itocstring(buf, systab->vol[i]->stats.phyrd);
            }

            if (strncasecmp((char *) subs[2]->buf, "phywt\0", 6) == 0) {
                return itocstring(buf, systab->vol[i]->stats.phywt);
            }

            if (strncasecmp((char *) subs[2]->buf, "diskerrors\0", 11) == 0) {
                return itocstring(buf, systab->vol[i]->stats.diskerrors);
            }
        }                                                                       // end of "VOL"

        return -ERRM38;                                                         // junk
    }                                                                           // end of switch

    return -ERRM38;                                                             // can't get here?
}

short SS_Set(mvar *var, cstring *data)                                          // set SSVN data
{
    int     i = 0;                                                              // useful int
    int     j;                                                                  // and another
    short   s;                                                                  // for functions
    int     cnt;                                                                // count of bytes used
    var_u   n;                                                                  // for names
    u_char  tmp[1024];                                                          // temp string space
    int     ptmp = 0;                                                           // pointer into this
    int     nsubs = 0;                                                          // count subscripts
    cstring *subs[4];                                                           // where to put them
    trantab tt;                                                                 // for translations

    while (i < var->slen) {                                                     // for all subs
        cnt = 0;                                                                // flag no rabbit ears quotes
        if (nsubs > 3) return -ERRM38;                                          // junk
        subs[nsubs] = (cstring *) &tmp[ptmp];                                   // point at the buffer

        s = UTIL_Key_Extract(&var->key[i],                                      // key from here
                             subs[nsubs]->buf,                                  // where to put it
                             &cnt);                                             // the count

        if (s < 0) return s;                                                    // die on error
        subs[nsubs++]->len = s;                                                 // save the size (incr count)
        ptmp += s + sizeof(short) + 1;                                          // move up temp area
        i = i + cnt;                                                            // count used bytes
    }

    s = SS_Norm(var);                                                           // normalize the name
    if (s < 0) return s;                                                        // return on error

    switch (var->name.var_cu[1]) {                                              // check initial of name
    case 'C':                                                                   // $CHARACTER
        return -ERRM29;                                                         // SET on SSVN not on

    case 'D':                                                                   // $DEVICE
        return -ERRM29;                                                         // SET on SSVN not on

    case 'G':                                                                   // $GLOBAL
        if (nsubs == 2) {                                                       // two sub case
            if (strncasecmp((char *) subs[1]->buf, "journal\0", 8) == 0) {
                var->slen = strlen((char *) var->key) + 1;                      // first subscript only
                i = cstringtob(data);                                           // get value as 1/0
                if (!i) i = -1;                                                 // setup for a clear
                i = DB_SetFlags(var, i);                                        // set flags
                if (i < 0) return (short) i;                                    // if error then return it
                return 0;                                                       // done
            }                                                                   // end journal
        }                                                                       // end 2 subs

        return -ERRM29;                                                         // SET on SSVN not on

    case 'J':                                                                   // $JOB
        if (nsubs != 2) return -ERRM38;                                         // junk
        i = cstringtoi(subs[0]) - 1;                                            // make an int of job#
        if ((i < 0) || (i >= systab->maxjob)) return -ERRM23;                   // in range? no - complain
        if (systab->jobtab[i].pid == 0) return -ERRM23;                         // process id? complain if no such
        j = cstringtoi(data);                                                   // convert to int

        if ((partab.jobtab - systab->jobtab) == i) {                            // same job?
            if (strncasecmp((char *) subs[1]->buf, "global\0", 7) == 0) {
                if ((j < 1) || (j > UCIS)) return -ERRM26;                      // out of range
                systab->jobtab[i].uci = j;                                      // set it
                VAR_CLEAR(systab->jobtab[i].last_ref.name);                     // clear $REFERENCE
                return 0;                                                       // and quit
            }

            if (strncasecmp((char *) subs[1]->buf, "global_vol\0", 11) == 0) {
                if ((j < 1) || (j > MAX_VOL)) return -ERRM26;                   // out of range
                systab->jobtab[i].vol = j;                                      // set it
                VAR_CLEAR(systab->jobtab[i].last_ref.name);                     // clear $REFERENCE
                return 0;                                                       // and quit
            }

            if (strncasecmp((char *) subs[1]->buf, "lock\0", 5) == 0) {
                if ((j < 1) || (j > UCIS)) return -ERRM26;                      // out of range
                systab->jobtab[i].luci = j;                                     // set it
                return 0;                                                       // and quit
            }

            if (strncasecmp((char *) subs[1]->buf, "lock_vol\0", 9) == 0) {
                if ((j < 1) || (j > MAX_VOL)) return -ERRM26;                   // out of range
                systab->jobtab[i].lvol = j;                                     // set it
                return 0;                                                       // and quit
            }

            if (strncasecmp((char *) subs[1]->buf, "precision\0", 10) == 0) {
                if ((j < 0) || (j > MAX_PREC)) return -ERRM28;
                systab->jobtab[i].precision = j;
                return 0;
            }

            if (strncasecmp((char *) subs[1]->buf, "routine\0", 8) == 0) {
                if ((j < 1) || (j > UCIS)) return -ERRM26;                      // out of range
                systab->jobtab[i].ruci = j;                                     // set it
                return 0;                                                       // and quit
            }

            if (strncasecmp((char *) subs[1]->buf, "routine_vol\0", 12) == 0) {
                if ((j < 1) || (j > MAX_VOL)) return -ERRM26;                   // out of range
                systab->jobtab[i].rvol = j;                                     // set it
                return 0;                                                       // and quit
            }
        }

        if (priv()) {                                                           // is it priveleged ?
            if (strncasecmp((char *) subs[1]->buf, "owner_id\0", 9) == 0) {
                systab->jobtab[i].user = j;                                     // SHOULD HAVE SOME CHECKS HERE
                return 0;                                                       // and quit
            }

            if (strncasecmp((char *) subs[1]->buf, "priority\0", 9) == 0) {
                errno = 0;

                if (setpriority(PRIO_PROCESS, systab->jobtab[i].pid, j) == -1) {
                    return -(ERRMLAST + ERRZLAST + errno);
                }

                return 0;
            }

            if (strncasecmp((char *) subs[1]->buf, "priv\0", 5) == 0) {
                systab->jobtab[i].priv = (j || 0);                              // set to 0 or 1
                if (!j) j = setuid(partab.jobtab->user);                        // if clearing PRIV then attempt to change user
                return 0;
            }
        }

        return -ERRM29;                                                         // SET or KILL on SSVN not on

    case 'L':                                                                   // $LOCK
        return -ERRM29;                                                         // SET on SSVN not on

    case 'R':                                                                   // $ROUTINE
        // We may eventually allow SET ^$ROUTINE(rou,0) with tests
        if (nsubs > 2) return -ERRM38;                                          // junk
        return -ERRM29;                                                         // SET on SSVN not on

    case 'S':                                                                   // $SYSTEM
        if (!priv()) return -ERRM38;                                            // need privs

        if ((nsubs == 1) && (strncasecmp((char *) subs[0]->buf, "$nextok\0", 8) == 0)) {
            if (cstringtob(data)) {
                systab->historic |= HISTORIC_DNOK;
            } else {
                systab->historic &= ~HISTORIC_DNOK;
            }

            return 0;                                                           // and exit
        }

        if ((nsubs == 1) && (strncasecmp((char *) subs[0]->buf, "eok\0", 4) == 0)) {
            if (cstringtob(data)) {
                systab->historic |= HISTORIC_EOK;
            } else {
                systab->historic &= ~HISTORIC_EOK;
            }

            return 0;                                                           // and exit
        }

        if ((nsubs == 1) && (strncasecmp((char *) subs[0]->buf, "offok\0", 6) == 0)) {
            if (cstringtob(data)) {
                systab->historic |= HISTORIC_OFFOK;
            } else {
                systab->historic &= ~HISTORIC_OFFOK;
            }

            return 0;                                                           // and exit
        }

        if ((nsubs == 1) && (strncasecmp((char *) subs[0]->buf, "precision\0", 10) == 0)) {
            j = cstringtoi(data);
            if ((j < 0) || (j > MAX_PREC)) return -ERRM28;
            systab->precision = j;
            return 0;                                                           // and exit
        }

        if (strncasecmp((char *) subs[0]->buf, "trantab\0", 8) == 0) {
            cnt = cstringtoi(subs[1]) - 1;                                      // make an int of entry#
            if (!(cnt < MAX_TRANTAB) || (cnt < 0)) return -ERRM38;              // validate it, junk
            if (nsubs != 2) return -ERRM38;                                     // must be 2 subs

            if (data->len == 0) {                                               // if null
                bzero(&systab->tt[cnt], sizeof(trantab));                       // clear it
                systab->max_tt = 0;                                             // clear this for now

                for (i = MAX_TRANTAB; i; i--) {                                 // look for last used
                    if (systab->tt[i - 1].to_uci) {                             // if found
                        systab->max_tt = i;                                     // save here
                        break;                                                  // exit
                    }
                }

                return 0;                                                       // and exit
            }

            subs[2] = (cstring *) tmp;                                          // some space
            subs[3] = (cstring *) &tmp[512];                                    // some more

            for (i = 0; ; i++) {                                                // scan input
                if (data->buf[i] == '=') {                                      // found =
DISABLE_WARN(-Warray-bounds)
                    subs[3]->buf[i] = '\0';                                     // null terminate
                    subs[3]->len = i++;                                         // save length (incr i)
                    break;                                                      // and exit loop
                }

                subs[3]->buf[i] = data->buf[i];                                 // copy
            }                                                                   // destination created

            j = 0;                                                              // clear index
            while ((subs[2]->buf[j++] = data->buf[i++])) continue;              // and other one
ENABLE_WARN
            s = UTIL_MvarFromCStr(subs[2], &partab.src_var);                    // encode
            if (s < 0) return s;                                                // complain on error

            if (partab.src_var.uci == UCI_IS_LOCALVAR) {                        // if local var, just return
                return 0;
            } else if (!partab.src_var.uci) {                                   // if no UCI
                if (partab.src_var.name.var_cu[0] == '%') {                     // if % var
                    partab.src_var.uci = 1;                                     // manager UCI
                } else {
                    partab.src_var.uci = partab.jobtab->uci;                    // default
                }
            }

            if (!partab.src_var.volset) partab.src_var.volset = partab.jobtab->vol; // if no volset, set default
            bcopy(&partab.src_var, &tt.from_global, sizeof(var_u) + 2);
            s = UTIL_MvarFromCStr(subs[3], &partab.src_var);                    // encode
            if (s < 0) return s;                                                // complain on error

            if (partab.src_var.uci == UCI_IS_LOCALVAR) {                        // if local var, just return - remove for triggers
                return 0;
            } else if (!partab.src_var.uci) {                                   // if no UCI
                if (partab.src_var.name.var_cu[0] == '%') {                     // if % var
                    partab.src_var.uci = 1;                                     // manager UCI
                } else {
                    partab.src_var.uci = partab.jobtab->uci;                    // default
                }
            }

            if (!partab.src_var.volset) partab.src_var.volset = partab.jobtab->vol; // if no volset, set default

            /*
            if (partab.src_var.uci == UCI_IS_LOCALVAR) {                        // use trantab for triggers via local as to_global
                partab.src_var.volset = 0;
                partab.src_var.uci = 0;
            }
            */

            bcopy(&partab.src_var, &tt.to_global, sizeof(var_u) + 2);
            bcopy(&tt, &systab->tt[cnt], sizeof(trantab));
            if ((cnt + 1) > systab->max_tt) systab->max_tt = cnt + 1;           // check flag and ensure current is there
            return 0;
        }                                                                       // end trantab stuff

        if ((nsubs == 4) && (strncasecmp((char *) subs[0]->buf, "vol\0", 4) == 0) &&
          (strncasecmp((char *) subs[2]->buf, "uci\0", 4) == 0)) {              // ^$SYSTEM("VOL",n,"UCI",n)
            i = cstringtoi(subs[1]) - 1;                                        // get vol#
            j = cstringtoi(subs[3]) - 1;                                        // and UCI#
            if ((i < 0) || (i >= MAX_VOL)) return -ERRM60;                      // out of range
            if ((j < 0) || (j >= UCIS)) return -ERRM60;                         // out of range

            if ((data->len < 1) || (data->len > VAR_LEN)) {
                return -(ERRZ12 + ERRMLAST);                                    // syntx
            }

            VAR_CLEAR(n);                                                       // clear name

            for (s = 0; s < data->len; s++) {
                if (isalpha(data->buf[s]) == 0)
                return -(ERRZ12 + ERRMLAST);                                    // syntx
                n.var_cu[s] = data->buf[s];                                     // copy to name
            }

            return DB_UCISet(i + 1, j + 1, n);                                  // do it and return
        }

        if ((nsubs == 3) && (strncasecmp((char *) subs[0]->buf, "vol\0", 4) == 0)) { // ^$SYSTEM("VOL",n,..)
            i = cstringtoi(subs[1]) - 1;                                        // get vol#
            if ((i < 0) || (i >= MAX_VOL)) return -ERRM60;                      // out of range

            if (strncasecmp((char *) subs[2]->buf, "file\0", 5) == 0) {         // mount new volume to volume set
                /*
                if (data->len > VOL_FILENAME_MAX) return -ERRM56;               // too long
                s = DB_Mount((char *) data->buf, i, 1, 1);                      // file, volume number
                if (s < 0) return s;                                            // die on error
                systab->vol[i]->map_dirty_flag = 1;                             // tell them to write it
                return 0;
                */

                return -ERRM38;                                                 // not yet supported
            }

            if ((strncasecmp((char *) subs[2]->buf, "journal_file\0", 13) == 0) && (systab->maxjob == 1)) {
                if (data->len > JNL_FILENAME_MAX) return -ERRM56;               // too long
                (void) strcpy(systab->vol[i]->vollab->journal_file, (char *) data->buf);
                systab->vol[i]->map_dirty_flag = 1;                             // tell them to write it
                return 0;
            }

            if (strncasecmp((char *) subs[2]->buf, "journal_requested\0", 18) == 0) {
                systab->vol[i]->vollab->journal_requested = cstringtob(data);

                if (!systab->vol[i]->vollab->journal_requested) {
                    DB_StopJournal(i + 1, JRN_STOP);                            // stop journaling
                }

                systab->vol[i]->map_dirty_flag = 1;                             // tell them to write it
                return 0;
            }

            if ((strncasecmp((char *) subs[2]->buf, "journal_size\0", 13) == 0) && (cstringtoi(data) == 0)) { // clear journal
                while (SemOp(SEM_GLOBAL, -systab->maxjob)) continue;            // lock GLOBAL
                ClearJournal(i);                                                // do it
                SemOp(SEM_GLOBAL, systab->maxjob);                              // unlock global
                return 0;                                                       // done
            }

            if ((strncasecmp((char *) subs[2]->buf, "name\0", 5) == 0) && (systab->maxjob == 1) &&
              (data->len > 0) && (data->len < VAR_LEN)) {
                for (j = 0; j < data->len; j++) {
                    if (isalpha(data->buf[j]) == 0) return -ERRM38;
                }

                VAR_CLEAR(systab->vol[i]->vollab->volnam);                      // zot name
                bcopy(data->buf, systab->vol[i]->vollab->volnam.var_cu, data->len);
                systab->vol[i]->map_dirty_flag = 1;                             // tell them to write it
                return 0;
            }

            if ((strncasecmp((char *) subs[2]->buf, "size\0", 5) == 0) && (systab->maxjob == 1)) {
                u_int vsiz;                                                     // for the size

                vsiz = (u_int) atol((char *) data->buf);                        // get the new

                if (vsiz <= systab->vol[i]->vollab->max_block) return -ERRM38;
                vsiz |= 7;                                                      // fix size
                if (vsiz > MAX_DATABASE_BLKS) return -ERRM38;

                if (vsiz > (((systab->vol[i]->vollab->header_bytes - sizeof(label_block)) * 8) | 7)) {
                    return -ERRM38;
                }

                return DB_Expand(i, vsiz);                                      // do it
            }

            if (strncasecmp((char *) subs[2]->buf, "writelock\0", 10) == 0) {
                if (abs(systab->vol[i]->writelock) == (MAX_JOBS + 1)) return 0; // do nothing if system shutting down with (rsm -k)
                systab->vol[i]->writelock = (cstringtob(data) ? -(partab.jobtab - systab->jobtab + 1) : 0); // set it or clear it
                return 0;                                                       // return OK
            }
        }

        return -ERRM38;                                                         // do vol mount next vers
    }

    return -ERRM38;                                                             // can't get here?
}

short SS_Data(mvar *var, u_char *buf)                                           // get $DATA()
{
    int     i = 0;                                                              // useful int
    short   s;                                                                  // for functions
    int     cnt;                                                                // count of bytes used
    u_char  tmp[1024];                                                          // temp string space
    int     ptmp = 0;                                                           // pointer into this
    int     nsubs = 0;                                                          // count subscripts
    mvar    *vp;                                                                // variable ptr
    cstring *subs[4];                                                           // where to put them

    while (i < var->slen) {                                                     // for all subs
        cnt = 0;                                                                // flag no rabbit ears quotes
        if (nsubs > 3) return -ERRM38;                                          // junk
        subs[nsubs] = (cstring *) &tmp[ptmp];                                   // point at the buffer

        s = UTIL_Key_Extract(&var->key[i],                                      // key from here
                             subs[nsubs]->buf,                                  // where to put it
                             &cnt);                                             // the count

        if (s < 0) return s;                                                    // die on error
        subs[nsubs++]->len = s;                                                 // save the size (incr count)
        ptmp += s + sizeof(short) + 1;                                          // move up temp area
        i = i + cnt;                                                            // count used bytes
    }

    s = SS_Norm(var);                                                           // normalize the name
    if (s < 0) return s;                                                        // return on error

    switch (var->name.var_cu[1]) {                                              // check initial of name
    case 'C':                                                                   // $CHARACTER
        return -ERRM38;                                                         // junk

    case 'D':                                                                   // $DEVICE
        return -ERRM38;                                                         // junk

    case 'G':                                                                   // $GLOBAL
        if (nsubs > 1) return -ERRM38;                                          // junk
        return DB_Data(var, buf);                                               // let the database module do it

    case 'J':                                                                   // $JOB
        if (nsubs != 1) return -ERRM38;                                         // junk
        i = cstringtoi(subs[0]);                                                // make an int of job#
        if ((i < 1) || (i > systab->maxjob)) return -ERRM23;                    // in range? no - complain
        buf[0] = '1';                                                           // assume true
        buf[1] = '\0';                                                          // null terminate
        if (systab->jobtab[i - 1].pid == 0) buf[0] = '0';                       // process id? false if no such
        return 1;                                                               // return the count

    case 'L':                                                                   // $LOCK
        if (nsubs != 1) return -ERRM38;                                         // junk
        if (subs[0]->len > 511) return -(ERRZ12 + ERRMLAST);                    // junk
        vp = (mvar *) &tmp[512];                                                // some temp space
        s = UTIL_MvarFromCStr(subs[0], vp);                                     // convert to mvar
        if (s < 0) return s;                                                    // quit on error
        s = UTIL_mvartolock(vp, subs[0]->buf);                                  // cvt to locktab style
        if (s < 0) return s;                                                    // quit on error
        subs[0]->len = s;                                                       // save the length
        s = LCK_Get(subs[0], buf);                                              // try to get it
        if (s < 0) return s;                                                    // quit on error
        buf[0] = (s ? '1' : '0');                                               // set the answer
        buf[1] = '\0';                                                          // null terminate
        return 1;                                                               // and return

    case 'R':                                                                   // $ROUTINE
        if (nsubs > 2) return -ERRM38;                                          // junk
        return DB_Data(var, buf);                                               // let the database module do it

    case 'S':                                                                   // $SYSTEM
        return -ERRM38;                                                         // junk
    }

    return -ERRM38;                                                             // can't get here?
}

short SS_Kill(mvar *var)                                                        // remove sub-tree
{
    int             i = 0;                                                      // useful int
    int             j;                                                          // and another
    short           s;                                                          // for functions
    int             cnt;                                                        // count of bytes used
    var_u           rou;                                                        // for routine name
    u_char          tmp[1024];                                                  // temp string space
    int             ptmp = 0;                                                   // pointer into this
    int             nsubs = 0;                                                  // count subscripts
    mvar            *vp;                                                        // variable ptr
    cstring         *subs[4];                                                   // where to put them
    struct shmid_ds sbuf;                                                       // for shmctl (shutdown)

    while (i < var->slen) {                                                     // for all subs
        cnt = 0;                                                                // flag no rabbit ears quotes
        if (nsubs > 3) return -ERRM38;                                          // junk
        subs[nsubs] = (cstring *) &tmp[ptmp];                                   // point at the buffer

        s = UTIL_Key_Extract(&var->key[i],                                      // key from here
                             subs[nsubs]->buf,                                  // where to put it
                             &cnt);                                             // the count

        if (s < 0) return s;                                                    // die on error
        subs[nsubs++]->len = s;                                                 // save the size (incr count)
        ptmp += s + sizeof(short) + 1;                                          // move up temp area
        i += cnt;                                                               // count used bytes
    }

    s = SS_Norm(var);                                                           // normalize the name
    if (s < 0) return s;                                                        // return on error

    switch (var->name.var_cu[1]) {                                              // check initial of name
    case 'C':                                                                   // $CHARACTER
        return -ERRM29;                                                         // KILL on SSVN not on

    case 'D':                                                                   // $DEVICE
        return -ERRM29;                                                         // KILL on SSVN not on

    case 'G':                                                                   // $GLOBAL
        if (nsubs > 1) return -ERRM38;                                          // junk
        return -ERRM29;                                                         // KILL on SSVN not on

    case 'J':                                                                   // $JOB
        if (nsubs > 1) return -ERRM38;                                          // junk

        if (nsubs == 1) {                                                       // if there is a job
            j = cstringtoi(subs[0]) - 1;                                        // make an int of it
            if ((j < 0) || (j >= systab->maxjob)) return -ERRM23;               // in range? no - complain
            i = systab->jobtab[j].pid;                                          // get process id
            if (i == 0) return -ERRM23;                                         // complain if no such

            if (!priv() && (systab->jobtab[j].user != partab.jobtab->user)) {
                return -ERRM29;                                                 // KILL on SSVN not on
            }

            if (!kill(i, SIGTERM)) return 0;                                    // tell in to go home
            systab->jobtab[j].trap = 1U << SIGTERM;                             // say go away
            systab->jobtab[j].attention = 1;                                    // look at it
            return 0;                                                           // say it worked
        }

        if (!priv()) return -ERRM29;                                            // SET or KILL on SSVN not on
        systab->start_user = -1;                                                // Say 'shutting down'
        i = shmctl(systab->vol[0]->shm_id, IPC_RMID, &sbuf);                    // remove the share

        for (i = 0; i < systab->maxjob; i++) {                                  // for each job
            cnt = systab->jobtab[i].pid;                                        // get pid

            if ((cnt != partab.jobtab->pid) && cnt) {
                if (!kill(cnt, SIGTERM)) {                                      // kill this one
                    systab->jobtab[i].trap = 1U << SIGTERM;                     // say go away
                    systab->jobtab[i].attention = 1;                            // look at it
                }
            }
        }

        DB_Dismount(1);                                                         // dismount main vol
        tcsetattr(0, TCSANOW, &tty_settings);                                   // reset terminal
        exit(0);                                                                // and exit

    case 'L':                                                                   // $LOCK
        if (nsubs != 1) return -ERRM38;                                         // junk
        if (!priv()) return -ERRM29;                                            // SET or KILL on SSVN not on
        if (subs[0]->len > 511) return -(ERRZ12 + ERRMLAST);                    // junk
        vp = (mvar *) &tmp[512];                                                // some temp space
        s = UTIL_MvarFromCStr(subs[0], vp);                                     // convert to mvar
        if (s < 0) return s;                                                    // quit on error
        s = UTIL_mvartolock(vp, subs[0]->buf);                                  // cvt to locktab style
        if (s < 0) return s;                                                    // quit on error
        subs[0]->len = s;                                                       // save the length
        while (SemOp(SEM_LOCK, -systab->maxjob)) sleep(1);                      // until success, get semaphore
        s = LCK_Kill(subs[0]);                                                  // do it
        i = SemOp(SEM_LOCK, systab->maxjob);                                    // drop semaphore
        return s;                                                               // do it and exit

    case 'R':                                                                   // $ROUTINE
        if (nsubs > 1) return -ERRM38;                                          // junk

        if (var->slen == '\0') {                                                // if unsubscripted
            u_char tmp[16];                                                     // for below
            if (!priv()) return -ERRM29;                                        // SET or KILL on SSVN not on
            s = DB_Data(var, tmp);                                              // see if it's defined
            if (s < 0) return s;                                                // quit on error
            if (s > 1) return -ERRM33;                                          // KILL ^$ROUTINE routine exists
            return DB_Kill(var);                                                // give it to the database
        }                                                                       // end KILLing ^$ROUTINE

        if (!priv() && ((partab.jobtab->ruci != var->uci) || (partab.jobtab->rvol != var->volset))) { // check privs
            return -ERRM29;                                                     // SET or KILL on SSVN not on
        }

        VAR_CLEAR(rou);                                                         // clear routine name

        for (i = 0; i < VAR_LEN; i++) {
            if ((rou.var_cu[i] = subs[0]->buf[i]) == '\0') break;
        }

        s = SemOp(SEM_ROU, -systab->maxjob);                                    // lock it
        if (s < 0) return s;                                                    // quit if no go
        s = DB_Kill(var);                                                       // give it to the database
        if (s >= 0) Routine_Delete(rou, var->uci);                              // if OK then mark as deleted
        i = SemOp(SEM_ROU, systab->maxjob);                                     // release the lock
        return s;                                                               // exit

    case 'S':                                                                   // $SYSTEM
        if ((nsubs == 4) && priv() && (strncasecmp((char *) subs[0]->buf, "vol\0", 4) == 0) &&
          (strncasecmp((char *) subs[2]->buf, "uci\0", 4) == 0)) {              // ^$SYSTEM("VOL",n,"UCI",n)
            i = cstringtoi(subs[1]) - 1;                                        // get vol#
            j = cstringtoi(subs[3]) - 1;                                        // and UCI#
            if ((i < 0) || (i >= MAX_VOL)) return -ERRM60;                      // out of range
            if ((j < 0) || (j >= UCIS)) return -ERRM60;                         // out of range
            return DB_UCIKill(i + 1, j + 1);                                    // do it and return
        }

        return -ERRM38;                                                         // Do vol dismount later
    }

    return -ERRM38;                                                             // can't get here?
}

short SS_Order(mvar *var, u_char *buf, int dir) // get next subscript
{
    int     i = 0;                                                              // useful int
    int     j;                                                                  // and another
    short   s;                                                                  // for functions
    int     cnt;                                                                // count of bytes used
    u_char  tmp[1024];                                                          // temp string space
    int     ptmp = 0;                                                           // pointer into this
    int     nsubs = 0;                                                          // count subscripts
    mvar    *vp;                                                                // variable ptr
    cstring *subs[4];                                                           // where to put them

    while (i < var->slen) {                                                     // for all subs
        cnt = 0;                                                                // flag no rabbit ears quotes
        if (nsubs > 3) return -ERRM38;                                          // junk
        subs[nsubs] = (cstring *) &tmp[ptmp];                                   // point at the buffer

        s = UTIL_Key_Extract(&var->key[i],                                      // key from here
                             subs[nsubs]->buf,                                  // where to put it
                             &cnt);                                             // the count

        if (s < 0) return s;                                                    // die on error
        subs[nsubs++]->len = s;                                                 // save the size (incr count)
        ptmp += s + sizeof(short) + 1;                                          // move up temp area
        i = i + cnt;                                                            // count used bytes
    }

    s = SS_Norm(var);                                                           // normalize the name
    if (s < 0) return s;                                                        // return on error

    switch (var->name.var_cu[1]) {                                              // check initial of name
    case 'C':                                                                   // $CHARACTER
        return -ERRM38;                                                         // junk

    case 'D':                                                                   // $DEVICE
        if (nsubs != 1) return -ERRM38;                                         // junk
        i = cstringtoi(subs[0]);                                                // make an int of I/O channel#
        buf[0] = '\0';                                                          // null terminate

        if (dir < 0) {                                                          // backwards
            if (subs[0]->buf[0] == '\0') i = MAX_SEQ_IO;                        // setup the seed
            if (i == 0) return 0;

            for (i -= 1; i > -1; i--) {                                         // scan backwards
                if (partab.jobtab->seqio[i].type != 0) break;                   // found one
            }
        } else {                                                                // forward
            if (subs[0]->buf[0] == '\0') return itocstring(buf, 0);

            for (i += 1; i < MAX_SEQ_IO; i++) {                                 // scan the list
                if (partab.jobtab->seqio[i].type != 0) break;                   // found one
            }
        }

        if (i != MAX_SEQ_IO) return itocstring(buf, i);                         // return channel number
        return 0;                                                               // or nothing

    case 'G':                                                                   // $GLOBAL
        if (nsubs != 1) return -ERRM38;                                         // junk
        return DB_Order(var, buf, dir);                                         // let the database module do it

    case 'J':                                                                   // $JOB
        if (nsubs != 1) return -ERRM38;                                         // junk
        i = cstringtoi(subs[0]);                                                // make an int of job#
        buf[0] = '\0';                                                          // null terminate

        if (dir < 0) {                                                          // backwards
            if (i == 0) i = systab->maxjob + 1;                                 // setup the seed

            for (i = i - 2; i > -1; i--) {                                      // scan backwards
                if (systab->jobtab[i].pid != 0) {                               // found one
                    if ((kill(systab->jobtab[i].pid, 0)) && (errno == ESRCH)) { // check the job, clean it out if it doesn't exist
                        CleanJob(i + 1);                                        // zot if not there
                    } else {
                        break;                                                  // else OK
                    }
                }
            }

            i++;                                                                // convert back to job#
        } else {                                                                // forward
            for (; i < systab->maxjob; i++) {                                   // scan the list
                if (systab->jobtab[i].pid != 0) {                               // found one
                    if ((kill(systab->jobtab[i].pid, 0)) && (errno == ESRCH)) { // check the job, clean it out if it doesn't exist
                        CleanJob(i + 1);                                        // zot if not there
                    } else {
                        break;                                                  // else OK
                    }
                }
            }

            i++;                                                                // convert back to job#
            if (i > systab->maxjob) i = 0;
        }

        if (i) return itocstring(buf, i);                                       // return job number
        return 0;                                                               // or nothing

    case 'L':                                                                   // $LOCK
        if (nsubs != 1) return -ERRM38;                                         // junk
        if (subs[0]->len > 511) return -(ERRZ12 + ERRMLAST);                    // junk
        vp = (mvar *) &tmp[512];                                                // some temp space
        s = UTIL_MvarFromCStr(subs[0], vp);                                     // convert to mvar
        if (s < 0) return s;                                                    // quit on error
        s = UTIL_mvartolock(vp, subs[0]->buf);                                  // cvt to locktab style
        if (s < 0) return s;                                                    // quit on error
        subs[0]->len = s;                                                       // save the length
        return LCK_Order(subs[0], buf, dir);                                    // do it and exit

    case 'R':                                                                   // $ROUTINE
        if (nsubs > 2) return -ERRM38;                                          // junk
        return DB_Order(var, buf, dir);                                         // let the database module do it

    case 'S':                                                                   // $SYSTEM
        if ((nsubs == 2) && (strncasecmp((char *) subs[0]->buf, "vol\0", 4) == 0)) {
            i = cstringtoi(subs[1]) - 1;                                        // get vol#
            if ((i < -1) || (i >= MAX_VOL)) return -ERRM60;                     // out of range
            buf[0] = '\0';                                                      // JIC

            if (dir > 0) {                                                      // forward?
                for (j = i + 1; j < MAX_VOL; j++) {
                    if (systab->vol[j] != NULL) break;
                }

                if (j == MAX_VOL) return 0;                                     // ran out
                return itocstring(buf, j + 1);                                  // return vol#
            }

            if (i == -1) i = MAX_VOL;                                           // fix the seed

            for (j = i - 1; j >= 0; j--) {
                if (systab->vol[j] != NULL) break;
            }

            if (j < 0) return 0;                                                // ran out
            return itocstring(buf, j + 1);                                      // return vol#
        }

        if ((nsubs == 4) && (strncasecmp((char *) subs[0]->buf, "vol\0", 4) == 0) &&
          (strncasecmp((char *) subs[2]->buf, "uci\0", 4) == 0)) {              // ^$SYSTEM("VOL",n,"UCI",n)
            i = cstringtoi(subs[1]) - 1;                                        // get vol#
            j = cstringtoi(subs[3]) - 1;                                        // and UCI#
            if ((i < 0) || (i >= MAX_VOL)) return -ERRM60;                      // out of range
            if ((j < -1) || (j >= UCIS)) return -ERRM60;                        // out of range
            if (systab->vol[i] == NULL) return -ERRM60;                         // not mounted
            buf[0] = '\0';                                                      // JIC

            if (dir > 0) {                                                      // forward?
                for (j = j + 1; j < UCIS; j++) {
                    if (!var_empty(systab->vol[i]->vollab->uci[j].name)) break;
                }

                if (j == UCIS) return 0;                                        // ran out
                return itocstring(buf, j + 1);                                  // return UCI#
            }

            if (j == -1) j = UCIS;                                              // fix the seed

            for (j = j - 1; j >= 0; j--) {
                if (!var_empty(systab->vol[i]->vollab->uci[j].name)) break;
            }

            if (j < 0) return 0;                                                // ran out
            return itocstring(buf, j + 1);                                      // return UCI#
        }

        return -ERRM38;                                                         // junk
    }

    return -ERRM38;                                                             // can't get here?
}
