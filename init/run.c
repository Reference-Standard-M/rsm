/*
 * Package: Reference Standard M
 * File:    rsm/init/run.c
 * Summary: module init - startup (main) code
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

#include "init.h"                                                               // init prototypes
#include "compile.h"
#include "database.h"
#include "error.h"                                                              // standard errors
#include "opcode.h"
#include "proto.h"                                                              // standard prototypes
#include <errno.h>                                                              // error stuff
#include <fcntl.h>                                                              // file stuff
#include <signal.h>
#include <stdio.h>                                                              // always include
#include <stdlib.h>                                                             // these two
#include <string.h>                                                             // for memcpy
#include <termios.h>                                                            // for tcgetattr
#include <unistd.h>                                                             // database access
#include <sys/shm.h>                                                            // shared memory

partab_struct  partab;                                                          // setup partab
systab_struct  *systab;
struct termios tty_settings;                                                    // man 3 termios

u_char *addstk[MAX_ASTK];                                                       // address stack
u_char strstk[MAX_SSTK];                                                        // string stack
u_char indstk[MAX_ISTK];                                                        // indirect stack
long   isp;                                                                     // indirect stack pointer
int    failed_tty = -1;                                                         // flag for tty reset
int    gbd_expired = GBD_EXPIRED;                                               // Set this
u_char *rsmpc;                                                                  // RSM prog pointer

char    history[MAX_HISTORY][MAX_STR_LEN];                                      // history buffer
u_short hist_next = 0;                                                          // next history pointer
u_short hist_curr = 0;                                                          // history entry pointer
short   in_hist = FALSE;                                                        // are we in the history buffer
u_short prompt_len = 8;                                                         // length of the current direct mode prompt

static void ser(int t)                                                          // display errors
{
    cstring *cptr;                                                              // cstring ptr
    u_char  junk[100];

    if (t == -(ERRZ27 + ERRMLAST)) panic("Channel zero has gone away");         // if RSM is totally confused then die
    cptr = (cstring *) junk;                                                    // some space
    if (t < 0) t = -t;                                                          // make error positive
    UTIL_strerror(t, &cptr->buf[0]);                                            // get the text
    if (t > (ERRMLAST + ERRZLAST)) panic((char *) &cptr->buf[0]);               // if OS is totally confused then die
    fprintf(stderr, "\r\nERROR occurred %d\r\n%s\r\n", t, &cptr->buf[0]);       // print it
    return;                                                                     // and return
}

static void controlc(void)                                                      // say ^C
{
    cstring *sptr;                                                              // string ptr
    u_char  junk[10];
    int     t;                                                                  // for returns

    sptr = (cstring *) junk;                                                    // where to put it
    t = SQ_WriteFormat(SQ_LF);                                                  // new line
    if (t < 0) ser(t);                                                          // check for error
    memcpy(sptr->buf, "^C", 2);                                                 // copy in the prompt
DISABLE_WARN(-Warray-bounds)
    sptr->buf[3] = '\0';                                                        // null terminate
    sptr->len = 2;                                                              // and the length
ENABLE_WARN
    t = SQ_Write(sptr);                                                         // write the prompt
    if (t < 0) ser(t);                                                          // check for error
    return;                                                                     // exit
}

/*****************************************************\
* Attach to an environment - switches are:            *
*      database file name   (1 to VAR_LEN)        Req *
*   -e environment (UCI)    (1 to VAR_LEN)        Opt *
*   -x xecute command       (1 to MAX_STR_LEN)    Opt *
\*****************************************************/
int init_run(char *file, const char *env, char *cmd)
{
    int          i;                                                             // an int
    int          t;                                                             // for functions
    int          pid;                                                           // job number
    int          dbfd = 0;                                                      // database file descriptor
    int          ret = 0;                                                       // return value
    int          env_num = 1;                                                   // startup environment number
    int          ssp = 0;                                                       // string stack pointer
    int          asp = 0;                                                       // address stack pointer
    var_u        tmp;                                                           // temp descriptor
    const u_char *volnam;                                                       // for volume name
    mvar         *var;                                                          // a variable pointer
    uci_tab      *uci_ptr;                                                      // for UCI search
    cstring      *cptr = NULL;                                                  // a handy pointer
    cstring      *sptr = NULL;                                                  // cstring pointer
    u_char       start_type = TYPE_RUN;                                         // how we started
    gid_t        gidset[MAX_GROUPS];                                            // for getgroups()
    label_block  *vol_label;                                                    // current volume label

start:
#if defined(__APPLE__) || defined(__FreeBSD__)
    srandomdev();                                                               // randomize
#endif
    partab.jobtab = (jobtab *) NULL;                                            // clear jobtab pointer

    if (start_type == TYPE_RUN) {                                               // if not from JOB
        dbfd = open(file, O_RDONLY);                                            // open the main database for read

        if (dbfd == -1) {                                                       // if that failed
            fprintf(stderr, "RSM database error - %s\n", file);
            return errno;
        }

        i = UTIL_Share(file, 0);                                                // attach to shared memory

        if (i != 0) {                                                           // quit on error
            fprintf(stderr, "RSM environment is not initialized\n");
            return i;
        }
    }

    if (systab->start_user == -1) goto exit;                                    // shutting down, so no need to run

    if (systab->vol[0] == NULL) {
        fprintf(stderr, "Error occurred in process - Environment does not match runtime image version\n");
        ret = -1;
        goto exit;
    }

    partab.job_table = SOA(systab->jobtab);                                     // adjusted pointer to job table
    partab.vol[0] = SOA(systab->vol[0]);                                        // adjusted pointer to main volume
    vol_label = SOA(partab.vol[0]->vollab);                                     // adjusted pointer to main volume label

    if (env != NULL) {                                                          // passed in UCI ?
        env_num = 0;                                                            // clear UCI number
        uci_ptr = &vol_label->uci[0];                                           // get pointer to UCI table
        VAR_CLEAR(tmp);                                                         // zero entire name

        for (i = 0; i < VAR_LEN; i++) {                                         // copy in name
            if (env[i] == '\0') break;                                          // done at null
            tmp.var_cu[i] = env[i];                                             // copy character
        }

        for (i = 0; i < UCIS; i++) {                                            // scan all UCIs
            if (var_equal(uci_ptr[i].name, tmp)) {                              // if we have a match
                env_num = i + 1;                                                // save it
                break;                                                          // and exit loop
            }
        }

        if (env_num == 0) {
            ret = ENOENT;                                                       // complain on fail
            goto exit;                                                          // and exit
        }
    }

    pid = (int) getpid();                                                       // get process id

    for (u_int j = 0; j < systab->maxjob; j++) {                                // scan the slots
        ret = partab.job_table[j].pid;                                          // get PID

        if ((ret != pid) && ret) {                                              // if one there and not us
            if (kill(ret, 0) == -1) {                                           // check the job
                if (errno == ESRCH) {                                           // doesn't exist
                    CleanJob(j + 1);                                            // zot if not there
                    break;                                                      // have at least one
                }
            }
        } else {                                                                // it's free or ours
            break;                                                              // quit
        }
    }

    ret = SemOp(SEM_SYS, SEM_WRITE);                                            // lock systab
    if (ret < 0) goto exit;                                                     // give up on error

    for (u_int j = 0; j < systab->maxjob; j++) {                                // look for a free slot
        if (((partab.job_table[j].pid == 0) && (start_type == TYPE_RUN)) ||     // this one ?
          ((partab.job_table[j].pid == pid) && (start_type == TYPE_JOB))) {     // or already done (JOB)
            memset(&partab.job_table[j], 0, sizeof(jobtab));                    // yes - zot the lot
            partab.jobtab = &partab.job_table[j];                               // and save our jobtab address
            partab.jobtab->pid = pid;                                           // copy in our PID
            break;                                                              // end loop
        }
    }

    ret = SemOp(SEM_SYS, -SEM_WRITE);                                           // unlock systab

    if (partab.jobtab == NULL) {                                                // if that failed
        ret = ENOMEM;                                                           // error message
        goto exit;                                                              // and exit
    }

    partab.jobtab->user = (int) getuid();                                       // get user number

    if ((partab.jobtab->user == systab->start_user) || (partab.jobtab->user == 0)) { // if he started it or is root
        partab.jobtab->priv = 1;                                                // say yes
    } else {
        if (systab->maxjob == 1) {                                              // if single job
            ret = ENOMEM;                                                       // error message
            partab.jobtab = NULL;                                               // clear this
            goto exit;                                                          // and exit
        }

        i = getgroups(MAX_GROUPS, gidset);                                      // get groups

        if (i < 0) {                                                            // if an error
            ret = errno;                                                        // get the error
            goto exit;                                                          // and exit
        }

        while (i > 0) {                                                         // for each group
            if (gidset[i - 1] == PRVGRP) {                                      // if it's "wheel" or "admin"
                partab.jobtab->priv = 1;                                        // say yes
                break;                                                          // and exit
            }

            i--;                                                                // decrement i
        }
    }

    partab.jobtab->precision = systab->precision;                               // decimal precision
    partab.jobtab->uci = env_num;                                               // UCI number
    partab.jobtab->vol = 1;                                                     // volset
    partab.jobtab->luci = env_num;                                              // UCI number
    partab.jobtab->lvol = 1;                                                    // volset
    partab.jobtab->ruci = env_num;                                              // UCI number
    partab.jobtab->rvol = 1;                                                    // volset
    partab.jobtab->start_len = Vhorolog(partab.jobtab->start_dh);               // store start date/time
    partab.jobtab->dostk[0].type = TYPE_RUN;                                    // ensure slot 0 has a value
    failed_tty = tcgetattr(STDIN_FILENO, &tty_settings);
    i = SQ_Init();                                                              // have seqio setup chan 0
    if (i < 0) ser(i);                                                          // check for error

    for (i = 0; i < MAX_VOL; i++) {                                             // scan vol list
        if (systab->vol[i] == NULL) continue;
        systab->last_blk_used[LBU_OFF(i)] = 0;                                  // clear last global block
    }

    partab.debug = BREAK_OFF;                                                   // clear debug flag
    partab.strstk_start = &strstk[0];                                           // address of strstk
    partab.strstk_last =  &strstk[MAX_SSTK];                                    // and the last char
    partab.varlst = NULL;                                                       // used by compiler
    partab.vol_fds[0] = dbfd;                                                   // make sure FD is right
    ST_Init();                                                                  // initialize symbol table

    if (vol_label->journal_available && vol_label->journal_requested) {         // if journaling
        partab.jnl_fds[0] = open(vol_label->journal_file, O_RDWR);

        if (partab.jnl_fds[0] == -1) {
            fprintf(stderr, "Failed to open journal file: %s\r\nerrno = %d\r\n", vol_label->journal_file, errno);
            ret = errno;
            if (cmd != NULL) goto exit;
        }
    }

    if (start_type == TYPE_RUN) {                                               // if not from JOB
        for (i = 1; i < MAX_VOL; i++) {
            if (systab->vol[i] == NULL) continue;
            partab.vol[i] = SOA(systab->vol[i]);                                // adjusted pointer to supplemental volume
            dbfd = open(partab.vol[i]->file_name, O_RDONLY);                    // open the supplemental database for read

            if (dbfd == -1) {                                                   // if that failed
                fprintf(stderr, "RSM database error - %s\r\nerrno = %d\r\n", partab.vol[i]->file_name, errno);
                ret = errno;
                if (cmd != NULL) goto exit;
            }

            vol_label = SOA(partab.vol[i]->vollab);                             // adjusted pointer to supplemental volume label
            partab.vol_fds[i] = dbfd;                                           // make sure FD is right

            // if journaling
            if (vol_label->journal_available && vol_label->journal_requested) {
                partab.jnl_fds[i] = open(vol_label->journal_file, O_RDWR);

                if (partab.jnl_fds[i] == -1) {
                    fprintf(stderr, "Failed to open journal file: %s\r\nerrno = %d\r\n", vol_label->journal_file, errno);

                    ret = errno;
                    if (cmd != NULL) goto exit;
                }
            }
        }
    }

    if (cmd != NULL) {                                                          // command specified ?
        source_ptr = (u_char *) cmd;                                            // where the code is

        if (start_type == TYPE_RUN) {
            sptr = (cstring *) &strstk[0];                                      // front of string stack
            sptr->len = strlen(cmd);                                            // find the length
            memcpy(sptr->buf, source_ptr, sptr->len);                           // copy in the source
            addstk[asp++] = (u_char *) sptr;                                    // save address of string
            ssp = sptr->len + sizeof(u_short) + 1;                              // point past it
        } else {
            addstk[asp++] = (u_char *) &strstk[0];                              // save address of string
        }

        cptr = (cstring *) &strstk[ssp];                                        // where the compiled goes
        comp_ptr = cptr->buf;                                                   // the data bit
        parse();
        *comp_ptr++ = CMQUIT;                                                   // add the quit
        *comp_ptr++ = ENDLIN;                                                   // JIC
        *comp_ptr++ = ENDLIN;                                                   // JIC
        i = &comp_ptr[0] - &cptr->buf[0];                                       // get number of bytes
        cptr->len = i;                                                          // save for ron
        ssp += i + sizeof(u_short) + 1;                                         // point past it
        rsmpc = &cptr->buf[0];                                                  // setup the rsmpc
        partab.jobtab->dostk[0].routine = (u_char *) SBM(cmd);                  // where we started
        partab.jobtab->dostk[0].pc = rsmpc;                                     // where we started
        partab.jobtab->dostk[0].symbol = NULL;                                  // nowhere
        partab.jobtab->dostk[0].newtab = NULL;                                  // nowhere
        partab.jobtab->dostk[0].endlin = rsmpc + i - 4;                         // ENDLIN
        VAR_CLEAR(partab.jobtab->dostk[0].rounam);                              // zero the routine name
        partab.jobtab->dostk[0].vol = partab.jobtab->vol;                       // current volume
        partab.jobtab->dostk[0].uci = partab.jobtab->uci;                       // current UCI
        partab.jobtab->dostk[0].line_num = 0;                                   // no line number
        partab.jobtab->dostk[0].type = start_type;                              // how we started
        partab.jobtab->dostk[0].estack = 0;                                     // estack offset
        partab.jobtab->dostk[0].level = 0;                                      // where we started
        partab.jobtab->dostk[0].flags = 0;                                      // no flags
        partab.jobtab->dostk[0].savasp = asp;                                   // address stack pointer
        partab.jobtab->dostk[0].savssp = ssp;                                   // string stack
        partab.jobtab->dostk[0].asp = asp;                                      // address stack pointer
        partab.jobtab->dostk[0].ssp = ssp;                                      // string stack
        partab.jobtab->attention = 0;
        partab.jobtab->trap = 0;
        partab.jobtab->async_error = 0;
        isp = 0;                                                                // clear indirect pointer
        t = run(asp, ssp);
        if (t == OPHALT) goto exit;                                             // look after HALT
        if (t == JOBIT) goto jobit;                                             // look after new JOB
        if (start_type == TYPE_JOB) goto exit;                                  // look after current JOB
        partab.jobtab->io = 0;                                                  // force chan 0
        var = (mvar *) &strstk[0];                                              // space to setup a var
        VAR_CLEAR(var->name);
        memcpy(&var->name.var_cu[0], "$ECODE", 6);
        var->volset = 0;
        var->uci = UCI_IS_LOCALVAR;
        var->slen = 0;                                                          // setup for $ECODE
        cptr = (cstring *) &strstk[sizeof(mvar)];                               // for result
        memcpy(cptr->buf, "$ECODE=", 7);
        t = ST_Get(var, &cptr->buf[7]);

        if (t > 1) {                                                            // ignore if nothing there
            cptr->len = t + 7;
            t = SQ_WriteFormat(SQ_LF);                                          // new line
            if (t < 0) ser(t);                                                  // check for error
            t = SQ_Write(cptr);                                                 // write the prompt
            if (t < 0) ser(t);                                                  // check for error
            t = SQ_WriteFormat(SQ_LF);                                          // new line
            if (t < 0) ser(t);                                                  // check for error
            cptr = (cstring *) (((u_char *) cptr) + 8);

            if (cptr->buf[0] != 'U') {
                cptr->len = 4;                                                  // max error size
                cptr->len = Xcall_errmsg((char *) cptr->buf, cptr, cptr);       // convert to error string
                t = SQ_Write(cptr);                                             // write the error
                if (t < 0) ser(t);                                              // check for error
                t = SQ_WriteFormat(SQ_LF);                                      // new line
                if (t < 0) ser(t);                                              // check for error
            }

            ret = EPERM;                                                        // set an error for exit
        }

        goto exit;                                                              // and halt
    }

    while (TRUE) {                                                              // forever
        sptr = (cstring *) &strstk[0];                                          // front of string stack
        asp = 0;                                                                // zot address stack
        ssp = 0;                                                                // and the string stack
        partab.jobtab->io = 0;                                                  // force chan 0

        if (strcmp((char *) partab.jobtab->seqio[0].name, "Not a tty") != 0) {  // stdin is not a file or heredoc (pipe), etc.
            volnam = SOA(partab.vol[partab.jobtab->vol - 1]->vollab)->volnam.var_cu; // get current volume name
            uci_ptr = &SOA(partab.vol[partab.jobtab->vol - 1]->vollab)->uci[partab.jobtab->uci - 1]; // get pointer to UCI
            sptr->len = strlen((char *) volnam) + strlen((char *) uci_ptr->name.var_cu) + 9; // find the length
            prompt_len = sptr->len;                                             // update the prompt length for direct mode editing

            // copy in the prompt
            if (snprintf((char *) sptr->buf, sptr->len + 1, "RSM [%s,%s]> ", uci_ptr->name.var_cu, volnam) < 0) {
                return errno;
            }

            if (partab.jobtab->seqio[0].dx) {                                   // if not at left margin
                t = SQ_WriteFormat(SQ_LF);                                      // new line
                if (t < 0) ser(t);                                              // check for error
            }

            t = SQ_Write(sptr);                                                 // write the prompt
            if (t < 0) ser(t);                                                  // check for error
        }

        t = SQ_Read(sptr->buf, UNLIMITED, UNLIMITED);                           // get a string
        i = attention();                                                        // check signals
        if (i == OPHALT) break;                                                 // exit on HALT
        if (i == -(ERRZ51 + ERRMLAST)) controlc();                              // <Control-C>

        if (t < 0) {
            ser(t);                                                             // complain on error
            t = 0;
        }

        sptr->len = t;                                                          // save the length
        if (t == 0) continue;                                                   // ignore null

        if (!hist_next || strcmp(history[hist_next - 1], (char *) sptr->buf)) {
            strcpy(history[hist_next], (char *) sptr->buf);

            if (hist_next == (MAX_HISTORY - 1)) {
                hist_next = 0;
            } else {
                hist_next++;
            }
        }

        hist_curr = hist_next;
        addstk[asp++] = (u_char *) sptr;                                        // save address of string
        ssp += t + sizeof(u_short) + 1;                                         // point past it

        if (strcmp((char *) partab.jobtab->seqio[0].name, "Not a tty") != 0) {  // stdin is not a file or heredoc (pipe), etc.
            t = SQ_WriteFormat(SQ_LF);                                          // new line
            if (t < 0) ser(t);                                                  // check for error
        }

        source_ptr = sptr->buf;                                                 // where the code is
        cptr = (cstring *) &strstk[ssp];                                        // where the compiled goes
        comp_ptr = cptr->buf;                                                   // the data bit
        parse();
        *comp_ptr++ = CMQUIT;                                                   // add the quit
        *comp_ptr++ = ENDLIN;                                                   // JIC
        *comp_ptr++ = ENDLIN;                                                   // JIC
        i = &comp_ptr[0] - &cptr->buf[0];                                       // get number of bytes
        cptr->len = i;                                                          // save for ron
        ssp += i + sizeof(u_short) + 1;                                         // point past it
        rsmpc = &cptr->buf[0];                                                  // setup the rsmpc
        partab.jobtab->dostk[0].routine = SBM(&sptr->buf[0]);                   // where we started
        partab.jobtab->dostk[0].pc = rsmpc;                                     // where we started
        partab.jobtab->dostk[0].symbol = NULL;                                  // nowhere
        partab.jobtab->dostk[0].newtab = NULL;                                  // nowhere
        partab.jobtab->dostk[0].endlin = rsmpc + i - 4;                         // ENDLIN
        VAR_CLEAR(partab.jobtab->dostk[0].rounam);                              // zero the routine name
        partab.jobtab->dostk[0].vol = partab.jobtab->vol;                       // current volume
        partab.jobtab->dostk[0].uci = partab.jobtab->uci;                       // current UCI
        partab.jobtab->dostk[0].line_num = 0;                                   // no line number
        partab.jobtab->dostk[0].type = TYPE_RUN;                                // how we started
        partab.jobtab->dostk[0].estack = 0;                                     // estack offset
        partab.jobtab->dostk[0].level = 0;                                      // where we started
        partab.jobtab->dostk[0].flags = 0;                                      // no flags
        partab.jobtab->dostk[0].savasp = asp;                                   // address stack pointer
        partab.jobtab->dostk[0].savssp = ssp;                                   // string stack
        partab.jobtab->dostk[0].asp = asp;                                      // address stack pointer
        partab.jobtab->dostk[0].ssp = ssp;                                      // string stack
        partab.jobtab->attention = 0;
        partab.jobtab->trap = 0;
        partab.jobtab->async_error = 0;
        isp = 0;                                                                // clear indirect pointer
        t = run(asp, ssp);
        if (partab.debug > BREAK_OFF) partab.debug = BREAK_OFF;                 // reset debug flag
        if (t == JOBIT) goto jobit;                                             // look after new JOB
        if (t == OPHALT) break;                                                 // exit on HALT
        partab.jobtab->io = 0;                                                  // force chan 0

        if (t == -(ERRZ51 + ERRMLAST)) {
            controlc();                                                         // <Control-C>
        } else if (t < 0) {
            ser(t);
        }

        partab.jobtab->error_frame = 0;                                         // and that one
        var = (mvar *) &strstk[0];                                              // space to setup a var
        VAR_CLEAR(var->name);
        memcpy(&var->name.var_cu[0], "$ECODE", 6);
        var->volset = 0;
        var->uci = UCI_IS_LOCALVAR;
        var->slen = 0;                                                          // setup for $ECODE
        cptr = (cstring *) &strstk[sizeof(mvar)];                               // for result
        memcpy(cptr->buf, "$ECODE=", 7);
        t = ST_Get(var, &cptr->buf[7]);
        if (t < 1) continue;                                                    // ignore if nothing there
        if ((t + 7) > MAX_STR_LEN) ser(-ERRM75);                                // $ECODE buffer overflow
        cptr->len = t + 7;
        t = SQ_Write(cptr);                                                     // write the prompt
        if (t < 0) ser(t);                                                      // check for error
        t = SQ_WriteFormat(SQ_LF);                                              // new line
        if (t < 0) ser(t);                                                      // check for error
        ST_Kill(var);                                                           // dong $ECODE
        cptr = (cstring *) (((u_char *) cptr) + 8);

        if (cptr->buf[0] != 'U') {
            cptr->len = 4;                                                      // max error size
            cptr->len = Xcall_errmsg((char *) cptr->buf, cptr, cptr);           // convert to error string
            t = SQ_Write(cptr);                                                 // write the error
            if (t < 0) ser(t);                                                  // check for error
            t = SQ_WriteFormat(SQ_LF);                                          // new line
            if (t < 0) ser(t);                                                  // check for error
        }
    }                                                                           // end command level loop

exit:                                                                           // general exit code
    if (partab.jobtab != NULL) CleanJob(0);                                     // if we have a jobtab, remove locks, detach symbols
    shmdt(systab);                                                              // detach the shared memory

    for (i = 0; i < MAX_VOL; i++) {
        if (partab.vol_fds[i]) close(partab.vol_fds[i]);                        // close the databases
        if (partab.jnl_fds[i]) close(partab.jnl_fds[i]);                        // close the journals
    }

    if (!failed_tty) failed_tty = tcsetattr(STDIN_FILENO, TCSANOW, &tty_settings); // reset terminal if possible
    if (start_type == TYPE_JOB) return 0;                                       // no error from JOB
    return ret;                                                                 // and exit

jobit:                                                                          // code for JOB
    start_type = TYPE_JOB;                                                      // what we are doing
    env_num = partab.jobtab->ruci;                                              // remember (current) rou UCI
    cmd = (char *) &strstk[0];                                                  // where the command is
    ssp = strlen((const char *) strstk);                                        // protect original command
    isp = 0;                                                                    // clear all these
    asp = 0;
    ret = 0;
    env = NULL;
    goto start;                                                                 // go do it
}
