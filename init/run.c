/*
 * Package:  Reference Standard M
 * File:     rsm/init/run.c
 * Summary:  module RSM - startup (main) code
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
#include <sys/types.h>                                                          // for u_char def
#include <signal.h>
#include <errno.h>                                                              // error stuff
#include <fcntl.h>                                                              // file stuff
#include <string.h>                                                             // for bcopy()
#include <strings.h>
#include <unistd.h>                                                             // database access
#include <sys/ipc.h>                                                            // shared memory
#include <sys/shm.h>                                                            // shared memory
#include <sys/sem.h>                                                            // semaphores
#include <termios.h>                                                            // for tcgetattr
#include "rsm.h"                                                                // standard includes
#include "proto.h"                                                              // standard prototypes
#include "init.h"                                                               // initialization prototypes
#include "error.h"                                                              // standard errors
#include "compile.h"
#include "opcode.h"
#include "database.h"

partab_struct  partab;                                                          // setup partab
systab_struct  *systab;
struct termios tty_settings;                                                    // man 3 termios

u_char  *addstk[MAX_ASTK];                                                      // address stack
u_char  strstk[MAX_SSTK];                                                       // string stack
u_char  indstk[MAX_ISTK];                                                       // indirect stack
long    isp;                                                                    // indirect stack pointer
int     failed_tty = -1;                                                        // flag for tty reset
int     gbd_expired = GBD_EXPIRED;                                              // Set this
u_char  *rsmpc;                                                                 // RSM prog pointer

char    history[MAX_HISTORY][MAX_STR_LEN];                                      // history buffer
u_short hist_next = 0;                                                          // next history pointer
u_short hist_curr = 0;                                                          // history entry pointer
short   in_hist = FALSE;                                                        // are we in the history buffer
u_short prompt_len = 8;                                                         // length of the current direct mode prompt

void ser(int s)                                                                 // display errors
{
    cstring *cptr;                                                              // cstring ptr
    u_char  junk[100];

    if (s == -(ERRZ27 + ERRMLAST)) panic("Chanel zero has gone away");          // if totally confused then die
    cptr = (cstring *) junk;                                                    // some space
    if (s < 0) s = -s;                                                          // make error positive
    (void) UTIL_strerror(s, &cptr->buf[0]);                                     // get the text
    fprintf(stderr, "\r\nERROR occurred %d\r\n%s\r\n", s, &cptr->buf[0]);       // print it
    return;                                                                     // and return
}

void controlc(void)                                                             // say ^C
{
    cstring *sptr;                                                              // string ptr
    u_char  junk[10];
    int     s;                                                                  // for returns

    sptr = (cstring *) junk;                                                    // where to put it
    s = SQ_WriteFormat(SQ_LF);                                                  // new line
    if (s < 0) ser(s);                                                          // check for error
    bcopy("^C", sptr->buf, 2);                                                  // copy in the prompt
DISABLE_WARN(-Warray-bounds)
    sptr->buf[3] = '\0';                                                        // null terminate
    sptr->len = 2;                                                              // and the length
ENABLE_WARN
    s = SQ_Write(sptr);                                                         // write the prompt
    if (s < 0) ser(s);                                                          // check for error
    return;                                                                     // exit
}

/*****************************************************\
* Attach to an environment - switches are:            *
*      database file name   (1 to VAR_LEN)        Req *
*   -e environment (UCI)    (1 to VAR_LEN)        Opt *
*   -x Xecute command       (1 to MAX_STR_LEN)    Opt *
\*****************************************************/
int INIT_Run(char *file,                                                        // database file
             char *env,                                                         // environment (UCI)
             char *cmd)                                                         // command
{
    int     i;                                                                  // an int
    int     dbfd = 0;                                                           // database file descriptor
    int     ret = 0;                                                            // return value
    int     env_num = 1;                                                        // startup environment number
    var_u   tmp;                                                                // temp descriptor
    uci_tab *uci_ptr;                                                           // for UCI search
    int     pid;                                                                // job number
    int     ssp = 0;                                                            // string stack ptr
    int     asp = 0;                                                            // address stack ptr
    mvar    *var;                                                               // a variable pointer
    cstring *cptr = NULL;                                                       // a handy pointer
    cstring *sptr = NULL;                                                       // cstring ptr
    int     s;                                                                  // for functions
    u_char  start_type = TYPE_RUN;                                              // how we started
    gid_t   gidset[MAX_GROUPS];                                                 // for getgroups()

start:
#if defined(__APPLE__) || defined(__FreeBSD__)
    srandomdev();                                                               // randomize
#endif
    partab.jobtab = (jobtab *) NULL;                                            // clear jobtab pointer

    if (start_type == TYPE_RUN) {                                               // if not from JOB
        dbfd = open(file, O_RDONLY);                                            // open the database for read

        if (dbfd < 0) {                                                         // if that failed
            fprintf(stderr, "RSM database error - %s\n", file);
            return errno;
        }

        i = UTIL_Share(file);                                                   // attach to shared mem

        if (i != 0) {                                                           // quit on error
            fprintf(stderr, "RSM environment is not initialized.\n");
            return i;
        }
    }

    if (systab->vol[0] == NULL) {
        fprintf(stderr, "Error occurred in process - Environment does not match runtime image version\n");
        ret = -1;
        goto exit;
    }

    if (env != NULL) {                                                          // passed in UCI ?
        env_num = 0;                                                            // clear UCI number
        uci_ptr = &systab->vol[0]->vollab->uci[0];                              // get ptr to UCI table
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

    for (i = 0; i < systab->maxjob; i++) {                                      // scan the slots
        ret = systab->jobtab[i].pid;                                            // get PID

        if ((ret != pid) && ret) {                                              // if one there and not us
            if (kill(ret, 0)) {                                                 // check the job
                if (errno == ESRCH) {                                           // doesn't exist
                    CleanJob(i + 1);                                            // zot if not there
                    break;                                                      // have at least one
                }
            }
        } else {                                                                // it's free or ours
            break;                                                              // quit
        }
    }

    ret = SemOp(SEM_SYS, -systab->maxjob);                                      // lock systab
    if (ret < 0) goto exit;                                                     // give up on error

    for (i = 0; i < systab->maxjob; i++) {                                      // look for a free slot
        if (((systab->jobtab[i].pid == 0) && (start_type == TYPE_RUN)) ||       // this one ?
          ((systab->jobtab[i].pid == pid) && (start_type == TYPE_JOB))) {       // or already done (JOB)
            bzero(&systab->jobtab[i], sizeof(jobtab));                          // yes - zot the lot
            partab.jobtab = &systab->jobtab[i];                                 // and save our jobtab address
            partab.jobtab->pid = pid;                                           // copy in our PID
            break;                                                              // end loop
        }
    }

    ret = SemOp(SEM_SYS, systab->maxjob);                                       // unlock systab

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
    failed_tty = tcgetattr(0, &tty_settings);
    i = SQ_Init();                                                              // have seqio setup chan 0
    systab->last_blk_used[partab.jobtab - systab->jobtab] = 0;                  // clear last global block
    partab.debug = 0;                                                           // clear debug flag
    partab.strstk_start = &strstk[0];                                           // address of strstk
    partab.strstk_last =  &strstk[MAX_SSTK];                                    // and the last char
    partab.varlst = NULL;                                                       // used by compiler
    partab.vol_fds[0] = dbfd;                                                   // make sure fd is right
    ST_Init();                                                                  // initialize symbol table

    if (systab->vol[0]->vollab->journal_available && systab->vol[0]->vollab->journal_requested) { // if journaling
        partab.jnl_fds[0] = open(systab->vol[0]->vollab->journal_file, O_RDWR);

        if (partab.jnl_fds[0] < 0) {
            fprintf(stderr, "Failed to open journal file: %s\r\nerrno = %d\r\n", systab->vol[0]->vollab->journal_file, errno);
            ret = -1;
            if (cmd != NULL) goto exit;
        }
    }

    if (cmd != NULL) {                                                          // command specified ?
        source_ptr = (u_char *) cmd;                                            // where the code is

        if (start_type == TYPE_RUN) {
            sptr = (cstring *) &strstk[0];                                      // front of string stack
            sptr->len = strlen(cmd);                                            // find the length
            bcopy(source_ptr, sptr->buf, sptr->len);                            // copy in the source
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
        ssp = ssp + i + sizeof(u_short) + 1;                                    // point past it
        rsmpc = &cptr->buf[0];                                                  // setup the rsmpc
        partab.jobtab->dostk[0].routine = (u_char *) cmd;                       // where we started
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
        partab.jobtab->dostk[0].test = -1;                                      // no NEW $TEST
        partab.jobtab->dostk[0].savasp = asp;                                   // address stack ptr
        partab.jobtab->dostk[0].savssp = ssp;                                   // string stack
        partab.jobtab->dostk[0].asp = asp;                                      // address stack ptr
        partab.jobtab->dostk[0].ssp = ssp;                                      // string stack
        partab.jobtab->attention = 0;
        partab.jobtab->trap = 0;
        partab.jobtab->async_error = 0;
        isp = 0;                                                                // clear indirect pointer
        s = run(asp, ssp);
        if (s == OPHALT) goto exit;                                             // look after halt
        if (s == JOBIT) goto jobit;                                             // look after JOB
        partab.jobtab->io = 0;                                                  // force chan 0
        var = (mvar *) &strstk[0];                                              // space to setup a var
        VAR_CLEAR(var->name);
        bcopy("$ECODE", &var->name.var_cu[0], 6);
        var->volset = 0;
        var->uci = UCI_IS_LOCALVAR;
        var->slen = 0;                                                          // setup for $ECODE
        cptr = (cstring *) &strstk[sizeof(mvar)];                               // for result
        bcopy("$ECODE=", cptr->buf, 7);
        s = ST_Get(var, &cptr->buf[7]);

        if (s > 1) {                                                            // ignore if nothing there
            cptr->len = s + 7;
            s = SQ_WriteFormat(SQ_LF);                                          // new line
            s = SQ_Write(cptr);                                                 // write the prompt
            s = SQ_WriteFormat(SQ_LF);                                          // new line
            cptr = (cstring *) (((u_char *) cptr) + 8);

            if (cptr->buf[0] != 'U') {
                cptr->len = 4;                                                  // max error size
                cptr->len = Xcall_errmsg((char *) cptr->buf, cptr, cptr);       // cvt to str
                s = SQ_Write(cptr);                                             // write the error
                s = SQ_WriteFormat(SQ_LF);                                      // new line
            }

            ret = ESRCH;                                                        // set an error for exit
        }

        goto exit;                                                              // and halt
    }

    while (TRUE) {                                                              // forever
        if (in_hist == FALSE) {
            sptr = (cstring *) &strstk[0];                                      // front of string stack
            asp = 0;                                                            // zot address stack
            ssp = 0;                                                            // and the string stack
            uci_ptr = &systab->vol[partab.jobtab->vol - 1]->vollab->uci[partab.jobtab->uci - 1]; // get ptr to UCI
            sptr->len = strlen((char *) uci_ptr->name.var_cu) + 8;              // find the length
            prompt_len = sptr->len;                                             // update the prompt length for direct mode editing

            if (snprintf((char *) sptr->buf, sptr->len + 1, "RSM [%s]> ", uci_ptr->name.var_cu) < 0) {
                return errno;                                                   // copy in the prompt
            }

            partab.jobtab->io = 0;                                              // force chan 0

            if (partab.jobtab->seqio[0].dx) {                                   // if not at left margin
                s = SQ_WriteFormat(SQ_LF);                                      // new line
                if (s < 0) ser(s);                                              // check for error
            }

            s = SQ_Write(sptr);                                                 // write the prompt
            if (s < 0) ser(s);                                                  // check for error
        }

        s = SQ_Read(sptr->buf, -1, -1);                                         // get a string
        i = attention();                                                        // check signals
        if (i == OPHALT) break;                                                 // exit on halt
        if (i == -(ERRZ51 + ERRMLAST)) controlc();                              // control c

        if (s < 0) {
            ser(s);                                                             // complain on error
            s = 0;
        }

        sptr->len = s;                                                          // save the length
        if (s == 0) continue;                                                   // ignore null
        strcpy(history[hist_next], (char *) sptr->buf);

        if (hist_next == (MAX_HISTORY - 1)) {
            hist_next = 0;
        } else {
            hist_next++;
        }

        hist_curr = hist_next;
        addstk[asp++] = (u_char *) sptr;                                        // save address of string
        ssp = ssp + s + sizeof(u_short) + 1;                                    // point past it
        s = SQ_WriteFormat(SQ_LF);                                              // new line
        if (s < 0) ser(s);                                                      // check for error
        source_ptr = sptr->buf;                                                 // where the code is
        cptr = (cstring *) &strstk[ssp];                                        // where the compiled goes
        comp_ptr = cptr->buf;                                                   // the data bit
        parse();
        *comp_ptr++ = CMQUIT;                                                   // add the quit
        *comp_ptr++ = ENDLIN;                                                   // JIC
        *comp_ptr++ = ENDLIN;                                                   // JIC
        i = &comp_ptr[0] - &cptr->buf[0];                                       // get number of bytes
        cptr->len = i;                                                          // save for ron
        ssp = ssp + i + sizeof(u_short) + 1;                                    // point past it
        rsmpc = &cptr->buf[0];                                                  // setup the rsmpc
        partab.jobtab->dostk[0].routine = sptr->buf;                            // where we started
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
        partab.jobtab->dostk[0].test = -1;                                      // no NEW $TEST
        partab.jobtab->dostk[0].savasp = asp;                                   // address stack ptr
        partab.jobtab->dostk[0].savssp = ssp;                                   // string stack
        partab.jobtab->dostk[0].asp = asp;                                      // address stack ptr
        partab.jobtab->dostk[0].ssp = ssp;                                      // string stack
        partab.jobtab->attention = 0;
        partab.jobtab->trap = 0;
        partab.jobtab->async_error = 0;
        isp = 0;                                                                // clear indirect pointer
        s = run(asp, ssp);
        if (s == JOBIT) goto jobit;                                             // look after JOB
        if (s == OPHALT) break;                                                 // exit on halt
        partab.jobtab->io = 0;                                                  // force chan 0

        if (s == -(ERRZ51 + ERRMLAST)) {
            controlc();                                                         // control c
        } else if (s < 0) {
            ser(s);
        }

        partab.jobtab->error_frame = 0;                                         // and that one
        var = (mvar *) &strstk[0];                                              // space to setup a var
        VAR_CLEAR(var->name);
        bcopy("$ECODE", &var->name.var_cu[0], 6);
        var->volset = 0;
        var->uci = UCI_IS_LOCALVAR;
        var->slen = 0;                                                          // setup for $EC
        cptr = (cstring *) &strstk[sizeof(mvar)];                               // for result
        bcopy("$ECODE=", cptr->buf, 7);
        s = ST_Get(var, &cptr->buf[7]);
        if (s < 1) continue;                                                    // ignore if nothing there
        cptr->len = s + 7;
        s = SQ_Write(cptr);                                                     // write the prompt
        if (s < 0) ser(s);                                                      // check for error
        s = SQ_WriteFormat(SQ_LF);                                              // new line
        if (s < 0) ser(s);                                                      // check for error
        s = ST_Kill(var);                                                       // dong $EC
        cptr = (cstring *) (((u_char *) cptr) + 8);

        if (cptr->buf[0] != 'U') {
            cptr->len = 4;                                                      // max error size
            cptr->len = Xcall_errmsg((char *) cptr->buf, cptr, cptr);           // cvt to str
            s = SQ_Write(cptr);                                                 // write the error
            if (s < 0) ser(s);                                                  // check for error
            s = SQ_WriteFormat(SQ_LF);                                          // new line
        }
    }                                                                           // end command level loop

exit:                                                                           // general exit code
    if (partab.jobtab != NULL) CleanJob(0);                                     // if we have a jobtab then remove all locks etc.
    i = shmdt(systab);                                                          // detach the shared mem
    if (dbfd) i = close(dbfd);                                                  // close the database
    if (!failed_tty) failed_tty = tcsetattr(0, TCSANOW, &tty_settings);         // reset terminal if possible
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
