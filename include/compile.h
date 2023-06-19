/*
 * Package:  Reference Standard M
 * File:     rsm/include/compile.h
 * Summary:  module RSM header file - routine structures etc.
 *
 * David Wicksell <dlw@linux.com>
 * Copyright © 2020-2023 Fourth Watch Software LC
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

#ifndef _RSM_COMPILE_H_                                                         // only do this once
#define _RSM_COMPILE_H_

#define UNVAR { \
    comperror(-ERRM8); \
    return; \
}                                                                               // compile undef spec var

#define EXPRE { \
    comperror(-(ERRZ12 + ERRMLAST)); \
    return; \
}                                                                               // compile expr error

#define SYNTX { \
    comperror(-(ERRZ13 + ERRMLAST)); \
    return; \
}                                                                               // compile syntax error

#define ERROR(E) { \
    partab.jobtab->async_error = E; \
    partab.jobtab->attention = 1; \
    break; \
}                                                                               // report an error

#define INDSNOK(size) (((size * 2) + (sizeof(int) * 2) + isp) > MAX_ISTK)       // For testing indirection size - a guess
#define INDANOK(addr) ((addr + (sizeof(long) * 2) + 1) >= &indstk[MAX_ISTK])    // For testing the address of compiled indirection

#define RBD_OVERHEAD (sizeof(rbd *) + (sizeof(u_int) * 2) + sizeof(time_t) \
                     + sizeof(var_u) + (sizeof(u_char) * 2) + sizeof(u_short))

#define RESERVE_TIME    (20 * 60)                                               // 20 minutes
#define SIZE_CLOSE      1024                                                    // routine size match

#define FOR_TYP_0       0                                                       // no args
#define FOR_TYP_1       1                                                       // one arg
#define FOR_TYP_2       2                                                       // two args
#define FOR_TYP_3       3                                                       // three args
#define FOR_NESTED      16                                                      // we are not an outside for

// Funny op code stuff
#define BREAK_NOW       256                                                     // break (not really an opcode)
#define JOBIT           512                                                     // JOB (ditto)
#define BREAK_QN        16384                                                   // return a QUIT n

// Variable types follow
#define TYPMAXSUB       63                                                      // max subscripts
#define TYPVARNAM       0                                                       // name only (NAME_LEN bytes)
#define TYPVARLOCMAX    (TYPVARNAM + TYPMAXSUB)                                 // local is 1->63 subs
#define TYPVARIDX       64                                                      // 1 byte index (+ #subs)
#define TYPVARGBL       128                                                     // first global
#define TYPVARGBLMAX    (TYPVARGBL + TYPMAXSUB)                                 // global 128->191 subs
#define TYPVARNAKED     252                                                     // global naked reference
#define TYPVARGBLUCI    253                                                     // global with UCI
#define TYPVARGBLUCIENV 254                                                     // global with UCI and env
#define TYPVARIND       255                                                     // indirect

extern u_char *source_ptr;                                                      // pointer to source code
extern u_char *comp_ptr;                                                        // pointer to compiled code
extern u_char indstk[];                                                         // indirect stack
extern long   isp;                                                              // indirect stack pointer

typedef struct __attribute__ ((__packed__)) FOR_STACK {                         // saved FOR details
    short  type;                                                                // type of for (see above)
    short  svar;                                                                // syment of simple var
                                                                                // (if -1 use var)
    mvar   *var;                                                                // mvar on strstk of variable
    u_char *nxtarg;                                                             // where to jump for next
    u_char *startpc;                                                            // where the actual code starts
    u_char *quit;                                                               // where to quit to
    u_char *increment;                                                          // normalized incr string
    u_char *done;                                                               // normalized end point
} for_stack;                                                                    // end of FOR stuff

typedef struct __attribute__ ((__packed__)) TAGS {                              // define routine tags
    var_u   name;                                                               // tag name
    u_short code;                                                               // start of code this tag
} tags;                                                                         // end tags struct

typedef struct __attribute__ ((__packed__)) RBD {                               // define routine buf desciptor
    struct RBD *fwd_link;                                                       // forward link this hash
    u_int      chunk_size;                                                      // bytes in this chunk
    u_int      attached;                                                        // processes attached
    time_t     last_access;                                                     // last used (sec since 1970)
    var_u      rnam;                                                            // routine name
    u_char     uci;                                                             // UCI num for this rou
    u_char     vol;                                                             // vol num for this rou
    u_short    rou_size;                                                        // rou->len of routine node

    // what follows is the routine from disk (up to MAX_STR_LEN bytes + a NULL)
    u_short    comp_ver;                                                        // compiler version
    u_short    comp_user;                                                       // compiled by user#
    int        comp_date;                                                       // date compiled (M form)
    int        comp_time;                                                       // time compiled (M form)
    u_short    tag_tbl;                                                         // offset to tag table
    u_short    num_tags;                                                        // number of tags in table
    u_short    var_tbl;                                                         // offset to var table
    u_short    num_vars;                                                        // number of vars in table
    u_short    code;                                                            // offset to compiled code
    u_short    code_size;                                                       // bytes of code
} rbd;                                                                          // end RBD struct

// Compile only prototypes follow
void  parse_close(void);                                                        // CLOSE
void  parse_do(int runtime);                                                    // DO
void  parse_goto(int runtime);                                                  // GOTO
void  parse_hang(void);                                                         // HANG
void  parse_if(long i);                                                         // IF
void  parse_job(int runtime);                                                   // JOB
void  parse_kill(int indkillb);                                                 // KILL
void  parse_lock(int runtime);                                                  // LOCK
void  parse_merge(void);                                                        // MERGE
void  parse_new(void);                                                          // NEW
void  parse_open(void);                                                         // OPEN
void  parse_read(void);                                                         // READ
void  parse_set(void);                                                          // SET
void  parse_use(void);                                                          // USE
void  parse_write(void);                                                        // WRITE
void  parse_xecute(void);                                                       // XECUTE
void  parse(void);                                                              // parse - main loop
short localvar(void);                                                           // evaluate local variable
void  eval(void);                                                               // eval a string
void  atom(void);                                                               // evaluate source
void  comperror(short err);                                                     // compile error

// Debug prototypes

/*
#ifdef __NetBSD__
void  Debug_GBD(short e);
#endif
*/
void  Debug_off(void);                                                          // turn off debugging
short Debug_on(cstring *param);                                                 // turn on/modify debug
short Debug(int savasp, int savssp, int dot);                                   // drop into debug

#endif                                                                          // !_RSM_COMPILE_H_
