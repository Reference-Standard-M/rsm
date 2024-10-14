/*
 * Package: Reference Standard M
 * File:    rsm/database/main.c
 * Summary: module database - main database functions
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

#include <stdio.h>                                                              // always include
#include <stddef.h>                                                             // for offsetof
#include <stdlib.h>                                                             // these two
#include <string.h>                                                             // for memcpy
#include <unistd.h>                                                             // for file reading
#include <fcntl.h>                                                              // for file stuff
#include <ctype.h>                                                              // for GBD stuff
#include <errno.h>                                                              // errno
#include <signal.h>                                                             // for kill
#include <sys/types.h>                                                          // for semaphores
#include <sys/ipc.h>                                                            // for semaphores
#include <sys/sem.h>                                                            // for semaphores
#include "rsm.h"                                                                // standard includes
#include "database.h"                                                           // database protos
#include "proto.h"                                                              // standard prototypes
#include "error.h"                                                              // error strings

int     curr_lock;                                                              // lock on globals
mvar    db_var;                                                                 // local copy of var
int     volnum;                                                                 // current volume
gbd     *blk[MAXTREEDEPTH];                                                     // current tree
int     level;                                                                  // level in above - 0 = global dir
u_int   rekey_blk[MAXREKEY];                                                    // to be re-keyed
int     rekey_lvl[MAXREKEY];                                                    // from level
u_int   Index;                                                                  // Index # into above
cstring *chunk;                                                                 // chunk at Index
cstring *record;                                                                // record at Index - points at dbc
u_char  keybuf[MAX_KEY_SIZE + 5];                                               // for storing keys
u_short *idx;                                                                   // for Indexes
int     *iidx;                                                                  // int ver of Index
int     writing;                                                                // set when writing
int     hash_start = 0;                                                         // start searching here

/*
 * Function: Copy2local
 * Summary:  Copy passed in mvar to db_var, adjusting volset and UCI
 *           The local copy of the mvar, db_var, is then used by all
 *           other database code. Only DB_QueryD uses the original
 *           DB_Compress also updates the original so that it can be watched
 * Input(s): Pointer to mvar to copy from
 * Return:   0 -> Ok, negative M error
 *
 * Note:     No locks are held at this stage.
 */
short Copy2local(const mvar *var)
{
    partab.jobtab->grefs++;                                                     // count global ref
    for (int i = 0; i < MAXTREEDEPTH; blk[i++] = NULL) continue;                // clear blk[]
    curr_lock = 0;                                                              // ensure this is clear
    writing = 0;                                                                // assume reading
    level = -1;                                                                 // no claimed GBDs yet
    memcpy(&db_var, var, sizeof(var_u) + 4 + var->slen);                        // copy the data
    if (db_var.volset == 0) db_var.volset = partab.jobtab->vol;                 // if volset is zero then get current volset
    if (db_var.uci == 0) db_var.uci = partab.jobtab->uci;                       // if UCI is zero then get current UCI
    if (db_var.name.var_cu[0] == '%') db_var.uci = 1;                           // if routine is %, change to UCI 1 (vol too?)
    if (db_var.volset > MAX_VOL) return -ERRM26;                                // within limits? if not - error
    if (systab->vol[db_var.volset - 1] == NULL) return -ERRM26;                 // is it mounted? if not - error
    if (db_var.uci > UCIS) return -ERRM26;                                      // too big

    if ((var->volset == 0) && (var->uci == 0)) {                                // no vol or UCI
        for (int i = 0; i < systab->max_tt; i++) {                              // scan trantab
            if (memcmp(&db_var, &systab->tt[i], sizeof(var_u) + 2) == 0) {      // if a match
                if (systab->tt[i].to_vol == 0) return i + 1;                    // flag routine proc (for triggers in the future)
                memcpy(&db_var.name, (char *) &systab->tt[i] + offsetof(trantab, to_global), sizeof(var_u) + 2);
                break;
            }                                                                   // end found one
        }
    }                                                                           // end trantab lookup

    if (SOA(partab.vol[db_var.volset - 1]->vollab)->uci[db_var.uci - 1].name.var_cu[0] == '\0') { // does UCI exist?
        return -ERRM26;                                                         // no - error
    }

    if ((db_var.name.var_cu[0] == '%') && (db_var.uci != 1)) return -ERRM26;    // if a %global AND UCI is not 1 then error
    volnum = db_var.volset;                                                     // save this for ron
    return 0;                                                                   // else return ok
}

/*
 * Function: DB_Get
 * Summary:  Locate and return data described in passed in mvar
 * Input(s): Pointer to mvar to get
 *           Pointer to buffer for data
 * Return:   String length -> Ok, negative M error
 */
int DB_Get(const mvar *var, u_char *buf)                                        // get global data
{
    int t;                                                                      // for returns

    t = Copy2local(var);                                                        // get local copy
    if (t < 0) return t;                                                        // exit on error

    /*
    if (t > 0) {                                                                // routine process
        t--;                                                                    // point at trantab entry
         * This code needs to invoke XXX^<systab->tt[t].to_global.var_cu>
         * as a routine where XXX is GET (this example), SET, KILL etc.
         * with mvar *var converted to cstring as arg1 and buf as
         * argument 2 passed by-reference.
         *
         * This code must then be copied to all 10 other calls to Copy2local
    }
    */

    partab.vol[volnum - 1]->stats.dbget++;                                      // update stats
    t = Get_data(0);                                                            // attempt to get it

    if (t >= 0) {                                                               // if worked
        if (memcmp(&db_var.name.var_cu[0], "$GLOBAL\0", 8) == 0) {              // if ^$GLOBAL
            t = ultocstring(buf, *((u_int *) record));                          // block number
        } else {
            t = mcopy(record->buf, buf, record->len);                           // copy the data
        }
    }

    if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                               // if locked then release global lock
    return t;                                                                   // return the count
}

/*
 * Function: DB_Set
 * Summary:  Set data passed to location described in mvar passed
 * Input(s): Pointer to mvar to set
 *           Pointer to cstring containing data
 * Return:   String length -> Ok, negative M error
 */
int DB_Set(const mvar *var, cstring *data)                                      // set global data
{
    int t;                                                                      // for returns
    int i;                                                                      // a handy int

    t = Copy2local(var);                                                        // get local copy
    if (t < 0) return t;                                                        // exit on error

    /*
    if (t > 0) {                                                                // ROU process
        t--;                                                                    // point at trantab entry
    }
    */

    i = 4 + db_var.slen + 2 + data->len;                                        // space required
    if (i & 3) i += (4 - (i & 3));                                              // if required then round up
    i += 4;                                                                     // add Index

    if (i > (int) (SOA(partab.vol[volnum - 1]->vollab)->block_size - sizeof(DB_Block))) {
        return -ERRM75;                                                         // if too big then return an error
    }

    partab.vol[volnum - 1]->stats.dbset++;                                      // update stats
    writing = 1;                                                                // say we are writing

    while (partab.vol[volnum - 1]->writelock) {                                 // check for write lock
        sleep(1);                                                               // wait a bit
        if (partab.jobtab->attention) return -(ERRZ51 + ERRZLAST);              // for <Control-C>
    }                                                                           // end writelock check

    i = SOA(partab.vol[volnum - 1]->vollab)->max_block >> 3;                    // last map byte necessary for current database size

    while (i) {                                                                 // check from the end
        if ((((u_char *) SOA(partab.vol[volnum - 1]->map))[i--]) == 0) break;   // OK if byte is free
    }

    if (!i) return -(ERRZ11 + ERRZLAST);                                        // complain if failed
    t = Set_data(data);                                                         // do the set
    if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                               // if locked then release global lock
    return t;                                                                   // return the result
}

/*
 * Function: DB_Data
 * Summary:  Return $DATA() for the passed in mvar
 * Input(s): Pointer to mvar to check
 *           Pointer to buffer for return result (0, 1, 10 or 11)
 * Return:   String length -> Ok, negative M error
 */
short DB_Data(const mvar *var, u_char *buf)                                     // get $DATA()
{
    int t;                                                                      // for returns
    int i;                                                                      // a handy int

    t = Copy2local(var);                                                        // get local copy
    if (t < 0) return t;                                                        // exit on error

    /*
    if (t > 0) {                                                                // ROU process
        t--;                                                                    // point at trantab entry
    }
    */

    partab.vol[volnum - 1]->stats.dbdat++;                                      // update stats
    t = Get_data(0);                                                            // attempt to get it
    i = 1;                                                                      // assume data found

    if (t == -ERRM7) {                                                          // undefined global?
        i = 0;                                                                  // yes - no data

        if (level == 0) {
            if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                       // if locked then release global lock

            if ((!db_var.slen) && (memcmp(&db_var.name.var_cu[0], "$GLOBAL\0", 8) == 0)) { // top level for $GLOBAL
                buf[0] = '1';                                                   // one to return
                buf[1] = '0';                                                   // zero to return
                buf[2] = '\0';                                                  // null terminated
                return 2;                                                       // and exit
            }

            buf[0] = '0';                                                       // zero to return
            buf[1] = '\0';                                                      // null terminated
            return 1;                                                           // and exit
        }
    } else if (t < 0) {                                                         // if it failed
        if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                           // if locked then release global lock
        return (short) t;                                                       // and exit
    }

    if (!db_var.slen && !i) Index++;                                            // pointing at 1st

    if (i || (Index > SOA(blk[level]->mem)->last_idx)) {                        // found or passed end
        t = Locate_next();                                                      // get next record

        if (t == -ERRM7) {                                                      // any more?
            if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                       // if locked then release global lock
            return ltocstring(buf, i);                                          // return result
        } else if (t < 0) {                                                     // error?
            if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                       // if locked then release global lock
            return (short) t;                                                   // and exit
        }
    }                                                                           // got next record

    // if smaller key AND a descendant?
    if (((db_var.slen < keybuf[0]) && (memcmp(&keybuf[1], db_var.key, db_var.slen) == 0)) || !db_var.slen) {
        i += 10;                                                                // add 10 to result
    }

    if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                               // if locked then release global lock
    return ltocstring(buf, i);                                                  // return result
}

/*
 * Function: DB_Kill
 * Summary:  Remove the sub-tree described by the passed in mvar
 * Input(s): Pointer to mvar to remove
 * Return:   0 -> Ok, negative M error
 */
short DB_Kill(const mvar *var)                                                  // remove sub-tree
{
    int t;                                                                      // for returns

    t = Copy2local(var);                                                        // get local copy
    if (t < 0) return (short) t;                                                // exit on error

    /*
    if (t > 0) {                                                                // ROU process
        t--;                                                                    // point at trantab entry
    }
    */

    partab.vol[volnum - 1]->stats.dbkil++;                                      // update stats

    while (partab.vol[volnum - 1]->writelock) {                                 // check for write lock
        sleep(1);                                                               // wait a bit
        if (partab.jobtab->attention) return -(ERRZ51 + ERRZLAST);              // for <Control-C>
    }                                                                           // end writelock check

    t = Get_data(0);                                                            // attempt to get it

    if (((t == -ERRM7) && (level == 0)) || ((t < 0) && (t != -ERRM7))) {        // if no such OR an error
        if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                           // if locked then release global lock
        if (t == -ERRM7) t = 0;                                                 // if undefined, that is OK
        return (short) t;                                                       // nothing to do
    }

    if ((t == -ERRM7) && db_var.slen) {                                         // if undefined
        if (Index <= SOA(blk[level]->mem)->last_idx) {                          // and still in block
            if ((db_var.slen > keybuf[0]) || (memcmp(&keybuf[1], db_var.key, db_var.slen) != 0)) { // smaller key - not descendant?
                if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                   // if locked then release global lock
                return 0;                                                       // nothing to do
            }
        } else {                                                                // end still in block
            t = Locate_next();                                                  // point at next block

            if (!t) {                                                           // found one
                if ((db_var.slen > keybuf[0]) || (memcmp(&keybuf[1], db_var.key, db_var.slen) != 0)) { // smaller - not descendant?
                    t = -ERRM7;                                                 // flag for later
                }
            }

            if (t < 0) {                                                        // no such or error
                if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                   // if locked then release global lock
                if (t == -ERRM7) t = 0;
                return (short) t;                                               // nothing to do
            }
        }
    }

    t = Kill_data();                                                            // do the kill
    if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                               // if locked then release global lock
    return (short) t;                                                           // return the result
}

/*
 * Function: DB_Order
 * Summary:  Return the next/prev subscript at the supplied level
 * Input(s): Pointer to mvar to search from
 *           Pointer to buffer to hold result
 *           Direction, 1 = fwd, -1 = bck
 * Return:   String length -> Ok, negative M error
 */
short DB_Order(const mvar *var, u_char *buf, int dir)                           // get next subscript
{
    int t;                                                                      // for returns
    int cnt;                                                                    // for character count
    int last_key;                                                               // start of last key

    t = Copy2local(var);                                                        // get local copy
    if (t < 0) return t;                                                        // exit on error

    /*
    if (t > 0) {                                                                // ROU process
        t--;                                                                    // point at trantab entry
    }
    */

    partab.vol[volnum - 1]->stats.dbord++;                                      // update stats
    last_key = UTIL_Key_Last(&db_var);                                          // get start of last
    buf[0] = '\0';                                                              // null terminate ret

    if (dir < 0) {                                                              // if it's backward
        t = Get_data(-1);                                                       // get the previous

        if ((t < 0) && (t != -ERRM7)) {                                         // check for errors
            if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                       // if locked then release global lock
            return (short) t;                                                   // and return the error
        }

        if ((level == 0) && (t == -ERRM7) && (memcmp(&db_var.name.var_cu[0], "$GLOBAL\0", 8) != 0)) { // if no global - not ^$GLOBAL
            if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                       // if locked then release global lock
            return 0;                                                           // and return
        }

        Index--;                                                                // backup the Index
        if (Index < IDX_START) panic("DB_Order: Problem with negative direction"); // can't happen?
        chunk = (cstring *) &iidx[idx[Index]];                                  // point at the chunk
        record = (cstring *) &chunk->buf[chunk->buf[1] + 4];                    // point at the dbc
    } else {                                                                    // end backwards - it's forward
        db_var.key[db_var.slen++] = 255;                                        // force next key
        t = Get_data(0);                                                        // try to find that

        if (t != -ERRM7) {                                                      // MUST be undefined
            if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                       // if locked then release global lock
            return ((t < 0) ? (short) t : -(ERRZ61 + ERRMLAST));                // and return the error
        }

        if ((level == 0) && (memcmp(&db_var.name.var_cu[0], "$GLOBAL\0", 8) != 0)) { // if no such global AND not ^$GLOBAL
            if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                       // if locked then release global lock
            return 0;                                                           // and return
        }

        if (Index > SOA(blk[level]->mem)->last_idx) {                           // no more available
            t = Locate_next();                                                  // get next (if there)

            if (t < 0) {                                                        // failed?
                if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                   // if locked then release global lock
                return ((t == -ERRM7) ? 0 : t);                                 // done
            }
        }
    }                                                                           // end forwards

    for (u_int i = IDX_START; i <= Index; i++) {                                // scan to current
        chunk = (cstring *) &iidx[idx[i]];                                      // point at the chunk
        memcpy(&keybuf[chunk->buf[0] + 1], &chunk->buf[2], chunk->buf[1]);      // update the key
        keybuf[0] = chunk->buf[0] + chunk->buf[1];                              // and the size
    }

    if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                               // if locked then release global lock
    if ((keybuf[0] < (last_key + 1)) || (memcmp(&keybuf[1], db_var.key, last_key) != 0)) return 0; // check for past it and done
    cnt = 0;                                                                    // clear flag
    return UTIL_Key_Extract(&keybuf[last_key + 1], buf, &cnt);                  // extract the key and return
}

/*
 * Function: DB_Query
 * Summary:  Return the next/prev full key to the supplied one
 * Input(s): Pointer to mvar to search from
 *           Pointer to buffer to hold result
 *           Direction, 1 = fwd, -1 = bck
 * Return:   String length -> Ok, negative M error
 */
short DB_Query(mvar *var, u_char *buf, int dir)                                 // get next key
{
    int t;                                                                      // for returns

    t = Copy2local(var);                                                        // get local copy
    if (t < 0) return (short) t;                                                // exit on error

    /*
    if (t > 0) {                                                                // ROU process
        t--;                                                                    // point at trantab entry
    }
    */

    partab.vol[volnum - 1]->stats.dbqry++;                                      // update stats

    if (dir < 0) {                                                              // if it's backward
        t = Get_data(-1);                                                       // get the previous

        if ((t < 0) && (t != -ERRM7)) {                                         // check for errors
            if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                       // if locked then release global lock
            return (short) t;                                                   // and return the error
        }

        if ((level == 0) && (t == -ERRM7)) {                                    // if no such global
            buf[0] = '\0';                                                      // null terminate ret
            if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                       // if locked then release global lock
            return 0;                                                           // and return
        }

        Index--;                                                                // backup the Index
        if (Index < IDX_START) panic("DB_Query: Problem with negative direction"); // can't happen?
        chunk = (cstring *) &iidx[idx[Index]];                                  // point at the chunk
        record = (cstring *) &chunk->buf[chunk->buf[1] + 4];                    // point at the dbc

        if (!chunk->buf[0] && !chunk->buf[1] && ((partab.jobtab->last_block_flags & GL_TOP_DEFINED) == 0)) { // if first node
            buf[0] = '\0';                                                      // null terminate ret
            if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                       // if locked then release global lock
            return 0;                                                           // and return
        }
    } else {                                                                    // end backwards - it's forward
        t = Get_data(0);                                                        // try to find that

        if ((t < 0) && (t != -ERRM7)) {                                         // check for errors
            if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                       // if locked then release global lock
            return (short) t;                                                   // and return the error
        }

        if ((level == 0) && (t == -ERRM7)) {                                    // if no such global
            buf[0] = '\0';                                                      // null terminate ret
            if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                       // if locked then release global lock
            return 0;                                                           // and return
        }

        if ((t < 0) && !db_var.slen) Index++;

        if ((Index > SOA(blk[level]->mem)->last_idx) || (t >= 0)) {             // want next one
            t = Locate_next();                                                  // point at next

            if (t < 0) {                                                        // not found or error
                if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                   // if locked then release global lock
                buf[0] = '\0';                                                  // null terminate ret
                if (t == -ERRM7) t = 0;                                         // undefined? if yes - clear it
                return (short) t;                                               // done
            }
        }
    }

    for (u_int i = IDX_START; i <= Index; i++) {                                // scan to current
        chunk = (cstring *) &iidx[idx[i]];                                      // point at the chunk
        memcpy(&keybuf[chunk->buf[0] + 1], &chunk->buf[2], chunk->buf[1]);      // update the key
        keybuf[0] = chunk->buf[0] + chunk->buf[1];                              // and the size
    }

    if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                               // if locked then release global lock
    db_var.uci = var->uci;                                                      // copy
    db_var.volset = var->volset;                                                //   original & new
    VAR_COPY(db_var.name, var->name);                                           //     data
    db_var.slen = keybuf[0];                                                    //       to
    memcpy(&db_var.key[0], &keybuf[1], keybuf[0]);                              //         db_var
    return UTIL_String_Mvar(&db_var, buf, MAX_NUM_SUBS);                        // convert and return
}

/*
 * Function: DB_QueryD
 * Summary:  Return the next full key to the supplied one
 * Input(s): Pointer to mvar to search from
 *           Pointer to buffer to hold result
 * Return:   Length of returned string or negative error number
 *           Updated mvar if not error
 *           Data from updated mvar (if no error)
 */
short DB_QueryD(mvar *var, u_char *buf)                                         // get next key
{
    int t;                                                                      // for returns

    t = Copy2local(var);                                                        // get local copy
    if (t < 0) return (short) t;                                                // exit on error

    /*
    if (t > 0) {                                                                // ROU process
        t--;                                                                    // point at trantab entry
    }
    */

    t = Get_data(0);                                                            // try to find that

    if ((t < 0) && (t != -ERRM7)) {                                             // check for errors
        if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                           // if locked then release global lock
        return (short) t;                                                       // and return the error
    }

    if ((level == 0) && (t == -ERRM7)) {                                        // if no such global
        buf[0] = '\0';                                                          // null terminate ret
        if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                           // if locked then release global lock
        return -(ERRZ55 + ERRMLAST);                                            // and return
    }

    if ((t < 0) && db_var.slen) {                                               // If we had a "real"
        Index--;                                                                // <UNDEF> last time
    }                                                                           // back up Index

    t = Locate_next();                                                          // point at next

    if (t < 0) {                                                                // not found or error
        if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                           // if locked then release global lock
        if (t == -ERRM7) t = -(ERRZ55 + ERRMLAST);                              // if no more then say 'at end'
        return (short) t;                                                       // done
    }

    /*
    for (u_int i = IDX_START; i <= Index; i++) {                                // scan to current
        chunk = (cstring *) &iidx[idx[i]];                                      // point at the chunk
        memcpy(&keybuf[chunk->buf[0] + 1], &chunk->buf[2], chunk->buf[1]);      // update the key
        keybuf[0] = chunk->buf[0] + chunk->buf[1];                              // and the size
    }
    */

    memcpy(var->key, &keybuf[1], (int) keybuf[0]);                              // copy in the key
    var->slen = keybuf[0];                                                      // update the length
    t = mcopy(record->buf, buf, record->len);                                   // copy the data
    if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                               // if locked then release global lock
    return (short) t;                                                           // return the count
}

/*
 * Function: DB_GetLen
 * Summary:  Locate and return length of data described in passed in mvar
 *           If buf is not NULL, return the data there
 *           The global module is always unlocked on an error
 * Input(s): Pointer to mvar to get length of
 *           State to leave SEM_GLOBAL lock (1 -> leave locked, -1 -> unlock)
 *           A state of -1, JUST does an unlock and returns 0
 *           Buffer for routine (if not NULL)
 * Return:   String length -> Ok, negative M error
 * Note:     There may be NO intervening calls to other DB modules
 *           when the GBD has been left locked
 */
int DB_GetLen(const mvar *var, int lock, u_char *buf)                           // length of node
{
    int t;                                                                      // for returns
    int sav;                                                                    // save curr_lock

    if ((lock == -1) && (buf == NULL)) {                                        // just unlock?
        if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                           // if locked then unlock it
        return 0;                                                               // exit
    }

    sav = curr_lock;                                                            // save this
    t = Copy2local(var);                                                        // get local copy
    curr_lock = sav;                                                            // restore current lock

    if (t < 0) {                                                                // check for error
        if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                           // if locked then release global lock
        return t;                                                               // and return
    }

    /*
    if (t > 0) {                                                                // ROU process
        t--;                                                                    // point at trantab entry
    }
    */

    t = Get_data(0);                                                            // attempt to get it

    if (t < 0) {                                                                // check for error
        if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                           // if locked then release global lock
        return t;                                                               // and return
    }

    if (buf != NULL) t = mcopy(record->buf, buf, record->len);                  // want data? then copy the data
    if ((lock != 1) && curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                // preserve lock? if no - release it
    return t;                                                                   // and exit
}

/*
 * Function: DB_Free
 * Summary:  Return number of free blocks in volume set
 * Input(s): Volume set number to examine
 * Return:   Number of free blocks
 */
int DB_Free(int vol)                                                            // total free blocks
{
    short s;                                                                    // for funcs
    int   count = 0;                                                            // block count

    s = SemOp(SEM_GLOBAL, SEM_READ);                                            // lock the globals
    if (s < 0) return s;                                                        // return any errors

    // start at block 1, while still in map, going up by one
    for (u_int i = 1; i <= SOA(partab.vol[vol - 1]->vollab)->max_block; i++) {
        count += (((((u_char *) SOA(partab.vol[vol - 1]->map))[i >> 3]) & (1U << (i & 7))) == 0); // add up blocks
    }

    SemOp(SEM_GLOBAL, -curr_lock);                                              // unlock the globals
    return count;                                                               // return the count
}                                                                               // end DB_Free

/*
 * Function: DB_Expand
 * Summary:  Expand volume set
 * Input(s): Internal volume set number to expand
 *           New size in blocks (checks have been done)
 * Return:   0 or error
 */
short DB_Expand(int vol, u_int vsiz)                                            // expand it
{
    off_t  fptr;                                                                // for lseek
    off_t  fres;                                                                // ditto
    u_int  vexp;                                                                // expand by
    u_char *p;                                                                  // for malloc
    int    dbfd;                                                                // for open

    p = malloc(SOA(partab.vol[vol]->vollab)->block_size);                       // get some space
    if (p == NULL) return -(ERRMLAST + ERRZLAST + errno);                       // die
    memset(p, 0, SOA(partab.vol[vol]->vollab)->block_size);                     // clear it
    dbfd = open(partab.vol[vol]->file_name, O_RDWR);                            // open database read-write

    if (dbfd == -1) {                                                           // if failed
        free(p);                                                                // free memory
        return -(ERRMLAST + ERRZLAST + errno);                                  // and die
    }

    fptr = (off_t) SOA(partab.vol[vol]->vollab)->max_block;                     // start here

    fptr = (fptr * (off_t) SOA(partab.vol[vol]->vollab)->block_size)
         + (off_t) SOA(partab.vol[vol]->vollab)->header_bytes;

    fres = lseek(dbfd, fptr, SEEK_SET);                                         // Seek to eof

    if (fres != fptr) {                                                         // if failed
        free(p);                                                                // free memory
        return -(ERRMLAST + ERRZLAST + errno);                                  // and die
    }

    vexp = vsiz - SOA(partab.vol[vol]->vollab)->max_block;                      // expand by

    while (vexp) {
        int i;

        i = write(dbfd, p, SOA(partab.vol[vol]->vollab)->block_size);

        if (i == -1) {                                                          // if failed
            free(p);                                                            // free memory
            return -(ERRMLAST + ERRZLAST + errno);                              // and die
        }

        vexp--;                                                                 // count 1
    }

    free(p);                                                                    // free memory
    close(dbfd);                                                                // close DB file
    SOA(partab.vol[vol]->vollab)->max_block = vsiz;                             // store new size
    partab.vol[vol]->map_dirty_flag = 1;                                        // say write this
    return 0;
}

/*
 * Function: DB_Dismount
 * Summary:  Dismount volume set
 * Input(s): Volume set number to dismount
 * Return:   0
 */
int DB_Dismount(int vol)                                                        // dismount a volume
{
    DB_StopJournal(vol, JRN_ESTOP);
    partab.vol[vol - 1]->dismount_flag = 1;                                     // set the flag

    while (partab.vol[vol - 1]->wd_tab[0].pid) {                                // wait until main daemon has stopped
        if (kill(partab.vol[vol - 1]->wd_tab[0].pid, 0) == -1) break;           // main daemon died (or no perm) before clearing PID
        sleep(1);
    }

    return 0;                                                                   // that's all for now
}

/*
 * Function: DB_StopJournal
 * Summary:  Stop journaling on a volume
 * Input(s): Volume set number to stop
 *           Reason (currently JRN_STOP and JRN_ESTOP)
 * Return:   None
 */
void DB_StopJournal(int vol, u_char action)                                     // Stop journal
{
    jrnrec jj;

    volnum = vol;                                                               // set common var
    if (!SOA(partab.vol[vol - 1]->vollab)->journal_available) return;           // if no journal then just exit
    while (SemOp(SEM_GLOBAL, SEM_WRITE)) sleep(1);
    jj.action = action;
    jj.uci = 0;
    VAR_CLEAR(jj.name);
    jj.slen = 0;
    DoJournal(&jj, NULL);
    SOA(partab.vol[vol - 1]->vollab)->journal_available = 0;
    SemOp(SEM_GLOBAL, -SEM_WRITE);                                              // release global lock
    return;
}

/*
 * Function: DB_GetFlags
 * Summary:  Get global flags
 * Input(s): Pointer to mvar -> ^$GLOBAL("name")
 * Return:   Flags or negative M error
 */
int DB_GetFlags(const mvar *var)                                                // get flags
{
    int t;                                                                      // for returns
    int i;                                                                      // a handy int

    t = Copy2local(var);                                                        // get local copy
    if (t < 0) return t;                                                        // exit on error

    /*
    if (t > 0) {                                                                // ROU process
        t--;                                                                    // point at trantab entry
    }
    */

    t = Get_data(0);                                                            // try to find that

    if ((t < 0) && (t != -ERRM7)) {                                             // check for errors
        if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                           // if locked then release global lock
        return t;                                                               // and return the error
    }

    i = ((int *) record)[1];                                                    // get the value
    if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                               // if locked then release global lock
    return i;                                                                   // return the flags
}

/*
 * Function: DB_SetFlags
 * Summary:  Set global flags
 * Input(s): Pointer to mvar -> ^$GLOBAL("name")
 *           Positive flags to set or negative flags to clear
 * Return:   New flags or negative M error
 */
int DB_SetFlags(const mvar *var, int flags)                                     // set flags
{
    int clearit = 0;
    int i;
    int t;                                                                      // for returns

    if (flags < 0) {
        clearit = 1;                                                            // setup to clear
        flags = -flags;                                                         // get flags correct
    }

    t = Copy2local(var);                                                        // get local copy
    if (t < 0) return t;                                                        // exit on error

    /*
    if (t > 0) {                                                                // ROU process
        t--;                                                                    // point at trantab entry
    }
    */

    partab.vol[volnum - 1]->stats.dbset++;                                      // update stats
    writing = 1;                                                                // say we are writing

    while (partab.vol[volnum - 1]->writelock) {                                 // check for write lock
        sleep(1);                                                               // wait a bit
        if (partab.jobtab->attention) return -(ERRZ51 + ERRZLAST);              // for <Control-C>
    }                                                                           // end writelock check

    Get_GBDs(1);                                                                // ensure this many
    t = Get_data(0);                                                            // try to find that

    if ((t < 0) && (t != -ERRM7)) {                                             // check for errors
        if (curr_lock) SemOp(SEM_GLOBAL, -curr_lock);                           // if locked then release global lock
        return t;                                                               // return error
    }

    i = ((int *) record)[1];                                                    // get current flags

    if (clearit) {
        i &= ~flags;                                                            // clear flags
    } else {
        i |= flags;                                                             // set flags
    }

    ((int *) record)[1] = i;                                                    // set back to GD

    if (blk[level]->dirty == (gbd *) 1) {                                       // if reserved
        blk[level]->dirty = SBA(blk[level]);                                    // terminate list
        Queit();                                                                // queue for write
    }

    SemOp(SEM_GLOBAL, -curr_lock);                                              // release global lock
    return i;                                                                   // return current flags
}

/*
 * Function: DB_Compress
 * Summary:  Compress a global on-line
 * Input(s): Where to start in global (mvar) Must ---> partab.jobtab->last_ref
 *           Level to process 0 -> 15 (data level or more means data level)
 * Return:   Actual level number processed or error number
 */
short DB_Compress(mvar *var, int flags)                                         // Compress global
{
    int   i;
    int   t;
    short retlevel;                                                             // the ACTUAL level

    flags &= 15;                                                                // clear high bits
    t = Copy2local(var);                                                        // get local copy
    if (t < 0) return (short) t;                                                // exit on error

    /*
    if (t > 0) {                                                                // ROU process
        t--;                                                                    // point at trantab entry
    }
    */

    memset(rekey_blk, 0, MAXREKEY * sizeof(u_int));                             // clear that table
    memset(rekey_lvl, 0, MAXREKEY * sizeof(int));                               // and that table
    memcpy(var, &db_var, sizeof(mvar));                                         // copy the data back
    t = Get_data(flags);                                                        // get to level 'flags'
    retlevel = level;                                                           // save real level

    if (!level) {                                                               // give up if no such
        SemOp(SEM_GLOBAL, -curr_lock);                                          // release global lock
        return -ERRM7;
    }

    chunk = (cstring *) &iidx[idx[IDX_START]];                                  // point at the first
    memcpy(&var->slen, &chunk->buf[1], chunk->buf[1] + 1);                      // save the real key

    while (TRUE) {
        memcpy(&db_var, var, sizeof(mvar));                                     // get next key
        writing = 0;                                                            // flag we are reading

        while (partab.vol[volnum - 1]->writelock) {                             // check for write lock
            sleep(1);                                                           // wait a bit
            if (partab.jobtab->attention) return -(ERRZ51 + ERRZLAST);          // for <Control-C>
        }                                                                       // end writelock check

        if (partab.jobtab->attention) return -(ERRZ51 + ERRZLAST);              // for <Control-C>
        t = Get_data(retlevel);                                                 // get the block
        if ((t == -ERRM7) && !db_var.slen) t = 0;                               // if first node then it exists

        if (t == -ERRM7) {                                                      // if key changed
            if (SOA(blk[level]->mem)->right_ptr) {                              // if more
                chunk = (cstring *) &iidx[idx[IDX_START]];                      // point at the first
                memcpy(&db_var.slen, &chunk->buf[1], chunk->buf[1] + 1);        // save real key
                SemOp(SEM_GLOBAL, -curr_lock);                                  // release global lock
                continue;                                                       // go again
            }

            SemOp(SEM_GLOBAL, -curr_lock);                                      // release global lock
            return retlevel;                                                    // all done, exit
        }

        if (t < 0) {
            SemOp(SEM_GLOBAL, -curr_lock);                                      // release global lock
            return (short) t;                                                   // exit on error
        }

        if (!SOA(blk[level]->mem)->right_ptr) {                                 // if no more
            SemOp(SEM_GLOBAL, -curr_lock);                                      // release global lock

            if ((retlevel == 2) && !db_var.slen) {                              // if only block lvl 2
                t = Compress1();                                                // do that
                SemOp(SEM_GLOBAL, -curr_lock);                                  // release write lock
                if (t < 0) return (short) t;                                    // exit on error
            }

            return retlevel;                                                    // all done, exit
        }

        level++;
        t = Get_block(SOA(blk[level - 1]->mem)->right_ptr);

        if (t < 0) {                                                            // if error
            SemOp(SEM_GLOBAL, -curr_lock);                                      // release global lock
            return (short) t;                                                   // exit on error
        }

        i = ((SOA(blk[level - 1]->mem)->last_free * 2 + 1 - SOA(blk[level - 1]->mem)->last_idx) * 2)
          + ((SOA(blk[level]->mem)->last_free * 2 + 1 - SOA(blk[level]->mem)->last_idx) * 2);

        // if REALLY not enough space (NOTE: make this a param)
        if (i < 1024) {
            chunk = (cstring *) &iidx[idx[IDX_START]];                          // point at first in RL
            memcpy(&var->slen, &chunk->buf[1], chunk->buf[1] + 1);              // save the real key
            SemOp(SEM_GLOBAL, -curr_lock);                                      // release global lock
            continue;                                                           // go again
        }

        level = retlevel;
        SemOp(SEM_GLOBAL, -curr_lock);                                          // release read lock
        t = Compress1();                                                        // do that
        SemOp(SEM_GLOBAL, -curr_lock);                                          // release write lock
        if (t < 0) return (short) t;                                            // exit on error
        if (!var->volset) return retlevel;                                      // if done
    }
}
