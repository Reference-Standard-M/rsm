/*
 * Package:  Reference Standard M
 * File:     rsm/include/symbol.h
 * Summary:  module RSM header file - includes for symbol module
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

#ifndef _RSM_SYMBOL_H_                                                          // only do this once
#define _RSM_SYMBOL_H_

#define DTBLKSIZE (sizeof(short) + sizeof(u_short) + sizeof(u_char) + (sizeof(ST_depend *) * 2)) // ST_data - empty data
#define DTMINSIZE (sizeof(short) + sizeof(u_short) + (sizeof(u_char) * 20) + (sizeof(ST_depend *) * 2)) // ST_data - 20 for data
#define DPBLKSIZE ((sizeof(u_char) * 2) + sizeof(u_short) + sizeof(ST_depend *)) // ST_depend - empty bytes

/*
#define NTBLKSIZE ((sizeof(short) * 2) + sizeof(short *) + sizeof(ST_newtab *) + sizeof(ST_locdata *)) // not currently used

struct ST_DATA;                                                                 // defined below

typedef struct __attribute__ ((__packed__)) NEW_STACK {                         // define new stack - not used
    short  type;                                                                // type of new
    short  ptr;                                                                 // ptr to variable
    struct ST_DATA *data;                                                       // data address
} new_stack;                                                                    // end of struct new_stack
*/

// SYMTAB definitions
#define ST_HASH 1023                                                            // hash size of symtab
#define ST_FREE ST_HASH                                                         // head of free list
#define ST_MAX  ((ST_HASH + 1) * 3)                                             // max number of ST entries
#define STORAGE ST_MAX                                                          // $STORAGE uses this to calculate free slots

// Structures for symbol table data
#define SIZE_KEY_DATA (MAX_KEY_SIZE + MAX_STR_LEN + 5)                          // for the following

typedef struct __attribute__ ((__packed__)) ST_DEPEND {                         // symbol dependent block
    struct ST_DEPEND *deplnk;                                                   // dependents link
    u_char           keylen;                                                    // length of key (bytes)
    u_char           bytes[SIZE_KEY_DATA];                                      // key bytes then data bytes
} ST_depend;                                                                    // end ST_depend structure

typedef struct __attribute__ ((__packed__)) ST_DATA {                           // symbol data block
    ST_depend *deplnk;                                                          // dependents link
    ST_depend *last_key;                                                        // last key used
    short     attach;                                                           // variable attach count
    u_short   dbc;                                                              // data byte count
    u_char    data[MAX_STR_LEN + 1];                                            // data bytes
} ST_data;                                                                      // end st_data structure

#define ST_DEPEND_NULL (ST_depend *) NULL                                       // define null pointer
#define ST_DATA_NULL   (ST_data *) NULL                                         // define null pointer

typedef struct __attribute__ ((__packed__)) SYMTAB {                            // define symtab structure
    short          fwd_link;                                                    // link to next entry
    short          usage;                                                       // usage count
    struct ST_DATA *data;                                                       // data block pointer
    var_u          varnam;                                                      // variable name union
} symtab_struct;                                                                // end symtab structure

extern short         st_hash[];                                                 // allocate hashing table
extern symtab_struct symtab[];                                                  // and symbol table

typedef struct __attribute__ ((__packed__)) ST_LOCDATA {
    short   stindex;                                                            // location in symtab
    ST_data *data;                                                              // pointer to data
} ST_locdata;

typedef struct __attribute__ ((__packed__)) ST_NEWTAB {
    struct ST_NEWTAB *fwd_link;                                                 // link to another newalltab
    short            count_enn;                                                 // existing non new count
    short            *stindex;                                                  // symtab indexes of enn vars
    short            count_new;                                                 // count of new'd vars
    ST_locdata       *locdata;                                                  // location of var and data
} ST_newtab;

typedef struct __attribute__ ((__packed__)) KEY_STRUCT {                        // start struct KEY
    u_char slen;                                                                // length of key
    u_char key[MAX_KEY_SIZE + 1];                                               // the actual key
} key_s;                                                                        // have MAX_KEY_SIZE + 1 chars

short ST_Locate(var_u var);                                                     // locate a var name
short ST_LocateIdx(int idx);                                                    // locate in symtab by index
short ST_Create(var_u var);                                                     // create and/or locate a var

void ST_RemDp(ST_data *dblk, ST_depend *prev, ST_depend *dp, mvar *mvardr);
void ST_Restore(ST_newtab *newtab);

#endif                                                                          // !_RSM_SYMBOL_H_
