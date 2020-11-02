/*
 * Package:  Reference Standard M
 * File:     rsm/compile/localvar.c
 * Summary:  module compile - parse a local variable
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

#include <stdio.h>                              // always include
#include <stdlib.h>                             // these two
#include <sys/types.h>                          // for u_char def
#include <string.h>
#include <ctype.h>
#include <errno.h>                              // error stuff
#include <limits.h>                     	// for LONG_MAX etc
#include <math.h>
#include "rsm.h"                                // standard includes
#include "proto.h"                              // standard prototypes
#include "error.h"                              // and the error defs
#include "opcodes.h"                            // and the opcodes
#include "compile.h"				// compile stuff

// function localvar entered with source_ptr pointing at the source
// variable to evaluate and comp_ptr pointing at where to put the code.
//
// Return       Means
// -ERR         Nothing compiled, error returned
// off		Offset from starting point of comp_ptr for OPVAR
//
// Following the OPVAR is a byte indicating type of
// variable as per the following:
//	TYPMAXSUB       63                      // max subscripts
//	TYPVARNAM       0                       // name only (8 bytes)
//	TYPVARLOCMAX    TYPVARNAM+TYPMAXSUB     // local is 1->63 subs
//	TYPVARIDX       64                      // 1 byte index (+ #subs)
//	TYPVARGBL       128                     // first global
//	TYPVARGBLMAX    TYPVARGBL+TYPMAXSUB     // global 128->191 subs
//	TYPVARNAKED	252			// global naked reference
//	TYPVARGBLUCI	253			// global with uci
//	TYPVARGBLUCIENV	254			// global with uci and env
//	TYPVARIND	255			// indirect
//
//  For TYPVARNAM: 	OPVAR TYPVARNAM (var_u) name
//	TYPVARLOC: 	subscripts OPVAR TYPVARNAM+#subs (var_u) name
//	TYPVARIDX:	subscripts OPVAR TYPVARIDX+#subs (u_char) idx
//	TYPVARGBL:	subscripts OPVAR TYPVARGBL+#subs (var_u) name
//	TYPVARNAKED:	subscripts OPVAR TYPVARNAKED #subs
//	TYPVARGBLUCI:	subscripts uci OPVAR TYPVARGBLUCI #subs (var_u) name
//	TYPVARGBLUCIENV: subs uci env OPVAR TYPVARGBLUCIENV #subs (var_u) name
//	TYPVARIND:	(str on addstk[]) [subs] OPVAR TYPEVARIND #subs

short localvar()                                // evaluate local variable
{ char c;                                       // current character
  u_char idx = 0;				// index
  int i;                                        // a usefull int
  int count = 0;				// count subscripts
  var_u var;                                    // to hold variable names
  u_char *ptr;					// to save comp_ptr
  short type = TYPVARNAM;			// the type code
  short ret;					// the return

  ptr = comp_ptr;				// save comp_ptr
  c = *source_ptr++;                            // get a character
  if (c == '^')					// if it's global
  { type = TYPVARGBL;				// say it's global
    c = *source_ptr++;				// point past it
    if (c == '[')				// uci/env specified
    { type = TYPVARGBLUCI;			// we have a uci specified
      atom();					// eval the argument
      c = *source_ptr++; 			// get next
      if (c == ',')				// if it's a comma
      { type = TYPVARGBLUCIENV;			// say we have vol as well
	atom();					// eval the argument
	c = *source_ptr++; 			// get next
      }
      if (c != ']') return (-(ERRZ12+ERRMLAST)); // that's junk
      c = *source_ptr++; 			// get next
    }
    else if (c == '(')				// naked reference ?
    { type = TYPVARNAKED;			// set type
      source_ptr--;				// back up source
      goto subs;
    }
  }
  else if (c == '@')				// indirect ?
  { type = TYPVARIND;				// yes @...@ ... on addstk[]
    if (*source_ptr == '(') goto subs;		// go do subscripts
    return (-(ERRZ12+ERRMLAST));		// else it's junk
  }
  if ((isalpha((int)c) == 0) && (c != '%') && (c != '$')) // check for a variable
    return (-(ERRZ12+ERRMLAST));                // return the error
  if ((c == '$') && (type == TYPVARNAM))	// check $...
  { if (isalpha(*source_ptr) == 0)		// next must be alpha
    { return (-(ERRZ12+ERRMLAST));              // return the error
    }
    i = toupper(*source_ptr);			// get the next character
    if (strchr("DEHIJKPQRSTXY", i) == NULL)	// if letter is invalid
    { return -ERRM8;				// complain
    }
  }
  var.var_qu = 0;                               // clear the variable name
  var.var_cu[0] = c;                            // save first char
  for (i = 1; i<8; i++)                         // scan for rest of name
  { c = *source_ptr++;                          // get next char
    if (isalnum((int)c) == 0)                   // if not alpha numeric
    { --source_ptr;                             // point back at it
      break;                                    // and exit
    }
    var.var_cu[i] = c;                          // save in the variable
  }
  while (isalnum(*source_ptr) !=0) source_ptr++; // skip extended name
subs:
  if (*source_ptr == '(')                       // see if it's subscripted
  { source_ptr++;				// skip the bracket
    while (TRUE)				// loop
    { eval();					// get a subscript
      count++;					// count it
      c = *source_ptr++;			// get next character
      if (c == ')') break;			// quit when done
      if (c != ',') return (-(ERRZ12+ERRMLAST)); // return the error
    }
  }
  if (count > TYPMAXSUB)			// too many
    return -(ERRZ15+ERRMLAST);			// error
  ret = comp_ptr - ptr;				// remember here
  *comp_ptr++ = OPVAR;				// opcode
  if ((type < TYPVARGBL) &&			// candidate for index?
      (partab.varlst != NULL) &&		// and in a routine compile
      (var.var_cu[0] != '$'))			// and it's not $...
  { for (i = 0; ; i++)				// scan list
    { if (i == 256) break;			// too many
      if (partab.varlst[i].var_qu == var.var_qu)
        break;					// found it
      if (partab.varlst[i].var_qu == 0)
      { partab.varlst[i].var_qu = var.var_qu;	// set it
	break;
      }
    }
    if (i != 256)
    { type |= TYPVARIDX;			// change the type
      idx = i;					// save index
    }
  }
  if (type < TYPVARNAKED)			// normal local or global var
  { type = type + count;			// add the count
    *comp_ptr++ = (u_char) type;		// store it
    if (type & TYPVARIDX)			// index type?
      *comp_ptr++ = idx;			// save the index
  }
  else						// funny type
  { *comp_ptr++ = (u_char) type;		// store the type
    *comp_ptr++ = count;			// then the subscripts
  }
  if (((type < TYPVARIDX) ||			// if simple local (not idx)
       (type >= TYPVARGBL)) &&			// a 'normal' global
      (type != TYPVARNAKED) &&			// and not naked
      (type != TYPVARIND))			// or indirect
    for (i = 0; i<8; i++)                       // scan the name
      *comp_ptr++ = var.var_cu[i];              // copy into compiled code
  return ret;					// say what we did
}                                               // end variable parse

