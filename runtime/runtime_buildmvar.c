/*
 * Package:  Reference Standard M
 * File:     rsm/runtime/runtime_buildmvar.c
 * Summary:  module runtime - build an mvar
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
#include <strings.h>
#include <ctype.h>
#include <errno.h>                              // error stuff
#include "rsm.h"                                // standard includes
#include "proto.h"                              // standard prototypes
#include "error.h"				// standard errors
#include "opcodes.h"				// the op codes
#include "compile.h"				// rbd structure

// This module is the runtime code to build an mvar.
// It is passed the addres of the mvar and reads from *rsmpc++.
// see comments in rsm/compile/localvar.c for more info.
// If nul_ok is true, a null subscript as the last is OK.
// Returns new asp or -err
//

short getvol(cstring *vol)			// get vol number for vol
{ int i;					// a handy int
  short s;					// for cstring length
  s = vol->len;					// get len
  if (s < VAR_LEN) s++;				// include term null if poss
  for (i = 0; i < MAX_VOL; i++)			// scan the volumes
  { if (systab->vol[i]->vollab == NULL) continue; // continue if none in slot
    if (bcmp(&systab->vol[i]->vollab->volnam.var_cu[0], vol->buf, s) != 0)
      continue;					// if not the same continue
    return (short) i + 1;			// return vol number
  }
  return (-ERRM26);				// complain - no such
}

short getuci(cstring *uci, int vol)		// get uci number
{ int i;					// for loops
  short s;					// for cstring length
  s = uci->len;					// get len
  if (s < VAR_LEN) s++;				// include term null if poss
  if (vol == 0) vol = partab.jobtab->vol;	// get current vol
  vol--;					// make internal reference
  for (i = 0; i < UCIS; i++)			// scan the ucis
  { if (bcmp(&systab->vol[vol]->vollab->uci[i].name.var_cu[0], uci->buf, s) == 0)
      return (short) i + 1;
  }
  return (-ERRM26);				// complain - no such
}

short buildmvar(mvar *var, int nul_ok, int asp) // build an mvar
{ u_char type;					// variable type
  int subs;					// subscript count
  int i;					// a handy int
  cstring *ptr;					// and a handy pointer
  short s;					// for returns
  var_u *vt;					// var table pointer
  rbd *p;					// a handy pointer
  mvar *ind;					// ind mvar ptr

  type = *rsmpc++;				// get the type
  if (type < TYPVARNAKED)			// subs in type
  { subs = (type & TYPMAXSUB);			// the low bits
    type = (type & ~TYPMAXSUB);			// and the type
  }
  else
    subs = *rsmpc++;				// get in line
  var->volset = 0;				// default vol set
  var->uci = (type < TYPVARGBL) ?  UCI_IS_LOCALVAR : 0;	// assume local var or uci 0
  var->slen = 0;				// and no subscripts
  if (type == TYPVARNAKED)			// if it's a naked
  { if (var_empty(partab.jobtab->last_ref.name))
      return (-ERRM1); 				// say "Naked indicator undef"
    i = UTIL_Key_Last(&partab.jobtab->last_ref); // start of last key
    if (i < 0) return (-ERRM1); 		// say "Naked indicator undef"
    bcopy(&(partab.jobtab->last_ref), var, sizeof(var_u) + 5 + i); // copy naked naked
    var->slen = (u_char) i;			// stuff in the count
  }
  else if (type == TYPVARIND)			// it's an indirect
  { ind = (mvar *) addstk[asp-subs-1];		// point at mvar so far
    bcopy(ind, var, ind->slen + sizeof(var_u) + 5); // copy it in
  }
  else if ((type & TYPVARIDX) &&		// if it's the index type
	   (type < TYPVARGBL))			// and it's local
  { i = *rsmpc++;				// get the index
    if (i < 255)				// can't do the last one
    { var->volset = i + 1;			// save the index (+ 1)
      VAR_CLEAR(var->name);			// clear the name
    }
    else
    { p = (rbd *) (partab.jobtab->dostk[partab.jobtab->cur_do].routine);
      vt = (var_u *) (((u_char *) p) + p->var_tbl); // point at var table
      VAR_COPY(var->name, vt[i]);		// get the var name
    }
  }
  else
  { bcopy(rsmpc, &var->name, VAR_LEN);
    rsmpc += VAR_LEN;
    //var->name = *((var_u *)rsmpc)++;		// get the variable name
  }

  for (i = 0; i < subs; i++)			// for each subscript
  { ptr = (cstring *) addstk[asp-subs+i];	// point at the string
    if ((ptr->len == 0)	&&			// if it's a null
        ((!nul_ok) || (i != (subs-1))))		// not ok or not last subs
      return (-(ERRZ16+ERRMLAST));		// complain
    s = UTIL_Key_Build(ptr, &var->key[var->slen]); // get one subscript
    if (s < 0) return s;			// die on error
    if ((s + var->slen) > 255)			// check how big
      return (-(ERRZ2+ERRMLAST));		// complain on error
    var->slen = s + var->slen; 			// add it in
  }

  if (type == TYPVARGBLUCIENV)			// need vol?
  { ptr = (cstring *) addstk[asp-subs-1];	// point at the string
    s = getvol(ptr);				// get volume
    if (s < 0) return s;			// die on error
    var->volset = (u_char) s;			// save the value
  }
  if ((type == TYPVARGBLUCI) || (type == TYPVARGBLUCIENV)) // need UCI?
  { ptr = (cstring *) addstk[asp - subs - 1 - (type == TYPVARGBLUCIENV)]; // point at the string
    s = getuci(ptr, var->volset);		// get UCI
    if (s < 0) return s;			// die on error
    var->uci = (u_char) s;			// save the value
  }
  if (type == TYPVARIND) asp--;			// fixup asp for return
  return asp - subs - (type == TYPVARGBLUCI) - ((type == TYPVARGBLUCIENV) * 2);	// all done
}
