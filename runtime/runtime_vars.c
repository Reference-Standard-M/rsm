/*
 * Package:  Reference Standard M
 * File:     rsm/runtime/runtime_vars.c
 * Summary:  module runtime - RunTime Variables
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
#include "error.h"                              // standard errors

// All variables use the following structure
//
// short Vname(u_char *ret_buffer)
//
// The argument is of type *u_char and is the destination
// for the value returned by the function (max size is 32767).
// The function returns a count of characters in the return string
// or a negative error number (to be defined).
// The function name is Vvarname where the variable call is $varname.
//

//***********************************************************************
// $ECODE
//
short Vecode(u_char *ret_buffer)                // $ECODE
{ mvar *var;					// for ST_Get
  short s;
  var = (mvar *) ret_buffer;			// use here for the mvar
  VAR_CLEAR(var->name);
  bcopy("$ECODE", &var->name.var_cu[0], 6);	// get the name
  var->volset = 0;				// clear volset
  var->uci = UCI_IS_LOCALVAR;			// local var
  var->slen = 0;				// no subscripts
  s =  ST_Get(var, ret_buffer);			// get it
  if (s == -ERRM6)
  { s = 0;					// ignore undef
    ret_buffer[0] = '\0';			// null terminate
  }
  return s;
}

//***********************************************************************
// $ETRAP
//
short Vetrap(u_char *ret_buffer)                // $ETRAP
{ mvar *var;					// for ST_Get
  short s;
  var = (mvar *) ret_buffer;			// use here for the mvar
  VAR_CLEAR(var->name);
  bcopy("$ETRAP", &var->name.var_cu[0], 6);	// get the name
  var->volset = 0;				// clear volset
  var->uci = UCI_IS_LOCALVAR;			// local var
  var->slen = 0;				// no subscripts
  s = ST_Get(var, ret_buffer);			// exit with result
  if (s == -ERRM6) s = 0;			// ignore undef
  return s;
}

//***********************************************************************
// $HOROLOG
//
short Vhorolog(u_char *ret_buffer)              // $HOROLOG
{ time_t sec = current_time(TRUE);              // get secs from 1 Jan 1970 with local offset
  int day = sec / SECDAY + YRADJ;               // get number of days
  sec = sec % SECDAY;                           // and number of seconds
  return sprintf((char *) ret_buffer, "%d,%d", day, (int) sec); // return count and $H
}

//***********************************************************************
// $KEY
//
short Vkey(u_char *ret_buffer)                  // $KEY
{ SQ_Chan *ioptr;				// ptr to current $IO
  ioptr = &partab.jobtab->seqio[(int) partab.jobtab->io]; // point at it
  return mcopy(&ioptr->dkey[0],			// copy from here
                ret_buffer,                     // to here
		ioptr->dkey_len);		// this many bytes
}

//***********************************************************************
// $REFERENCE
//
short Vreference(u_char *ret_buffer)            // $REFERENCE
{ mvar *var;					// variable pointer
  var = &partab.jobtab->last_ref;		// point at $R
  ret_buffer[0] = '\0';				// null JIC
  if (var->name.var_cu[0] == '\0') return 0;	// return null string if null
  return UTIL_String_Mvar(var, ret_buffer, 32767); // do it elsewhere
}

//***********************************************************************
// $SYSTEM
//
short Vsystem(u_char *ret_buffer)               // $SYSTEM
{ int i;                                        // to copy value
  i = itocstring(ret_buffer, RSM_SYSTEM); 	// copy assigned #
  ret_buffer[i++] = ',';                        // and a comma
  i = i + rsm_version(&ret_buffer[i]);          // do it elsewhere
  return i;                                     // return the count
}

//***********************************************************************
// $X
//
short Vx(u_char *ret_buffer)                    // $X
{ SQ_Chan *ioptr;				// ptr to current $IO
  ioptr = &partab.jobtab->seqio[(int) partab.jobtab->io]; // point at it
  return itocstring(ret_buffer,
  		  ioptr->dx);			// return len with data in buf
}

//***********************************************************************
// $Y
//
short Vy(u_char *ret_buffer)                    // $Y
{ SQ_Chan *ioptr;				// ptr to current $IO
  ioptr = &partab.jobtab->seqio[(int) partab.jobtab->io]; // point at it
  return itocstring(ret_buffer,
  		  ioptr->dy);			// return len with data in buf
}

//***********************************************************************
// Set special variables - those that may be set are:
//	$EC[ODE]
//	$ET[RAP]
//	$K[EY]
//	$R[EFERENCE]
//	$X
//	$Y

short Vset(mvar *var, cstring *cptr)		// set a special variable
{ int i;
  if (var->slen != 0) return -ERRM8;		// no subscripts permitted
  if ((strncasecmp((char *)&var->name.var_cu[1], "ec", 2) == 0) ||
      (strncasecmp((char *)&var->name.var_cu[1], "ecode", 5) == 0)) // $EC[ODE]
  { if ((cptr->len > 1) && (cptr->buf[0] == ',')    // If it starts with a comma
        && (cptr->buf[cptr->len - 1] == ','))        // and ends with a comma
    {  cptr->len--;
       bcopy(&cptr->buf[1], &cptr->buf[0], cptr->len--);  // Ignore the commas
       cptr->buf[cptr->len] = '\0';                     // and nul terminate
    }
    if ((cptr->len == 0) ||			// set to null ok
	(cptr->buf[0] == 'U'))			// or Uanything
    { VAR_CLEAR(var->name);
      bcopy("$ECODE", &var->name.var_cu[0], 6); // ensure name correct
      partab.jobtab->error_frame = 0;		// and where the error happened
      partab.jobtab->etrap_at = 0;		// not required
      if (cptr->len == 0)			// if we are clearing it
        return ST_Kill(var);			// kill it
      return USRERR;				// do it elsewhere
    }
    return -ERRM101;				// can't do that
  }
  if ((strncasecmp((char *)&var->name.var_cu[1], "et", 2) == 0) ||
      (strncasecmp((char *)&var->name.var_cu[1], "etrap", 5) == 0)) // $ET[RAP]
  { VAR_CLEAR(var->name);
    bcopy("$ETRAP", &var->name.var_cu[0], 6);	// ensure name correct
    if (cptr->len == 0) return ST_Kill(var);	// kill it
    return ST_Set(var, cptr);			// do it in symbol
  }
  if ((strncasecmp((char *)&var->name.var_cu[1], "k", 1) == 0) ||
      (strncasecmp((char *)&var->name.var_cu[1], "key", 3) == 0)) // $K[EY]
  { if (cptr->len > MAX_DKEY_LEN) return -ERRM75; // too big
    bcopy(cptr->buf,				// copy this
	  partab.jobtab->seqio[partab.jobtab->io].dkey, // to here
	  cptr->len+1);				// this many (incl null)
    partab.jobtab->seqio[partab.jobtab->io].dkey_len = cptr->len;
    return 0;
  }
  if ((strncasecmp( (char *) &var->name.var_cu[1], "r", 1) == 0) ||
      (strncasecmp( (char *) &var->name.var_cu[1], "reference", 9) == 0)) // $R[EFERENCE]
  { for (i = 0; i < cptr->len; i++)
    { if (i == 0)
      { if (cptr->buf[0] != '^')
          return -(ERRZ74 + ERRMLAST);		// invalid global value
      }
      else if (i == 1)
      { if ((cptr->buf[1] != '%') && (!isalpha(cptr->buf[1])))
          return -(ERRZ74 + ERRMLAST);		// invalid global value
      }
      else if (!isalnum(cptr->buf[i]) && cptr->buf[i] != '(' && cptr->buf[i] != ')' && cptr->buf[i] != ',')
        return -(ERRZ74 + ERRMLAST);		// invalid global value
    }
    UTIL_MvarFromCStr(cptr, &partab.jobtab->last_ref);
    return 0;
  }
  if (strncasecmp((char *)&var->name.var_cu[1], "x", 1) == 0)	// $X
  { i = cstringtoi(cptr);			// get val
    if (i < 0) i = 0;
    partab.jobtab->seqio[partab.jobtab->io].dx = (u_short) i;
    return 0;					// and return
  }
  if (strncasecmp((char *)&var->name.var_cu[1], "y", 1) == 0)	// $Y
  { i = cstringtoi(cptr);			// get val
    if (i < 0) i = 0;
    partab.jobtab->seqio[partab.jobtab->io].dy = (u_short) i;
    return 0;					// and return
  }
  return -ERRM8;				// else junk
}
