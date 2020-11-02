/*
 * Package:  Reference Standard M
 * File:     rsm/runtime/runtime_util.c
 * Summary:  module runtime - RunTime Utilities
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
#include <sys/utsname.h>                        // defines struct utsname
#include <limits.h>
#include <math.h>
#include "rsm.h"                                // standard includes
#include "proto.h"                              // standard includes
#include "error.h"                              // standard includes

int cstringtoi(cstring *str)                    // convert cstring to int
{ int ret = 0;                                  // return value
  int i;                                        // for loops
  int minus = FALSE;                            // sign check

  for (i=0; (i < (int)str->len)&&
            ((str->buf[i] == '-') ||
             (str->buf[i] == '+')); i++)        // check leading characters
    if (str->buf[i] == '-') minus = !minus;     // count minus signs
  for (; i < (int)str->len; i++)                // for each character
  { if ((str->buf[i] > '9')||(str->buf[i] < '0')) break; // check for digit
    if (ret > 214748363) return INT_MAX;	// check for possible overflow
    ret = (ret * 10) + ((int)str->buf[i] - 48); // Cvt to int
  }                                             // end convert loop
  if ((systab->historic & HISTORIC_EOK)
     && (i < (str->len - 1)) && (str->buf[i] == 'E'))
  { int exp = 0;				// an exponent
    int expsgn = 1;				// and the sign
    int j = 10;					// for E calc

    i++;					// point past the 'E'
    if (str->buf[i] == '-')			// if a minus
    { expsgn = -1;				// flag it
      i++;					// and increment i
    }
    else if (str->buf[i] == '+')		// if a plus
    { i++;					// just increment i
    }
    for (; i < str->len; i++)			// scan remainder
    { if ((str->buf[i] < '0') || (str->buf[i] > '9'))
      { break;					// quit when done
      }
      exp = (exp * 10) + (str->buf[i] - '0');	// add to exponent
    }
    if (exp)					// if there was an exponent
    { while (exp > 1)				// for each
      { j = j * 10;				// multiply
        exp--;					// and count it
      }
      if (expsgn > 0)				// if positive
      { ret = ret * j;				// hope it fits
      }
      else					// if negative
      { ret = ret / j;				// do this
      }
    }
  }
  if (minus) ret = -ret;                        // change sign if reqd
  return ret;                                   // return the value
}                                               // end cstringtoi()

int cstringtob(cstring *str)                    // convert cstring to boolean
{ int ret = 0;                                  // return value
  int i;                                        // for loops
  int dp = 0;					// decimal place flag
  for (i=0; (i < (int)str->len)&&
            ((str->buf[i] == '-') ||
            (str->buf[i] == '+')); i++);        // check leading characters
  for (; i < (int)str->len; i++)                // for each character
  { if (str->buf[i] == '0') continue;		// ignore zeroes
    if (str->buf[i] == '.')			// check for a dot
    { if (dp) break;				// quit if not the first
      dp = 1;					// flag it
      continue;					// go for next
    }
    if ((str->buf[i] >= '1') &&
        (str->buf[i] <= '9'))			// check for digit
      ret = 1;					// got a true value
    break;
  }                                             // end convert loop
  return ret;                                   // return the value
}                                               // end cstringtob()

short itocstring(u_char *buf, int n)		// convert int to string
{ int i = 0;					// array index
  int p = 0;					// string index
  int a[12];					// array for digits
  a[0] = 0;					// ensure first is zero
  if (n < 0)					// if negative
  { buf[p++] = '-';				// store the sign
    n = -n;					// negate the number
  }
  while (n)					// while there is a value
  { a[i++] = n % 10;				// get low decimal digit
    n = n/10;					// reduce number
  }
  while (i) buf[p++] = a[--i] + 48;		// copy digits backwards
  if (!p) buf[p++] = '0';			// ensure we have something
  buf[p] = '\0';				// null terminate
  return (short) p;				// and exit
}

short uitocstring(u_char *buf, u_int n)		// convert u_int to string
{ int i = 0;					// array index
  int p = 0;					// string index
  int a[12];					// array for digits
  a[0] = 0;					// ensure first is zero
  while (n)					// while there is a value
  { a[i++] = n % 10;				// get low decimal digit
    n = n/10;					// reduce number
  }
  while (i) buf[p++] = a[--i] + 48;		// copy digits backwards
  if (!p) buf[p++] = '0';			// ensure we have something
  buf[p] = '\0';				// null terminate
  return (short) p;				// and exit
}

// Set data into $ECODE
// Returns 0 if no previous error at this level
short Set_Error(int err, cstring *user, cstring *space)
{ short t;					// for function calls
  int j;					// a handy int
  int flag;					// to remember
  mvar *var;					// a handy mvar
  cstring *tmp;					// spare cstring ptr
  char temp[20];				// and some space

  var = &partab.src_var;			// a spare mvar
  var->slen = 0;				// no subs
  // note - the uci and volset were setup by the caller

  bcopy("$ECODE\0\0", &var->name.var_cu[0], 8);	// get the name
  t = ST_Get(var, space->buf);			// get it
  if (t < 0) t = 0;				// ignore undefined
  flag = t;					// remember if some there
  if (t < MAX_ECODE)				// if not too big
  { if ((t == 0) || (space->buf[t-1] != ','))
      space->buf[t++] = ',';			// for new $EC
    j = -err;					// copy the error (-ve)
    if (err == USRERR)				// was it a SET $EC
    { bcopy(user->buf, &space->buf[t], user->len); // copy the error
      t += user->len;				// add the length
    }
    else					// not user error
    { if (j > ERRMLAST)				// implementation error?
      { space->buf[t++] = 'Z';			// yes, Z type
        j -= ERRMLAST;				// subtract it
      }
      else
        space->buf[t++] = 'M';			// MDC error
      t += itocstring(&space->buf[t], j); 	// convert the number
    }						// end 'not user error'
    space->buf[t++] = ',';			// trailing comma
    space->buf[t] = '\0';			// null terminate
    space->len = t;
    t = ST_Set(var, space);			// set it
    tmp = (cstring *) temp;			// temp space
    tmp->len = itocstring(tmp->buf, partab.jobtab->cur_do);
    var->slen = UTIL_Key_Build(tmp, var->key);
    if (flag)					// if not first one
    { t = ST_Get(var, space->buf);		// get it
      if (t < 0) t = 0;				// ignore undefined
      flag = t;					// remember for the return
      if ((t == 0) || (space->buf[t-1] != ','))
        space->buf[t++] = ',';			// for new $EC
      j = -err;					// copy the error (-ve)
      if (err == USRERR)			// was it a SET $EC
      { bcopy(user->buf, &space->buf[t], user->len); // copy the error
        t += user->len;				// add the length
      }
      else					// not user error
      { if (j > ERRMLAST)			// implementation error?
        { space->buf[t++] = 'Z';		// yes, Z type
          j -= ERRMLAST;			// subtract it
        }
        else
        space->buf[t++] = 'M';			// MDC error
        t += itocstring(&space->buf[t], j); 	// convert the number
      }
      space->buf[t++] = ',';			// trailing comma
      space->buf[t] = '\0';			// null terminate
      space->len = t;
    }
    t = ST_Set(var, space);			// set it
  }						// end "TOO BIG" test
  return (short) flag;				// done
}

int rsm_version(u_char *ret_buffer)             // return version string
{ int i;                                        // to copy value
  int j = 0;                                    // for returned strings
  struct utsname uts;                           // struct for uname
  i = uname(&uts);                              // get system info
  if (i == -1) return (-1);                      // exit on error
  bcopy("Reference Standard M V", ret_buffer, 22); // copy in Reference Standard M V
  i = 22;					// point past it
  if (VERSION_TEST)				// if an internal version
  { i += sprintf((char *)&ret_buffer[i], "%d.%d.%d T%d for ",
		 VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH, VERSION_TEST);
  }
  else						// else normal release
  { i += sprintf((char *)&ret_buffer[i], "%d.%d.%d for ",
		 VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH);
  }
  j = 0;                                        // clear src ptr
  while ((ret_buffer[i++] = uts.sysname[j++])); // copy name
  ret_buffer[i-1] = ' ';                        // and a space over the null
  j = 0;                                        // clear src ptr
  while ((ret_buffer[i++] = uts.machine[j++])); // copy hardware
  ret_buffer[i-1] = ' ';                        // and a space over the null
  i += sprintf((char *)&ret_buffer[i], "Built %s at %s", __DATE__ , __TIME__);
  return i-1;                                   // and return count
}
