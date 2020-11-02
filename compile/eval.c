/*
 * Package:  Reference Standard M
 * File:     rsm/compile/eval.c
 * Summary:  module compile - evaluate
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
#include <limits.h>                     	// for LONG_MAX etc
#include <math.h>
#include "rsm.h"                                // standard includes
#include "proto.h"                              // standard prototypes
#include "error.h"                              // and the error defs
#include "opcodes.h"				// and the opcodes
#include "compile.h"				// compiler stuff

u_char *source_ptr;                             // pointer to source code
u_char *comp_ptr;                               // pointer to compiled code

void comperror(short err)                       // compile error
{ short s;					// for functions
  cstring *line;				// line of code
  u_char *src;					// current src ptr
  int i;					// a handy int
  u_char tmp[128];				// some space

  *comp_ptr++ = OPERROR;                        // say it's an error
  bcopy(&err, comp_ptr, 2);
  comp_ptr += 2;
  *comp_ptr++ = OPNOP;				// in case of IF etc
  *comp_ptr++ = OPNOP;				// in case of IF etc
  if (!partab.checkonly) goto scan;		// done
  if (partab.checkonly == *partab.ln) return; 	// done this one once
  partab.checkonly = *partab.ln;		// record done
  line = *partab.lp;				// get the line address
  src = *partab.sp;				// and the current source
  s = SQ_Write(line);				// write the line
  if (s < 0) goto scan;				// exit on error
  s = SQ_WriteFormat(SQ_LF);			// return
  if (s < 0) goto scan;				// exit on error
  i = (src - line->buf) - 1;			// get the offset
  if (i > 0)
  { s = SQ_WriteFormat(i);			// tab
    if (s < 0) goto scan;			// exit on error
  }
  line = (cstring *) tmp;			// some space
  line->buf[0] = '^';				// point
  line->buf[1] = ' ';				// and a space
  s = UTIL_strerror(err, &line->buf[2]);	// get the error
  if (s < 0) goto scan;				// exit on error
  line->len = s + 2;				// the length
  bcopy(" - At line ", &line->buf[line->len], 11); // front bit
  s = itocstring(&line->buf[line->len + 11], *partab.ln); // format line number
  if (s < 0) goto scan;				// exit on error
  line->len = line->len + s + 11;		// the length
  s = SQ_Write(line);				// write the line
  if (s >= 0)					// if no error error
    s = SQ_WriteFormat(SQ_LF);			// return
scan:
  while (*source_ptr) source_ptr++;		// skip rest of line
  return;					// and done
}

// function atom entered with source_ptr pointing at the source
// atom to evaluate and comp_ptr pointing at where to put the code.
//
void atom()                                     // evaluate source
{ char c;                                       // current character
  int j;                                        // and another
  short s;                                      // for function returns
  u_char *p;                                    // a pointer

  c = *source_ptr++;                            // get a character
  if (c == '@')					// indirection?
  { atom();					// eval what follows
    if (*source_ptr != '@')			// another one?
    { *comp_ptr++ = INDEVAL;			// no, eval what follows
      return;					// and just exit
    }
    *comp_ptr++ = INDMVAR;			// make an mvar out of it
    s = localvar();				// parse the rest of it
    if (s < 0)					// if we got an error
    { comperror(s);				// compile it
      return;					// and exit
    }
    return;					// and exit
  }
  if ((isalpha((int)c) != 0) || (c == '%') ||   // check for local variable
      (c == '^'))				// or a global var
  { --source_ptr;				// backup to first character
    s = localvar();				// parse the variable
    if (s < 0)					// if we got an error
    { comperror(s);				// compile it
      return;					// and exit
    }
    return;					// and exit
  }                                             // end variable parse

  if (c == '$')                                 // check for a function
  { dodollar();					// eval it
    return;                                     // and exit
  }                                             // end function parse

  if ((isdigit((int)c) != 0) || (c == '.'))     // check for number or dot
  { source_ptr--;                               // back up the source ptr
    *comp_ptr++ = OPSTR;                        // say string following
    s = ncopy(&source_ptr, comp_ptr+2);         // copy as number
    *((short *)comp_ptr) = s;                   // store string count
    comp_ptr = comp_ptr + sizeof(short) + s + 1; // allow for null byte
    return;
  }                                             // end numeric parse

  if (c == '"')                                 // rabit ear
  { *comp_ptr++ = OPSTR;                        // say string following
    p = comp_ptr;                               // possible destination
    j = 2;                                      // point at p->buf[0]
    while (TRUE)                                // scan the string
    { if (*source_ptr == '\0')                  // check for end of string
      { comp_ptr--;				// remove the OPSTR
        comperror(-(ERRZ12+ERRMLAST));          // compile an error
        return;                                 // and exit
      }                                         // end of error bit
      if ((*source_ptr == '"') &&
          (source_ptr[1] != '"'))               // check end of literal
      { p[j] = '\0';                            // null terminate it
        source_ptr++;                           // point past it
        break;                                  // and exit
      }                                         // end 'end of str' code
      p[j++] = *source_ptr++;                   // copy the character
      if ((*(source_ptr-1) == '"') &&
          (*source_ptr == '"'))                 // check for rabit ear
        source_ptr++;                           // point past the second one
    }                                           // end of copy loop
    *((short *)p) = (short) j-2;                // store cstring count
    comp_ptr = comp_ptr + j + 1;                // point past str and null
    return;
  }                                             // end string literal

  if (c == '\'')                                // check for single quote
  { atom();                                     // get the following
    *comp_ptr++ = OPNOT;                        // do the NOT
    return;
  }                                             // end NOT parsing

  if (c == '+')                                 // check for plus
  { atom();                                     // get the following
    *comp_ptr++ = OPPLUS;                       // do the plus
    return;
  }                                             // end NOT parsing

  if (c == '-')                                 // check for unary minus
  { atom();                                     // get the following
    *comp_ptr++ = OPMINUS;                      // do the minus
    return;
  }                                             // end NOT parsing

  if (c == '(')                                 // open bracket
  { eval();                                     // eval content of ()
    if (*source_ptr++ != ')')                   // error if no trailing ) found
    { comperror(-(ERRZ12+ERRMLAST));            // compile an error
      return;                                   // and exit
    }                                           // end error
    return;
  }                                             // end open bracket parse

  comperror(-(ERRZ12+ERRMLAST));                // compile an error
  return;                                       // and exit
}

int operator()                                  // extract an operator
{ char c;                                       // the character
  int not = 0;                                  // not flag
  c = *source_ptr++;                            // get next char
  if (c == '\'')				// a NOT?
  { if (not) return 0;				// can't have two
    not = 1;					// set the not
    c = *source_ptr++;                          // get next char
  }
  switch(c)
  { case '+':                                   // add
      if (not) return 0;			// a not here is junk
      return OPADD;                             // save opcode
    case '-':                                   // subtract
      if (not) return 0;			// a not here is junk
      return OPSUB;                             // save opcode
    case '*':                                   // multiply (or power)
      if (not) return 0;			// a not here is junk
      if (*source_ptr == '*')                   // if there is another
      { source_ptr++;                           // advance the pointer
        return OPPOW;                           // it's a power
      }
      return OPMUL;                             // set as a multiply
    case '/':                                   // divide
      if (not) return 0;			// a not here is junk
      return OPDIV;                             // set the op code
    case '\\':                                  // back-slash
      if (not) return 0;			// a not here is junk
      return OPINT;                             // integer divide
    case '#':                                   // hash
      if (not) return 0;			// a not here is junk
      return OPMOD;                             // modulus
    case '_':                                   // underscore
      if (not) return 0;			// a not here is junk
      return OPCAT;                             // concatenate
    case '=':                                   // equal sign
      return not ? OPNEQL : OPEQL;              // equal or not
    case '<':                                   // less than
      return not ? OPNLES : OPLES;              // less than or not
    case '>':                                   // greater than
      return not ? OPNGTR : OPGTR;              // greater than or not
    case '&':                                   // and
      return not ? OPNAND : OPAND;              // and or nand
    case '!':                                   // exclam
      return not ? OPNIOR : OPIOR;              // or or nor
    case '[':                                   // left square bracket
      return not ? OPNCON : OPCON;              // contains or not
    case ']':                                   // right square bracket
      if (*source_ptr == ']')                   // if there is another
      { source_ptr++;                           // advance the pointer
        return not ? OPNSAF : OPSAF;            // sorts after or not
      }
      return not ? OPNFOL : OPFOL;              // follows or not
    case '?':                                   // question
      return not ? OPNPAT : OPPAT;              // matches or not
    default:                                    // stuffed up
      return 0;                                 // clear op
  }                                             // end of switch for operators
}

// function eval entered with source_ptr pointing at the source
// expression to evaluate and comp_ptr pointing at where to put the code.
//
void eval()                                     // evaluate source
{ int op;                                       // operator
  int q;					// in quotes indicator
  int patmat = 0;				// for pattern match funny
  cstring *ptr;					// spare pointer
  u_char c;
  atom();                                       // get first operand
  if ((*source_ptr == ')') ||			// do it at a higher level
      (*source_ptr == ',') ||			// ditto
      (*source_ptr == ':') ||			// ditto
      (*source_ptr == '\0') ||			// end of string
      (*source_ptr == '^') ||			// start of routine ref
      (*source_ptr == '@') ||			// end of name indirection
      (*source_ptr == ' '))                     // end of command
    return;                                     // exit

  while (TRUE)                                  // until the end
  { op = operator();                            // get the operator
    if (op == 0)                                // an error??
    { comperror(-(ERRZ12+ERRMLAST));            // compile the error
      return;                                   // and exit
    }
    patmat = ((op == OPPAT) || (op == OPNPAT));	// bloody pattern match
    if (patmat && (*source_ptr == '@'))		// indirect pattern
    { source_ptr++;				// skip the @
      patmat = 0;				// clear funny pattern match
    }
    if (patmat)					// normal (not @) pattern match
    { q = 0;					// not in quotes
      *comp_ptr++ = OPSTR;			// pretend it's a string
      ptr = (cstring *) comp_ptr;		// remember for ron
      comp_ptr = comp_ptr + sizeof(short);	// skip the count
      while (TRUE)				// loop
      { c = *source_ptr++;			// get next char
	if (q)					// if in quotes
	{ *comp_ptr++ = c;			// copy char
	  if (c == '"') q = 0;			// check for a quote
	  continue;				// go for more
	}
	if (c == '"')				// if it's a quote
	{ q = 1;				// turn on
	  *comp_ptr++ = c;			// copy char
	  continue;				// go for more
	}
	if ((isalnum(c) != 0) ||		// alpha numeric
	    (c == '.'))				// or a dot
	{ *comp_ptr++ = c;			// copy char
	  continue;				// go for more
	}
	if (c == '(')				// open bracket
	{ patmat++;				// count it
	  *comp_ptr++ = c;			// copy char
	  continue;				// go for more
	}
	if ((c == ')') && (patmat > 1))		// close bracket
	{ --patmat;				// count it
	  *comp_ptr++ = c;			// copy char
	  continue;				// go for more
	}
	if ((patmat > 1) && (c == ','))		// comma inside ()
	{ *comp_ptr++ = c;			// copy char
	  continue;				// go for more
	}
	source_ptr--;				// backup
	break;					// and exit
      }						// end while
      ptr->len = comp_ptr - ptr->buf;		// get the length
      *comp_ptr++ = '\0';			// null terminate its
    }
    else atom();                                // else get next operand
    *comp_ptr++ = (u_char) op;                  // store the operator
    if ((*source_ptr == ')') ||                 // do it at a higher level
        (*source_ptr == ',') ||                 // ditto
        (*source_ptr == ':') ||                 // ditto
        (*source_ptr == '\0') ||                // end of string
        (*source_ptr == '^') ||			// start of routine ref
        (*source_ptr == ' '))                   // end of command
      return;                                   // exit
  }
}
