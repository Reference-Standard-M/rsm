/*
 * Package:  Reference Standard M
 * File:     rsm/runtime/math.c
 * Summary:  module runtime - decimal arithmetic
 *
 * David Wicksell <dlw@linux.com>
 * Copyright © 2020-2023 Fourth Watch Software LC
 * https://gitlab.com/Reference-Standard-M/rsm
 *
 * Based on MUMPS V1 by Raymond Douglas Newman
 * Copyright (c) 1999-2018
 * https://gitlab.com/Reference-Standard-M/mumpsv1
 *
 * Originally based on FreeMUMPS
 * Copyright (c) 1998
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
#include <limits.h>
#include <string.h>
#include <ctype.h>
#include "opcode.h"                                                             // the op codes
#include "rsm.h"                                                                // standard includes
#include "proto.h"                                                              // standard prototypes
#include "error.h"                                                              // standard errors

#define NUMBASE   10
#define PLUS      '+'
#define MINUS     '-'
#define POINT     '.'
#define point     (POINT - ZERO)
#define ZERO      '0'
#define ONE       (ZERO + 1)
#define TWO       (ZERO + 2)
#define THREE     (ZERO + 3)
#define FIVE      (ZERO + (NUMBASE / 2))
#define NINE      (ZERO + NUMBASE - 1)
#define EOL       ((char) '\000')
#define toggle(A) (A ^= 01)

// Add b to a
short runtime_add(char *a, char *b)
{
    short dpa;                                                                  // decimal point of 'a'
    short dpb;                                                                  // decimal point of 'b'
    short lena;                                                                 // length of 'a'
    short lenb;                                                                 // length of 'b'
    int   mi;                                                                   // minus flag
    short sign;                                                                 // sign flag:
                                                                                // if a < 0 < b sign = -1;
                                                                                // if a > 0 > b sign = 1;
                                                                                // else sign = 0;
    int   i;
    int   ch;
    int   j;
    int   carry;

    if (b[0] == ZERO) return (short) strlen(a);

    if (a[0] == ZERO) {
        strcpy(a, b);
        return (short) strlen(a);
    }

    // look at the signs
    mi = 0;
    sign = 0;

    if ((a[0] == b[0]) && (a[0] == MINUS)) {
        mi++;
        a[0] = b[0] = ZERO;
    } else if (a[0] == MINUS) {
        sign--;
        a[0] = NINE;
        i = 0;

        while ((ch = a[++i]) != EOL) {
            if (ch != POINT) a[i] = ZERO + NINE - ch;
        }

        a[--i]++;
    } else if (b[0] == MINUS) {
        sign++;
        b[0] = NINE;
        i = 0;

        while ((ch = b[++i]) != EOL) {
            if (ch != POINT) b[i] = ZERO + NINE - ch;
        }

        b[--i]++;
    }

    // search decimal points and length
    dpa = dpb = -1;
    i = 0;

    while (a[i] != EOL) {
        if (a[i] == POINT) dpa = i;
        i++;
    }

    lena = i;
    if (dpa < 0) dpa = i;

again:
    i = 0;

    while (b[i] != EOL) {
        if (b[i] == POINT) dpb = i;
        i++;
    }

    lenb = i;
    if (dpb < 0) dpb = i;

    if (i == 1) {
        if ((b[0] == ONE) && (sign == 0) && (dpa > 0)) {
            // frequent special case: add 1
            i = dpa - 1;

            while (++a[i] > NINE) {
                a[i--] = ZERO;

                if (i < 0) {
                    i = lena;

                    while (i >= 0) {
                        a[i + 1] = a[i];
                        i--;
                    }

                    a[0] = ONE;
                    return (short) strlen(a);
                }
            }

            return (short) strlen(a);
        }
    }

    // copy additional trailing digits from b to a
    if ((lenb - dpb) > (lena - dpa)) {
        j = dpa - dpb;

        if ((lenb + j) > MAX_NUM_BYTES) {                                       // round off that monster!
            i = MAX_NUM_BYTES - j;

            if (b[i] < FIVE) {
                b[i] = EOL;
                lenb--;

                while (b[--i] == ZERO) {
                    b[i] = EOL;
                    lenb--;
                }
            } else {
                for (;;) {
                    if (i >= dpb) {
                        b[i] = EOL;
                        lenb--;
                    } else {
                        b[i] = ZERO;
                    }

                    if (--i < 0) {
                        for (i = lenb; i >= 0; i--) b[i + 1] = b[i];
                        b[0] = ONE;
                        dpb = ++lenb;
                        break;
                    }

                    if ((ch = b[i]) == POINT) {
                        dpb = i;
                        continue;
                    }

                    if ((ch < NINE) && (ch >= ZERO)) {
                        b[i] = ++ch;
                        break;
                    }
                }
            }

            goto again;                                                         // look what's left from b
        }

        lenb = i = lena - dpa + dpb;
        j = lena;
        while ((a[j++] = b[i++]) != EOL) continue;
        lena = --j;
        b[lenb] = EOL;
    }

    // $justify a or b
    i = dpa - dpb;

    if (i < 0) {
        j = lena;
        if ((i = (lena -= i)) > (MAX_NUM_BYTES - 2)) return -ERRM92;
        ch = ((sign >= 0) ? ZERO : NINE);
        while (j >= 0) a[i--] = a[j--];
        while (i >= 0) a[i--] = ch;
        dpa = dpb;
    } else if (i > 0) {
        j = lenb;
        if ((lenb = (i += lenb)) > (MAX_NUM_BYTES - 2)) return -ERRM92;
        ch = ((sign <= 0) ? ZERO : NINE);
        while (j >= 0) b[i--] = b[j--];
        while (i >= 0) b[i--] = ch;
        dpb = dpa;
    }

    // now add
    carry = 0;

    for (i = lenb - 1; i >= 0; i--) {
        if ((ch = a[i]) == POINT) continue;
        ch += b[i] - ZERO + carry;
        if ((carry = (ch > NINE))) ch -= NUMBASE;
        a[i] = ch;
    }

    while (a[lena] != EOL) lena++;

    if (carry) {
        if ((i = ++lena) > (MAX_NUM_BYTES - 2)) return -ERRM92;

        while (i > 0) {
            a[i] = a[i - 1];
            i--;
        }

        a[0] = ONE;
    }

    if (sign) {
        if (a[0] == ONE) {
            a[0] = ZERO;
        } else {
            i = 0;
            carry = 0;

            while ((ch = a[++i]) != EOL) {
                if (ch != POINT) a[i] = ZERO + NINE - ch;
            }

            while (--i > 0) {
                if (a[i] != POINT) {
                    if (++a[i] <= NINE) break;
                    a[i] = ZERO;
                }
            }

            mi = 1;
            a[0] = ZERO;
        }

        while (a[mi] == ZERO) {
            memmove(&a[mi], &a[mi + 1], strlen(&a[mi]));
            dpa--;
        }

        if (dpa < 0) dpa = 0;
    }

    // remove trailing zeroes
    i = dpa;
    while (a[i] != EOL) i++;

    if (--i > dpa) {
        while (a[i] == ZERO) a[i--] = EOL;
    }

    // remove trailing point
    if (a[i] == POINT) a[i] = EOL;

    if (mi) {
        if (a[0] != ZERO) {
            i = 0;
            while (a[i++] != EOL) continue;

            while (i > 0) {
                a[i] = a[i - 1];
                i--;
            }
        }

        a[0] = MINUS;
    }

    if (a[mi] == EOL) {
        a[0] = ZERO;
        a[1] = EOL;
    }

    return (short) strlen(a);
}

// Multiply a by b
short runtime_mul(char *a, char *b)
{
    char  c[(MAX_NUM_BYTES + 1) * 2];
    short alen;
    short blen;
    short clen;
    short mi;
    short tmpx;
    int   acur;
    int   bcur;
    int   ccur;
    int   carry;

    // if zero or one there's not much to do
    if (b[1] == EOL) {
        if (b[0] == ZERO) {
            a[0] = ZERO;
            a[1] = EOL;
            return 1;
        }

        if (b[0] == ONE) return (short) strlen(a);

        if (b[0] == TWO) {

multwo:
            acur = 0;
            while (a[++acur] != EOL) continue;
            mi = (a[acur - 1] == FIVE);
            carry = 0;
            ccur = acur;

            // while (acur >= 0) {
            while (acur > 0) {
                if ((bcur = a[--acur]) < ZERO) continue;
                bcur = bcur * 2 - ZERO + carry;
                carry = 0;

                if (bcur > NINE) {
                    carry = 1;
                    bcur -= NUMBASE;
                }

                a[acur] = bcur;
            }

            if (carry) {
                acur = ccur;
                if (acur > (MAX_NUM_BYTES - 1)) return -ERRM92;

                while (acur >= 0) {
                    a[acur + 1] = a[acur];
                    acur--;
                }

                a[a[0] == MINUS] = ONE;
            }

            if (mi) {
                if (carry) ccur++;
                acur = 0;

                while (acur < ccur) {
                    if (a[acur++] == POINT) {
                        a[--ccur] = EOL;

                        if (acur == ccur) {
                            a[--ccur] = EOL;
                            return (short) strlen(a);
                        }
                    }
                }
            }

            return (short) strlen(a);
        }
    }

    if (a[1] == EOL) {
        if (a[0] == ZERO) return (short) strlen(a);

        if (a[0] == ONE) {
            strcpy(a, b);
            return (short) strlen(a);
        }

        if (a[0] == TWO) {
            strcpy(a, b);
            goto multwo;
        }
    }

    // get length of strings and convert ASCII to decimal - have a look at the signs
    if ((mi = (a[0] == MINUS))) a[0] = ZERO;

    if (b[0] == MINUS) {
        b[0] = ZERO;
        toggle(mi);
    }

    carry = 0;
    alen = 0;

    while (a[alen] != EOL) {
        a[alen] -= ZERO;
        if (a[alen++] == point) carry = alen;
    }

    // append a point on the right side if there was none
    if (--carry < 0) {
        carry = alen;
        a[alen++] = point;
        a[alen] = 0;
    }

    ccur = 0;
    blen = 0;

    while (b[blen] != EOL) {
        b[blen] -= ZERO;
        if (b[blen++] == point) ccur = blen;
    }

    if (--ccur < 0) {
        ccur = blen;
        b[blen++] = point;
        b[blen] = 0;
    }

    carry += ccur;

    if (carry > (MAX_NUM_BYTES - 3)) {
        a[0] = EOL;
        return -ERRM92;
    }

    ccur = clen = alen + blen;

    // init c to zero
    while (ccur >= 0) c[ccur--] = 0;
    c[carry] = point;
    bcur = blen;
    clen = alen + blen - 1;
    carry = 0;

    while (bcur > 0) {
        if (b[--bcur] == point) continue;
        if (c[clen] == point) clen--;
        acur = alen;
        ccur = clen--;

        while (acur > 0) {
            if (a[--acur] == point) continue;
            if (c[--ccur] == point) --ccur;
            tmpx = a[acur] * b[bcur] + c[ccur] + carry;
            carry = tmpx / NUMBASE;
            c[ccur] = tmpx % NUMBASE;
        }

        while (carry) {
            if (c[--ccur] == point) ccur--;

            if ((c[ccur] += carry) >= NUMBASE) {
                c[ccur] -= NUMBASE;
                carry = 1;
            } else
                carry = 0;
        }
    }

    // copy result to a and convert it
    a[ccur = clen = acur = (alen += blen)] = EOL;

    while (--ccur >= 0) {
        if (c[ccur] < NUMBASE) {
            a[ccur] = c[ccur] + ZERO;
        } else {
            a[alen = ccur] = POINT;
        }
    }

    // oversize string
    if (acur > MAX_NUM_BYTES) {
        if (a[alen = acur = MAX_NUM_BYTES] >= FIVE) {
            int l1 = MAX_NUM_BYTES;

            if (a[l1] >= FIVE) {
                for (;;) {
                    if (a[--l1] == POINT) l1--;

                    if (l1 < (int) (a[0] == MINUS)) {
                        for (l1 = MAX_NUM_BYTES; l1 > 0; l1--) a[l1] = a[l1 - 1];
                        a[a[0] == MINUS] = ONE;
                        break;
                    }

                    if (++a[l1] == (NINE + 1)) {
                        a[l1] = ZERO;
                    } else {
                        break;
                    }
                }
            }
        }

        a[acur] = EOL;
    }

    // remove trailing zeroes
    if (acur >= alen) {
        while (a[--acur] == ZERO) a[acur] = EOL;
    }

    // remove trailing point
    if (a[acur] == POINT) a[acur] = EOL;

    // remove leading zeroes
    while (a[mi] == ZERO) {
        acur = mi;
        while ((a[acur] = a[acur + 1]) != EOL) acur++;
    }

    if (a[0] == EOL) {
        a[0] = ZERO;
        a[1] = EOL;
        mi = 0;
    }

    if (mi) {
        if (a[0] != ZERO) {
            acur = clen;

            while (acur > 0) {
                a[acur] = a[acur - 1];
                acur--;
            }
        }

        a[0] = MINUS;
    }

    return (short) strlen(a);
}

/*
 * Divide string arithmetic
 *
 * Arguments:
 *     Dividend and Result
 *     Divisor
 *     Type: OPDIV - '/' or OPINT - '\' or OPMOD - '#'
 */
short runtime_div(char *uu, char *v, short typ)
{
    char  q[MAX_NUM_BYTES + 2];                                                 // quotient
    char  u[(MAX_NUM_BYTES + 1) * 2];                                           // intermediate result
    char  vv[MAX_NUM_BYTES + 1];
    short d;
    short d1;
    short k1;
    short m;
    short ulen;
    short vlen;
    short dpu;
    short dpv;
    short guess;
    short mi;
    short plus;
    short v1;
    int   i;
    int   j;
    int   k;
    int   carry = 0;

    if ((v[0] == ZERO) && !v[1]) return -ERRM9;
    if (uu[0] == ZERO) return 1;

    // look at the signs
    strcpy(u, uu);
    mi = 0;
    plus = 0;

    if (typ != OPMOD) {
        if (u[0] == MINUS) {
            u[0] = ZERO;
            mi = 1;
        }

        if (v[0] == MINUS) {
            v[0] = ZERO;
            toggle(mi);
        }
    } else {
        strcpy(vv, v);

        if (u[0] == MINUS) {
            u[0] = ZERO;
            plus = 1;
        }

        if (v[0] == MINUS) {
            v[0] = ZERO;
            mi = 1;
            toggle(plus);
        }
    }

    // convert from ASCII to 'number'
    i = 0;
    dpv = -1;
    k = 0;

    while ((j = v[i]) != EOL) {
        j -= ZERO;
        if (j == point) dpv = i;
        if (j == 0) k++;
        v[i++] = j;
    }

    v[vlen = i] = 0;
    v[i + 1] = 0;
    v[i + 2] = 0;

    if (v[0] != 0) {
        while (i >= 0) {
            v[i + 1] = v[i];
            i--;
        }

        v[0] = 0;
        dpv++;
    } else {
        vlen--;
    }

    d1 = 0;
    i = 0;
    dpu = -1;

    while (u[i] != EOL) {
        u[i] -= ZERO;
        if (u[i] == point) dpu = i;
        i++;
    }

    if (dpu < 0) u[dpu = i++] = point;
    // u[ulen = i] = 0; u[i + 1] = 0; u[i + 2] = 0;
    ulen = i;

    while (i < (MAX_NUM_BYTES * 2)) u[i++] = 0;
    i = ulen;                                                                   // seems to be necessary, check in to it

    if (u[0] != 0) {
        while (i >= 0) {
            u[i + 1] = u[i];
            i--;
        }

        u[0] = 0;
        dpu++;
    } else {
        ulen--;
    }

    if (((vlen + partab.jobtab->precision) > MAX_NUM_BYTES) && ((dpv + partab.jobtab->precision) < vlen)) {
        vlen -= partab.jobtab->precision;
    }

    if (dpv > 0) {                                                              // make v an integer - shift v
        d1 = vlen - dpv;
        for (i = dpv; i < vlen; i++) v[i] = v[i + 1];
        vlen--;

        // remove leading zeroes
        while (v[1] == 0) {
            for (i = 1; i <= vlen; i++) v[i] = v[i + 1];
            vlen--;
        }

        v[vlen + 1] = 0;
        v[vlen + 2] = 0;

        // shift u
        i = dpu;

        for (j = 0; j < d1; j++) {
            if (i >= ulen) {
                u[i + 1] = 0;
                ulen++;
            }

            u[i] = u[i + 1];
            i++;
        }

        u[i] = point;
        dpu = i;
    }

    d = dpu + 1 - ulen;
    if (dpv > dpu) d += dpv - dpu;
    if (typ == OPDIV) d += partab.jobtab->precision;
    if ((d + ulen) > MAX_NUM_BYTES) return -ERRM92;

    while (d > 0) {
        u[++ulen] = 0;
        d--;
    }

    // normalize
    if ((d = NUMBASE / (v[1] + 1)) > 1) {
        i = ulen;
        carry = 0;

        while (i > 0) {
            if (u[i] != point) {
                carry += u[i] * d;
                u[i] = carry % NUMBASE;
                carry = carry / NUMBASE;
            }

            i--;
        }

        u[0] = carry;
        i = vlen;
        carry = 0;

        while (i > 0) {
            carry += v[i] * d;
            v[i] = carry % NUMBASE;
            carry = carry / NUMBASE;
            i--;
        }

        v[0] = carry;
    }

    // initialize
    j = 0;
    m = ulen - vlen + 1;
    if (m <= dpu) m = dpu + 1;
    for (i = 0; i <= m; q[i++] = ZERO) continue;
    if (typ == OPMOD) m = dpu - vlen;
    v1 = v[1];

    while (j < m) {
        if (u[j] != point) {                                                    // calculate guess
            if ((k = (u[j] * NUMBASE + ((u[j + 1] == point) ? u[j + 2] : u[j + 1]))) == 0) {
                j++;
                continue;
            }

            k1 = (((u[j + 1] == point) || (u[j + 2] == point)) ? u[j + 3] : u[j + 2]);
            guess = ((u[j] == v1) ? (NUMBASE - 1) : (k / v1));

            if ((v[2] * guess) > ((k - guess * v1) * NUMBASE + k1)) {
                guess--;
                if ((v[2] * guess) > ((k - guess * v1) * NUMBASE + k1)) guess--;
            }

            // multiply and subtract
            i = vlen;
            carry = 0;
            k = j + i;
            if ((j < dpu) && (k >= dpu)) k++;

            while (k >= 0) {
                if (u[k] == point) k--;

                if (i >= 0) {
                    u[k] -= v[i--] * guess + carry;
                } else {
                    if (carry == 0) break;
                    u[k] -= carry;
                }

                carry = 0;

                while (u[k] < 0) {
                    u[k] += NUMBASE;
                    carry++;
                }

                k--;
            }

            // test remainder and add back
            if (carry) {
                guess--;
                i = vlen;
                carry = 0;
                k = j + i;
                if ((j < dpu) && (k >= dpu)) k++;

                while (k >= 0) {
                    if (u[k] == point) k--;

                    if (i >= 0) {
                        u[k] += v[i--] + carry;
                    } else {
                        if (carry == 0) break;
                        u[k] += carry;
                    }

                    carry = u[k] / NUMBASE;
                    u[k] = u[k] % NUMBASE;
                    k--;
                }
            }

            q[j++] = guess + ZERO;
            u[0] = 0;
        } else {
            q[j++] = POINT;
        }
    }

    // unnormalize
    if (typ != OPMOD) {
        i = 0;

        while (i <= m) {
            if ((u[i] = q[i]) == POINT) dpv = i;
            i++;
        }

        k = vlen;
        k1 = dpv;

        while (k-- > 0) {
            while (k1 <= 0) {
                for (i = ++m; i > 0; i--) u[i] = u[i - 1];
                k1++;
                u[0] = ZERO;
            }

            u[k1] = u[k1 - 1];
            u[--k1] = POINT;
            dpv = k1;
        }

        u[m] = EOL;

        // rounding
        if (typ != OPDIV) {
            u[dpv + 1] = EOL;
        } else {
            k = dpv + partab.jobtab->precision;
            k1 = u[k + 1] >= FIVE;
            u[k + 1] = EOL;

            if (k1) {
                do {
                    if (u[k] != POINT) {
                        if ((carry = (u[k] == NINE))) {
                            u[k] = ZERO;
                        } else {
                            u[k]++;
                        }
                    }

                    k--;
                } while (carry);
            }
        }
    } else {
        // return the remainder
        carry = 0;

        if (d > 1) {
            for (i = 0; i <= ulen; i++) {
                if (u[i] == point) {
                    u[i] = POINT;
                    dpu = i;
                } else {
                    u[i] = (j = carry + u[i]) / d + ZERO;
                    carry = j % d * NUMBASE;
                }
            }
        } else {
            for (i = 0; i <= ulen; i++) {
                if (u[i] == point) {
                    u[dpu = i] = POINT;
                } else {
                    u[i] += ZERO;
                }
            }
        }

        u[i] = EOL;

        if (d1 > 0) {
            u[i + 1] = EOL;
            u[i + 2] = EOL;
            i = dpu;

            for (j = 0; j < d1; j++) {
                u[i] = u[i - 1];
                i--;
            }

            u[i] = POINT;
        }
    }

    // remove trailing zeroes
    i = 0;
    k = -1;

    while (u[i] != EOL) {
        if (u[i] == POINT) k = i;
        i++;
    }

    i--;

    if (k >= 0) {
        while ((u[i] == ZERO) && (i > k)) u[i--] = EOL;
    }

    // remove trailing point
    if (u[i] == POINT) u[i] = EOL;

    // remove leading zeroes
    while (u[0] == ZERO) {
        i = 0;
        while ((u[i] = u[i + 1]) != EOL) i++;
    }

    if (u[0] == EOL) {
        u[0] = ZERO;
        u[1] = EOL;
        mi = 0;
    }

    if ((mi || plus) && (u[0] != ZERO)) {
        if (mi != plus) {
            i = (int) strlen(u) + 1;

            do {
                u[i] = u[i - 1];
                i--;
            } while (i > 0);

            u[0] = MINUS;
        }

        if (plus) {
            short s = runtime_add(u, vv);

            if (s < 0) return s;
        }
    }

    strcpy(uu, u);
    return (short) strlen(uu);
}                                                                               // end runtime_div()

// square root
int g_sqrt(char *a)
{
    char  tmp1[MAX_NUM_BYTES + 2];
    char  tmp2[MAX_NUM_BYTES + 2];
    char  XX[MAX_NUM_BYTES + 2];
    char  XXX[MAX_NUM_BYTES + 2];

    if (a[0] == ZERO) return 1;
    if (a[0] == MINUS) return -ERRM9;
    strcpy(XX, a);

    // look for good initial value
    if ((a[0] > ONE) || ((a[0] == ONE) && (a[1] != POINT))) {
        int i = 0;
        int ch;

        while ((ch = a[i++]) != EOL) {
            if (ch == POINT) break;
        }

        if ((i = (i + 1) / 2)) a[i] = EOL;
    } else if (a[0] != ONE) {
        a[0] = ONE;
        a[1] = EOL;
    }

    // Newton's algorithm with quadratic convergence
    partab.jobtab->precision++;

    do {
        short s;

        strcpy(XXX, a);
        strcpy(tmp1, XX);
        strcpy(tmp2, a);
        s = runtime_div(tmp1, tmp2, OPDIV);
        if (s < 0) return s;
        s = runtime_add(a, tmp1);
        if (s < 0) return s;
        tmp2[0] = TWO;
        tmp2[1] = EOL;
        s = runtime_div(a, tmp2, OPDIV);
        if (s < 0) return s;
    } while (runtime_comp(a, XXX));

    partab.jobtab->precision--;
    return (int) strlen(a);
}                                                                               // end g_sqrt()

// n.th root
int root(char *a, int n)
{
    int   i;
    int   ch;
    short s;
    char  tmp1[MAX_NUM_BYTES + 2];
    char  tmp2[MAX_NUM_BYTES + 2];
    char  XX[MAX_NUM_BYTES + 2];
    char  XXX[MAX_NUM_BYTES + 2];
    short again;

    if (a[0] == ZERO) return 1;
    if (a[0] == MINUS || n == 0) return -ERRM9;
    strcpy(XX, a);

    // look for good initial value
    if ((a[0] > ONE) || ((a[0] == ONE) && (a[1] != POINT))) {
        i = 0;
        while (((ch = a[i++]) != EOL) && (ch != POINT)) continue;

        if ((i = (i + n - 2) / n) > 0) {
            a[0] = THREE;
            a[i] = EOL;
        }
    } else if (a[0] != ONE) {
        a[0] = ONE;
        a[1] = EOL;
    }

    // Newton's algorithm with quadratic convergence
    if (partab.jobtab->precision <= 3) {
        again = 0;                                                              // speedup div with small precision
    } else {
        again = partab.jobtab->precision;
        partab.jobtab->precision = 2;
    }

second:
    partab.jobtab->precision++;

    for (;;) {
        strcpy(XXX, a);
        s = itocstring((u_char *) tmp1, n - 1);
        if (s < 0) return s;
        strcpy(tmp2, a);
        s = runtime_power(tmp2, tmp1);
        if (s < 0) return s;
        strcpy(tmp1, XX);
        s = runtime_div(tmp1, tmp2, OPDIV);
        if (s < 0) break;
        s = itocstring((u_char *) tmp2, n - 1);
        if (s < 0) return s;
        s = runtime_mul(a, tmp2);
        if (s < 0) return s;
        s = runtime_add(a, tmp1);
        if (s < 0) return s;
        s = itocstring((u_char *) tmp2, n);
        if (s < 0) return s;
        s = runtime_div(a, tmp2, OPDIV);
        if (s < 0) return s;
        strcpy(tmp2, a);
        s = runtime_div(XXX, tmp2, OPDIV);
        if (s < 0) return s;
        tmp2[0] = ONE;

        if (partab.jobtab->precision <= 0) {
            tmp2[1] = EOL;
        } else {
            tmp2[1] = POINT;
            for (i = 2; i < partab.jobtab->precision; i++) tmp2[i] = ZERO;
            tmp2[i++] = FIVE;
            tmp2[i] = EOL;
        }

        if (!runtime_comp(XXX, tmp2)) continue;
        if (partab.jobtab->precision <= 0) break;
        tmp2[0] = POINT;
        for (i = 1; i < partab.jobtab->precision; i++) tmp2[i] = NINE;
        tmp2[i - 1] = FIVE;
        tmp2[i] = EOL;
        if (runtime_comp(tmp2, XXX)) break;
    }

    partab.jobtab->precision--;

    if (again) {
        partab.jobtab->precision = again;
        again = 0;
        goto second;
    }

    return (int) strlen(a);
}                                                                               // end root()

/* rounding
 * 'a' is assumed to be a 'canonic' numeric string
 * it is rounded to 'digits' fractional digits
 */
void roundit(char *a, int digits)
{
    int i;
    int pointpos;
    int lena;

    pointpos = -1;
    i = 0;
    i = 0;

    while (a[i] != EOL) {
        if (a[i] == POINT) pointpos = i;
        i++;
    }

    lena = i;
    if (pointpos < 0) pointpos = i;
    if ((pointpos + digits + 1) >= i) return;                                   // nothing to round
    i = pointpos + digits + 1;

    if (a[i] < FIVE) {
        a[i] = EOL;
        while (a[--i] == ZERO) a[i] = EOL;

        if (a[i] == POINT) {
            a[i] = EOL;
            if ((i == 0) || ((i == 1) && (a[0] == MINUS))) a[0] = ZERO;
        }

        return;
    }

    for (;;) {
        int ch;

        if (i >= pointpos) {
            a[i] = EOL;
        } else {
            a[i] = ZERO;
        }

        if (--i < (a[0] == MINUS)) {
            for (i = lena; i >= 0; i--) a[i + 1] = a[i];
            a[a[0] == MINUS] = ONE;
            break;
        }

        if ((ch = a[i]) == POINT) continue;

        if (a[i] < NINE && ch >= ZERO) {
            a[i] = ++ch;
            break;
        }
    }

    return;
}                                                                               // end roundit

// raise a to the b-th power
short runtime_power(char *a, char *b)
{
    short s;
    char  c[MAX_NUM_BYTES + 2];
    char  d[(MAX_NUM_BYTES + 1) * 4];
    /*
     * Is a memory leak resulting in wrong results
     * with fractional powers, e.g., 2**(3/7),
     * even a value of 513 is too small?
     *
     * TODO: test this soon, as we have more than 513 now
     */
    char  e[MAX_NUM_BYTES + 2];
    long  i;
    long  j;
    char  Z[MAX_NUM_BYTES + 2];

    // if zero or one there's not much to do
    if (a[1] == EOL) {
        if (a[0] == ZERO) {
            if ((b[1] == EOL) && (b[0] == ZERO)) return -ERRM9;
            return 1;
        }                                                                       // undef

        if (a[0] == ONE) return 1;
    }

    if (b[0] == MINUS) {
        s = runtime_power(a, &b[1]);
        if (s < 0) return s;
        strcpy(c, a);
        a[0] = ONE;
        a[1] = EOL;
        s = runtime_div(a, c, OPDIV);
        return s;
    }

    if (b[1] == EOL) {
        switch (b[0]) {
        case ZERO:
            a[0] = ONE;
            a[1] = EOL;
            // fall through

        case ONE:
            return (short) strlen(a);

        case TWO:
            strcpy(c, a);
            return runtime_mul(a, c);
        }
    }

    // look for decimal point
    e[0] = EOL;
    i = 0;

    while (b[i] != EOL) {
        if (b[i] == POINT) {
            if (a[0] == MINUS) return -ERRM9;

            if ((b[i + 1] == FIVE) && (b[i + 2] == EOL)) {                      // half-integer: extra solution
                if (i) {
                    strcpy(c, b);
                    s = runtime_add(b, c);
                    if (s < 0) return s;
                    s = runtime_power(a, b);
                    if (s < 0) return s;
                }

                s = (short) g_sqrt(a);
                return s;
            }

            strcpy(e, &b[i]);
            b[i] = EOL;
            break;
        }

        i++;
    }

    strcpy(d, a);
    i = atoi(b);
    if (i == INT_MAX) return -ERRM92;

    /*
     * Do it with a small number of multiplications
     * the number of multiplications is not optimum, but reasonably small
     * see Donald E. Knuth "The Art of Computer Programming" Vol.II p.441
     */
    if (i == 0) {
        a[0] = ONE;
        a[1] = EOL;
    } else {
        j = 1;

        while (j < i) {
            j = j * 2;
            if (j < 0) return -ERRM92;
        }

        if (i != j) j = j / 2;
        j = j / 2;

        while (j) {
            strcpy(c, a);
            s = runtime_mul(a, c);
            if (s < 0) return s;

            if (i & j) {
                strcpy(c, d);
                s = runtime_mul(a, c);
                if (s < 0) return s;
            }

            j = j / 2;
        }

        if (e[0] == EOL) return (short) strlen(a);
    }

    /*
     * non integer exponent
     * state of computation at this point:
     * d == saved value of a
     * a == d^^int(b);
     * e == frac(b);
     */

    // is fraction the inverse of an integer?
    Z[0] = ONE;
    Z[1] = EOL;
    strcpy(c, e);
    s = runtime_div(Z, c, OPDIV);
    if (s < 0) return s;
    i = 0;

    for (;;) {
        if ((j = Z[i++]) == EOL) {
            j = atoi(Z);
            if (j == INT_MAX) return -ERRM92;
            break;
        }

        if (j != POINT) continue;
        j = atoi(Z);
        if (j == INT_MAX) return -ERRM92;
        if (Z[i] == NINE) j++;
        break;
    }

    Z[0] = ONE;
    Z[1] = EOL;
    s = (short) itocstring((u_char *) c, j);
    if (s < 0) return s;
    s = runtime_div(Z, c, OPDIV);
    if (s < 0) return s;

    // if integer
    if (strcmp(Z, e) == 0) {
        strcpy(Z, d);
        s = (short) root(Z, j);

        if (s < 0) {
            s = runtime_mul(a, Z);
            return s;
        }                                                                       // on error try other method
    }

    Z[0] = ONE;
    Z[1] = EOL;
    partab.jobtab->precision += 2;

    for (;;) {
        c[0] = TWO;
        c[1] = EOL;
        s = runtime_mul(e, c);
        if (s < 0) return s;
        s = (short) g_sqrt(d);
        if (s < 0) return s;

        if (e[0] == ONE) {
            e[0] = ZERO;
            //numlit(e);                                                          // not required !!
            strcpy(c, d);
            s = runtime_mul(Z, c);
            if (s < 0) return s;
            roundit(Z, partab.jobtab->precision);
        }

        // TODO: commenting out improves accuracy of fractional numbers - look at soon
        //if (e[0] == ZERO) break;
        i = 0;
        j = ((d[0] == ONE) ? ZERO : NINE);

        for (;;) {
            ++i;
            if ((d[i] != j) && (d[i] != POINT)) break;
        }

        if ((d[i] == EOL) || (i > partab.jobtab->precision)) break;
    }

    partab.jobtab->precision -= 2;
    s = runtime_mul(a, Z);
    if (s < 0) return s;
    roundit(a, partab.jobtab->precision + 1);
    return strlen(a);
}                                                                               // end power()

// s and t are strings representing M numbers, runtime_comp returns t>s
short runtime_comp(char *s, char *t)
{
    int  s1;
    int  t1;

    s1 = s[0];
    t1 = t[0];

    if (s1 != t1) {
        if (s1 == MINUS) return TRUE;                                           // s<0<t
        if (t1 == MINUS) return FALSE;                                          // t<0<s
        if ((s1 == POINT) && (t1 == ZERO)) return FALSE;                        // s>0; t==0
        if ((t1 == POINT) && (s1 == ZERO)) return TRUE;                         // t>0; s==0
    }

    if (t1 == MINUS) {
        char *a;

        a = &t[1];
        t = &s[1];
        s = a;
    }

    s1 = 0;
    while (s[s1] > POINT) s1++;                                                 // Note: EOL<'.'
    t1 = 0;
    while (t[t1] > POINT) t1++;
    if (t1 > s1) return TRUE;
    if (t1 < s1) return FALSE;

    while (*t == *s) {
        if (*t == EOL) return FALSE;
        t++;
        s++;
    }

    if (*t > *s) return TRUE;
    return FALSE;
}                                                                               // end of runtime_comp()
