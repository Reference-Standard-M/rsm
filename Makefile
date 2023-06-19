#
# Package:  Reference Standard M
# File:     rsm/Makefile
# Summary:  Makefile for FreeBSD, NetBSD, and OpenBSD
#           See rsm/GNUmakefile for Linux, MacOS X, Solaris, and Raspberry Pi
#
# David Wicksell <dlw@linux.com>
# Copyright © 2020-2023 Fourth Watch Software LC
# https://gitlab.com/Reference-Standard-M/rsm
#
# Based on MUMPS V1 by Raymond Douglas Newman
# Copyright (c) 1999-2018
# https://gitlab.com/Reference-Standard-M/mumpsv1
#
# This program is free software: you can redistribute it and/or modify it
# under the terms of the GNU Affero General Public License (AGPL) as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public
# License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program. If not, see http://www.gnu.org/licenses/.

OS      != uname
CC      := gcc
PROG    := rsm
RM      := rm -f
DEPS    := include/*.h
SRC     != ls */*.c
OBJ     := $(SRC:.c=.o)
CFLAGS  := -Wall -Wextra -fsigned-char -fwrapv -std=gnu99 -Iinclude
LDFLAGS := -lm -lcrypt

.if ($(OS) == OpenBSD)
    LDFLAGS := -lm
.endif

.ifmake profile
    CONFIG  := -O0 -g3
    CFLAGS  += -pg
    LDFLAGS += -pg -lc
.elifmake debug
    CONFIG  := -O0 -g3
.else
    CONFIG  := -O3
    CFLAGS  += -DNDEBUG
.endif

.ifdef dbver
    CFLAGS  += -DRSM_DBVER=$(dbver)
.endif

.ifdef path
    EXECDIR := $(path)
.else
    EXECDIR := /usr/local/bin
.endif

.c.o: ${DEPS}
	${CC} ${CONFIG} ${CFLAGS} -o $@ -c $<

${PROG}: ${OBJ}
	${CC} -o ${PROG} ${OBJ} ${LDFLAGS}

all: ${PROG}

debug: ${PROG}

profile: ${PROG}

install: ${PROG}

	@if [ "$${USER}" != "root" ]; then \
	    echo "You must install ${PROG} as root"; \
	    exit 1; \
	fi

	@if [ -d ${EXECDIR} ]; then \
	    echo install -o root -g 0 -m 755 -s ${PROG} ${EXECDIR}; \
	    install -o root -g 0 -m 755 -s ${PROG} ${EXECDIR}; \
	else \
	    echo "${EXECDIR} does not exist"; \
	    exit 1; \
	fi

uninstall:

	@if [ "$${USER}" != "root" ]; then \
	    echo "You must uninstall ${PROG} as root"; \
	    exit 1; \
	fi

	@if [ -f ${EXECDIR}/${PROG} -a -x ${EXECDIR}/${PROG} ]; then \
	    echo ${RM} ${EXECDIR}/${PROG}; \
	    ${RM} ${EXECDIR}/${PROG}; \
	fi

clean:
	${RM} ${OBJ} ${PROG} $(wildcard ${PROG}.core)

.PHONY: all debug profile install uninstall clean
