#
# Package:  Reference Standard M
# File:     rsm/Makefile
# Summary:  Makefile for FreeBSD, NetBSD, and OpenBSD
#           See rsm/GNUmakefile for Linux, macOS, Solaris, AIX, HP-UX, and RPi
#
# David Wicksell <dlw@linux.com>
# Copyright © 2020-2024 Fourth Watch Software LC
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
PREFIX  := /usr/local
GIT_SHA != git rev-parse --short=10 HEAD 2>/dev/null; true

.ifdef $(GIT_SHA)
    CFLAGS  += -DGIT_SHA=$(GIT_SHA)
.endif

.ifdef dbver
    CFLAGS  += -DRSM_DBVER=$(dbver)
.endif

.if ($(OS) == OpenBSD)
    LDFLAGS := -lm
.endif

.ifmake debug
    CONFIG  := -O0 -g3

.   ifdef options
.       if ($(options) == profile)
            CFLAGS  += -pg
            LDFLAGS += -lc -pg
.       elif ($(options) == sanitize)
            CFLAGS  += -fsanitize=address,undefined
            LDFLAGS += -fsanitize=address,undefined
.       endif
.   endif
.else
    CONFIG  := -O3
    CFLAGS  += -DNDEBUG
.endif

all: ${PROG}

debug: ${PROG}

${PROG}: ${OBJ}
	${CC} -o ${PROG} ${OBJ} ${LDFLAGS}

.c.o: ${DEPS}
	${CC} ${CONFIG} ${CFLAGS} -o $@ -c $<

clean:
	${RM} ${OBJ} ${PROG} $(wildcard ${PROG}.core)

install: ${PROG}
	@if [ "$${USER}" != "root" ]; then \
	    echo "You must install ${PROG} as root"; \
	    exit 1; \
	elif [ -d ${PREFIX}/bin ]; then \
	    echo install -o root -g 0 -m 755 -s ${PROG} ${PREFIX}/bin; \
	    install -o root -g 0 -m 755 -s ${PROG} ${PREFIX}/bin; \
	else \
	    echo "${PREFIX}/bin does not exist"; \
	    exit 1; \
	fi

uninstall:
	@if [ "$${USER}" != "root" ]; then \
	    echo "You must uninstall ${PROG} as root"; \
	    exit 1; \
	elif [ -f ${PREFIX}/bin/${PROG} -a -x ${PREFIX}/bin/${PROG} ]; then \
	    echo ${RM} ${PREFIX}/bin/${PROG}; \
	    ${RM} ${PREFIX}/bin/${PROG}; \
	fi

.PHONY: all debug clean install uninstall
