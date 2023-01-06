#
# Package:  Reference Standard M
# File:     rsm/GNUmakefile
# Summary:  Makefile for Linux, MacOS X, Solaris, and Raspberry Pi
#           See rsm/Makefile for FreeBSD, NetBSD, and OpenBSD
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

OS      := $(shell uname)
CC      := gcc
PROG    := rsm
RM      := rm -f
DEPS    := $(wildcard include/*.h)
SRC     := $(wildcard */*.c)
OBJ     := $(SRC:.c=.o)
CFLAGS  := -Wall -Wextra -fsigned-char -fwrapv -std=gnu99 -Iinclude -D_FILE_OFFSET_BITS=64
LDFLAGS := -lcrypt

ifeq ($(MAKECMDGOALS),debug)
    CONFIG := -O0 -g3
else
    CONFIG := -O3
    CFLAGS += -DNDEBUG
endif

ifneq ($(OS),AIX)
    LDFLAGS += -lm
endif

ifeq ($(OS),SunOS)
    LDFLAGS += -lnsl -lsocket -lrt
endif

ifeq ($(OS),Darwin)
    CFLAGS  += -Wno-deprecated-declarations
    LDFLAGS := -lm -framework CoreServices -framework DirectoryService -framework Security
endif

ifdef dbver
    CFLAGS += -DRSM_DBVER=$(dbver)
endif

ifdef path
    EXECDIR := $(path)
else
    EXECDIR := /usr/local/bin
endif

%.o: %.c ${DEPS}
	${CC} ${CONFIG} ${CFLAGS} -o $@ -c $<

all: ${OBJ}
	${CC} -o ${PROG} $^ ${LDFLAGS}

debug: ${OBJ}
	${CC} -o ${PROG} $^ ${LDFLAGS}

install: ${PROG}

	@if [ "$(USER)" != "root" ]; then \
	    echo "You must install ${PROG} as root"; \
	    exit 1; \
	fi

ifeq ($(OS),AIX)
	@if [ -d ${EXECDIR} ]; then \
	    echo install -O root -G 0 -M 755 -S -f ${EXECDIR} ${PROG}; \
	    install -O root -G 0 -M 755 -S -f ${EXECDIR} ${PROG}; \
	else \
	    echo "${EXECDIR} does not exist"; \
	    exit 1; \
	fi
else
	@if [ -d ${EXECDIR} ]; then \
	    echo install -o root -g 0 -m 755 -s ${PROG} ${EXECDIR}; \
	    install -o root -g 0 -m 755 -s ${PROG} ${EXECDIR}; \
	else \
	    echo "${EXECDIR} does not exist"; \
	    exit 1; \
	fi
endif

uninstall:

	@if [ "$(USER)" != "root" ]; then \
	    echo "You must uninstall ${PROG} as root"; \
	    exit 1; \
	fi

	@if [ -f ${EXECDIR}/${PROG} -a -x ${EXECDIR}/${PROG} ]; then \
	    echo ${RM} ${EXECDIR}/${PROG}; \
	    ${RM} ${EXECDIR}/${PROG}; \
	fi

clean:
	${RM} ${OBJ} ${PROG} $(wildcard *core)

.PHONY: all debug install uninstall clean
