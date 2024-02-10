#
# Package:  Reference Standard M
# File:     rsm/GNUmakefile
# Summary:  Makefile for Linux, macOS, Solaris, AIX, HP-UX, and RPi
#           See rsm/Makefile for FreeBSD, NetBSD, and OpenBSD
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

OS      := $(shell uname)
CC      := gcc
PROG    := rsm
RM      := rm -f
DEPS    := $(wildcard include/*.h)
SRC     := $(wildcard */*.c)
OBJ     := $(SRC:.c=.o)
CFLAGS  := -Wall -Wextra -fsigned-char -fwrapv -std=gnu99 -Iinclude -D_FILE_OFFSET_BITS=64
PREFIX  := /usr/local
GIT_SHA := $(shell git rev-parse --short=10 HEAD 2>/dev/null; true)

ifdef GIT_SHA
    CFLAGS  += -DGIT_SHA=$(GIT_SHA)
endif

ifdef dbver
    CFLAGS  += -DRSM_DBVER=$(dbver)
endif

ifeq ($(OS),HP-UX)
    LDFLAGS := -lm
else
    LDFLAGS := -lcrypt
endif

ifneq ($(OS),AIX)
    ifneq ($(OS),HP-UX)
        LDFLAGS += -lm
    endif

    INSTALL := install -o root -g 0 -m 755 -s ${PROG} ${PREFIX}/bin
else
    INSTALL := install -O root -G 0 -M 755 -S -f ${PREFIX}/bin ${PROG}
endif

ifeq ($(OS),SunOS)
    LDFLAGS += -lnsl -lsocket -lrt
endif

ifeq ($(OS),Darwin)
    CFLAGS  += -Wno-deprecated-declarations
    LDFLAGS := -framework CoreServices -framework DirectoryService -framework Security -lm
endif

ifeq ($(MAKECMDGOALS),debug)
    CONFIG  := -O0 -g3

    ifdef options
        ifeq ($(options),profile)
            CFLAGS  += -pg
            LDFLAGS += -lc -pg
        else
            ifeq ($(options),sanitize)
                CFLAGS  += -fsanitize=address,undefined
                LDFLAGS += -fsanitize=address,undefined
            endif
        endif
    endif
else
    CONFIG  := -O3
    CFLAGS  += -DNDEBUG
endif

all: ${PROG}

debug: ${PROG}

${PROG}: ${OBJ}
	${CC} -o ${PROG} $^ ${LDFLAGS}

%.o: %.c ${DEPS}
	${CC} ${CONFIG} ${CFLAGS} -o $@ -c $<

clean:
	${RM} ${OBJ} ${PROG} $(wildcard *core)

install: ${PROG}
	@if [ "$(USER)" != "root" ]; then \
	    echo "You must install ${PROG} as root"; \
	    exit 1; \
	elif [ -d ${PREFIX}/bin ]; then \
	    echo ${INSTALL}; \
	    ${INSTALL}; \
	else \
	    echo "${PREFIX}/bin does not exist"; \
	    exit 1; \
	fi

uninstall:
	@if [ "$(USER)" != "root" ]; then \
	    echo "You must uninstall ${PROG} as root"; \
	    exit 1; \
	elif [ -f ${PREFIX}/bin/${PROG} -a -x ${PREFIX}/bin/${PROG} ]; then \
	    echo ${RM} ${PREFIX}/bin/${PROG}; \
	    ${RM} ${PREFIX}/bin/${PROG}; \
	fi

.PHONY: all debug clean install uninstall
