#
# Package:  Reference Standard M
# File:     rsm/Makefile
# Summary:  Makefile for Linux, MacOS X, Solaris, and Raspberry Pi
#           See BSDmakefile for FreeBSD, NetBSD, and OpenBSD
#
# David Wicksell <dlw@linux.com>
# Copyright © 2020-2021 Fourth Watch Software LC
# https://gitlab.com/Reference-Standard-M/rsm
#
# Based on MUMPS V1 by Raymond Douglas Newman
# with help from Sam Habiel
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

OS    := $(shell uname)
CC     = gcc
FLAGS  = -fsigned-char -fwrapv -Wall -std=gnu99 -Iinclude -D_FILE_OFFSET_BITS=64
LIBS   = -lcrypt

ifeq ($(MAKECMDGOALS),test)
    EXTRA = -O0 -g3
else
    EXTRA = -O3
endif

ifdef dbver
    FLAGS += -DRSM_DBVER=$(dbver)
endif

ifdef path
    DIR=$(path)
else
    DIR=/usr/local/bin
endif

ifneq ($(OS),AIX)
    LIBS += -lm
endif

ifeq ($(OS),SunOS)
    LIBS += -lnsl -lsocket -lrt
endif

ifeq ($(OS),Darwin)
    LIBS   = -lm -framework CoreServices -framework DirectoryService -framework Security
    FLAGS += -Wno-deprecated-declarations
endif

DIRS = compile database init runtime seqio symbol util xcall
RM   = rm -f
PROG = rsm

OBJS = compile/dollar.o \
       compile/eval.o \
       compile/localvar.o \
       compile/parse.o \
       compile/routine.o \
       database/buffer.o \
       database/daemon.o \
       database/get.o \
       database/ic.o \
       database/kill.o \
       database/locate.o \
       database/main.o \
       database/rekey.o \
       database/set.o \
       database/uci.o \
       database/util.o \
       database/view.o \
       init/create.o \
       init/rsm.o \
       init/run.o \
       init/start.o \
       runtime/attn.o \
       runtime/buildmvar.o \
       runtime/debug.o \
       runtime/func.o \
       runtime/math.o \
       runtime/pattern.o \
       runtime/run.o \
       runtime/ssvn.o \
       runtime/util.o \
       runtime/var.o \
       seqio/device.o \
       seqio/file.o \
       seqio/pipe.o \
       seqio/seqio.o \
       seqio/signal.o \
       seqio/socket.o \
       seqio/tcpip.o \
       seqio/util.o \
       symbol/new.o \
       symbol/util.o \
       util/key.o \
       util/lock.o \
       util/memory.o \
       util/routine.o \
       util/share.o \
       util/strerror.o \
       xcall/xcall.o

.c.o:
	${CC} ${EXTRA} ${FLAGS} -c $< -o $@

all: ${OBJS}
	${CC} ${EXTRA} ${FLAGS} -o ${PROG} ${OBJS} ${LIBS}

test: ${OBJS}
	${CC} ${EXTRA} ${FLAGS} -o ${PROG} ${OBJS} ${LIBS}

install: ${PROG}
	@if [ "$${USER}" != "root" ]; then \
	    echo "You must install ${PROG} as root"; \
	    exit 1; \
	fi

	@if [ ! -d ${DIR} ]; then \
	    mkdir -p ${DIR}; \
	fi

ifeq ($(OS),AIX)
	install -O root -G 0 -M 755 -S -f ${DIR} ${PROG}
else
	install -o root -g 0 -m 755 -s ${PROG} ${DIR}
endif

uninstall:
	@if [ "$${USER}" != "root" ]; then \
	    echo "You must uninstall ${PROG} as root"; \
	    exit 1; \
	fi

	@if [ -f ${DIR}/${PROG} -a -x ${DIR}/${PROG} ]; then \
	    ${RM} ${DIR}/${PROG}; \
	fi

clean:
	${RM} ${OBJS} ${PROG} *core

.PHONY: all test install uninstall clean
