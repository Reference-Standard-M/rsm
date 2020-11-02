#
# Package:  Reference Standard M
# File:     rsm/BSDmakefile
# Summary:  Makefile for FreeBSD, NetBSD, and OpenBSD
#
# David Wicksell <dlw@linux.com>
# Copyright © 2020 Fourth Watch Software LC
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

OS    != uname
CC    = gcc
LIBS  = -lm -lcrypt

.ifmake test
EXTRA = -O0 -g -fsigned-char -fwrapv -Wall -Iinclude
.else
EXTRA = -O3 -fsigned-char -fwrapv -Wall -Iinclude
.endif

.if ($(OS) == OpenBSD)
LIBS  = -lm
.endif

DIRS  = compile database init runtime seqio symbol util xcall
RM    = rm -f
PROG  = rsm

OBJS  = compile/dollar.o \
        compile/eval.o \
        compile/localvar.o \
        compile/parse.o \
        compile/routine.o \
        database/db_buffer.o \
        database/db_daemon.o \
        database/db_get.o \
        database/db_ic.o \
        database/db_kill.o \
        database/db_locate.o \
        database/db_main.o \
        database/db_rekey.o \
        database/db_set.o \
        database/db_uci.o \
        database/db_util.o \
        database/db_view.o \
        init/init_create.o \
        init/init_run.o \
        init/init_start.o \
        init/rsm.o \
        runtime/runtime_attn.o \
        runtime/runtime_buildmvar.o \
        runtime/runtime_debug.o \
        runtime/runtime_func.o \
        runtime/runtime_math.o \
        runtime/runtime_pattern.o \
        runtime/runtime_run.o \
        runtime/runtime_ssvn.o \
        runtime/runtime_util.o \
        runtime/runtime_vars.o \
        seqio/SQ_Util.o \
        seqio/SQ_Signal.o \
        seqio/SQ_Device.o \
        seqio/SQ_File.o \
        seqio/SQ_Pipe.o \
        seqio/SQ_Seqio.o \
        seqio/SQ_Socket.o \
        seqio/SQ_Tcpip.o \
        symbol/symbol_new.o \
        symbol/symbol_util.o \
        util/util_key.o \
        util/util_lock.o \
        util/util_memory.o \
        util/util_routine.o \
        util/util_share.o \
        util/util_strerror.o \
        xcall/xcall.o

.c.o:
	${CC} ${EXTRA} -c $< -o $@

all: ${OBJS}
	${CC} ${EXTRA} -o ${PROG} ${OBJS} ${LIBS}

test: ${OBJS}
	${CC} ${EXTRA} -o ${PROG} ${OBJS} ${LIBS}

clean:
	${RM} ${OBJS} ${PROG} ${PROG}.core
