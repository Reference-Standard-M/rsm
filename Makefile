#
# Package: Reference Standard M
# File:    rsm/Makefile
# Summary: Makefile for FreeBSD, NetBSD, and OpenBSD
#          See rsm/GNUmakefile for Linux, macOS, Solaris, AIX, HP-UX, and RPi
#
# David Wicksell <dlw@linux.com>
# Copyright © 2020-2024 Fourth Watch Software LC
# https://gitlab.com/Reference-Standard-M/rsm
#
# Based on MUMPS V1 by Raymond Douglas Newman
# Copyright © 1999-2018
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
# along with this program. If not, see https://www.gnu.org/licenses/.
#
# SPDX-FileCopyrightText:  © 2020 David Wicksell <dlw@linux.com>
# SPDX-License-Identifier: AGPL-3.0-or-later

OS      != uname
CC      := gcc
PROG    := rsm
RM      := rm -f
DEPS    := include/*.h
SRC     != ls */*.c
OBJ     := $(SRC:.c=.o)
CFLAGS  := -Wall -Wextra -fsigned-char -fwrapv -std=gnu99 -Iinclude
LDLIBS  := -lm -lcrypt
PREFIX  := /usr/local
GIT_SHA != git rev-parse --short=10 HEAD 2>/dev/null; true
DOCS    := doc/adoc/*.adoc

.if ($(GIT_SHA) != "")
    CFLAGS  += -DGIT_SHA=$(GIT_SHA)
.endif

.ifdef dbver
    CFLAGS  += -DRSM_DBVER=$(dbver)
.endif

.if ($(OS) == OpenBSD)
    LDLIBS  := -lm
.endif

.ifmake debug
    CONFIG  := -O0 -g3

.   ifdef options
.       if ($(options) == profile)
            CFLAGS  += -pg
            LDLIBS  += -lc -pg
.       elif ($(options) == sanitize)
            CFLAGS  += -fsanitize=address,undefined
            LDLIBS  += -fsanitize=address,undefined
.       endif
.   endif
.else
    CONFIG  := -O3
    CFLAGS  += -DNDEBUG
.endif

.PHONY: all
all: ${PROG}

.PHONY: debug
debug: ${PROG}

${PROG}: ${OBJ}
	${CC} -o ${PROG} ${OBJ} ${LDLIBS}

.c.o: ${DEPS}
	${CC} ${CONFIG} ${CFLAGS} -o $@ -c $<

.PHONY: clean
clean:
	${RM} ${OBJ} ${PROG} $(wildcard ${PROG}.core)

.PHONY: install
install: ${PROG}
	@if [ "$${USER}" != "root" ]; then \
	    echo "You must install ${PROG} as root"; \
	    exit 1; \
	elif [ -d ${PREFIX}/bin ]; then \
	    echo install -o 0 -g 0 -m 755 -s ${PROG} ${PREFIX}/bin; \
	    install -o 0 -g 0 -m 755 -s ${PROG} ${PREFIX}/bin; \
	else \
	    echo "${PREFIX}/bin does not exist"; \
	    exit 1; \
	fi

.PHONY: uninstall
uninstall:
	@if [ "$${USER}" != "root" ]; then \
	    echo "You must uninstall ${PROG} as root"; \
	    exit 1; \
	elif [ -f ${PREFIX}/bin/${PROG} -a -x ${PREFIX}/bin/${PROG} ]; then \
	    echo ${RM} ${PREFIX}/bin/${PROG}; \
	    ${RM} ${PREFIX}/bin/${PROG}; \
	fi

.PHONY: install-utils
install-utils:
	@if [ "$${USER}" != "root" ]; then \
	    echo "You must install utils.rsm as root"; \
	    exit 1; \
	elif [ -d ${PREFIX}/share ]; then \
	    echo install -d -o 0 -g 0 -m 755 ${PREFIX}/share/rsm; \
	    install -d -o 0 -g 0 -m 755 ${PREFIX}/share/rsm; \
	    echo install -o 0 -g 0 -m 644 utils.rsm ${PREFIX}/share/rsm; \
	    install -o 0 -g 0 -m 644 utils.rsm ${PREFIX}/share/rsm; \
	else \
	    echo "${PREFIX}/share does not exist"; \
	    exit 1; \
	fi

.PHONY: uninstall-utils
uninstall-utils:
	@if [ "$${USER}" != "root" ]; then \
	    echo "You must uninstall utils.rsm as root"; \
	    exit 1; \
	elif [ -d ${PREFIX}/share/rsm ]; then \
	    echo ${RM} -r ${PREFIX}/share/rsm; \
	    ${RM} -r ${PREFIX}/share/rsm; \
	fi

.PHONY: install-docs
install-docs:
	@if [ "$${USER}" != "root" ]; then \
	    echo "You must install documentation as root"; \
	    exit 1; \
	elif [ -d ${PREFIX}/share/doc ]; then \
	    echo install -d -o 0 -g 0 -m 755 ${PREFIX}/share/doc/rsm; \
	    install -d -o 0 -g 0 -m 755 ${PREFIX}/share/doc/rsm; \
	    echo install -o 0 -g 0 -m 644 ${DOCS} ${PREFIX}/share/doc/rsm; \
	    install -o 0 -g 0 -m 644 ${DOCS} ${PREFIX}/share/doc/rsm; \
	    echo gzip -r -f9 ${PREFIX}/share/doc/rsm; \
	    gzip -r -f9 ${PREFIX}/share/doc/rsm; \
	    if [ -d ${PREFIX}/share/man ]; then \
	        echo install -d -o 0 -g 0 -m 755 ${PREFIX}/share/man/man1; \
	        install -d -o 0 -g 0 -m 755 ${PREFIX}/share/man/man1; \
	        echo install -o 0 -g 0 -m 644 ${DOCS} ${PREFIX}/share/man/man1; \
	        install -o 0 -g 0 -m 644 ${DOCS} ${PREFIX}/share/man/man1; \
	        echo gzip -f9 ${PREFIX}/share/man/man1/rsm.1; \
	        gzip -f9 ${PREFIX}/share/man/man1/rsm.1; \
	        echo mandb -q; \
	        mandb -q; \
	    else \
	        echo "${PREFIX}/share/man does not exist"; \
	        exit 1; \
	    fi \
	else \
	    echo "${PREFIX}/share/doc does not exist"; \
	    exit 1; \
	fi

.PHONY: uninstall-docs
uninstall-docs:
	@if [ "$${USER}" != "root" ]; then \
	    echo "You must uninstall documentation as root"; \
	    exit 1; \
	elif [ -d ${PREFIX}/share/doc/rsm ]; then \
	    echo ${RM} -r ${PREFIX}/share/doc/rsm; \
	    ${RM} -r ${PREFIX}/share/doc/rsm; \
	    if [ -f ${PREFIX}/share/man/man1/rsm.1.gz ]; then \
	        echo ${RM} ${PREFIX}/share/man/man1/rsm.1.gz; \
	        ${RM} ${PREFIX}/share/man/man1/rsm.1.gz; \
	        echo mandb -q; \
	        mandb -q; \
	    fi \
	fi
