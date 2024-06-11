#
# Package: Reference Standard M
# File:    rsm/GNUmakefile
# Summary: Makefile for Linux, macOS, Solaris, AIX, HP-UX, and RPi
#          See rsm/Makefile for FreeBSD, NetBSD, and OpenBSD
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
DOCS    := $(wildcard doc/adoc/*.adoc)

ifdef GIT_SHA
    CFLAGS  += -DGIT_SHA=$(GIT_SHA)
endif

ifdef dbver
    CFLAGS  += -DRSM_DBVER=$(dbver)
endif

ifeq ($(OS),HP-UX)
    LDLIBS  := -lm
else
    LDLIBS  := -lcrypt
endif

ifneq ($(OS),AIX)
    ifneq ($(OS),HP-UX)
        LDLIBS  += -lm
    endif

    INSTLPR := install -o 0 -g 0 -m 755 -s ${PROG} ${PREFIX}/bin
    INSTLUT := install -D -o 0 -g 0 -m 644 utils.rsm -t ${PREFIX}/share/rsm
    INSTLDC := install -D -o 0 -g 0 -m 644 ${DOCS} -t ${PREFIX}/share/doc/rsm
    INSTLMN := install -D -o 0 -g 0 -m 644 doc/man/rsm.1 -t ${PREFIX}/share/man/man1
else
    INSTLPR := install -O 0 -G 0 -M 755 -S -f ${PREFIX}/bin ${PROG}
    INSTLUT := install -O 0 -G 0 -M 644 -f ${PREFIX}/share/rsm utils.rsm
    INSTLDC := install -O 0 -G 0 -M 644 -f ${PREFIX}/share/doc/rsm ${DOCS}
    INSTLMN := install -O 0 -G 0 -M 644 -f ${PREFIX}/share/man/man1 doc/man/rsm.1
endif

ifeq ($(OS),SunOS)
    LDLIBS  += -lnsl -lsocket -lrt
endif

ifeq ($(OS),Darwin)
    CFLAGS  += -Wno-deprecated-declarations
    LDLIBS  := -framework CoreServices -framework DirectoryService -framework Security -lm
endif

ifeq ($(MAKECMDGOALS),debug)
    CONFIG  := -O0 -g3

    ifdef options
        ifeq ($(options),profile)
            CFLAGS  += -pg
            LDLIBS  += -lc -pg
        else
            ifeq ($(options),sanitize)
                CFLAGS  += -fsanitize=address,undefined
                LDLIBS  += -fsanitize=address,undefined
            endif
        endif
    endif
else
    CONFIG  := -O3
    CFLAGS  += -DNDEBUG
endif

.PHONY: all
all: ${PROG}

.PHONY: debug
debug: ${PROG}

${PROG}: ${OBJ}
	${CC} -o ${PROG} $^ ${LDLIBS}

%.o: %.c ${DEPS}
	${CC} ${CONFIG} ${CFLAGS} -o $@ -c $<

.PHONY: clean
clean:
	${RM} ${OBJ} ${PROG} $(wildcard *core)

.PHONY: install
install: ${PROG}
	@if [ "$(USER)" != "root" ]; then \
	    echo "You must install ${PROG} as root"; \
	    exit 1; \
	elif [ -d ${PREFIX}/bin ]; then \
	    echo ${INSTLPR}; \
	    ${INSTLPR}; \
	else \
	    echo "${PREFIX}/bin does not exist"; \
	    exit 1; \
	fi

.PHONY: uninstall
uninstall:
	@if [ "$(USER)" != "root" ]; then \
	    echo "You must uninstall ${PROG} as root"; \
	    exit 1; \
	elif [ -f ${PREFIX}/bin/${PROG} -a -x ${PREFIX}/bin/${PROG} ]; then \
	    echo ${RM} ${PREFIX}/bin/${PROG}; \
	    ${RM} ${PREFIX}/bin/${PROG}; \
	fi

.PHONY: install-utils
install-utils:
	@if [ "$(USER)" != "root" ]; then \
	    echo "You must install utils.rsm as root"; \
	    exit 1; \
	elif [ -d ${PREFIX}/share ]; then \
	    echo ${INSTLUT}; \
	    ${INSTLUT}; \
	else \
	    echo "${PREFIX}/share does not exist"; \
	    exit 1; \
	fi

.PHONY: uninstall-utils
uninstall-utils:
	@if [ "$(USER)" != "root" ]; then \
	    echo "You must uninstall utils.rsm as root"; \
	    exit 1; \
	elif [ -d ${PREFIX}/share/rsm ]; then \
	    echo ${RM} -r ${PREFIX}/share/rsm; \
	    ${RM} -r ${PREFIX}/share/rsm; \
	fi

.PHONY: install-docs
install-docs:
	@if [ "$(USER)" != "root" ]; then \
	    echo "You must install documentation as root"; \
	    exit 1; \
	elif [ -d ${PREFIX}/share/doc ]; then \
	    echo ${INSTLDC}; \
	    ${INSTLDC}; \
	    echo gzip -r -f9 ${PREFIX}/share/doc/rsm; \
	    gzip -r -f9 ${PREFIX}/share/doc/rsm; \
	    if [ -d ${PREFIX}/share/man ]; then \
	        echo ${INSTLMN}; \
	        ${INSTLMN}; \
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
	@if [ "$(USER)" != "root" ]; then \
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
