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

CC       := gcc
CFLAGS   := -std=gnu99 -Wall -Wextra -fsigned-char -fwrapv
CPPFLAGS := -Iinclude
LDLIBS   := -lcrypt -lm
PROG     := rsm
SRCS     != ls */*.c
OBJS     := $(SRCS:.c=.o)
DEPS     := include/*.h
UTILS    := utils.rsm
DOCS     := doc/adoc/*.adoc
MAN      := doc/man/rsm.1
RM       := rm -f
GZIP     := gzip -f9
PREFIX   := /usr/local
OS       != uname
GIT_SHA  != git rev-parse --short=10 HEAD 2>/dev/null; true

.if ($(OS) == OpenBSD)
    LDLIBS := -lm
.endif

.ifmake debug
    CFLAGS += -O0 -g3

.   ifdef options
.       if ($(options) == profile)
            CFLAGS  += -pg
            LDLIBS  += -lc
            LDFLAGS := -pg
.       elif ($(options) == sanitize)
            CFLAGS  += -fsanitize=address,undefined
            LDFLAGS := -fsanitize=address,undefined
.       endif
.   endif
.else
    CFLAGS   += -O3
    CPPFLAGS += -DNDEBUG
.endif

.if ($(GIT_SHA) != "")
    CPPFLAGS += -DGIT_SHA=$(GIT_SHA)
.endif

.ifdef dbver
    CPPFLAGS += -DRSM_DBVER=$(dbver)
.endif

CFLAGS += $(CPPFLAGS)
LDLIBS += $(LDFLAGS)

.PHONY: all
all: $(PROG)

.PHONY: debug
debug: $(PROG)

$(PROG): $(OBJS)
	$(CC) -o $(PROG) $(OBJS) $(LDLIBS)

.c.o: $(DEPS)
	$(CC) $(CFLAGS) -o $@ -c $<

.PHONY: clean
clean:
	$(RM) $(OBJS) $(PROG) $(wildcard $(PROG).core)

.PHONY: install
install: $(PROG)
	@if [ "$${USER}" != "root" ]; then \
	    echo "You must install $(PROG) and $(UTILS) as root"; \
	    exit 1; \
	else \
	    echo install -d -o 0 -g 0 -m 755 $(PREFIX)/bin; \
	    install -d -o 0 -g 0 -m 755 $(PREFIX)/bin; \
	    echo install -o 0 -g 0 -m 755 -s $(PROG) $(PREFIX)/bin; \
	    install -o 0 -g 0 -m 755 -s $(PROG) $(PREFIX)/bin; \
	    echo install -d -o 0 -g 0 -m 755 $(PREFIX)/share/rsm; \
	    install -d -o 0 -g 0 -m 755 $(PREFIX)/share/rsm; \
	    echo install -o 0 -g 0 -m 644 $(UTILS) $(PREFIX)/share/rsm; \
	    install -o 0 -g 0 -m 644 $(UTILS) $(PREFIX)/share/rsm; \
	fi

.PHONY: uninstall
uninstall:
	@if [ "$${USER}" != "root" ]; then \
	    echo "You must uninstall $(PROG) and $(UTILS) as root"; \
	    exit 1; \
	else \
	    echo $(RM) -r $(PREFIX)/share/rsm; \
	    $(RM) -r $(PREFIX)/share/rsm; \
	    echo $(RM) $(PREFIX)/bin/$(PROG); \
	    $(RM) $(PREFIX)/bin/$(PROG); \
	fi

.PHONY: install-docs
install-docs:
	@if [ "$${USER}" != "root" ]; then \
	    echo "You must install documentation as root"; \
	    exit 1; \
	else \
	    echo install -d -o 0 -g 0 -m 755 $(PREFIX)/share/doc/rsm; \
	    install -d -o 0 -g 0 -m 755 $(PREFIX)/share/doc/rsm; \
	    echo install -o 0 -g 0 -m 644 $(DOCS) $(PREFIX)/share/doc/rsm; \
	    install -o 0 -g 0 -m 644 $(DOCS) $(PREFIX)/share/doc/rsm; \
	    echo $(GZIP) -r $(PREFIX)/share/doc/rsm; \
	    $(GZIP) -r $(PREFIX)/share/doc/rsm; \
	    echo install -d -o 0 -g 0 -m 755 $(PREFIX)/man/man1; \
	    install -d -o 0 -g 0 -m 755 $(PREFIX)/man/man1; \
	    echo install -o 0 -g 0 -m 644 $(MAN) $(PREFIX)/man/man1; \
	    install -o 0 -g 0 -m 644 $(MAN) $(PREFIX)/man/man1; \
	    echo $(GZIP) $(PREFIX)/man/man1/rsm.1; \
	    $(GZIP) $(PREFIX)/man/man1/rsm.1; \
	    if command -v makewhatis >/dev/null; then \
	        echo makewhatis; \
	        makewhatis; \
	    fi \
	fi

.PHONY: uninstall-docs
uninstall-docs:
	@if [ "$${USER}" != "root" ]; then \
	    echo "You must uninstall documentation as root"; \
	    exit 1; \
	else \
	    echo $(RM) $(PREFIX)/man/man1/rsm.1*; \
	    $(RM) $(PREFIX)/man/man1/rsm.1*; \
	    echo $(RM) -r $(PREFIX)/share/doc/rsm; \
	    $(RM) -r $(PREFIX)/share/doc/rsm; \
	fi
