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

CC       := gcc
CFLAGS   += -std=gnu99 -Wall -Wextra -pedantic -fsigned-char -fwrapv
CPPFLAGS := -Iinclude -D_FILE_OFFSET_BITS=64
LDLIBS   := -lcrypt -lm
PROG     := rsm
SRCS     := $(wildcard */*.c)
OBJS     := $(SRCS:.c=.o)
DEPS     := $(wildcard include/*.h)
UTILS    := utils.rsm
DOCS     := $(wildcard doc/adoc/*.adoc)
MAN      := doc/man/rsm.1
RM       := rm -f
GZIP     := gzip -f9
PREFIX   := /usr/local
OS       := $(shell uname)
GIT_SHA  := $(shell git rev-parse --short=10 HEAD 2>/dev/null; true)
INSTALL  := install-default
INSTDOC  := install-docs-default

ifeq ($(OS),Darwin)
    CFLAGS  += -Wno-deprecated-declarations
    LDLIBS  := -lm
    LDFLAGS += -framework CoreServices -framework DirectoryService -framework Security
endif

ifeq ($(OS),AIX)
    LDLIBS  := -lcrypt
    INSTALL := install-aix
    INSTDOC := install-docs-aix
endif

ifeq ($(OS),HP-UX)
    LDLIBS := -lm
endif

ifeq ($(OS),SunOS)
    LDLIBS += -lnsl -lsocket -lrt
endif

ifeq ($(MAKECMDGOALS),debug)
    CFLAGS += -O0 -g3

    ifdef options
        ifeq ($(options),profile)
            CFLAGS  += -pg
            LDLIBS  += -lc
            LDFLAGS += -pg
        else
            ifeq ($(options),sanitize)
                ifeq ($(OS),Linux)
                    CFLAGS  += -fsanitize=address,undefined,leak
                    LDFLAGS += -fsanitize=address,undefined,leak
                else
                    CFLAGS  += -fsanitize=address,undefined
                    LDFLAGS += -fsanitize=address,undefined
                endif
            endif
        endif
    endif
else
    CFLAGS   += -O3
    CPPFLAGS += -DNDEBUG
endif

ifdef GIT_SHA
    CFLAGS += -DGIT_SHA=$(GIT_SHA)
endif

ifdef dbver
    CFLAGS += -DRSM_DBVER=$(dbver)
endif

CFLAGS += $(CPPFLAGS)
LDLIBS += $(LDFLAGS)

.PHONY: all
all: $(PROG)

.PHONY: debug
debug: $(PROG)

$(PROG): $(OBJS)
	$(CC) -o $(PROG) $^ $(LDLIBS)

%.o: %.c $(DEPS)
	$(CC) $(CFLAGS) -o $@ -c $<

.PHONY: clean
clean:
	$(RM) $(OBJS) $(PROG) $(wildcard *core)

.PHONY: install
install: $(INSTALL)

.PHONY: install-default
install-default: $(PROG)
	@echo install -d -m 755 $(PREFIX)/bin; \
	install -d -m 755 $(PREFIX)/bin; \
	echo install -m 755 -s $(PROG) $(PREFIX)/bin; \
	install -m 755 -s $(PROG) $(PREFIX)/bin; \
	echo install -d -m 755 $(PREFIX)/share/$(PROG); \
	install -d -m 755 $(PREFIX)/share/$(PROG); \
	echo install -m 644 $(UTILS) $(PREFIX)/share/$(PROG); \
	install -m 644 $(UTILS) $(PREFIX)/share/$(PROG)

.PHONY: install-aix
install-aix: $(PROG)
	@echo mkdir -p $(PREFIX)/bin; \
	mkdir -p $(PREFIX)/bin; \
	echo install -M 755 -S -f $(PREFIX)/bin $(PROG); \
	install -M 755 -S -f $(PREFIX)/bin $(PROG); \
	echo mkdir -p $(PREFIX)/share/$(PROG); \
	mkdir -p $(PREFIX)/share/$(PROG); \
	echo install -M 644 -f $(PREFIX)/share/$(PROG) $(UTILS); \
	install -M 644 -f $(PREFIX)/share/$(PROG) $(UTILS)

.PHONY: uninstall
uninstall:
	@echo $(RM) -r $(PREFIX)/share/$(PROG); \
	$(RM) -r $(PREFIX)/share/$(PROG); \
	echo $(RM) $(PREFIX)/bin/$(PROG); \
	$(RM) $(PREFIX)/bin/$(PROG)

.PHONY: install-docs
install-docs: $(INSTDOC)

.PHONY: install-docs-default
install-docs-default:
	@echo install -d -m 755 $(PREFIX)/share/doc/$(PROG); \
	install -d -m 755 $(PREFIX)/share/doc/$(PROG); \
	echo install -m 644 $(DOCS) $(PREFIX)/share/doc/$(PROG); \
	install -m 644 $(DOCS) $(PREFIX)/share/doc/$(PROG); \
	echo $(GZIP) -r $(PREFIX)/share/doc/$(PROG); \
	$(GZIP) -r $(PREFIX)/share/doc/$(PROG); \
	echo install -d -m 755 $(PREFIX)/share/man/man1; \
	install -d -m 755 $(PREFIX)/share/man/man1; \
	echo install -m 644 $(MAN) $(PREFIX)/share/man/man1; \
	install -m 644 $(MAN) $(PREFIX)/share/man/man1; \
	if [ "$(OS)" != "SunOS" ]; then \
	    echo $(GZIP) $(PREFIX)/share/man/man1/rsm.1; \
	    $(GZIP) $(PREFIX)/share/man/man1/rsm.1; \
	fi; \
	if command -v mandb >/dev/null; then \
	    echo mandb -q; \
	    mandb -q; \
	fi

.PHONY: install-docs-aix
install-docs-aix:
	@echo mkdir -p $(PREFIX)/share/doc/$(PROG); \
	mkdir -p $(PREFIX)/share/doc/$(PROG); \
	for doc in $(DOCS); do \
	    echo install -M 644 -f $(PREFIX)/share/doc/$(PROG) $${doc}; \
	    install -M 644 -f $(PREFIX)/share/doc/$(PROG) $${doc}; \
	done; \
	echo $(GZIP) -r $(PREFIX)/share/doc/$(PROG); \
	$(GZIP) -r $(PREFIX)/share/doc/$(PROG); \
	echo mkdir -p $(PREFIX)/share/man/man1; \
	mkdir -p $(PREFIX)/share/man/man1; \
	echo install -M 644 -f $(PREFIX)/share/man/man1 $(MAN); \
	install -M 644 -f $(PREFIX)/share/man/man1 $(MAN)

.PHONY: uninstall-docs
uninstall-docs:
	@echo $(RM) $(PREFIX)/share/man/man1/rsm.1*; \
	$(RM) $(PREFIX)/share/man/man1/rsm.1*; \
	if command -v mandb >/dev/null; then \
	    echo mandb -q; \
	    mandb -q; \
	fi; \
	echo $(RM) -r $(PREFIX)/share/doc/$(PROG); \
	$(RM) -r $(PREFIX)/share/doc/$(PROG)
