# Package: Reference Standard M
# File:    GNUmakefile
# Summary: Makefile for Linux, macOS, Solaris, AIX, HP-UX, and RPi
#          See Makefile for FreeBSD, NetBSD, and OpenBSD
#
# SPDX-FileCopyrightText:  © 2020-2026 Fourth Watch Software LC
# SPDX-FileContributor:    David Wicksell <dlw@linux.com>
# SPDX-FileComment:        https://gitlab.com/Reference-Standard-M/rsm
# SPDX-FileComment:        Derived from MUMPS V1 (BSD-3-Clause)
# SPDX-FileComment:        Original work by Raymond Douglas Newman (1999-2018)
# SPDX-License-Identifier: AGPL-3.0-or-later
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

CC       := gcc
CFLAGS   += -MMD -std=c11 -Wall -Wextra -Wpedantic -Wmissing-prototypes -fsigned-char -fwrapv
CPPFLAGS := -Iinclude -D_FILE_OFFSET_BITS=64 -D_DEFAULT_SOURCE
LDLIBS   := -lcrypt -lm
PROG     := rsm
SRCS     := $(wildcard */*.c)
OBJS     := $(SRCS:.c=.o)
DEPS     := $(OBJS:.o=.d)
UTILS    := utils.rsm
DOCS     := $(wildcard doc/adoc/*.adoc)
MAN      := doc/man/$(PROG).1
RM       := rm -f
GZIP     := gzip -f9
OS       := $(shell uname)
DBG_FILE := .debug-build
INSTPGM  := _install-default
INSTDOC  := _install-docs-default
GIT_SHA  := $(shell git rev-parse --short=10 HEAD 2>/dev/null; true)
STRIP     = $(if $(wildcard $(DBG_FILE)),,-s)

ifdef prefix
    PREFIX := $(prefix)
else
    PREFIX := /usr/local
endif

ifeq ($(OS),Darwin)
    CFLAGS  += -Wno-deprecated-declarations
    LDFLAGS += -framework CoreServices -framework DirectoryService -framework Security
    LDLIBS  := -lm
endif

ifeq ($(OS),AIX)
    LDLIBS  := -lcrypt
    INSTPGM := _install-aix
    INSTDOC := _install-docs-aix
    STRIP    = $(if $(wildcard $(DBG_FILE)),,-S)
endif

ifeq ($(OS),HP-UX)
    LDLIBS := -lm
endif

ifeq ($(OS),SunOS)
    CPPFLAGS += -D__EXTENSIONS__
    LDLIBS   += -lnsl -lsocket -lrt
endif

ifeq ($(MAKECMDGOALS),debug)
    CFLAGS += -O0 -g3
else
    CFLAGS   += -O3
    CPPFLAGS += -DNDEBUG
endif

ifdef options
    ifeq ($(options),profile)
        CFLAGS  += -pg
        LDFLAGS += -pg
        LDLIBS  += -lc
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

ifdef SNAPCRAFT_PROJECT_NAME
    BUILD_INFO := Snap
else
    ifdef RSM_DOCKER_BUILDER
        BUILD_INFO := Docker
    endif
endif

ifdef GIT_SHA
    ifdef BUILD_INFO
        CPPFLAGS += -DBUILD_INFO="$(BUILD_INFO) $(GIT_SHA)"
    else
        CPPFLAGS += -DBUILD_INFO=$(GIT_SHA)
    endif
else
    ifdef BUILD_INFO
        CPPFLAGS += -DBUILD_INFO=$(BUILD_INFO)
    endif
endif

ifdef dbver
    CPPFLAGS += -DRSM_DBVER=$(dbver)
endif

.PHONY: all
all: $(PROG)

.PHONY: debug
debug: $(PROG)
	@touch $(DBG_FILE)

$(PROG): $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $^ $(LDLIBS)
	@$(RM) $(DBG_FILE)

-include $(DEPS)

.PHONY: clean
clean:
	$(RM) $(OBJS) $(DEPS) $(PROG) $(DBG_FILE) $(wildcard *core)

.PHONY: install
install: $(INSTPGM)

.PHONY: _install-default
_install-default: uninstall
	$(if $(wildcard $(PROG)),,$(MAKE) $(PROG))
	install -d -m 755 $(DESTDIR)$(PREFIX)/bin
	install -m 755 $(STRIP) $(PROG) $(DESTDIR)$(PREFIX)/bin
	install -d -m 755 $(DESTDIR)$(PREFIX)/share/$(PROG)
	install -m 644 $(UTILS) $(DESTDIR)$(PREFIX)/share/$(PROG)

.PHONY: _install-aix
_install-aix: uninstall
	$(if $(wildcard $(PROG)),,$(MAKE) $(PROG))
	mkdir -p $(DESTDIR)$(PREFIX)/bin
	install -M 755 $(STRIP) -f $(DESTDIR)$(PREFIX)/bin $(PROG)
	mkdir -p $(DESTDIR)$(PREFIX)/share/$(PROG)
	install -M 644 -f $(DESTDIR)$(PREFIX)/share/$(PROG) $(UTILS)

.PHONY: uninstall
uninstall:
	$(RM) -r $(DESTDIR)$(PREFIX)/share/$(PROG)
	$(RM) $(DESTDIR)$(PREFIX)/bin/$(PROG)

.PHONY: install-docs
install-docs: $(INSTDOC)

.PHONY: _install-docs-default
_install-docs-default: uninstall-docs
	install -d -m 755 $(DESTDIR)$(PREFIX)/share/doc/$(PROG)
	install -m 644 $(DOCS) $(DESTDIR)$(PREFIX)/share/doc/$(PROG)
	$(GZIP) -r $(DESTDIR)$(PREFIX)/share/doc/$(PROG)
	install -d -m 755 $(DESTDIR)$(PREFIX)/share/man/man1
	install -m 644 $(MAN) $(DESTDIR)$(PREFIX)/share/man/man1
	@if [ "$(OS)" != "SunOS" ]; then \
	    echo $(GZIP) $(DESTDIR)$(PREFIX)/share/man/man1/$(PROG).1; \
	    $(GZIP) $(DESTDIR)$(PREFIX)/share/man/man1/$(PROG).1; \
	fi
	@if command -v mandb >/dev/null; then \
	    echo mandb -q; \
	    mandb -q; \
	fi

.PHONY: _install-docs-aix
_install-docs-aix: uninstall-docs
	mkdir -p $(DESTDIR)$(PREFIX)/share/doc/$(PROG)
	@for doc in $(DOCS); do \
	    echo install -M 644 -f $(DESTDIR)$(PREFIX)/share/doc/$(PROG) $${doc}; \
	    install -M 644 -f $(DESTDIR)$(PREFIX)/share/doc/$(PROG) $${doc}; \
	done
	$(GZIP) -r $(DESTDIR)$(PREFIX)/share/doc/$(PROG)
	mkdir -p $(DESTDIR)$(PREFIX)/share/man/man1
	install -M 644 -f $(DESTDIR)$(PREFIX)/share/man/man1 $(MAN)

.PHONY: uninstall-docs
uninstall-docs:
	$(RM) $(DESTDIR)$(PREFIX)/share/man/man1/$(PROG).1*
	@if command -v mandb >/dev/null; then \
	    echo mandb -q; \
	    mandb -q; \
	fi
	$(RM) -r $(DESTDIR)$(PREFIX)/share/doc/$(PROG)
