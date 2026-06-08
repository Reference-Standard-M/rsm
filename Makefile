# Package: Reference Standard M
# File:    Makefile
# Summary: Makefile for FreeBSD, NetBSD, and OpenBSD
#          See GNUmakefile for Linux, macOS, Solaris, AIX, HP-UX, and RPi
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

CC       ?= cc
CFLAGS   += -std=c11 -Wall -Wextra -Wpedantic -Wmissing-prototypes -fsigned-char -fwrapv
CPPFLAGS := -Iinclude
LDLIBS   := -lcrypt -lm
PROG     := rsm
SRCS     != ls */*.c
OBJS     := $(SRCS:.c=.o)
DEPS     != ls include/*.h
UTILS    := utils.rsm
DOCS     != ls doc/adoc/*.adoc
MAN      := doc/man/$(PROG).1
RM       := rm -f
GZIP     := gzip -f9
OS       != uname
DBG_FILE := .debug-build
GIT_SHA  != git rev-parse --short=10 HEAD 2>/dev/null || true

.ifdef prefix
    PREFIX := $(prefix)
.else
    PREFIX := /usr/local
.endif

.if ($(OS) == OpenBSD)
    LDLIBS := -lm
.endif

.ifmake debug
    CFLAGS += -O0 -g3
.else
    CFLAGS   += -O3
    CPPFLAGS += -DNDEBUG
.endif

.ifdef options
.   if ($(options) == profile)
        CFLAGS  += -pg
        LDLIBS  += -lc
        LDFLAGS += -pg
.   elif ($(options) == sanitize)
        CFLAGS  += -fsanitize=address,undefined
        LDFLAGS += -fsanitize=address,undefined
.   endif
.endif

.ifdef SNAPCRAFT_PROJECT_NAME
    BUILD_INFO := Snap
.elif defined(RSM_DOCKER_BUILDER)
    BUILD_INFO := Docker
.endif

.ifdef GIT_SHA
.   ifdef BUILD_INFO
        CPPFLAGS += -DBUILD_INFO="$(BUILD_INFO) $(GIT_SHA)"
.   else
        CPPFLAGS += -DBUILD_INFO=$(GIT_SHA)
.   endif
.else
.   ifdef BUILD_INFO
        CPPFLAGS += -DBUILD_INFO=$(BUILD_INFO)
.   endif
.endif

.ifdef dbver
    CPPFLAGS += -DRSM_DBVER=$(dbver)
.endif

.if exists($(DBG_FILE))
    STRIP :=
.else
    STRIP := -s
.endif


.PHONY: all
all: $(PROG)

.PHONY: debug
debug: $(PROG)
	@touch $(DBG_FILE)

$(PROG): $(OBJS)
	$(CC) $(LDFLAGS) -o $(PROG) $(OBJS) $(LDLIBS)
	@$(RM) $(DBG_FILE)

.c.o: $(DEPS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $@ -c $<

.PHONY: clean
clean:
	$(RM) $(OBJS) $(PROG) $(DBG_FILE) $(PROG).core

.PHONY: install
install: uninstall
	@if [ ! -f $(PROG) ]; then \
	    ${MAKE} $(PROG); \
	fi
	install -d -m 755 $(DESTDIR)$(PREFIX)/bin
	install -m 755 $(STRIP) $(PROG) $(DESTDIR)$(PREFIX)/bin
	install -d -m 755 $(DESTDIR)$(PREFIX)/share/$(PROG)
	install -m 644 $(UTILS) $(DESTDIR)$(PREFIX)/share/$(PROG)

.PHONY: uninstall
uninstall:
	$(RM) -r $(DESTDIR)$(PREFIX)/share/$(PROG)
	$(RM) $(DESTDIR)$(PREFIX)/bin/$(PROG)

.PHONY: install-docs
install-docs: uninstall-docs
	install -d -m 755 $(DESTDIR)$(PREFIX)/share/doc/$(PROG)
	install -m 644 $(DOCS) $(DESTDIR)$(PREFIX)/share/doc/$(PROG)
	$(GZIP) -r $(DESTDIR)$(PREFIX)/share/doc/$(PROG)
	install -d -m 755 $(DESTDIR)$(PREFIX)/share/man/man1
	install -m 644 $(MAN) $(DESTDIR)$(PREFIX)/share/man/man1
	$(GZIP) $(DESTDIR)$(PREFIX)/share/man/man1/$(PROG).1
	@if command -v makewhatis >/dev/null; then \
	    echo makewhatis $(DESTDIR)$(PREFIX)/share/man; \
	    makewhatis $(DESTDIR)$(PREFIX)/share/man; \
	fi

.PHONY: uninstall-docs
uninstall-docs:
	$(RM) $(DESTDIR)$(PREFIX)/share/man/man1/$(PROG).1*
	@if command -v makewhatis >/dev/null; then \
	    echo makewhatis $(DESTDIR)$(PREFIX)/share/man; \
	    makewhatis $(DESTDIR)$(PREFIX)/share/man; \
	fi
	$(RM) -r $(DESTDIR)$(PREFIX)/share/doc/$(PROG)
