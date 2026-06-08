# Package: Reference Standard M
# File:    Dockerfile
# Summary: Create an RSM Docker image
#
# SPDX-FileCopyrightText:  © 2022-2026 Fourth Watch Software LC
# SPDX-FileContributor:    David Wicksell <dlw@linux.com>
# SPDX-FileComment:        https://gitlab.com/Reference-Standard-M/rsm
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
#
#
# syntax=docker/dockerfile:1

# Stage 1: Builder
FROM ubuntu:26.04 AS builder
USER root

# Install build dependencies
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update && apt-get -qq --no-install-recommends install libc6-dev libcrypt-dev file gcc clang make git

# The /opt directory is the typical place to install self-contained packages
WORKDIR /opt/rsm
COPY . /opt/rsm

# The 'make install' command below requires USER set to root
ENV USER=root RSM_DBFILE=/opt/rsm/tst.dat

# Build the rsm executable, install it system-wide for $PATH, and clean up the working directory
# Passing '--build-arg CC=clang' will use clang instead of gcc to compile RSM
ARG CC=gcc
RUN make CC=$CC RSM_DOCKER_BUILDER=rsm -j $(nproc) && make install && make clean

# Setup the environment and configure the database
# The 'bsize' and 'blocks' arguments can be passed to 'docker build...' via '--build-arg bsize=<bsize> --build-arg blocks=<blocks>'
# Journaling can be turned on by passing 'journal' to 'docker build...' via '--build-arg journal=on'
ARG bsize=16 blocks=16384 journal=off

# Create the database and load the vendor utility routines and turn on journaling if requested
RUN if [ "$journal" = "on" ]; \
    then \
        rsm -v TST -b $bsize -s $blocks; \
        rsm -j 1; \
        rsm -x 'open 1:("utils.rsm":"read") use 1 read code xecute code'; \
        rsm -x 'set ^$system("vol",1,"journal_file")="/opt/rsm/tst.jnl"'; \
        rsm -x 'set ^$system("vol",1,"journal_requested")=1'; \
        rsm -x 'set ^$global("$GLOBAL","journal")=1'; \
        rsm -k; \
    else \
        rsm -v TST -b $bsize -s $blocks; \
        rsm -j 1; \
        rsm -x 'open 1:("utils.rsm":"read") use 1 read code xecute code'; \
        rsm -k; \
    fi

# Install the Bash completion script
RUN mkdir -p /usr/local/share/bash-completion/completions && cp etc/rsm /usr/local/share/bash-completion/completions/

# Stage 2: Final image
FROM ubuntu:26.04 AS final
LABEL com.fourthwatchsoftware.vendor="Fourth Watch Software LC" \
      com.fourthwatchsoftware.maintainer="David Wicksell <dlw@linux.com>" \
      com.fourthwatchsoftware.description="Reference Standard M Docker Image" \
      com.fourthwatchsoftware.version="1.83.0" \
      com.fourthwatchsoftware.licenses="AGPL-3.0-or-later" \
      com.fourthwatchsoftware.url="https://gitlab.com/Reference-Standard-M/rsm"

USER root

# Copy artifacts from builder
COPY --from=builder /usr/local /usr/local
COPY --from=builder /opt/rsm/log /opt/rsm/log
COPY --from=builder /opt/rsm/bin/docker /opt/rsm/bin/
COPY --from=builder /opt/rsm/etc/magic /opt/rsm/etc/
COPY --from=builder /opt/rsm/tst.* /opt/rsm/

# Install runtime dependencies and clean up
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update && apt-get -qq --no-install-recommends install file vim-nox bash-completion && \
    apt-get -qq clean && rm -rf /var/lib/apt/lists/*

# Start from here for bin/docker
WORKDIR /opt/rsm

# SHELL is needed for the MCL to shell out with an OS command
ENV SHELL=/bin/bash RSM_DBFILE=/opt/rsm/tst.dat

# Install and compile the local RSM magic file, and turn Bash completion on
RUN cp etc/magic $HOME/.magic && cd && file -C -m $HOME/.magic && cd - >/dev/null && \
    sed -i '/^#if.*bash_completion/,/^#fi/ s/^#//' /root/.bashrc

# Open port 80 so that the container can host the RSM Web Server (which defaults to port 80)
EXPOSE 80/tcp

# The default behavior will hold the container open on an RSM direct mode prompt
ENTRYPOINT ["bin/docker"]
