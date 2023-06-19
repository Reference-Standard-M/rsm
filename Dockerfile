#
# Package:  Reference Standard M
# File:     rsm/Dockerfile
# Summary:  Create an RSM Docker image
#
# David Wicksell <dlw@linux.com>
# Copyright © 2022-2023 Fourth Watch Software LC
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
#
# syntax=docker/dockerfile:1

# LTS release
FROM ubuntu:22.04
MAINTAINER David Wicksell <dlw@linux.com>
USER root

# Install dependencies and upgrade packages
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update && \
    apt-get -qq --no-install-recommends install apt-utils libc6-dev file make gcc vim-nox bash-completion 2>/dev/null && \
    apt-get -qq upgrade && \
    apt-get -qq clean

# The /opt directory is the typical place to install self-contained packages
WORKDIR /opt/rsm
COPY . /opt/rsm

# The 'make install' command below requires USER set to root and SHELL is needed for the MCL to shell out
ENV USER=root SHELL=/bin/bash RSM_DBFILE=/opt/rsm/tst.dat

# Build the rsm executable, install it system-wide, and clean up the working directory
RUN make -j && make install && make clean

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

# Install and compile the local RSM magic file
RUN cp etc/magic $HOME/.magic && cd && file -C -m $HOME/.magic && cd - >/dev/null

# Install the Bash completion script
RUN sed -i '/^#if.*bash_completion/,/^#fi/ s/^#//' /root/.bashrc && \
    mkdir -p /usr/local/share/bash-completion/completions && \
    cp etc/rsm /usr/local/share/bash-completion/completions/

# Open port 80 so that the container can host the RSM Web Server (which defaults to port 80)
EXPOSE 80/tcp

# The default behavior will hold the container open on an RSM direct mode prompt
ENTRYPOINT ["bin/docker"]
