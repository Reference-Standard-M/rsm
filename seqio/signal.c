/*
 * Package: Reference Standard M
 * File:    rsm/seqio/signal.c
 * Summary: module IO - sequential IO signal handling
 *
 * David Wicksell <dlw@linux.com>
 * Copyright © 2020-2024 Fourth Watch Software LC
 * https://gitlab.com/Reference-Standard-M/rsm
 *
 * Based on MUMPS V1 by Raymond Douglas Newman
 * Copyright © 1999-2016
 * https://gitlab.com/Reference-Standard-M/mumpsv1
 *
 * This program is free software: you can redistribute it and/or modify it
 * under the terms of the GNU Affero General Public License (AGPL) as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see https://www.gnu.org/licenses/.
 *
 * SPDX-FileCopyrightText:  © 2020 David Wicksell <dlw@linux.com>
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 *
 * Extended Summary:
 *
 * This module can be viewed as the facility for catching all signals that
 * may be delivered to a process (except SIGSTOP and SIGKILL), in order to
 * handle them internally. It provides the following function:
 *
 *     setSignal -  Sets up specific signals
 *     setSignals - Sets up the system defined set of signals
 */

#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include "seqio.h"

// Local functions

/*
 * This function handles all caught signals.
 *
 * NOTE: Refer to the function "setSignalBitMask" in the file rsm/seqio/util.c
 */
void signalHandler(int sig)                                                     // Caught signal
{
    setSignalBitMask(sig);
}

// Signal functions

/*
 * write(2), read(2), send(2), recv(2), sendto(2), recvfrom(2), sendmsg(2), and
 * recvmsg(2) on a communications channel or a low speed device and during a
 * ioctl(2) or wait(2). However, calls that have already committed are not
 * restarted, but instead return a partial success (for example, a short read
 * count).
 */
int setSignal(int sig, int flag)
{
    struct sigaction action;
    sigset_t         handlermask;
    int              ret;

    sigemptyset(&handlermask);
    sigfillset(&handlermask);
    action.sa_mask = handlermask;
    action.sa_flags = 0;

    if (flag == CATCH) {
        action.sa_handler = signalHandler;
    } else {
        action.sa_handler = SIG_IGN;
    }

    ret = sigaction(sig, &action, NULL);
    if (ret == -1) return getError(SYS, errno);
    return ret;
}

/*
 * This function tells the system that the set of all signals that can be
 * caught should be delivered to the designated signal handler. Upon success,
 * 0 is returned. Otherwise, a negative integer value is returned to indicate
 * the error that has occurred.
 *
 * NOTE: If a signal arrives during one of the primitive operations (i.e., open,
 *       read etc.), the operation will exit with -1, with "errno" set to EINTR
 */
int setSignals(void)
{
    struct sigaction action;                                                    // man 2 sigaction
    sigset_t         handlermask;                                               // man 2 sigaction

    sigemptyset(&handlermask);
    sigfillset(&handlermask);
    action.sa_mask = handlermask;
    action.sa_flags = 0;
    action.sa_handler = SIG_IGN;
    if (sigaction(SIGHUP, &action, NULL) == -1) return getError(SYS, errno);
    action.sa_handler = signalHandler;
    if (sigaction(SIGINT, &action, NULL) == -1) return getError(SYS, errno);
    action.sa_flags = SA_RESTART;
    if (sigaction(SIGQUIT, &action, NULL) == -1) return getError(SYS, errno);
    action.sa_flags = 0;
    action.sa_handler = SIG_DFL;                                                // let UNIX do this one
    if (sigaction(SIGILL, &action, NULL) == -1) return getError(SYS, errno);
    action.sa_handler = signalHandler;
    if (sigaction(SIGTRAP, &action, NULL) == -1) return getError(SYS, errno);
    if (sigaction(SIGABRT, &action, NULL) == -1) return getError(SYS, errno);
    action.sa_handler = SIG_DFL;                                                // let UNIX do this one

#if defined(__FreeBSD__) || defined(__NetBSD__)
    if (sigaction(SIGEMT, &action, NULL) == -1) return getError(SYS, errno);
#endif

    if (sigaction(SIGFPE, &action, NULL) == -1) return getError(SYS, errno);
    if (sigaction(SIGBUS, &action, NULL) == -1) return getError(SYS, errno);
    if (sigaction(SIGSEGV, &action, NULL) == -1) return getError(SYS, errno);   // SIGSEGV is desirable

#if defined(__FreeBSD__) || defined(__NetBSD__)
    if (sigaction(SIGSYS, &action, NULL) == -1) return getError(SYS, errno);
#endif

    action.sa_handler = signalHandler;
    if (sigaction(SIGPIPE, &action, NULL) == -1) return getError(SYS, errno);
    if (sigaction(SIGALRM, &action, NULL) == -1) return getError(SYS, errno);
    if (sigaction(SIGTERM, &action, NULL) == -1) return getError(SYS, errno);
    if (sigaction(SIGURG, &action, NULL) == -1) return getError(SYS, errno);
    action.sa_handler = SIG_DFL;                                                // let UNIX do this one
    if (sigaction(SIGTSTP, &action, NULL) == -1) return getError(SYS, errno);
    if (sigaction(SIGCONT, &action, NULL) == -1) return getError(SYS, errno);

    /* Setting this to SIG_IGN should stop zombies
    action.sa_handler = SIG_IGN;
    if (sigaction(SIGCHLD, &action, NULL) == -1) return getError(SYS, errno);
    action.sa_handler = SIG_DFL;                                                // let UNIX do this one
    */

    if (sigaction(SIGTTIN, &action, NULL) == -1) return getError(SYS, errno);
    if (sigaction(SIGTTOU, &action, NULL) == -1) return getError(SYS, errno);
    action.sa_handler = signalHandler;
    if (sigaction(SIGIO, &action, NULL) == -1) return getError(SYS, errno);
    if (sigaction(SIGXCPU, &action, NULL) == -1) return getError(SYS, errno);
    if (sigaction(SIGXFSZ, &action, NULL) == -1) return getError(SYS, errno);
    if (sigaction(SIGVTALRM, &action, NULL) == -1) return getError(SYS, errno);
    if (sigaction(SIGPROF, &action, NULL) == -1) return getError(SYS, errno);
    action.sa_handler = SIG_IGN;                                                // Ignore for now
    if (sigaction(SIGWINCH, &action, NULL) == -1) return getError(SYS, errno);
    action.sa_handler = signalHandler;

#if defined(__FreeBSD__) || defined(__NetBSD__)
    action.sa_flags = SA_RESTART;
    if (sigaction(SIGINFO, &action, NULL) == -1) return getError(SYS, errno);
    action.sa_flags = 0;
#endif

    if (sigaction(SIGUSR1, &action, NULL) == -1) return getError(SYS, errno);
    if (sigaction(SIGUSR2, &action, NULL) == -1) return getError(SYS, errno);
    return 0;
}
