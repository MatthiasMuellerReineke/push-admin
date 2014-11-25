# -*- coding: utf8 -*-

# Copyright (C) 2011  Matthias Müller-Reineke

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>


import select
from sys import stdin, stderr, stdout
import tty
import termios

from utilities import tunix


class ForwardToStd:
    def __init__(self, host=None):
        self.host = host

    def take_stdout(self, s, peculiarities):
        stdout.write(s)
        # flush makes questions appearing reliably:
        stdout.flush()

    def take_stderr(self, s):
        stderr.write(s)

    def peculiarities(self, chan):
        if stderr.isatty():
            chan.get_pty()
            return Tty()
        else:
            return NoTty()


class AlwaysPrintDestination(ForwardToStd):
    def take_stdout(self, s, peculiarities):
        peculiarities.reset_settings()
        self.host.print_dest()
        peculiarities.manipulate_settings()

        real_take_stdout = ForwardToStd().take_stdout
        real_take_stdout(s, peculiarities)
        self.take_stdout = real_take_stdout


class PrintDestinationForOutput(AlwaysPrintDestination):
    def take_stdout(self, s, peculiarities):
        if s:
             AlwaysPrintDestination.take_stdout(self, s, peculiarities)


class CatchSomeOutput(ForwardToStd):
    def peculiarities(self, chan):
        return NoTty()


class CatchStdout(CatchSomeOutput):
    def __init__(self):
        self.stdout = ''

    def take_stdout(self, s, peculiarities):
        self.stdout += s


class CatcherStderrMsg(CatchSomeOutput):
    def __init__(self, msg):
        self.__msg = msg

    def take_stderr(self, s):
        if s != self.__msg + '\n':
            stderr.write(s)


class CatchStdoutCatcherStderrMsg(CatchStdout, CatcherStderrMsg):
    # Otherwise we would inherit ForwardToStd.take_stderr via CatchStdout:
    take_stderr = CatcherStderrMsg.take_stderr

    def __init__(self, msg):
        CatchStdout.__init__(self)
        CatcherStderrMsg.__init__(self, msg)


class SelectableWrapper:
    eof = False

    def __getattr__(self, name):
        return getattr(self.selectable, name)


class FileWrapper(SelectableWrapper):
    """
    This forwards a file's content to the remote program via Paramiko.
    """

    def __init__(self, f):
        self.selectable = f

    def process(self):
        self.chan.send(self.selectable.read())
        self.chan.shutdown_write()
        self.eof = True

    def append_to_always_ready(self, always_ready):
        always_ready.append(self)

    def append_to_selectables(self, all_selectables):
        pass


class StdinWrapper(SelectableWrapper):
    selectable = stdin

    def process(self):
        r = stdin.read(1)
        if r:
            self.chan.send(r)
        self.eof = not r

    def append_to_always_ready(self, always_ready):
        pass

    def append_to_selectables(self, all_selectables):
        all_selectables.append(self)


class ChanWrapper(SelectableWrapper):
    """
    This recives remote program's output.
    """
    def __init__(self, chan, output_catcher, peculiarities):
        self.chan = chan
        self.output_catcher = output_catcher
        self.selectable = chan
        self.peculiarities = peculiarities

    def process(self):
        chan = self.chan
        s_out = copy_from_chan(chan.recv_ready, chan.recv)
        output_catcher = self.output_catcher
        output_catcher.take_stdout(s_out, self.peculiarities)
        s_err = copy_from_chan(chan.recv_stderr_ready, chan.recv_stderr)
        output_catcher.take_stderr(s_err)
        self.eof = not (s_out or s_err)


def copy_from_chan(recv_ready, recv):
    if recv_ready():
        return recv(1024)
    else:
        return ''


def process_ready_files(all_selectables, always_ready, *timeout):
    ready_for_reading, w, e = select.select(all_selectables,
            [], [], *timeout)
    for r in ready_for_reading + always_ready:
        r.process()
    return ready_for_reading


class NoTty:
    def __getattr__(self, name):
        return tunix


class Tty:
    f = stderr

    def save_settings(self):
        self.oldtty = termios.tcgetattr(self.f)

    def manipulate_settings(self):
        fileno = self.f.fileno()
        tty.setraw(fileno)
        tty.setcbreak(fileno)

    def reset_settings(self):
        termios.tcsetattr(self.f, termios.TCSADRAIN, self.oldtty)

