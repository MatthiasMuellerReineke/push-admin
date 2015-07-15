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
from subprocess import PIPE, CalledProcessError
import tty
import termios

from utilities import tunix


class ForwardToStd(object):
    remotes_stdout = PIPE

    def __init__(self, host=None):
        self.host = host
        self.tty = Tty()

    def take_stdout(self, s, peculiarities):
        self.pre_output(s, peculiarities)
        stdout.write(s)
        # flush makes questions appearing reliably:
        stdout.flush()

    def take_stderr(self, s, peculiarities):
        self.pre_output(s, peculiarities)
        stderr.write(s)

    @property
    def allocate_tty(self):
        return self.peculiarities().allocate_tty

    def peculiarities(self):
        if stderr.isatty():
            return self.tty
        else:
            return NoTty()

    pre_output = tunix


class AlwaysPrintDestination(ForwardToStd):
    def print_dest(self, s, peculiarities):
        peculiarities.reset_settings()
        self.host.print_dest()
        peculiarities.manipulate_settings()

    pre_output = print_dest


class PrintDestinationForOutput(AlwaysPrintDestination):
    def pre_output(self, s, peculiarities):
        if s:
             self.print_dest(s, peculiarities)


class CatchSomeOutput(ForwardToStd):
    def peculiarities(self):
        return NoTty()


class CatchStdout(CatchSomeOutput):
    def __init__(self):
        self.stdout = ''

    def take_stdout(self, s, peculiarities):
        self.stdout += s


class CatcherStderrMsg(CatchSomeOutput):
    def __init__(self, msg):
        self.__buffer = MatchBuffer(msg + '\n')

    def take_stderr(self, s, peculiarities):
        stderr.write(self.__buffer.buffer_value(s))


class MatchBuffer:
    def __init__(self, to_be_matched):
        self.to_be_matched = to_be_matched
        self.to_be_matched_pos = 0
        self.values = ''

    def buffer_value(self, value):
        ret = ''
        self.values += value
        for c in self.values:
            if c == self.to_be_matched[self.to_be_matched_pos]:
                self.to_be_matched_pos += 1
                if self.to_be_matched_pos >= len(self.to_be_matched):
                    self.to_be_matched_pos = 0
            else:
                ret += self.to_be_matched[:self.to_be_matched_pos] + c
                self.to_be_matched_pos = 0
        self.values = ''
        return ret


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
    This forwards a file's content to the remote program via ssh.
    """

    def __init__(self, f):
        self.selectable = f
        self.remotes_stdin = PIPE

    def process(self):
        self.chan.write(self.selectable.read())
        self.chan.close()
        self.eof = True

    def append_to_always_ready(self, always_ready):
        always_ready.append(self)

    def append_to_selectables(self, all_selectables):
        pass


class StdinWrapper(SelectableWrapper):
    selectable = stdin
    remotes_stdin = PIPE

    def process(self):
        r = stdin.read()
        if r:
            self.chan.write(r)
        self.eof = not r

    def append_to_always_ready(self, always_ready):
        pass

    def append_to_selectables(self, all_selectables):
        all_selectables.append(self)


class StdWrapper(SelectableWrapper):
    """
    This recives remote program's output.
    """

    def __init__(self, chan, output_catcher, take):
        self.selectable = chan
        self.output_catcher = output_catcher
        self.take = take

    def process(self):
        out = self.selectable.read()
        self.take(out, self.output_catcher.peculiarities())
        self.eof = not out

    def fileno(self):
        return self.selectable.fileno()


def communicate_with_child(process, output_catcher, remotes_stdin,
        assert_condition, cmd):
    peculiarities = output_catcher.peculiarities()

    all_selectables = [
            StdWrapper(process.stdout, output_catcher,
                output_catcher.take_stdout),
            StdWrapper(process.stderr, output_catcher,
                output_catcher.take_stderr),
        ]
    always_ready = []
    remotes_stdin.chan = process.stdin
    remotes_stdin.append_to_always_ready(always_ready)
    remotes_stdin.append_to_selectables(all_selectables)
    peculiarities.save_settings()
    try:
        peculiarities.manipulate_settings()
        exit_code = None
        while True:
            if any([not o.eof for o in always_ready]):
                process_ready_files(all_selectables, always_ready, 0)
            else:
                process_ready_files(all_selectables, always_ready)
            assert_condition()
            exit_code = process.poll()
            if exit_code:
                raise CalledProcessError(exit_code, cmd)
            if exit_code == 0:
                process_ready_files(all_selectables, always_ready)
                break
    finally:
        peculiarities.reset_settings()


def process_ready_files(all_selectables, always_ready, *timeout):
    ready_for_reading, w, e = select.select(all_selectables,
            [], [], *timeout)
    for r in ready_for_reading + [o for o in always_ready if not o.eof]:
        r.process()
    return ready_for_reading


class NoTty:
    allocate_tty = []

    def __getattr__(self, name):
        return tunix


class Tty:
    f = stderr
    allocate_tty = ['-qtt']

    def save_settings(self):
        self.oldtty = termios.tcgetattr(self.f)

    def manipulate_settings(self):
        fileno = self.f.fileno()
        tty.setraw(fileno)
        tty.setcbreak(fileno)

    def reset_settings(self):
        termios.tcsetattr(self.f, termios.TCSADRAIN, self.oldtty)
