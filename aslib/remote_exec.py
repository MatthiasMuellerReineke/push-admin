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
        if stdout.isatty():
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


class StdWrapper:
    """
    This recives remote program's output.
    """

    eof = False

    def __init__(self, chan, output_catcher, take):
        self.selectable = chan
        self.output_catcher = output_catcher
        self.take = take

    def __getattr__(self, name):
        return getattr(self.selectable, name)

    def process(self, read_size=-1):
        out = self.selectable.read(read_size)
        self.take(out, self.output_catcher.peculiarities())

    def fileno(self):
        return self.selectable.fileno()


def communicate_with_child(process, output_catcher,
        assert_condition, cmd):
    peculiarities = output_catcher.peculiarities()

    all_selectables = [
            StdWrapper(process.stdout, output_catcher,
                output_catcher.take_stdout),
            StdWrapper(process.stderr, output_catcher,
                output_catcher.take_stderr),
        ]
    peculiarities.save_settings()
    try:
        while True:
            process_ready_files(all_selectables, 1)
            assert_condition()
            exit_code = process.poll()
            if exit_code:
                raise CalledProcessError(exit_code, cmd)
            if exit_code == 0:
                process_ready_files(all_selectables, -1)
                break
    finally:
        peculiarities.reset_settings()


def process_ready_files(all_selectables, read_size):
    ready_for_reading, w, e = select.select(all_selectables, [], [])
    assert not w
    assert not e
    for r in ready_for_reading:
        r.process(read_size)


class NoTty:
    allocate_tty = []

    def __getattr__(self, name):
        return tunix


class Tty:
    f = stdout
    allocate_tty = ['-qtt']

    def save_settings(self):
        self.oldtty = termios.tcgetattr(self.f)

    def manipulate_settings(self):
        fileno = self.f.fileno()
        tty.setraw(fileno)
        tty.setcbreak(fileno)

    def reset_settings(self):
        termios.tcsetattr(self.f, termios.TCSADRAIN, self.oldtty)
