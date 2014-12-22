# -*- coding: iso-8859-1 -*-

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


"""Python-Code, der auch außerhalb des VWS verwendet wird
(zur Zeit wird er auch in autoinstallation (Autoinstallationssystem
für Redhat-Linux und CentOS) verwendet).

Dieser Code muss ohne das VWS funktionieren!"""
from compiler.misc import mangle
from functools import partial
from os import makedirs
from os.path import isdir, isfile
from tempfile import mkdtemp
import time
from atexit import register
from shutil import rmtree
import unittest

import termcolor 


def colored(output, text, *args, **kwargs):
    if output.isatty():
        return termcolor.colored(text, *args, **kwargs)
    else:
        return text


def mkdir_p(path):
    if path and not isdir(path):
        makedirs(path)


def object_from_option(parser, name, objekt_wenn_gesetzt, default,
        dest=None):
    parser.add_argument('--' + name, action='store_const',
            const=objekt_wenn_gesetzt, default=default, dest=dest)


def tunix(*args, **kwargs):
    pass


class memoized(object):
   """Decorator that caches a function's return value each time it is called.
   If called later with the same arguments, the cached value is returned, and
   not re-evaluated.
   (von http://wiki.python.org/moin/PythonDecoratorLibrary#Memoize)
   """
   def __init__(self, func):
      self.func = func
      self.cache = {}
   def __call__(self, *args):
      try:
         return self.cache[args]
      except KeyError:
         self.cache[args] = value = self.func(*args)
         return value
      except TypeError:
         # uncachable -- for instance, passing a list as an argument.
         # Better to not cache than to blow up entirely.
         return self.func(*args)
   def __repr__(self):
      """Return the function's docstring."""
      return self.func.__doc__


class memoize(object):
    """cache the return value of a method
    
    This class is meant to be used as a decorator of methods. The return value
    from a given method invocation will be cached on the instance whose method
    was invoked. All arguments passed to a method decorated with memoize must
    be hashable.
    
    If a memoized method is invoked directly on its class the result will not
    be cached. Instead the method will be invoked like a static method:
    class Obj(object):
        @memoize
        def add_to(self, arg):
            return self + arg
    Obj.add_to(1) # not enough arguments
    Obj.add_to(1, 2) # returns 3, result is not cached

    From http://code.activestate.com/recipes/577452-a-memoize-decorator-for-instance-methods
    """
    def __init__(self, func):
        self.func = func
    def __get__(self, obj, objtype=None):
        if obj is None:
            return self.func
        return partial(self, obj)
    def __call__(self, *args, **kw):
        obj = args[0]
        try:
            cache = obj.__dict__[mangle('__cache', self.__class__.__name__)]
        except KeyError:
            cache = obj.__cache = {}
        key = (self.func, args[1:], frozenset(kw.items()))
        try:
            res = cache[key]
        except KeyError:
            res = cache[key] = self.func(*args, **kw)
        return res


def instantiate(cls):
    return cls()


def unittest_really_quiet():
    unittest.main(testRunner=TestRunner(verbosity=0))


class TestRunner(unittest.TextTestRunner):
    def run(self, test):
        """Aus unittest.TextTestRunner.run kopiert und Ausgaben im
        Nichtfehlerfall entfernt."""
        result = self._makeResult()
        startTime = time.time()
        test(result)
        stopTime = time.time()
        timeTaken = stopTime - startTime
        result.printErrors()
        run = result.testsRun
        if not result.wasSuccessful():
            self.stream.writeln(result.separator2)
            self.stream.writeln("Ran %d test%s in %.3fs" %
                                (run, run != 1 and "s" or "", timeTaken))
            self.stream.writeln()
            self.stream.write("FAILED (")
            failed, errored = map(len, (result.failures, result.errors))
            if failed:
                self.stream.write("failures=%d" % failed)
            if errored:
                if failed: self.stream.write(", ")
                self.stream.write("errors=%d" % errored)
            self.stream.writeln(")")
        return result


def ensure_contains(file_name, content):
    if not isfile(file_name) or content not in file_content(file_name):
        with open(file_name, 'a') as f:
            f.write(content)


def file_content(file_name):
    return open(file_name).read()


def write(file_name, content):
    with open(file_name, 'w') as f:
        f.write(content)


def on_exit_vanishing_dtemp(*args, **kwargs):
    try:
        tmp_dir = mkdtemp(*args, **kwargs)
    except OSError as e:
        raise NoMkdir(e)
    register(rmtree, tmp_dir)
    return tmp_dir


class NoMkdir(Exception):
    pass
