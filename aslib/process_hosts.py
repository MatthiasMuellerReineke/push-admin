#!/usr/bin/env python
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


from __future__ import print_function
import sys
from sys import stdout, stderr
from os.path import join, splitext, dirname, isdir, exists
from time import sleep
from tempfile import NamedTemporaryFile
from subprocess import check_call, PIPE, CalledProcessError
from shutil import copy
from argparse import ArgumentParser
from difflib import unified_diff

from utilities import object_from_option, colored, tunix,\
         write, file_content, mkdir_p

from remote_exec import ForwardToStd, FileWrapper
import predefined
# TODO: Alles, was hier importiert wird, sollte von predefined in
# andere Text(e). DontTouch und Override sind ausgenommen;
# vielleicht auch EmulationFunction.
from predefined import TargetNotAvailable, StderrCatcher, walk_files,\
         strip_leading_slash,\
         DontTouch, Override, FromExaminationOfSystem, EmulationFunction
from aslib import package

sys.path.insert(0, '.') # Search in . for hosts.py .
import hosts as site_configuration


def diff(a, b, a_filename, out=stdout, diffpdf=tunix):
    def diff_bin(diff_bin_func):
        if a != b:
            if a:
                explanation = not_printable_diff_text
                diff_bin_func()
            else:
                explanation = make_explanation('is empty')
            out.write(filename_prepared_for_output + explanation)

    def diff_bin_tunix():
        diff_bin(tunix)

    filename_prepared_for_output = colored(out, a_filename,
            on_color='on_yellow')

    {'pdf': lambda: diff_bin(diffpdf),
     'war': diff_bin_tunix, 'so': diff_bin_tunix,
    }.get(splitext(a_filename)[1][1:], lambda:
        out.write(''.join(map(lambda x: x + '\n',
                    unified_diff(a.splitlines(),
                        # \r must be stripped from windows files:
                        map(lambda x: x.rstrip('\r'), b.splitlines()),
                        filename_prepared_for_output))
                ))
    )()


def make_explanation(explanation):
    return '\n{0}.\n\n'.format(explanation)


not_printable_diff_text = make_explanation('differs and is not printable')


class RunMode:
    file_list_dir = 'file_lists'

    def __init__(self, all_object):
        self.all_object = all_object
        self.file_list = set()

    def rsync_additional_trees(self):
        # In Revision 327 wurde zu letzt rsync statt cp für die
        # Versionierten Bäume verwendet.
        for x in self.all_object.accumulate('additional_trees'):
            check_call(
            'rsync --verbose --recursive -i --times --omit-dir-times '
            + self.rsync_options
            + x[0] + '/ ' + self.in_remote_root(x[1]) + '/', shell=True)

    def conditional_cmds(self, conditional_cmds):
        for x in conditional_cmds:
            for c in x.commands():
                self.rcmd(c, x.output_catcher)

    def rcmds(self, cmds):
        for x in cmds:
            self.rcmd(x)

    def in_remote_root(self, dest_file):
        return join(self.all_object.remote_root(),
                strip_leading_slash(dest_file))

    def append_dir_tree_to_file_list(self, dir_tree):
        walk_files(dir_tree, self.append_to_file_list)

    def append_to_file_list(self, dest_file, full_path=None):
        self.file_list.add(dest_file)

    def file_list_file_name(self):
        return join(self.file_list_dir, self.all_object.name)


class DryRun(RunMode):
    rsync_options = '--dry-run '

    def do_file_list(self, out=stdout):
        file_list_file_name = self.file_list_file_name()
        if exists(file_list_file_name):
            stored_file_list = file_content(file_list_file_name
                    ).splitlines()
        else:
            stored_file_list = []
        cease_to_be_managed = set(stored_file_list) - self.file_list
        if cease_to_be_managed:
            out.write(colored(out,
                        'These files cease to be managed by this system:',
                        on_color='on_red') + '\n'
                    + '\n'.join(sorted(cease_to_be_managed)) + '\n')

    def store_remote(self, dest_file, content, manipulate):
        self.diff(content, dest_file)
        manipulate.execute_dry(self.in_remote_root(dest_file), dest_file)
        self.append_to_file_list(dest_file)

    def rsync(self, dir_tree):
        walk_files(dir_tree, lambda dest_file, full_path:
                # strip leading '/' for get_remote:
                self.diff(open(full_path).read(), dest_file)
                )
        self.append_dir_tree_to_file_list(dir_tree)

    def rcmd(self, cmd, output_catcher=None):
        print(colored(stdout, cmd, 'red'))

    def separate_output(self):
        print()

    def diff(self, b, dest_file):
        with NamedTemporaryFile(suffix='.pdf') as new:
            new.write(b)
            new.flush()
            # TODO: Test which executes the lambda:
            diff(self.all_object.get_remote(dest_file), b, dest_file,
                    diffpdf=
                    lambda: self.all_object.options.diffpdf(
                        self.in_remote_root(dest_file),
                        new.name))


class RealRun(RunMode):
    rsync_options = ''

    def do_file_list(self):
        mkdir_p(self.file_list_dir)
        write(self.file_list_file_name(),
                '\n'.join(sorted(self.file_list)) + '\n')

    def store_remote(self, dest_file, content, manipulate):
        usable_path = self.in_remote_root(dest_file)
        if self.all_object.get_remote(dest_file) != content:
            mkdir_p(dirname(usable_path))
            write(usable_path, content)
        manipulate.execute(usable_path)
        self.append_to_file_list(dest_file)

    def rsync(self, dir_tree, remote_dir=''):
        # cp -r keeps permissions and ownerships of already existing files:
        check_call('cp -r ' + join(dir_tree, '*') + ' ' +
            self.in_remote_root(remote_dir), shell=True)
        # XXX: I suspect that ^this and immediate power off of the target
        # system led to truncation of some affected files to length 0!
        self.append_dir_tree_to_file_list(dir_tree)

    def rcmd(self, cmd, output_catcher=ForwardToStd()):
        self.all_object.ssh(cmd, output_catcher)

    def separate_output(self):
        pass


def classes_from_examination_of_running_system(system_object):
    get_remote_file = system_object.get_remote_file
    try:
        f = get_remote_file('etc/os-release')
    except IOError:
        pretty_name = system_object.get_remote('etc/redhat-release')
    else:
        pretty_name = get_pretty_name(f)
    return classes_from_names(pretty_name.splitlines()[0].split()) + [
            get_conf_attr_from_str0(system_object.ssh_silent_stdout(
                        '/usr/bin/perl',
                        remotes_stdin=FileWrapper(open(join(package, 'imvirt')))))
           ]


def get_pretty_name(f):
    for l in f:
        name, value = l.split('=')
        if name == 'PRETTY_NAME':
            return value[1:-2]


def get_conf_attr_from_str0(s):
    try:
        return conf_attr_from_words(s.splitlines()[0].split())
    except IndexError, e:
        stderr.write('get_conf_attr_from_str0 got: ' + s + '\n')
        raise


def conf_attr_from_words(words):
    return get_conf_attr(words[0])


def classes_from_names(words):
    dist_name = words[0]
    for x in words:
        first_char = x[0]
        if first_char.isdigit():
            break
    dist_class = get_conf_attr(dist_name)
    classes = [dist_class, get_conf_attr(dist_name, first_char)]
    dist_class_is_emulation_function = isinstance(dist_class,
            EmulationFunction)
    if (dist_class_is_emulation_function
            and dist_class.super_class_name == 'El')\
        or (not dist_class_is_emulation_function
                and issubclass(dist_class, get_conf_attr('El'))):
        return [get_conf_attr('El', first_char)] + classes
    else:
        return classes


def get_conf_attr(name, release_major=''):
    if release_major:
        # In this case name is the name of a Linux distribution
        # and we are asked for the release class.
        # Below we create such a class when no suitable class exists.
        # But we only want to do that when name is the name of an existing
        # distribution class:
        get_conf_attr(name)
        name += release_major
    try:
        return site_configuration.__dict__[name]
    except KeyError:
        try:
            return predefined.__dict__[name]
        except KeyError:
            if release_major:
                return type(name, (FromExaminationOfSystem,),
                        {'release_major': release_major})
            else:
                raise


def options_from_runmode(runmode):
    class Options:
        run_mode = runmode
        diffpdf = tunix
    return Options


def hosts(distribution, options=options_from_runmode(DryRun)):
    h = site_configuration.hosts
    for x in h:
        x.init(distribution, options, get_conf_attr)
    return h


def process_relevant_hosts(shall_i_process_host, action, parser,
        output=stdout, hosts=hosts):
    args = parser.parse_args()
    host_count = 0
    for h in hosts(classes_from_examination_of_running_system, args):
        h.output = output
        name = h.name
        if name not in args.skip.split(','):
            try:
                if not shall_i_process_host(args.host_specification, h):
                    continue
                try:
                    action(h, args)
                except CalledProcessError as e:
                    exit(e)
            except TargetNotAvailable as e:
                output.write('{name}: {e}\n'.format(**locals()))
            except SystemExit:
                raise
            except:
                print('\nUncaught exception for:', name, file=output)
                raise
            host_count += 1
    if not host_count:
        exit(args.host_specification + " doesn't exist in hosts.")


def execute_action_on_selected_hosts(action, parser, metavar=None,
        add_second_parameter=lambda parser, metavar:
        parser.add_argument('second_parameter',
            metavar=metavar)):
    add_mandatory_host_specification(distribute_parser)
    add_second_parameter(parser, metavar)
    process_relevant_hosts(ShallIProcessHost(), action, parser)


distribute_parent_parser = ArgumentParser(add_help=False)
distribute_parent_parser.add_argument('--skip', default='')
object_from_option(distribute_parent_parser, 'dry-run', DryRun, RealRun,
        'run_mode')
distribute_parser = ArgumentParser(parents=[distribute_parent_parser])


def add_mandatory_host_specification(parser):
    parser.add_argument('host_specification')


class ShallIProcessHost(predefined.ShallIProcessHost):
    def __init__(self):
        predefined.ShallIProcessHost.__init__(self,
                site_configuration.__dict__)

    def overrides(self, host):
        """This throws TargetNotAvailable if the system is offline."""
        return host.overrides()


class DefaultAll(ShallIProcessHost):
    def exclude_condition(self, class_in):
        return not class_in(DontTouch)


def hosts_with_class(class_name):
    return predefined.hosts_with_class(
            class_name, site_configuration.__dict__)
