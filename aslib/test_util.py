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


from os import mkdir, chmod, chdir, getcwd
from os.path import join, lexists
from stat import S_IRUSR, S_IWUSR
import unittest

import utilities
from utilities import on_exit_vanishing_dtemp, write
import predefined
from predefined import CentOS, El5, pristine_for_rpms_bug_dir
from process_hosts import get_conf_attr, RunMode, options_from_runmode


def usermod(user_name, options):
    return 'usermod {1}{0}'.format(user_name, options)


class TestReplaceLibAttr(unittest.TestCase):
    def setUp(self):
        self.__saved = []

    def manipulate_module(self, module, attribute_name, value):
        self.__saved.append((module, attribute_name,
            getattr(module, attribute_name)))
        setattr(module, attribute_name, value)

    def tearDown(self):
        for saved in self.__saved:
            setattr(*saved)


class TestInTmpDir(unittest.TestCase):
    def setUp(self):
        self.cwd = getcwd()
        self.tmp_dir = on_exit_vanishing_dtemp()
        chdir(self.tmp_dir)

        self.cleanup_actions = []

    def tearDown(self):
        chdir(self.cwd)
        map(lambda cleanup_action: cleanup_action(), self.cleanup_actions)


class TestRpmsaveFromRpmsBug(TestInTmpDir):
    def execute_test(self):
        return execute_rpmnew_test(
                self.create_host(), self.rpmnew_content, '', 'c')


class TestRpmsaveFromRpmsBugAction(TestRpmsaveFromRpmsBug):
    rpmnew_content = 'c'

    def test_rpmnew_gone(self):
        self.assertFalse(lexists(self.execute_test() + '.rpmnew'))

    def test_conf_file_has_new_content(self):
        self.assertEqual(file_content(self.execute_test()), 'c')


def execute_rpmnew_test(host, rpmnew_content, directory, conf_file_name):
    complete_directory = join(host.remote_root(), directory)
    if directory:
        mkdir(complete_directory)
    complete_conf_file_name = join(complete_directory, conf_file_name)
    write_file(complete_conf_file_name + '.rpmnew', rpmnew_content)
    write_file(complete_conf_file_name, '')
    chmod(complete_conf_file_name, S_IRUSR|S_IWUSR)
#   from os import _exit
#   _exit(1)
    mkdir(pristine_for_rpms_bug_dir)
    write_file(join(pristine_for_rpms_bug_dir, 'c'), 'c')
    host.rsync()
    return complete_conf_file_name

def write_file(file_name, content):
    write(file_name, content + '\n')

def file_content(file_name):
    return utilities.file_content(file_name).rstrip('\n')


class RunModeClassDummy:
    def __init__(self, all_object):
        pass


class RunModeMock(RunMode):
    def __init__(self):
        self.cmds = []
        self.stored_remote = []

    def __call__(self, all_object):
        self.all_object = all_object
        return self

    def do_file_list(self):
        pass

    def store_remote(self, dest_file, content, manipulate):
        self.stored_remote.append((dest_file, content))

    def rsync(self, dir_tree):
        pass

    def rcmd(self, cmd, output_catcher=None):
        self.cmds.append(cmd)

    def separate_output(self):
        pass


def distribution_centos(get_remote):
    return [CentOS, El5]


def system_object_init(s,
        get_classes_from_examination_of_system=distribution_centos,
        run_mode=RunModeClassDummy):
    s.init(get_classes_from_examination_of_system,
            options_from_runmode(run_mode), get_conf_attr)
