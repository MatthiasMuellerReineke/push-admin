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


"""The working directory must be test_files
(otherwise these tests will fail)!"""
from os import environ, makedirs
from subprocess import PIPE, CalledProcessError
from os.path import join, isdir
import unittest

from aslib.utilities import tunix
from aslib.os_objects import option_with_values,\
         user_home_dir_cmd, getent_user,\
         mkdir_command, ChangeGroup, Directory, Make
from aslib.predefined import All, check_communicate, non_existent,\
         Override, Service, MakeOwnedRecursivelyBy, dir_of_tree
from aslib import process_hosts
from aslib.process_hosts import RealRun, DryRun

from aslib import test_util
from aslib.test_util import RunModeMock, on_exit_vanishing_dtemp,\
         distribution_centos, system_object_init, usermod


class TestRpmsaveFromRpmsBugAction(test_util.TestRpmsaveFromRpmsBugAction):
    def create_host(self):
        return RootRemoteInTmp(RunModeMock())


class TestConfigurationInTmp(test_util.TestInTmpDir):
    user_name = 'test_useradd'

    def test_owner_permission_kept(self):
        conf_file_name = 'admin_test_configuration'
        def write_conf_file(content):
            self.create_conf(conf_file_name, content=content)
        write_conf_file('1')
        r = InitializedAll(RealRun) #RootRemoteInTmp
        r.rsync()
        file_on_remote = '/' + conf_file_name #join('/tmp', split(r.remote_root())[1],
#               conf_file_name)
        def manipulate_remote_file(operation):
            r.rcmd(operation + ' ' + file_on_remote)
        manipulate_remote_file('chown daemon:daemon')
        manipulate_remote_file('chmod o-r')
        def ll():
            return r.ssh_silent_stdout('ls -l ' + file_on_remote)
        expected = ll()
        write_conf_file('2')
        r.rsync()
        result = ll()
        manipulate_remote_file('rm')
        self.assertEqual(result, expected)

    def test_usermod_append_existing_group(self):
        self.usermod_group_append(lambda execute_useradd, groups:
                execute_useradd(groups, groupdel_cleanup_actions(groups)),
                lambda execute_useradd, single_group, groups, system_object:
                system_object.ssh_silent_stdout(
                    usermod(self.user_name,
                        option_with_values('groups', ('test_group2',))[0]
                        + ' ')
                    ))

    def test_usermod_group_append(self):
        self.usermod_group_append(tunix,
                lambda execute_useradd, single_group, groups, system_object:
                execute_useradd(single_group,
                    groupdel_cleanup_actions(groups)))

    def usermod_group_append(self, call_execute_useradd_1,
        call_execute_useradd_2):
        user_name = self.user_name
        group_name = 'test_group'
        def execute_useradd(groups, cleanup_actions=()):
            return self.execute_useradd(
                [[user_name] + groups], groups,
                tunix, lambda system_object: system_object,
                cleanup_actions)
        groups = [group_name, group_name + '2']
        system_object = call_execute_useradd_1(execute_useradd, groups)
        call_execute_useradd_2(execute_useradd, [group_name], groups,
                system_object)
        system_object = execute_useradd(groups)
        # Check result:
        getent_entry, missing = system_object.getent('group', group_name)
        getent_entry[2:3] = [] # remove volatile gid
        self.assertEqual(getent_entry + [missing],
                [group_name, 'x', user_name, None])

    def test_useradd(self):
        self.execute_useradd_overlay_in_home()

    def test_useradd_home_recursively_owned(self):
        self.execute_useradd_overlay_in_home([MakeOwnedRecursivelyBy],
                self.user_name)

    def execute_useradd_overlay_in_home(self,
            additional_user_description=[], file_ownership='root'):
        user_name = self.user_name
        file_name = 'unimportant_name'
        def calc_result(r):
            def stat_home_dir(file_in_home=''):
                return ';stat --format=%U.%G '\
                       + test_user_home_dir + file_in_home
            test_user_home_dir = user_home_dir(r, user_name)
            return (isdir(r.remote_root() + test_user_home_dir),
                        r.ssh_silent_stdout(getent_user(user_name)
                            + '>/dev/null;echo $?' + stat_home_dir()
                            + stat_home_dir('/' + file_name)
                            ))

        self.assertEqual(self.execute_useradd(
                    [[user_name] + additional_user_description], [],
                    lambda: self.create_conf(file_name,
                        join('home', user_name)), calc_result),
                (True, '0\n{0}.{0}\n{1}.{1}'
                 .format(user_name, file_ownership)))

    def create_conf(self, conf_file_name, conf_dir='', content=''):
        """creates a rudimentary configuration directory tree like the one
           with the name "trees"."""
        conf_dir_complete = join(dir_of_tree('All'), conf_dir)
        if not isdir(conf_dir_complete):
            makedirs(conf_dir_complete)
        conf_file = open(join(conf_dir_complete, conf_file_name), 'w')
        conf_file.write(content)
        conf_file.close()

    def execute_useradd(self, user_description, group_description,
            create_conf, calc_result, cleanup_actions=()):
        class User(Override):
            def users(self):
                return user_description

            def groups(self):
                return group_description
        # Does the user home directory have the usual permissions when
        # this system puts a file from the trees there?
        # The user isn't existing now, so we can't use user_home_dir.
        create_conf()

        r = InitializedAll(RealRun, User)
        self.cleanup_actions.append(lambda: r.ssh_silent(
                    'userdel --remove --force ' + self.user_name
                    + ' 2>/dev/null||[ $? = 6 ]'))
        for a in cleanup_actions:
            self.cleanup_actions.append(a(r))
        r.rsync()
        result = calc_result(r)
        # Is it idempotent?
        r.rsync()
        return result


def groupdel_cleanup_actions(groups):
    return map(lambda group: lambda system_object:
            lambda: system_object.ssh_silent('groupdel ' + group), groups)


class TestCentos:
    def super_setup(self, run_mode):
        self.system_object = system_object = InitializedAll(run_mode)
        self.run_mode = system_object.run_mode

    def test_store_remote(self):
        d = check_communicate('date +%H%M%S%N', stdout=PIPE)
        s = self.system_object
        remote_file = 'root/test_store_remote'
        s.store_remote(remote_file, d)
        self.store_remote_assert(d, s.get_remote(remote_file))

    def test_check_communicate_false(self):
        self.assertEqual('', self.system_object.get_remote(non_existent))


class TestCentosDry(TestCentos, unittest.TestCase):
    def setUp(self):
        self.super_setup(DryRun)
        self.distribute_files_diff_orig = process_hosts.diff
        process_hosts.diff = tunix

    def tearDown(self):
        process_hosts.diff = self.distribute_files_diff_orig

    def store_remote_assert(self, a, b):
        """Assert that's really a dry-run (and not a real-run)."""
        self.assertNotEqual(a, b)

    # I don't move the following tests to TestSimple because
    # self.system_object is handy:
    def test_services_non_existent(self):
        s_name = non_existent
        s = Service(self.system_object, s_name)
        self.assertEqual(s.commands(), [s.create_single_command(s_name)])

    def test_services_existent(self):
        self.assertEqual(Service(self.system_object, 'network').commands(),
                [])

    def test_group(self):
        test_dir_name = 'a'
        test_dir_group = 'xyz'
        self.assertEqual(Directory(self.system_object,
                    (test_dir_name, Make(0755, group=test_dir_group))
                    ).commands(),
                [mkdir_command(test_dir_name), ChangeGroup(test_dir_group
                    ).create_single_command(test_dir_name)])

class TestCentosReal(TestCentos, unittest.TestCase):
    def setUp(self):
        self.super_setup(RealRun)

    def store_remote_assert(self, a, b):
        self.assertEqual(a, b)

    def test_get_remote(self):
        self.assertEqual("""ext4
ext3
ext2
nodev proc
nodev devpts
iso9660
vfat
hfs
hfsplus
""", self.system_object.get_remote('etc/filesystems'))

    def test_rcmd(self):
        self.assertRaises(CalledProcessError, self.run_mode.rcmd, 'false')

    def test_rcmd_true(self):
        self.run_mode.rcmd('true')

    def test_rcmd_2_words(self):
        self.run_mode.rcmd('touch /dev/null')

    # I don't move the following tests to TestSimple because
    # self.system_object is handy:
    def test_ssh_quoting(self):
        # In the case of insufficient quoting the shell command below
        # yields the name of the local system instead of the remote one.
        s = self.system_object
        self.ssh_silent_stdout('false||hostname --short', s.name)

    def test_ssh_silent_stdout_single_quotes(self):
        self.ssh_silent_stdout("echo '$A'", '$A')

    def test_ssh_silent_stdout_multilines(self):
        self.ssh_silent_stdout("echo A;echo B", 'A\nB')

    def test_ssh_silent_stdout_longlines(self):
        def insert_many(s):
            return s.format(50 * 'A', 50 * 'B', 50 * 'C')
        self.ssh_silent_stdout(insert_many('echo {0};echo {1};echo {2}'),
               insert_many('{0}\n{1}\n{2}'))

    def ssh_silent_stdout(self, command, expected):
        self.assertEqual(
                self.system_object.ssh_silent_stdout(command), expected)

    def test_query_installed_installed(self):
        self.query_installed([], [])

    def test_query_installed_not_existing(self):
        self.query_installed(['ysbfakjf'], [])

    def query_installed(self, package, expected):
        allways_installed = 'basesystem'
        self.assertEqual(self.system_object.query_installed(
                    [allways_installed] + package),
                [allways_installed] + expected)


class TestSimple(unittest.TestCase):
    def test_user_home_dir(self):
        self.assertEqual(user_home_dir(initialized_all_dry(), 'root'),
                '/root')

    def test_remote_dtemp(self):
        # Does remote_dtemp return any value?
        self.assertTrue(initialized_all_dry().remote_dtemp())


### utilities ###

def user_home_dir(remote_system, user_name):
    ret = remote_system.ssh_silent_stdout(user_home_dir_cmd(user_name))
    assert(ret)
    return ret


def initialized_all_dry():
    return InitializedAll(DryRun)


class InitializedAll(All):
    release_major = ''

    def __init__(self, run_mode, *args):
        All.__init__(self, environ['REAL_SYSTEM'], *args)
        # classes_from_examination_of_running_system can't be used
        # instead of distribution_centos, because the tests in
        # TestRpmsaveFromRpmsBugAction use something like
        # REAL_SYSTEM:/tmp/csadgds as remote_root:
        system_object_init(self, distribution_centos, run_mode)


class RootRemoteInTmp(InitializedAll):
    def remote_root(self):
        # XXX: remote_dtemp would use our remote_root.
        return on_exit_vanishing_dtemp(dir=join(All.remote_root(self), 'tmp'))


unittest.main()
