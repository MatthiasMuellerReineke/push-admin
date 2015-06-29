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
from imp import load_module, PY_SOURCE
from stat import S_IRUSR, S_IWUSR
from os import mkdir, stat, makedirs, symlink, chmod,\
         environ
from time import sleep
from subprocess import CalledProcessError, Popen
from os.path import join, lexists, isdir
from cStringIO import StringIO
import unittest

from aslib.utilities import tunix, write, memoize, ensure_contains
from aslib.remote_exec import MatchBuffer, StdWrapper, StdinWrapper,\
        CatchStdout,\
        AlwaysPrintDestination, PrintDestinationForOutput, NoTty,\
        communicate_with_child
from aslib.os_objects import option_with_values, User, UsersGroups,\
         Packages,\
         Files, Link, Directory, NoManipulation, Make, ChangeGroup,\
         mkdir_command, commands_from_instances
from aslib.predefined import All, Override, Offline, El,\
         non_existent, file_parts_entry,\
         FileNameCollision, MakeExecutable, MakeOwnedRecursivelyBy,\
         Debianish, DontTouch, hosts_with_class,\
         in_rcd_initd, link_in_same_dir, dir_of_tree,\
         NotSubclassOfFromExaminationOfSystem, FromExaminationOfSystem
from aslib import remote_exec
from aslib import predefined

hosts_file_name = join(environ['PUSH_ADMIN_DIR'],
                'aslib/test_sites/test_files/hosts.py')
load_module('hosts', open(hosts_file_name), hosts_file_name,
        ('.py', 'r', PY_SOURCE))

from aslib import process_hosts
from aslib.process_hosts import\
         classes_from_examination_of_running_system, get_conf_attr,\
         DryRun, RealRun, options_from_runmode, ShallIProcessHost,\
         process_relevant_hosts,\
         DefaultAll,\
         classes_from_names, diff, not_printable_diff_text, mkdir_p

from aslib import test_util
from aslib.test_util import file_content, write_file, RunModeMock,\
         on_exit_vanishing_dtemp, distribution_centos, system_object_init,\
         usermod, RunModeClassDummy, TestInTmpDir


shall_i_process_host = ShallIProcessHost()
packagemanagers_install_cmd = Packages.packagemanagers_install_cmd


class TestMatchBuffer(unittest.TestCase):
    to_be_matched = 'cx'

    def test_returns_non_matching(self):
        val = 'b'
        self.execute_one_buffer_value(val, val)        

    def test_returns_non_matching_twice(self):
        val = 'b'
        self.execute_one_buffer_value(val + self.to_be_matched + val,
                val * 2)        

    def test_match_reset(self):
        self.execute_one_buffer_value('ca' + self.to_be_matched, 'ca')

    def test_filters_matching(self):
        expected_result = 'd'
        self.execute_one_buffer_value(
                expected_result + self.to_be_matched, expected_result)

    def execute_one_buffer_value(self, value, expected_result):
        self.assertEqual(
                MatchBuffer(self.to_be_matched).buffer_value(value),
            expected_result)

    def test_two_buffer_values(self):
        mb = MatchBuffer(self.to_be_matched)
        v1 = mb.buffer_value('abc')
        v2 = mb.buffer_value('xde')
        self.assertEqual(v1 + v2, 'abde')


class TestPrintDestinationForOutput(unittest.TestCase):
    manipulated = ('stdout', 'stderr')

    def setUp(self):
        self.saved = []
        for i in self.manipulated:
            self.saved.append(getattr(remote_exec, i))
            setattr(remote_exec, i, StringIO())

    def tearDown(self):
        for manipulated, saved in zip(self.manipulated, self.saved):
            setattr(remote_exec, manipulated, saved)

    def test_nothing_used(self):
        self.execute(tunix, 0)

    def test_stdout_used(self):
        self.execute(lambda forward:
                self.call_take_val(forward.take_stdout))

    def test_stderr_used(self):
        self.execute(lambda forward:
                self.call_take_val(forward.take_stderr))

    def call_take_val(self, take):
        self.call_take(take, 'x')

    def call_take(self, take, val=''):
        take(val, NoTty())

    def execute(self, execute, expected_calls=1):
        class AllMock:
            calls = 0
            def print_dest(self):
                self.calls += 1

        am = AllMock()
        forward = PrintDestinationForOutput(am)
        execute(forward)
        # They are at least called once with an empty string
        # in productive use:
        self.call_take(forward.take_stdout)
        self.call_take(forward.take_stderr)

        self.assertEqual(am.calls, expected_calls)


class TestPackageNameMapping(unittest.TestCase):
    to_be_mapped = 'to-be-mapped-for-one-distribution'
    not_to_be_mapped = 'not-to-be-mapped'
    example_packages = [to_be_mapped, not_to_be_mapped]

    def test_packages_cmd_with_map(self):
        mapped = 'this-is-mapped'
        class MappingRequiringDistribution(Debianish):
            package_mapper = {self.to_be_mapped: mapped}
        self.packages_cmd_map_names(
                lambda get_remote: [MappingRequiringDistribution],
                [mapped, self.not_to_be_mapped])

    def test_packages_cmd_without_map(self):
        self.packages_cmd_map_names(distribution_centos,
                self.example_packages)

    def packages_cmd_map_names(self,
            get_classes_from_examination_of_system, expected_packages):
        class AllDerived(All):
            packages = self.example_packages
        sorted_expected_packages = sorted(expected_packages)
        s = AllDerived('')
        system_object_init(s, get_classes_from_examination_of_system)
        self.assertEqual(s.packages_cmd().all_commands(),
                [s.install_pkg_cmd
                + packagemanagers_install_cmd
                + ' '.join(sorted_expected_packages)
                + s.check_installation(sorted_expected_packages)])


class TestExecuteTestOverride(unittest.TestCase):
    user_name = 'user_name'
    user_add = User.cmd(user_name)
    group_name = 'group_name'

    def test_user_all_commands(self):
        self.assertEqual(User(None, self.user_name).all_commands(),
                [self.user_add])

    def test_user_create_commands(self):
        self.assertEqual(UsersGroups(None, self.user_name).create_commands(
                    [self.group_name], None), [self.usermod_group()])

    def test_usermod_opt_tuple_pre_overlay_2_groups(self):
        self.usermod_opt_tuple_pre_overlay_groups(
                ['1st_group', '2nd_group'])

    def test_usermod_opt_tuple_pre_overlay_1_group(self):
        self.usermod_opt_tuple_pre_overlay_groups([self.group_name])

    def usermod_opt_tuple_pre_overlay_groups(self, groups):
        return self.usermod_opt_tuple_pre_overlay(groups,
                groups_options(','.join(groups)))

    def test_files_chown_in_post_overlay(self):
        file_name = 'file_name'
        self.execute_post_overlay(
                (file_name, 'file_content', Make(group=self.group_name)),
                [ChangeGroup(self.group_name)
                 .create_single_command(file_name)], 'files')

    def test_usermod_opt_tuple_pre_overlay(self):
        self.execute_post_overlay([self.user_name], [self.user_add])

    def usermod_opt_tuple_pre_overlay(self, groups=[],
            additional_option=''):
        self.execute_post_overlay([self.user_name] + groups,
                [self.user_add, self.usermod(additional_option)])

    def test_users_in_pre_overlay(self):
        self.execute_post_overlay(self.user_name, [self.user_add])

    def test_user_with_group_in_pre_overlay(self):
        self.execute_post_overlay((self.user_name, self.group_name),
                [self.user_add, self.usermod_group()])

    def usermod_group(self):
        return self.usermod(groups_options(self.group_name))

    def usermod(self, options):
        return usermod(self.user_name, options)

    def test_user_group_order_in_pre_overlay(self):
        class GroupCreating(Override):
            def groups(inner_self):
                return [self.group_name]
        self.execute_os_system_object('pre_overlay_commands',
            'dummy', None, ['groupadd ' + self.group_name], GroupCreating)

    def execute_post_overlay(self, return_item, expected_commands,
            result_yielding_method_name='users', *overrides):
        self.execute_os_system_object('post_overlay_commands',
            result_yielding_method_name, return_item, expected_commands,
            *overrides)

    def test_commands_in_post_inst(self):
        cmd = 'abcd xyz'
        self.execute_post_inst('commands', cmd, [cmd])

    def execute_post_inst(self, method_name, return_item, expected_commands,
            *overrides):
        self.execute_os_system_object('post_inst',
            method_name, return_item, expected_commands, *overrides)

    def execute_os_system_object(self, result_yielding_method_name,
            method_name, return_item, expected_commands, *overrides):
        self.execute_test_override(method_name, return_item,
                expected_commands, lambda test_class:
                getattr(non_existent_centos_runmodemock(test_class,
                        *overrides), result_yielding_method_name)())

    def test_files_in_generated_files(self):
        test_file = ('a', 'b')
        self.execute_test_override('files', test_file, [test_file],
                lambda test_class: root_is_local(
                    test_class).generated_files())

    def test_default_inst_device_override(self):
        expected = '/dev/sdxyz'
        class WeOverrideInstDevice(Override):
            def inst_device(self):
                return expected
        self.assertEqual(non_existent_centos_runmodemock(
                    WeOverrideInstDevice).inst_device(), expected)

    def execute_test_override(self, method_name, return_item, expected,
            calculate_result):
        class TestClass(Override):
            pass
        setattr(TestClass, method_name, lambda self: [return_item])
        self.assertEqual(calculate_result(TestClass), expected)


def groups_options(groups):
    return '--append --groups {0} '.format(groups)


class TestCommands(unittest.TestCase):
    __name = 'a'
    source = 'b'

    def setUp(self):
        self.r = RootIsLocal()
        self.remote_name = join(self.r.remote_root(), self.__name)

    def test_symlink_in_non_existing_dir(self):
        source = self.source
        dirname = 'd'
        link = join(dirname, self.__name)
        l = Link(self.r, (self.source, link))
        self.execute(l,
                [Directory(self.r, dirname).create_single_command(None),
                 l.create_single_command(None)])

    def test_existing_correctly(self):
        symlink(self.source, self.remote_name)
        self.execute_link([])

    def test_wrong_existing(self):
        symlink('wrong_source', self.remote_name)
        self.execute_non_existing()

    def test_non_existing(self):
        self.execute_non_existing()

    def execute_non_existing(self):
        self.execute_link([Link(self.r, (self.source, self.__name))
                .create_single_command(None)])

    def execute_link(self, expected):
        self.execute(Link(self.r, (self.source, self.__name)), expected)

    def test_dir(self):
        name = self.remote_name
        mkdir(name)
        chmod(name, S_IRUSR|S_IWUSR)
        self.execute(Directory(self.r, (self.__name, MakeExecutable)),
                ['chmod 755 /' + self.__name])

    def test_dir_str_instead_of_tuple(self):
        self.execute(Directory(self.r, 'a'), [mkdir_command('a')])

    def execute(self, os_object, expected):
        self.assertEqual(os_object.commands(), expected)


class TestSimple(unittest.TestCase):
    def test_make_without_mode(self):
        Directory(None, ('a', Make())).all_commands()

    def test_make_execute_dry(self):
        # TODO: Make it possible to instantiate Make without parameter.
        Make(NoManipulation.desired_mode).execute_dry(
                '/non_existing_dir', None)

    def test_ensure_contains(self):
        data = 'd'
        name = join(on_exit_vanishing_dtemp(), 'a')
        ensure_contains(name, data)
        ensure_contains(name, data)
        self.assertEqual(file_content(name), data)

    def test_write_written_data_is_immediately_available(self):
        data = 'x'
        name = join(on_exit_vanishing_dtemp(), 'a')
        write(name, data)
        self.assertEqual(file_content(name), data)

    def test_overrides_are_instantiated_once_and_only_once(self):
        class Once:
            tally = []
            def __init__(self, system_object):
                self.tally.append(None)
        s = All(non_existent, Once)
        system_object_init(s)
        s.overrides()
        s.overrides()
        self.assertEqual(len(Once.tally), 1)

    def test_non_yet_existing_user_with_shell(self):
        shell = 'test_shell'
        class HasGetent:
            def getent(self, db, name):
                return None, name
        def command_lists(method):
            return [getattr(
                    x(HasGetent(), ('testusername', ('shell', shell))),
                    method)() for x in (User, UsersGroups)]
        self.assertEqual(command_lists('commands'),
                command_lists('all_commands'))

    def test_root_is_local_files(self):
        self.assertEqual(root_is_local().accumulate('files'), [])

    def test_make_executable_current_mode_file_doesnt_exist(self):
        default_mode = NoManipulation.desired_mode
        self.assertEqual(MakeExecutable.current_mode(
                    "/This file doesn't exist.", default_mode),
                default_mode)

    def test_option_with_values(self):
        self.assertEqual(option_with_values('option_name', ('value',)),
                ['--option_name value'])

    def test_el_services_only_once(self):
        class El(FromExaminationOfSystem):
            def test_accumulate(self):
                return [None]
        s = All(non_existent)
        s.init(distribution_centos, OptionsClassDummy,
                lambda name: {'El': El}[name])
        self.assertEqual(len(s.accumulate('test_accumulate')), 1)

    def test_accumulate_accumulates_plain_value(self):
        self.execute_accumulate_accumulates_from_super(2)

    def test_accumulate_accumulates_from_super(self):
        self.execute_accumulate_accumulates_from_super([2])

    def execute_accumulate_accumulates_from_super(self, subclass_ret_val):
        class Super(All):
            def to_be_accumulated(self):
                return [1]
        class Sub(Super):
            def to_be_accumulated(self):
                return subclass_ret_val
        s = Sub(non_existent)
        system_object_init(s)
        self.assertEqual(set(s.accumulate('to_be_accumulated')),
                set((1, 2)))

    def test_process_relevant_hosts(self):
        # Setup fixture:
        def raise_calledproceserror():
            raise CalledProcessError(7, 'command to be mentioned')

        class DummyOptionParser:
            def __getattr__(self, name):
                return lambda *args, **kwargs: None

            def parse_args(self):
                class Options:
                    run_mode = tunix
                    skip = ''
                    host_specification = None
                return Options()

        # Execution shall have stopped after exception:
        self.assertRaises(SystemExit, process_relevant_hosts,
                lambda args, host: True,
                lambda host, args: host.action(),
                DummyOptionParser(), StringIO(),
                lambda distribution, runmode:
                    [HostDummy(raise_calledproceserror), HostDummy(tunix)])

    def test_get_remote(self):
        """Reason for this test was a bug in All.get_remote where read
           was accidentally called against the filename instead of the
           file object."""
        s = RootIsLocal()
        file_name = join(s.remote_root(), 't')
        self.assertEqual('', s.get_remote('t'))

    def test_new_file_in_nonexisting_dir(self):
        non_existent_centos(RealRun).store_remote('a/b', 'c')

    def test_avoid_superfluous_update(self):
        def store_remote():
            s.store_remote(f_name, 'content')
        def call_stat():
            return stat(join(s.remote_root(), f_name))
        s = non_existent_centos(RealRun)
        f_name = 'f_name'
        store_remote()
        st1 = call_stat()
        sleep(1)
        store_remote()
        st2 = call_stat()
        self.assertEqual(st1.st_ctime, st2.st_ctime)

    def test_generated_files_are_stored_remote(self):
        remote_name = 'c/d/e'
        remote_content = 'store_remote test'
        self.assertEqual(run_all_root_is_local(
                    file_parts_class(Override, remote_name, remote_content)
                    ).stored_remote,
                [(remote_name, remote_content + '\n')])

    def test_generated(self):
        remote_name = 'y'
        FP1 = file_parts_class(Override, remote_name, 'c1')
        self.assertEqual(root_is_local(FP1,
                    file_parts_class(Override, remote_name, 'c2'))
                .generated_files(), [(remote_name, 'c1\n\nc2\n')])

    def test_system_object_file_parts(self):
        remote_name = 'w'
        class HasFileParts(RootIsLocal):
            def file_parts(self):
               return [(remote_name, 'c3')]
        s = HasFileParts()
        system_object_init(s, get_distribution_classes_empty,
                RunModeMock())
        self.assertEqual(s.generated_files(), [(remote_name, 'c3\n')])

    def test_non_existing_os_class_raises_releasenum(self):
        self.execute_non_existing_os_class_raises('6')

    def test_non_existing_os_class_raises(self):
        self.execute_non_existing_os_class_raises()

    def execute_non_existing_os_class_raises(self, *args):
        self.assertRaises(KeyError, get_conf_attr, 'SL', *args)

    def test_is_desired_object(self):
        self.for_all_distribution_objects(self.assertEqual, get_conf_attr,
                lambda o, o_name: (o.__class__.__name__, o_name))
#       for o in ('Ubuntu', 'Debian', 'CentOS'):
#           self.assertEqual(getattr(predefined, o)(None)
#                   .__class__.__name__, o)

    def test_superclass_from_get_conf_attr(self):
        class DesiredSuperclass(Override):
            pass
        self.for_all_distribution_objects(self.assertTrue,
                lambda name: DesiredSuperclass,
                lambda o, o_name:
                    (isinstance(o, DesiredSuperclass), o_name))

    def for_all_distribution_objects(self, test_func, get_conf_attr,
        test_result_func):
        for o_name in ('Ubuntu', 'Debian', 'CentOS'):
            s = RootIsLocal()
            s.init(None, OptionsClassDummy, get_conf_attr)
            test_func(*test_result_func(
                    getattr(predefined, o_name)(s),
                    o_name))

    def test_is_wanted_false(self):
        self.execute_test_is_wanted('Xyz', False)

    def test_is_wanted_true(self):
        self.execute_test_is_wanted('All', True)

    def execute_test_is_wanted(self, description, expected):
        s = All(non_existent)
        class CountCalls:
            count = 0
            def __call__(self, system_object):
                self.count += 1
                return []

        count_calls = CountCalls()
        system_object_init(s, count_calls)
        result = bool(s.is_wanted(description))
        self.assertEqual((result, count_calls.count), (expected, 0))

    def test_override_instances_from_examination_of_system_not_ok(self):
        self.assertRaises(NotSubclassOfFromExaminationOfSystem,
                self.execute_override_instances_from_examination_of_system,
                Override)

    def test_override_instances_from_examination_of_system_ok(self):
        self.execute_override_instances_from_examination_of_system(
                FromExaminationOfSystem)

    def execute_override_instances_from_examination_of_system(self,
            class_from_examination_of_system):
        s = All(non_existent)
        system_object_init(s,
                lambda get_remote: [class_from_examination_of_system])
        s.override_instances_from_examination_of_system()

    def test_default_all_dont_touch(self):
        test_shall_i_process_host_dont_touch(self.assertFalse,
                DefaultAll())

    def test_shall_i_process_host_count_instances(self):
        class All(predefined.All):
            instance_count = 0
        class CountsOnHostobject(Override):
            def __init__(self, system_object):
                Override.__init__(self, system_object)
                system_object.instance_count += 1

        s = All(non_existent, CountsOnHostobject)
        d = locals()
        s.init(get_distribution_classes_empty, OptionsClassDummy,
                lambda x: d[x])
        predefined.ShallIProcessHost(locals())('CountsOnHostobject', s)
        self.assertEqual(s.instance_count, 1)

    def test_shall_i_process_host_dont_touch(self):
        # shall_i_process_host/ShallIProcessHost is used by
        # distribute_command; distribute_command shall ignore DontTouch.
        test_shall_i_process_host_dont_touch(self.assertTrue,
                shall_i_process_host)

    def test_shall_i_process_host_centos(self):
        test_shall_i_process_host(self.assertTrue, '')

    def test_shall_i_process_host_centos_not_el(self):
        test_shall_i_process_host(self.assertFalse, '!El')

    def test_shall_i_process_host_centos_not_e(self):
        test_shall_i_process_host(self.assertTrue, '!E')

    def test_shall_i_process_host_xyz_not_e(self):
        # CentOS mustn't be processed when Debian is requested.
        test_shall_i_process_host(self.assertFalse, '!E', 'Debian')

    def test_shall_i_process_host(self):
        self.assertFalse(shall_i_process_host('a', All(non_existent)))

    def test_shall_i_process_host_el(self):
        self.assertTrue(shall_i_process_host('El',
                    non_existent_centos(RunModeClassDummy)))

    def test_hosts_with_class(self):
        class B(All):
            pass
        name_of_system_of_desired_class = 'Desired'
        hosts = [All('a'), B(name_of_system_of_desired_class)]
        map(system_object_init, hosts)
        self.assertEqual(hosts_with_class('B', {'hosts': hosts, 'B': B}),
                [name_of_system_of_desired_class])

    def test_dry_run_rcmd_takes_all_parameters(self):
        class CommandStub:
            def output_catcher(self):
                return None
            def commands(self):
                return [None]
        setattr(process_hosts, 'print', tunix)
        DryRun(None).conditional_cmds([CommandStub()])

    def test_packages_all_subclass(self):
        test_packages = ['pkg-xyz']
        class AllDerived(RootIsLocal):
            def packages(self):
                return test_packages
        s = run_all_any_system_class(AllDerived)
        self.assertEqual(s.cmds, ['yum install -y pkg-xyz'
            + s.all_object.check_installation(test_packages)])

    def test_without_rear(self):
        self.assertFalse(run_all_root_is_local().cmds)

    def test_services_in_post_inst(self):
        class TestOverride(Override):
            def services(self):
                return ['service1_override', 'service2_override']
        class AllDerived(All):
            def services(self):
                return ['service1_derived', 'service2_derived']
        self.assertEqual(['chkconfig service1_derived on',
                'chkconfig service2_derived on',
                'chkconfig service1_override on',
                'chkconfig service2_override on'],
                instantiate_init_noname(AllDerived, TestOverride).\
                                        post_overlay_commands())

    def test_services_in_links(self):
        class AllDerived(All):
            links = link_in_same_dir('t', in_rcd_initd('t'))
        self.assertEqual(instantiate_init_noname(AllDerived).services(),
                ['t'])

    def test_create_all_users_before_modifying_them(self):
        """All users need to be created first."""
        class AllDerived(All):
            users = [('user_a', 'user_as_group'),
                     ('user_b', 'user_bs_group')]
        self.assertEqual([x.split()[0] for x in sum(
                    [c.all_commands() for c in
                     instantiate_init_noname(AllDerived).users_cmds()], [])
                ], ['useradd', 'useradd', 'usermod', 'usermod'])

    def test_search_non_callable(self):
        expected = "this isn't callable"
        class OverrideToBeFound(Override):
            this_exists = expected
        self.execute_search_existing(OverrideToBeFound, expected)

    def test_search_existing(self):
        expected = 'expected result'
        class OverrideToBeFound(Override):
            def this_exists(self):
                return expected
        self.execute_search_existing(OverrideToBeFound, expected)

    def execute_search_existing(self, cls, expected):
        class SystemClass(RootIsLocal):
            def this_exists(self):
                return 'wrong result'
        self.assertEqual(non_existent_centos_any_system_class(SystemClass,
                    RunModeClassDummy, cls).search.this_exists, expected)

    def test_search_prefers_userdefined_overrides(self):
        expected = 'expected result'
        class A(Override):
            this_exists = expected
        class SystemClass(FromExaminationOfSystem):
            this_exists = 'wrong result'
        s = All(non_existent, A)
        system_object_init(s, lambda get_remote: [SystemClass],
                RunModeClassDummy)
        self.assertEqual(s.search.this_exists, expected)

    def test_search_nonexisting(self):
        self.assertRaises(AttributeError,
                lambda: non_existent_centos(RunModeClassDummy)
                .search.this_method_doesnt_exist)

    def test_derived_non_existent_centos(self):
        self.assertEqual(derived_non_existent_centos().generated_files(),
                [derived_non_existent_centos_file])

    def test_name_in_override(self):
        class OSub(Override):
            def services(self2):
                self.assertEqual(self2.name, non_existent)
                return []
        run_all_root_is_local(OSub)

    def test_diff_pdf_no_difference(self):
        self.diff_pdf('', '')

    def test_diff_pdf_difference(self):
        self.diff_pdf('y',
                self.diff_pdf_file_name + not_printable_diff_text)

    def diff_pdf(self, content_a, expected):
        out = StringIO()
        diff(content_a, '', self.diff_pdf_file_name, out)
        self.assertEqual(out.getvalue(), expected)

    diff_pdf_file_name = 'x.pdf'

    def test_print_dest_prints_once_per_host(self):
        output = StringIO()
        def print_dest():
            h.print_dest()
            return output.getvalue()

        h = All(non_existent)
        h.output = output
        out1 = print_dest()
        self.assertEqual(out1, print_dest())

    def test_forward_to_standard(self):
        class HasRead:
            def read(self):
                return ''

        class HasPrintDest:
            def print_dest(self):
                pass

        ap = AlwaysPrintDestination(HasPrintDest())
        sw = StdWrapper(HasRead(), ap, ap.take_stdout)
        peculiarities = ap.peculiarities()
        peculiarities.save_settings()
        try:
            sw.process()
            sw.process()
        finally:
            peculiarities.reset_settings()

    def test_communicate_with_child(self):
        test_val = 'A'
        output_catcher = CatchStdout()
        stdin_wrapper = StdinWrapper()
        remotes_stdin = stdin_wrapper.remotes_stdin
        communicate_with_child(Popen(['echo', '-n', test_val],
            # XXX: Is it possible to save StdinWrapper?
            stdout=output_catcher.remotes_stdout,
            stdin=remotes_stdin, stderr=remotes_stdin),
            output_catcher, stdin_wrapper, tunix, None)
        self.assertEqual(output_catcher.stdout, test_val)

    def test_memoize(self):
        b_ret = 13469
        class A:
            count = 0
            @memoize
            def b(self):
                self.count += 1
                return b_ret
        a = A()
        self.assertEqual((a.b(), a.b(), a.count), (b_ret, b_ret, 1))


def test_shall_i_process_host_dont_touch(assert_func,
        shall_i_process_host_object):
        s = All(non_existent, DontTouch)
        s.init(get_distribution_classes_empty, OptionsClassDummy,
                lambda x: All)
        assert_func(shall_i_process_host_object('All', s))


def test_shall_i_process_host(assert_func, opt_limit,
        positive_class='CentOS'):
    assert_func(shall_i_process_host(positive_class + opt_limit,
                non_existent_centos_runmodemock()))


def get_distribution_classes_empty(get_remote):
    return []


def file_parts_class(super_class, remote_name, remote_content):
    class HasFileParts(super_class):
        def file_parts(self):
            return [(remote_name, remote_content),]
    return HasFileParts

 
class CreateHost:
    def create_host(self):
        return non_existent_centos_runmodemock()


class TestRpmsaveFromRpmsBugNoAction(test_util.TestRpmsaveFromRpmsBug,
        CreateHost):
    rpmnew_content = 'x' # differs from file under pristine_for_rpms_bug

    def test_rpmnew_untouched(self):
        self.assertEqual(
                file_content(self.execute_test() + '.rpmnew'),
                'x')

    def test_conf_file_untouched(self):
        self.assertEqual(file_content(self.execute_test()), '')


class TestRpmsaveFromRpmsBugAction(test_util.TestRpmsaveFromRpmsBugAction,
        CreateHost):
    pass


class TestConfigurationInTmp(TestInTmpDir):
    colliding_name = 'colliding_name'

    def test_override_dir_name_collision(self):
        class HasSameFile(Override):
            dirs = self.colliding_name
        self.run_file_name_collision(HasSameFile)

    def test_override_link_name_collision(self):
        class HasSameFile(Override):
            links = ('', self.colliding_name)
        self.run_file_name_collision(HasSameFile)

    def test_override_name_collision(self):
        class HasSameFile(Override):
            files = (self.colliding_name, '')
        self.run_file_name_collision(HasSameFile)

    def test_generated_tree_name_collision(self):
        touch(join(self.mk_all(), self.colliding_name))
        self.run_file_name_collision()

    def run_file_name_collision(self, *overrides):
        s = RootIsLocal(file_parts_class(Override,
                        self.colliding_name, ''), *overrides)
        system_object_init(s, get_distribution_classes_empty)
        self.assertRaises(FileNameCollision, lambda: s.trees())

    def test_service_detection(self):
        self.mk_all(in_rcd_initd('test_service'))
        self.assertEqual(non_existent_centos_runmodemock().services(),
                ['test_service'])

    def mk_all(self, *dir_name):
        ret = join(dir_of_tree('All'), *dir_name)
        makedirs(ret)
        return ret

    def test_host_none_exclusion(self):
        dir_name = ['All!othernonexisting']
        self.run_all(dir_name, dir_name)

    def test_host_exclusion(self):
        self.run_all(['All!' + non_existent], [])

    def test_host_exclusion_all(self):
        self.run_all([non_existent + '!All'], [])

    def test_non_exclusion_of_first_item(self):
        should_be_included = ['All!Xyz']
        self.run_all(should_be_included, should_be_included)

    def test_double_exclusion(self):
        self.run_all(['All!TestOverride!TestOverride2'], [], TestOverride2)

    def test_system_class_exclusion(self):
        self.run_all(self.available_trees_class_exclusion,
                ['All', 'TestOverride'], TestOverride)

    def test_no_system_class_exclusion(self):
        self.run_all(self.available_trees_class_exclusion,
                ['All', 'All!TestOverride'])

    available_trees_class_exclusion\
        = ['TestOverride', 'All', 'All!TestOverride']

    def run_all(self, available_trees, expected, *args):
        for tree in available_trees:
            makedirs(dir_of_tree(tree))
        s = RootIsLocal(*args)
        s.init(distribution_centos, options_from_runmode(RunModeMock()),
                lambda name: globals()[name])
        self.assertEqual(s.trees(), set(expected))

class TestOverride(Override):
    pass

class TestOverride2(Override):
    pass

class Xyz(Override):
    pass


class TestInTmpDir(TestInTmpDir):
    def test_mkdir_p_rel_path(self):
        path = 'xyz'
        mkdir_p(path)
        self.assertTrue(isdir(path))

    def test_file_parts_entry(self):
        template_dir = join(self.tmp_dir, predefined.template_dir)
        mkdir(template_dir)
        write(join(template_dir, 'entry'),
                'begin {{ template_variable }} end\n')
        dest = 'file/parts/entry'
        self.assertEqual(file_parts_entry(dest,
                    template_variable='variable value'),
                # Posix files end with '\n':
                (dest, 'begin variable value end\n'))

    conf_content = 'a'
    conf_dir = 'etc'

    def test_rpmsave_eq_pristine_in_dir(self):
        self.assertFalse(
                self.rpmnew_exists(self.conf_content, self.conf_dir))

    def test_rpmsave_eq_pristine(self):
        self.assertFalse(self.rpmnew_exists(self.conf_content))

    def test_rpmsave_ne_pristine(self):
        self.assertTrue(self.rpmnew_exists('b'))

    def rpmnew_exists(self, rpmnew_content, directory=''):
        c = self.conf_content
        p5 = 'pristine/5'

        def mkdir_write(conf_dir):
            mkdir_p(conf_dir)
            write_file(join(conf_dir, c), c)
        mkdir_write(p5)
        mkdir_write(join(p5, self.conf_dir))

        return lexists(test_util.execute_rpmnew_test(
                    non_existent_centos_runmodemock(),
                    rpmnew_content, directory, 'a')
                + '.rpmnew')


class TestDoFileList(TestInTmpDir):
    def test_real_run_dry_run_do_file_list(self):
        def append_to_file_list(run_mode):
            run_mode.append_to_file_list('zzz')
            run_mode.append_to_file_list('yyy')
        run_mode = RealRun(HasName)
        append_to_file_list(run_mode)
        run_mode.do_file_list()

        self.dry_run_do_file_list(append_to_file_list)

    def test_dry_run_do_file_list(self):
        self.dry_run_do_file_list(tunix)

    def dry_run_do_file_list(self, action_on_run_mode):
        s = StringIO()
        run_mode = DryRun(HasName)
        action_on_run_mode(run_mode)
        run_mode.do_file_list(s)
        self.assertEqual(s.getvalue(), '')


class HasName:
    name = 'n'


class TestReplaceLibAttr(unittest.TestCase):
    def setUp(self):
        self.__get_conf_attr = process_hosts.get_conf_attr

    def tearDown(self):
        process_hosts.get_conf_attr = self.__get_conf_attr

    def test_release_other5(self):
        class Fictional(FromExaminationOfSystem):
            pass
        def get_conf_attr_f(name, release_major_=''):
            if name == 'Fictional' and release_major_:
                class Fictional5(FromExaminationOfSystem):
                    release_major = release_major_
                return Fictional5
            try:
                return {'Fictional': Fictional}[name]
            except KeyError:
                return self.__get_conf_attr(name, release_major_)
        process_hosts.get_conf_attr = get_conf_attr_f
        self.execute_release('Fictional Linux 5', 5, 'dvd')

    # The following functions don't use the tear down mechanism:
    def test_release_5(self):
        self.execute_release('CentOS release 5.7 (Final)', 5, 'hdc')

    def test_release_6(self):
        self.execute_release('Scientific Linux release 6.1 (Carbon)',
                6, 'dvd')

    def execute_release(self, issue, major_release, dvd_device):
        s = ClassesFromExaminationOfRunningSystem(issue)
        system_object_init(s, classes_from_examination_of_running_system,
                RunModeClassDummy)
        search = s.search
        self.assertEqual((search.release_major, search.inst_media_dev),
               (str(major_release), dvd_device))

    def test_get_pretty_name(self):
        self.assertEqual(process_hosts.get_pretty_name(StringIO(
"""NAME="Scientific Linux"
VERSION="7.1 (Nitrogen)"
ID="rhel"
ID_LIKE="fedora"
VERSION_ID="7.1"
PRETTY_NAME="Scientific Linux 7.1 (Nitrogen)"
ANSI_COLOR="0;31"
CPE_NAME="cpe:/o:redhat:enterprise_linux:7.1:GA"
HOME_URL="http://www.scientificlinux.org//"
BUG_REPORT_URL="scientific-linux-devel@listserv.fnal.gov"

REDHAT_BUGZILLA_PRODUCT="Scientific Linux 7"
REDHAT_BUGZILLA_PRODUCT_VERSION=7.1
REDHAT_SUPPORT_PRODUCT="Scientific Linux"
REDHAT_SUPPORT_PRODUCT_VERSION=7.1
""")), "Scientific Linux 7.1 (Nitrogen)")

    def test_non_el(self):
        classes_from_names(['Debian'])


def instantiate_init_noname(klass, *overrides):
    s = klass('', *overrides)
    system_object_init(s)
    return s


derived_non_existent_centos_file_name = 'a'
derived_non_existent_centos_file = (derived_non_existent_centos_file_name,
        'd')

def derived_non_existent_centos(*args):
    s = RootIsLocal(*args)
    def derived_centos(get_remote):
        class CentOS(FromExaminationOfSystem):
            def files(self):
                return [derived_non_existent_centos_file]
        return [CentOS]
    system_object_init(s, derived_centos)
    return s


def non_existent_centos_runmodemock(*args):
    return non_existent_centos(RunModeMock(), *args)


def non_existent_centos(run_mode, *args):
    return non_existent_centos_any_system_class(RootIsLocal,
            run_mode, *args)


def non_existent_centos_any_system_class(system_class, run_mode, *args):
    # RootIsLocal instead of All avoids Offline Exceptions:
    s = system_class(*args)
    system_object_init(s, distribution_centos, run_mode)
    s.release_major = '5' # according to
                          # aslib/test_sites/test_files/pristine/5
    return s


def root_is_local(*args):
    return root_is_local_sm(*args)[0]


def run_all_root_is_local(*args):
    return root_is_local_sm(*args)[1]


def root_is_local_sm(*args):
    m = RunModeMock()
    s = RootIsLocal(*args)
    system_object_init(s, get_distribution_classes_empty, m)
    s.rsync()
    return (s, m)


def run_all_any_system_class(system_class, *args):
    m = RunModeMock()
    s = non_existent_centos_any_system_class(system_class, m, *args)
    s.rsync()
    return m


class RootIsLocal(All):
    def __init__(self, *args):
        All.__init__(self, non_existent, *args)
        self.tmp_dir = on_exit_vanishing_dtemp()

    def remote_root(self):
        return self.tmp_dir

    def query_installed(self, names):
        return []


class ClassesFromExaminationOfRunningSystem(RootIsLocal):
    def __init__(self, issue, *args):
        RootIsLocal.__init__(self, *args)
        self.__issue = issue

    def get_remote(self, src_file):
        return self.__issue

    def remote_dtemp(self):
        return self.tmp_dir

    def ssh_silent_stdout(self, *args, **kwargs):
        return 'Physical'


class TestNonExistentAll(unittest.TestCase):
    def setUp(self):
        s = All(non_existent)
        system_object_init(s, classes_from_examination_of_running_system,
                RunModeClassDummy)
        s.filter_masters_stderr = list # reads stderr of master_openssh
        self.host = s

    def test_connection_failure(self):
        self.assertRaises(Offline,
                self.host.override_instances_from_examination_of_system)

    def test_connection_failure_direct_ssh(self):
        self.assertRaises(Offline, self.host.ssh, 'true')

    def test_shall_i_process_host(self):
        self.assertTrue(shall_i_process_host(non_existent, self.host))


class HostDummy:
    name = None
    def __init__(self, action):
        self.action = action


class OptionsClassDummy: # simulate options object
    @classmethod
    def run_mode(cls, all_object):
        pass

    diffpdf = tunix


def touch(file_name):
    open(file_name, 'w').close()


unittest.main()
