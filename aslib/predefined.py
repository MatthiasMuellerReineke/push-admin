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

import os
from os import rmdir, walk, remove, getcwd, rename, environ
import socket
import warnings
from os.path import join, exists, basename, dirname, expanduser
from tempfile import mkdtemp, TemporaryFile
from sys import stdin, stdout, stderr, exc_info, modules
from atexit import register
from glob import glob
import filecmp
import threading
from subprocess import check_call, Popen, CalledProcessError, PIPE

import inotifyx
from jinja2 import Environment, FileSystemLoader

from utilities import memoized, memoize,\
         on_exit_vanishing_dtemp, NoMkdir, colored, file_content, mkdir_p,\
         ensure_contains, tunix
from remote_exec import ForwardToStd, CatchStdout,\
         CatchStdoutCatcherStderrMsg,\
         communicate_with_child,\
         StdWrapper, StdinWrapper, process_ready_files
from os_objects import Packages, User, UsersGroups, Group,\
         Service, Link, Directory, Files,\
         commands_from_instances, SimpleConditionalCommand,\
         chown_user_home_dir_cmd, user_home_dir_cmd,\
         Make, NoManipulation, MakeExecutable

non_existent = 'doesntexist'


sudoers_mode = Make(0440)

def sudoersd_user_entry(user_name, sudoers_permission_description):
    return (join('etc/sudoers.d', user_name),
            user_name + ' ' + sudoers_permission_description + '\n',
            sudoers_mode)


def crond_entry_with_extended_path(file_name, content):
    return crond_entry(file_name, 'PATH=/{0}:/usr/bin:/bin\n{1}'
                                  .format(script_dir, content))


def crond_entry(file_name, content):
    return (join('etc/cron.d', file_name), content + '\n')


def in_postinst_d(name):
    return join('usr/local/postinst.d', name)

def in_rcd_initd(name):
    return join(rcd_initd_dir, name)


rcd_initd_dir = 'etc/rc.d/init.d'
script_dir = 'usr/local/bin'

def in_script_dir(name):
    return join(script_dir, name)


def link_in_same_dir(target, name):
    return ('/' + target, name)


class MakeOwnedRecursivelyBy(SimpleConditionalCommand):
    def no_commands_required(self, filesystem_object):
        name = self.names
        # If this is executed although the user doesn't exist yet, find
        # issues an error message like the following. It is suited to
        # certain versions of find and a testcase,
        # so it isn't sure that it is suppressed.
        # I'm uncertain whether it should be suppressed during
        # productive use because it never occured in practice.
        c = CatchStdoutCatcherStderrMsg(
                "find: `test_useradd' is not the name of a known user")
        try:
            self.system_object.ssh_silent(
                    r'find $({0}) \! -user {1} -o \! -group {1}'.format(
                        user_home_dir_cmd(name), name), c)
        except CalledProcessError:
            return False
        if c.stdout:
            return False
        else:
            return True

    def create_single_command(self, name):
        return chown_user_home_dir_cmd(name, '-R ')

    def path_from_system_root(self):
        return self.names


def hosts_with_class(class_name, site_configuration):
    return [host.name for host in filter(
            ShallIProcessHost(site_configuration)
            .true_on_match(class_name), site_configuration['hosts'])]


class ShallIProcessHost:
    def __init__(self, site_configuration):
        self.site_configuration = site_configuration

    def true_on_match(self, class_name):
        return lambda host: self(class_name, host)

    def __call__(self, cmd_host_class, host):
        if host.name == cmd_host_class:
            return True
        def get_class(module):
            return module[cmd_host_class.split('!')[0]]
        try:
            get_class(self.site_configuration)
        except KeyError:
            try:
                get_class(globals())
            except KeyError:
                return False
        def class_in(cls):
            return any(map(lambda o: isinstance(o, cls),
                        self.overrides(host) + [host]))
        return host.is_wanted(cmd_host_class)\
                and self.exclude_condition(class_in)

    def overrides(self, host):
        """This doesn't throw TargetNotAvailable.
        The result doesn't depend on the availability of the system."""
        return host.override_instances()

    def exclude_condition(self, class_in):
        return True


def name_content(description):
        return description[0:2]


def complete_file_description(description):
    l = len(description)
    if l == 2:
        return description + (NoManipulation,)
    else:
        if l == 3:
            return description
        else:
            exit('File description has length {0} instead of 2 or 3.\n{1}'
                    .format(l, description))


def file_parts_entry(dest_file_name, **kwargs):
    return (dest_file_name,
            render_template(basename(dest_file_name), **kwargs))


def render_template(template_file_name, **kwargs):
    e = Environment(loader=FileSystemLoader(template_dir))
    try:
        t = e.get_template(template_file_name)
    except UnicodeDecodeError as u:
        exit(join(template_dir, template_file_name) + ':\n' + str(u))
    return t.render(**kwargs).encode('utf-8') + '\n'


def check_communicate(cmd, **popen_args):
    p = Popen(cmd, shell=True, **popen_args)
    stdoutdata, stderrdata = p.communicate()
    exit_code = p.wait()
    if(exit_code):
        raise CalledProcessError(exit_code, cmd)
    return stdoutdata


def walk_files(dir_tree, func):
    for directory, dirs, files in walk(dir_tree):
        for name in files:
            dest_file = strip_leading_slash(
                    join(directory[len(dir_tree):], name))
            func(dest_file, join(directory, name))


def strip_leading_slash(s):
    return s.lstrip('/')


pristine_for_rpms_bug_dir = 'pristine_for_rpms_bug'
template_dir = 'templates'


def site_dir():
    return getcwd()

def dir_of_tree(name):
    return join(site_dir(), 'trees', name)


class ClassOfSystems(object):
    def add_supers_names(self, names):
        """This adds not only the names of the super class but also the
           name of klasse to names."""
        names.extend(map(lambda cls: cls.__name__, self.supers()))

    def supers(self):
        classes = []
        self.add_supers(self.__class__, classes)
        return classes

    @staticmethod
    def add_supers(klasse, classes):
        """This adds not only the classes of the super class but also the
           class of klasse to classes."""
        classes.append(klasse)
        for c in klasse.__bases__:
            if hasattr(c, 'add_supers'):
                c.add_supers(c, classes)

    def template_entry_munin_plugin(self, name):
        return self.template_entry_executable(
                join('etc/munin/plugins', name))

    def template_entry_executable(self, dest_file_name):
        return self.template_entry(dest_file_name) + (MakeExecutable,)

    def template_entry(self, dest_file_name, **kwargs):
        return file_parts_entry(dest_file_name,
                search=self.search, **kwargs)

    def other_systems(self, class_name, site_configuration):
        other_systems = hosts_with_class(class_name, site_configuration)
        other_systems.remove(self.name)
        return other_systems


class Override(ClassOfSystems):
    def __init__(self, system_object):
        self.system_object = system_object
        self.search = system_object.search

    def __getattr__(self, name):
        if name in ('name', 'ssh_silent_stdout'):
            return getattr(self.system_object, name)
        else:
            raise AttributeError(name)

    def check_suitable_for_fromexaminationofsystem(self):
        raise NotSubclassOfFromExaminationOfSystem(self.__class__.__name__)

    def cleanup(self, remote_root):
        pass


class DontTouch(Override):
    pass


sysctl_conf = 'etc/sysctl.conf'


class QueryPackagesMixin:
    dpkg_querys_line_termination = 'install ok installed'

    def packages_list(self, query_command, packages):
        return self.ssh_silent_stdout(
                '{0} {1}|grep "{2}$"|cut --fields=1'
                .format(query_command, ' '.join(packages),
                    self.dpkg_querys_line_termination)).splitlines()


class FromExaminationOfSystem(Override):
    def check_suitable_for_fromexaminationofsystem(self):
        pass


class NotSubclassOfFromExaminationOfSystem(Exception):
    pass


class El(FromExaminationOfSystem, QueryPackagesMixin):
    install_pkg_cmd = 'yum'

    def query_installed(self, packages):
        # Executing this rpm query with an empty packages list makes rpm
        # creating ugly error messages. So it is executed conditionally:
        if packages:
            return self.packages_list(
                    'rpm --query --queryformat "%{NAME}\t'
                    + self.dpkg_querys_line_termination + '\n"', packages)
        else:
            return []

    def cleanup(self, remote_root):
        """See
        http://www.redhat.com/archives/rhl-list/2003-December/msg04713.html
        """
        def process_rpmnew(directory, action):
            def process_rpmnew(dest_file, full_path):
                remote_file = join(remote_root(), dest_file)
                rpmnew = remote_file + '.rpmnew'
                try:
                    are_equal = filecmp.cmp(full_path, rpmnew, shallow=0)
                except OSError:
                    return
                if are_equal:
                    action(rpmnew, remote_file)
            walk_files(join(site_dir(), directory), process_rpmnew)

        # If the file exists in pristine and its content equals the
        # content of the corresponding .rpmnew (on the remote system),
        # we assume the configuration file is deliberately derived
        # from the up-to-date default configuration.
        # In this case the .rpmnew can be removed:
        process_rpmnew(join('pristine', self.search.release_major),
                lambda rpmnew, conf: remove(rpmnew))

        # For example /etc/sysconfig/rhn/up2date gets sometimes
        # up2date.rpmnew although the old file wasn't edited:
        def rename_remote(rpmnew, conf):
            """so remove the old configuration and keep the new one."""
            remove(conf)
            rename(rpmnew, conf)
        process_rpmnew(pristine_for_rpms_bug_dir, rename_remote)

    def files(self):
        system_object = self.system_object
        return [(in_postinst_d('10generated'), '#!/bin/bash\n'
                'set -ex\nyum update -y\n'
                + '\n'.join(system_object.packages_cmd().all_commands()
                    + system_object.post_overlay_commands()
                    + system_object.post_inst())
                + '\n', MakeExecutable)]


class HasServiceIsOff:
    def service_is_off(self, name):
        try:
            self.system_object.ssh_silent_stdout(
                    self.exits_with_0_if_service_on.format(name))
        except CalledProcessError:
            return True
        return False


chkconfig_command = 'chkconfig --list {0} 2>/dev/null'\
            "|grep '\t0:off\t1:off\t2:on\t3:on\t4:on\t5:on\t6:off$'"


class El5(FromExaminationOfSystem, HasServiceIsOff):
    exits_with_0_if_service_on = chkconfig_command
    inst_media_dev = 'hdc'


class El6(FromExaminationOfSystem, HasServiceIsOff):
    exits_with_0_if_service_on = chkconfig_command


class El7(FromExaminationOfSystem, HasServiceIsOff):
    exits_with_0_if_service_on = "systemctl is-enabled {0}"\
            "|grep '^enabled$'"


class Debianish(FromExaminationOfSystem, QueryPackagesMixin):
    install_pkg_cmd = 'apt-get'

    def query_installed(self, packages):
        return self.packages_list(
                "dpkg-query --showformat='${Package}\t${Status}\n' --show",
                packages)


class EmulationFunction:
    def __init__(self, super_class_name, class_name):
        self.super_class_name = super_class_name
        self.class_name = class_name
        setattr(modules[__name__], class_name, self)

    def __call__(self, system_object):
        return self.klass(system_object)(system_object)

    def klass(self, system_object):
        return self.klass_memoized(system_object.get_conf_attr)

    @memoize
    def klass_memoized(self, get_conf_attr):
        return type(self.class_name,
                (get_conf_attr(self.super_class_name),), dict())


EmulationFunction('El', 'CentOS')
EmulationFunction('El', 'Scientific')
EmulationFunction('Debianish', 'Ubuntu')
EmulationFunction('Debianish', 'Debian')


class Physical(FromExaminationOfSystem):
    pass


class VMware(FromExaminationOfSystem):
    pass


class VirtualBox(FromExaminationOfSystem):
    pass


class All(ClassOfSystems):
    additional_default_implementations = {}

    def __init__(self, name, *override):
        self.name = name
        self.override = override

        self.search = Search(self)
        self.ssh_master_socket = None

        # memoizing self.remote_root makes this program four times faster.
        # Doing it this way takes also effect on a overloaded remote_root.
        # Two test cases which return a temporary directory require the
        # result being cached for working properly.
        self.remote_root = memoized(self.remote_root)
        self.instantiate_overrides = memoized(self.instantiate_overrides)

    def init(self, get_classes_from_examination_of_system, options,
            get_conf_attr):
        self.override_classes_from_examination_of_system\
            = get_classes_from_examination_of_system
        self.options = options
        self.run_mode = options.run_mode(self)
        self.get_conf_attr = get_conf_attr

    def overrides(self):
        # This order is useful for creating fractional_files because
        # generic default configurations from distribution come first.
        return self.override_instances_from_examination_of_system()\
            + self.override_instances()

    def __getattr__(self, name):
        """Forward calls to the overrides."""
        for o in self.userdefined_overrides_1st():
            try:
                return getattr(o, name)
            except AttributeError:
                pass
        default_implementations = {'inst_device': lambda: 'sda',
            'inst_media_dev': 'dvd',
        }
        default_implementations.update(
                self.additional_default_implementations)
        try:
            return default_implementations[name]
        except KeyError:
            # Abort with AttributeError to avoid that accumulate searches
            # in the overrides when it wants to know that the attribute
            # doesn't exist in self:
            raise AttributeError(name)

    def userdefined_overrides_1st(self):
        return self.override_instances()\
            + self.override_instances_from_examination_of_system()

    def override_instances(self):
        """instantiates override classes and connects them with this
           system instance."""
        return self.instantiate_overrides(self.override)

    @memoize
    def override_instances_from_examination_of_system(self):
        overrides = self.instantiate_overrides(
                self.override_classes_from_examination_of_system(self))
        for o in overrides:
            o.check_suitable_for_fromexaminationofsystem()
        return overrides

    def instantiate_overrides(self, overrides):
        """Instantiate override classes.
           They are connected to the system object."""
        return [x(self) for x in overrides]

    def accumulate(self, attr_name):
        # Climb up the class hierarchy:
        def get_list(obj):
            def make_list_member(s):
                result = get_method_result(obj, attr_name,
                        CallClassAttr(s))
                if isinstance(result, list):
                    return result
                else:
                    # When the user wants one value to be accumulated,
                    # we don't want to force him to create a list:
                    return [result]
                    # But this shortcut doesn't work for files because
                    # a file entry is already a list.
            return sum(map(make_list_member, obj.supers()), [])
        s = get_list(self)
        for o in self.overrides():
            s.extend(get_list(o))
        return s

    def get_method_result(self, attr_name):
        for o in self.userdefined_overrides_1st():
            try:
                return get_method_result(o, attr_name)
            except AttributeError:
                pass
        return get_method_result(self, attr_name)

    def distribute_files_rsync(self):
        """"Entry point" for distribute_files"""
        self.print_dest()
        self.rsync()

    def rsync(self):
        run_mode = self.run_mode
        conditional_cmds = run_mode.conditional_cmds

        conditional_cmds(self.groups_cmds())
        run_mode.rsync_additional_trees()
        # Store files before installation of packages because package
        # installation sometimes creates users, sets ownerships and
        # permissions (e.g. Ubuntu 10.04 logcheck):
        for t in self.trees():
            run_mode.rsync(dir_of_tree(t))
        for file_path_name, content, manipulate in self.generated_files(
                complete_file_description):
            self.store_remote(file_path_name, content, manipulate)

        conditional_cmds([self.packages_cmd()])
        for o in self.overrides():
            o.cleanup(self.remote_root)
        run_mode.separate_output()
        conditional_cmds(self.post_overlay())
        run_mode.rcmds(self.post_inst()) # unconditional
        run_mode.do_file_list()

    def packages_cmd(self):
        def map_package_name(name):
            """CentOS contains a package lm_sensors which corresponds to
               lm-sensors in Debian."""
            try:
                package_mapper = self.package_mapper
            except AttributeError:
                return name
            try:
                return package_mapper[name]
            except KeyError:
                return name
        return Packages(self,
                map(map_package_name, self.accumulate('packages')))

    def generated_files(self, amount_of_file_description=name_content):
        accumulate = self.accumulate
        fractional_files = {}
        for f in accumulate('file_parts'):
            file_path_name = f[0]
            file_entry = fractional_files.get(file_path_name, ('', ''))
            # file_entry[0]: accumulated file content
            # file_entry[1]: paragraph separation after first chunk
            fractional_files[file_path_name] = (
                    file_entry[0] + file_entry[1] + f[1] + '\n', '\n')
        return map(amount_of_file_description,
                map(lambda i: (i[0], i[1][0]), fractional_files.items())
                + accumulate('files'))

    def services(self):
        s = map(basename, reduce(lambda x, y: x + y, map(
           lambda tree: glob(join(dir_of_tree(tree), in_rcd_initd('*'))),
           self.trees()),
           filter(lambda l: dirname(l) == rcd_initd_dir,
               [ link for (target, link) in self._links() ])))
        # Sort for reproduceable result:
        s.sort()
        return s

    def _links(self):
        return self.accumulate('links')

    def distribute_command_rcmd(self, cmd):
        """"Entry point" for distribute_command"""
        self.rcmd(cmd, self.options.output_filter(self))

    def rcmd(self, cmd, output_catcher=ForwardToStd()):
        self.run_mode.rcmd(cmd, output_catcher)

    def store_remote(self, dest_file, content,
                     manipulate=NoManipulation):
        self.run_mode.store_remote(dest_file, content, manipulate)

    def get_remote(self, src_file):
        """returns the content of a file on the remote system."""
        try:
            return self.get_remote_file(src_file).read()
        except IOError:
            return ''

    def get_remote_file(self, src_file):
        if src_file[0] == '/':
            print(src_file)
            raise AssertionError
        return open(self.join_remote_root(src_file))

    def trees(self):
        dir_trees = set()
        names = self.class_names()
        for name in names:
            if exists(dir_of_tree(name)):
                dir_trees.add(name) # host specific
            # Directories with names like "All!host_to_be_excluded" are included
            # except the name of the host is "host_to_be_excluded":
            for tree in glob(dir_of_tree(name) + '!*'):
                dir_name = basename(tree)
                if self.is_wanted(dir_name):
                    dir_trees.add(dir_name)

        file_sets = []
        for t in dir_trees:
            fs = set()
            walk_files(dir_of_tree(t),
                    lambda dest_file, full_path: fs.add(dest_file))
            file_sets.append(fs)
        def file_sets_extend(get_name, objs):
            file_sets.extend(
                    map(lambda obj: set([get_name(obj)]), objs))
        file_sets_extend(lambda gf: gf[0],
                self.generated_files(complete_file_description))
        file_sets_extend(lambda l: l[1], self._links())
        file_sets_extend(lambda dir: dir, self.accumulate('dirs'))
        for pos, fs1 in enumerate(file_sets):
            for fs2 in file_sets[pos + 1:]:
                intersection = fs1 & fs2
                if intersection:
                    raise FileNameCollision(intersection)

        return dir_trees

    def is_wanted(self, description):
        description_split = description.split('!')
        if any([ self.issubclass(name, FromExaminationOfSystem)
                 for name in description_split ]):
            class_names = self.class_names()
        else:
            class_names = self.class_names_from_override_instances()
        not_wanted = filter(lambda nw: self.issubclass(nw, ClassOfSystems),
                description_split[1:])
        return (description_split[0] in class_names or not_wanted
               ) and set(not_wanted).isdisjoint(class_names)

    def issubclass(self, name, klass):
        try:
            return issubclass(self.get_conf_attr(name), klass)
        except KeyError:
            if name == self.name:
                return True
            return False
        except TypeError: # from EmulationFunction
            return True

    def class_names(self):
        names = self.class_names_from_override_instances()
        for g in self.override_instances_from_examination_of_system():
            g.add_supers_names(names)
        return names

    def class_names_from_override_instances(self):
        """
        This doesn't use override_instances_from_examination_of_system.
        """
        names = [self.name]
        self.add_supers_names(names)
        for g in self.override_instances():
            g.add_supers_names(names)
        return names

    def getent(self, database, key):
        getent_entry = None
        missing = None
        try:
            getent_entry = self.ssh_silent_stdout(
                    'getent {0} {1}'.format(database, key)).split(':')
        except CalledProcessError as e:
            exit_code = e.returncode
            if exit_code == 2:
                missing = key
            else:
                raise
        return (getent_entry, missing)

    def ssh_silent_stdout(self, cmd, remotes_stdin=StdinWrapper()):
        c = CatchStdout()
        self.ssh_silent(cmd, c, remotes_stdin)
        return c.stdout.rstrip('\n')

    def ssh_silent(self, cmd, output_catcher=ForwardToStd(),
                   remotes_stdin=StdinWrapper()):
        """'silent' means that 'Connection to ... closed.' is suppressed.
        Other output becomes visible except it is caught."""
        self.ssh(cmd, output_catcher, remotes_stdin)

    def ssh(self, cmd, output_catcher=ForwardToStd(),
            remotes_stdin=StdinWrapper()):
        if not cmd:
            # XXX: Where do these empty commands come from?
            return
        def assert_master_openssh_running():
            if self.master_openssh.poll() == 255:
                raise Offline(self)

        ssh_master_socket = self.ssh_master_socket
        if not ssh_master_socket:
            ssh_master_socket_dir = on_exit_vanishing_dtemp()
            ssh_master_socket = join(ssh_master_socket_dir, 'socket')
            self.ssh_master_socket = ssh_master_socket
            fd = inotifyx.init()
            try:
                wd = inotifyx.add_watch(fd, ssh_master_socket_dir)
                self.master_openssh = master_openssh = self.openssh(
                        ['-M', '-N']
                        + remote_authorized_key_env(),
                        [], stderr=PIPE)
                filter_masters_stderr_thread = threading.Thread(
                        target=self.filter_masters_stderr,
                        args=(master_openssh.stderr,)
                        )
                filter_masters_stderr_thread.daemon = True
                filter_masters_stderr_thread.start()
                # Wait for termination in the case the target is
                # not available:
                while True:
                    if inotifyx.get_events(fd, 0.1):
                        register(master_openssh.kill)
                        break
                    assert_master_openssh_running()
            finally:
                os.close(fd)
        cmd_openssh = self.openssh(output_catcher.allocate_tty, [cmd],
                remotes_stdin.remotes_stdin, output_catcher.remotes_stdout,
                output_catcher.remotes_stdout)
        communicate_with_child(cmd_openssh, output_catcher, remotes_stdin,
                assert_master_openssh_running, cmd)

    def filter_masters_stderr(self, master_sshs_stderr):
        for l in master_sshs_stderr:
            if not l.startswith(
                'process_mux_new_session: tcgetattr: Invalid argument'):
                stderr.write(l)

    def openssh(self, options, cmd, stdin=None, stdout=None, stderr=None):
        return Popen(['ssh', '-l', 'root'] + options
                + ['-S', self.ssh_master_socket, self.name]
                + cmd, stdin=stdin, stdout=stdout, stderr=stderr)

    def print_dest(self):
# XXX: Muss in rcmd, rsync und search_for_output verwendet werden!
        output = self.output
        print(colored(output, '\nDestination: '
                    + colored(output, self.name, attrs=['bold']),
                    on_color='on_green'), file=output)
        self.print_dest = tunix

    def remote_dtemp(self):
        return convert_exception_to_targetnotavailable(
                on_exit_vanishing_dtemp, NoMkdir,
                dir=self.join_remote_root('tmp'))

    def join_remote_root(self, *args):
        return join(self.remote_root(), *args)

    def remote_root(self):
        """We're memoized so calling us many times calls us only once."""
        mount_point = mkdtemp()
        register(rmdir, mount_point)

        if remote_authorized_key_env():
            authorized_keys = OtherIdentityFile()
        else:
            authorized_keys = DefaultIdentityFile(mount_point)

        stderr_catcher = StderrCatcher()
        try:
            check_call(['bash', '-c', 'sshfs {0}root@{1}:/ {2}'.format(
                authorized_keys.opt_identity_file_option,
                self.name, mount_point)
                ],
                stderr=stderr_catcher.file)
        except CalledProcessError:
            raise Offline(self)
        finally:
            stderr_catcher.write_file('read: Connection reset by peer')
        def umount():
            check_call(['fusermount', '-u', mount_point])
        register(umount)

        authorized_keys.copy_to_remote_authorized_keys()

        return mount_point

    def lv_filesystems(self):
        return ''

    def pre_overlay_commands(self):
        return commands_from_instances(self.groups_cmds)

    def groups_cmds(self):
        return self.instances_from_accumulation(Group, 'groups')

    def users_cmds(self):
        """All users need to be created before any users are added to
           groups (with usermod), because usually a group with the name of
           the user is created on user creation and if another user is to
           be added to this group it's user needs to exist.
           So we need to create all users first and after that we add them
           to groups."""
        # Take the more complete user when more than one user with equal
        # names exist:
        user_names = {}
        for u in self.instances_from_accumulation(User, 'users'):
            name = u.user
            if u.groups or name not in user_names:
                user_names[name] = u
        users = user_names.values()
        return users + [UsersGroups(self, u.user_description)
            for u in users]

    def post_overlay_commands(self):
        return commands_from_instances(self.post_overlay)

    post_overlay_call_count = 0

    def post_overlay(self):
        if self.post_overlay_call_count:
            return []
        self.post_overlay_call_count += 1
        instances_from_accumulation = self.instances_from_accumulation
        ret_val = self.users_cmds()\
            + instances_from_accumulation(Directory, 'dirs')\
            + instances_from_accumulation(Link, 'links')\
            + [ Files(self, x)
                for x in self.generated_files(complete_file_description) ]\
            + instances_from_accumulation(Service, 'services')
            # services last because "chkconfig NON-EXISTING-STARTSCRIPT on"
            # doesn't work.
        self.post_overlay_call_count -= 1
        return ret_val

    def instances_from_accumulation(self, klass, attr_name):
        return [klass(self, x) for x in self.accumulate(attr_name)]

    def post_inst(self):
        """These commands will be executed at the end of rsync."""
        return self.accumulate('commands')


class CallClassAttr:
    def __init__(self, cls):
        self.cls = cls

    def __call__(self, obj, attr_name):
        attr = self.cls.__dict__.get(attr_name, lambda o: [])
        if callable(attr):
            return attr(obj)
        else:
            return attr


def call_object_attr(obj, attr_name):
    attr = getattr(obj, attr_name)
    if callable(attr):
        return attr()
    else:
        return attr


def get_method_result(obj, attr_name, call_attr=call_object_attr):
    try:
        return call_attr(obj, attr_name)
    except TypeError as e:
        # Add class name and referenced attribute name to make
        # easy finding the faulty class:
        c = obj.__class__
        raise TypeError(c.__module__ + '.py: '
                + c.__name__ + '.' + attr_name
                + ': ' + str(e)), None, exc_info()[2]


class Search:
    def __init__(self, system_object):
        self.system_object = system_object

    def __getattr__(self, name):
        return self.system_object.get_method_result(name)


class OtherIdentityFile:
    def __init__(self):
        self.opt_identity_file_option = '-o IdentityFile={0} '.format(
                remote_authorized_key_env()[1])

    def copy_to_remote_authorized_keys(self):
        # Don't know how to do it in my case.
        pass


class DefaultIdentityFile:
    opt_identity_file_option = ''

    def __init__(self, mount_point):
        self.mount_point = mount_point

    def copy_to_remote_authorized_keys(self):
        remote_ssh_dir = join(self.mount_point, 'root/.ssh')
        remote_authorized_keys = join(remote_ssh_dir, 'authorized_keys')
        our_public_key = file_content(expanduser('~/.ssh/id_rsa.pub'))
        mkdir_p(remote_ssh_dir)
        ensure_contains(remote_authorized_keys, our_public_key)


def remote_authorized_key_env():
    return environ.get('REMOTE_AUTHORIZED_KEY', '').split()


def gethostbyname(name):
    try:
        return convert_exception_to_targetnotavailable(
                socket.gethostbyname, socket.gaierror, name)
    except:
        print('name =', name, file=stderr)
        raise


def convert_exception_to_targetnotavailable(func, expected_exception,
        *args, **kwargs):
    try:
        return func(*args, **kwargs)
    except expected_exception as e:
        raise TargetNotAvailable(e)


class TargetNotAvailable(Exception):
    pass


class Offline(TargetNotAvailable):
    def __init__(self, system_object):
        self.system_object = system_object

    def __str__(self):
        return '{0} is OFFLINE'.format(self.system_object.name)


class FileNameCollision(Exception):
    pass


class StderrCatcher:
    def __init__(self):
        self.file = TemporaryFile()

    def write_file(self, known_boring):
        tmpfile = self.file
        tmpfile.seek(0)
        std_err_str = tmpfile.read()
        if std_err_str != known_boring + '\n':
            stderr.write(std_err_str)


class Epel(Override):
    def files(self):
        return self.template_entry('etc/yum.repos.d/epel.repo')


class OnRaidMixIn(Override):
    def inst_device(self):
        return 'cciss/c0d0'

    # XXX: Maybe this shoud be in a class Scsi?
    inst_media_dev = 'scd0'


# Small self test:
# Direct subclasses of Override are allowed to exist but their subclasses
# must be preferably taken from hosts.py.
# This can't be tested in test_selfcontaining.py because the direct
# subclasses are created dynamically so that they are yield by
# __subclasses__().
subsub_classes = []
for c in Override.__subclasses__():
    if c == FromExaminationOfSystem:
        # One Exception:
        subsub_classes.extend(El.__subclasses__())
    else:
        subsub_classes.extend(c.__subclasses__())
try:
    assert(subsub_classes == [])
except AssertionError:
    print(subsub_classes)
    raise
