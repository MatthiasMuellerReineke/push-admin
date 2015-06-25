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

from os import readlink, stat, chmod
from os.path import join, islink, lexists, isdir, dirname
from sys import stderr
from subprocess import CalledProcessError

from utilities import instantiate
from remote_exec import ForwardToStd, CatcherStderrMsg


class CommandsFromActions:
    def commands(self, filesystem_object=None):
        if not filesystem_object:
            filesystem_object = self
        return [ c.create_single_command(
                filesystem_object.path_from_system_root())
        for c in filter(lambda c:
                not c.no_commands_required(filesystem_object),
                self.actions) ]

    def all_commands(self, path=None):
        if path == None:
            path = self.path_from_system_root()
        return [ a.create_single_command(path) for a in self.actions ]


class SimpleConditionalCommand(CommandsFromActions):
    output_catcher = ForwardToStd()

    def __init__(self, system_object, names):
        self.set_system_object(system_object)
        self.names = names

    def create_commands(self, names):
        if names:
            return [self.create_single_command(names)]
        else:
            return []

    def make_mode_all_commands(self):
        return self.make_mode().all_commands(self.path_from_system_root())

    def set_system_object(self, system_object):
        self.system_object = system_object
        self.actions = [self]

    def path_in_remote_root(self):
        return self.system_object.join_remote_root(
                self.names[self.object_path_index])

    def path_from_system_root(self):
        return self.names[0]


class Directory(SimpleConditionalCommand):
    object_path_index = 0
    no_manipulation_desired_mode = 0755

    def __init__(self, system_object, dir_description):
        self.set_system_object(system_object)
        if isinstance(dir_description, str):
            self.names = (dir_description, MakeExecutable)
        else:
            self.names = dir_description

        make_mode = self.make_mode()
        self.actions = [self, make_mode] + make_mode.actions

    def no_commands_required(self, filesystem_object):
        return isdir(self.path_in_remote_root())

    def create_single_command(self, path):
        return mkdir_command(self.path_from_system_root())

    def make_mode(self):
        return self.names[1]


def mkdir_command(path):
    return 'mkdir -p /' + path


class Files(SimpleConditionalCommand):
    def commands(self):
        return self.make_mode().commands(self)

    def all_commands(self):
        return self.make_mode_all_commands()

    def make_mode(self):
        return self.names[2]


class Link(SimpleConditionalCommand):
    object_path_index = 1

    def __init__(self, system_object, description):
        self.set_system_object(system_object)
        self.names = description
        self.actions = [Directory(system_object,
                    dirname(self.path_from_system_root())), self]

    def no_commands_required(self, filesystem_object):
        path = self.path_in_remote_root()
        return islink(path) and readlink(path) == self.names[0]

    def create_single_command(self, names):
        return 'ln --force --no-target-directory -s {0} "/{1}"'\
            .format(*self.names)

    def path_from_system_root(self):
        return self.names[1]


class Service(SimpleConditionalCommand):
    def commands(self):
        name = self.names
        try:
# XXX: Das müsste nach no_commands_required!
            self.system_object.ssh_silent_stdout(
                "chkconfig --list " + name + ' 2>/dev/null'
                "|grep '\t0:off\t1:off\t2:on\t3:on\t4:on\t5:on\t6:off$'")
        except CalledProcessError:
            return self.create_commands(name)
        return []

    def create_single_command(self, name):
        return 'chkconfig ' + name + ' on'

    def path_from_system_root(self):
        return self.names


class Packages(SimpleConditionalCommand):
    packagemanagers_install_cmd = ' install -y '

    def commands(self):
        return self.create_commands(set(self.names)
                - set(self.system_object.query_installed(self.names)))

    def all_commands(self):
        return self.create_commands(self.names)

    def create_single_command(self, names):
        return self.system_object.install_pkg_cmd\
            + self.packagemanagers_install_cmd + ' '.join(names)


class UsersBase:
    shell = None

    def __init__(self, system_object, user_description):
        self.user_description = user_description
        self.system_object = system_object
        self.groups = []
        self.manipulations = []

        if isinstance(user_description, str):
            self.user = user_description
            return
        else:
            self.user = user_description[0]
            for i in user_description[1:]:
                if isinstance(i, tuple):
                    # XXX: Dangerous, write test
                    # which detects the danger ...!
                    self.shell = i[1]
                else:
                    if isinstance(i, str):
                        self.groups.append(i)
                    else:
                        self.manipulations.append(
                                i(system_object, self.user))

    def getent_user(self):
        return self.getent('passwd', self.user)

    def getent(self, *args):
        return self.system_object.getent(*args)


class User(UsersBase):
    output_catcher = CatcherStderrMsg(
            'useradd: warning: the home directory already exists.\n'\
                'Not copying any file from skel directory into it.')

    def commands(self):
        getent_entry, missing_user = self.getent_user()
        return self.create_commands(missing_user) + sum([x.commands()
                    for x in self.manipulations], [])

    def all_commands(self):
        return self.create_commands(self.user)\
            + commands_from_instances(lambda: self.manipulations)

    def create_commands(self, user):
        if user:
            return [self.cmd(user)]
        else:
            return []

    @staticmethod
    def cmd(user_name):
        """The ownership and group are set to the user because the
           user's home directory may be created by an overlay and thus
           be owned by root.
        """
        return 'useradd --create-home {0}&&{1}'.format(
                user_name, chown_user_home_dir_cmd(user_name))


class UsersGroups(UsersBase):
    output_catcher = ForwardToStd()

    def commands(self):
        missing_groups = []
        for g in self.groups:
            # We use getent instead of groups because groups yields also
            # the group from passwd:
            group_entry, missing_group = self.getent('group', g)
            if missing_group\
                or self.user not in group_entry[-1].split(','):
                missing_groups.append(g)

        shell = self.shell
        missing_shell = None
        if shell:
            getent_entry, missing_user = self.getent_user()
            if not getent_entry or getent_entry[6] != shell:
                missing_shell = shell

        return self.create_commands(missing_groups, missing_shell)

    def all_commands(self):
        return self.create_commands(self.groups, self.shell)

    def create_commands(self, groups, shell):
        commands = []
        if groups or shell:
            if shell:
                shell_tuple = (shell,)
            else:
                shell_tuple = ()
            commands.append('usermod {0} {1}'.format(' '.join(
                        option_with_values('groups', groups, '--append ')
                        + option_with_values('shell', shell_tuple)),
                        self.user))
        return commands


def chown_user_home_dir_cmd(user_name, options=''):
    return 'chown {2}{0}:{0} $({1})'.format(
                user_name, user_home_dir_cmd(user_name), options)


def user_home_dir_cmd(user_name):
    return getent_user(user_name) + '|cut --delimiter=: --fields=6'


def getent_user(user_name):
    return 'getent passwd ' + user_name


class Group(SimpleConditionalCommand):
    def commands(self):
        getent_entry, missing = self.system_object.getent(
                'group', self.names)
        return self.create_commands(missing)

    def create_single_command(self, group):
        return 'groupadd ' + group

    def path_from_system_root(self):
        return self.names


def option_with_values(name, values, combined_option=''):
    if values:
        return ['{2}--{0} {1}'.format(
                name, ','.join(values), combined_option)]
    else:
        return []


def commands_from_instances(instances_func):
    return sum(map(lambda o: o.all_commands(), instances_func()), [])


@instantiate
class NoManipulation:
    desired_mode = 0644

    def commands(*args):
        return []

    all_commands = commands

    def execute(self, path):
        pass

    def execute_dry(self, path, dest_file):
        pass


class Make(CommandsFromActions):
    def __init__(self, desired_mode=None, **kwargs):
        self.desired_mode = desired_mode
        actions = []
        for a in kwargs.items():
            actions.append({'group': ChangeGroup, 'user': ChangeUser}\
                    [a[0]](a[1]))
        self.actions = actions

    def execute(self, path):
        chmod(path, self.desired_mode)

    def execute_dry(self, path, dest_file):
        """This prints changes of the file mode in context with the diff
        output of the file. dispensing this feature would allow to unify
        the handling of the mode and ownerships and thus lead to a cleaner
        design.
        """
        default_mode = NoManipulation.desired_mode
        if not self.no_commands_required(RecreatedFilesystemObject(path)):
            print('/{2}: Current mode={0:04o}  desired mode={1:04o}'\
                .format(self.current_mode(path, default_mode),
                    self.desired_mode, dest_file))

    def current_mode(self, path, default_mode):
        if lexists(path):
            return stat(path).st_mode & 07777
        else:
            return default_mode

    def no_commands_required(self, filesystem_object):
        return self.current_mode(filesystem_object.path_in_remote_root(),
                filesystem_object.no_manipulation_desired_mode)\
            == self.desired_mode

    def create_single_command(self, path):
        desired_mode = self.desired_mode
        if desired_mode == None:
            return ''
        else:
            return 'chmod {0:03o} /{1}'.format(desired_mode, path)


class RecreatedFilesystemObject:
    no_manipulation_desired_mode = 0644

    def __init__(self, path):
        self.path = path

    def path_in_remote_root(self):
        return self.path


class ChangeFilesystemObject:
    def __init__(self, desired_value):
        self.desired_value = desired_value

    def no_commands_required(self, filesystem_object):
        try:
            return filesystem_object.system_object.ssh_silent_stdout(
                'stat --format=%{0} /{1} 2>/dev/null'.format(
                    self.stat_format,
                    filesystem_object.path_from_system_root())
                ) == self.desired_value
        except CalledProcessError:
            return False

    def create_single_command(self, path):
        return 'ch{0} {1} /{2}'.format(self.ch, self.desired_value, path)


class ChangeGroup(ChangeFilesystemObject):
    stat_format = 'G'
    ch = 'grp'


class ChangeUser(ChangeFilesystemObject):
    stat_format = 'U'
    ch = 'own'


@instantiate
class MakeExecutable(Make):
    def __init__(self):
        Make.__init__(self, 0755)
