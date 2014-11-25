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


#from os.path import splitext
from glob import glob
from distutils.core import setup
from aslib.predefined import walk_files

package_data = []
def collect_files(dest_file, full_path):
#   if splitext(dest_file)[1] != '.pyc':
        package_data.append(test_sites + '/' + dest_file)


lib = 'aslib'
test_sites = 'test_sites'
walk_files(lib + '/' + test_sites, collect_files)
setup(packages=[lib], scripts=glob('bin/*'),
                                         package_data={lib: package_data})
#data_files=[('share', ['test_sites'])])
