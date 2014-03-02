#! /usr/bin/env python
# encoding: utf-8

import os
import os.path
import sys
from waflib import Options

APPNAME = 'kodkod'
VERSION = '2.0'

def options(opt):
    solver_options = opt.add_option_group('solver options')
    solver_options.add_option('--no-solvers', dest='build_solvers', default=True, action='store_false', help="skips building native SAT solvers")
    opt.recurse('src lib tests')

def deps(ctx):
    ctx.recurse('tests')
    ctx.install_deps('deps')

def configure(conf):
    conf.env.DEPS_DIR = os.path.abspath('./deps')
    conf.recurse('src lib tests')

def build(bld):
    if not bld.variant:
        bld.recurse('src')
    if Options.options.build_solvers:
        bld.recurse('lib')

def test_build(bld):
    bld.recurse('tests')

def dist(dst):
    dst.base_name = APPNAME + '-' + VERSION
    dst.algo      = 'zip'
    dst.excl      = '**/.* **/*~ **/*.pyc **/*.swp **/CVS/** **/taglet/**'
    dst.files     = dst.path.ant_glob('LICENSE NEWS MANIFEST wscript src/** lib/** tests/**', excl=dst.excl)

def test(tst):
    tst.recurse('tests')

def all(ctx):
    Options.commands = [
        'build',
        'install',
        'test_build',
        'test',
        'dist'] + Options.commands



from waflib.Build import BuildContext
class TestBuildContext(BuildContext):
    '''builds the project's tests'''
    cmd = 'test_build'
    fun = 'test_build'

class TestContext(BuildContext):
    '''runs the project's tests'''
    cmd = 'test'
    fun = 'test'

from waflib.Context import Context
class DepsContext(Context):
    '''downloads project dependencies'''
    cmd = 'deps'
    fun = 'deps'
    deps = {}

    def add_dep(self, file, url):
        self.deps[file] = url

    def install_deps(self, deps_dir):
        if not os.path.exists(deps_dir):
          os.makedirs(deps_dir)

        deps_dir = os.path.abspath(deps_dir)
        for file, url in self.deps.iteritems():
            download_path = os.path.join(deps_dir, file)
            cmd = ["curl", url, "-o", download_path]
            self.exec_command(cmd)
