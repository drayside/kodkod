#! /usr/bin/env python
# encoding: utf-8

import os.path

APPNAME = 'kodkod'
VERSION = '1.5'

def options(opt):
    opt.recurse('src lib')
     
def configure(conf):
    conf.recurse('src lib')
     
def build(bld):
    if not bld.variant:
        bld.recurse('src lib')
    else:
        bld.recurse('lib')    
    
def dist(dst):
    dst.base_name = APPNAME + '-' + VERSION
    dst.algo      = 'zip'
    dst.excl      = '**/.waf-1* **/*~ **/*.pyc **/*.swp **/.lock-w* **/CVS/** **/taglet/**' 
    dst.files     = dst.path.ant_glob('LICENSE NEWS MANIFEST wscript src/** lib/**', excl=dst.excl)
