#!/usr/bin/env python2

# found in https://bbs.archlinux.org/viewtopic.php?id=83839
# original author is solsTiCe

# verify.py                                          Version 0.2      2009-10-18
#
# under the WTFPL. see http://sam.zoy.org/wtfpl/
#
#             DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE 
#                     Version 2, December 2004 
# 
#  Copyright (C) 2006 solsTiCe d'Hiver               <solstice.dhiver@gmail.com>
#  Everyone is permitted to copy and distribute verbatim or modified 
#  copies of this license document, and changing it is allowed as long 
#  as the name is changed. 
# 
#             DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE 
#    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION 
# 
#   0. You just DO WHAT THE FUCK YOU WANT TO.

'''a python script that compare the files in a package tarball and the files
installed on the system:
    It reports changed files, missing files'''

import sys
import os
import tarfile
from glob import glob
from StringIO import StringIO
import hashlib
import subprocess
try:
    import lzma
except ImportError:
    print >>sys.stderr, 'You need the python bindings for lzma: python-pyliblzma or pylzma'

import colorhelper

DB = '/var/lib/pacman'
CACHE = '/var/cache/pacman/pkg'
ARCH = subprocess.check_output('uname -m', shell = True).strip()

# TODO: use logging module ?
def error(s):
    short, rgb = colorhelper.rgb2short('FF0000')
    sys.stdout.write('\033[38;5;%smError: \033[0m%s\n' % (short, s))

def warning(s):
    short, rgb = colorhelper.rgb2short('d78700')
    sys.stdout.write('\033[38;5;%smWarning: \033[0m%s\n' % (short, s))

def getpkgname(pkgfile):
    '''return package name from its pkgfilename'''
    pkg = '.'.join(os.path.basename(pkgfile).split('.')[:-3]) # remove trailing .pkg.tar.gz
    pkg = '-'.join(pkg.split('-')[:-1])  # remove arch field
    return pkg

def ispkginstalled(pkgfile):
    '''return wether or not a package is installed'''
    return os.path.exists('/'.join([DB, 'local', getpkgname(pkgfile)]))

def checkpkg(pkgfile):
    '''check if the installed files of a package have changed compared to the
    files in the package tarball'''
    missing = 0
    changed = 0
    md5s = pkgmd5s(pkgfile)
    for filename,md5 in md5s.items():
        ff = '/' + filename
        ff = ff.replace('GNUSparseFile/', '')
        if os.path.exists(ff):
            try:
                with open(ff) as g:
                    if hashlib.md5(g.read()).hexdigest() != md5:
                        warning('%s has changed' % ff)
                        changed += 1
            except IOError as e:
                error('%s: %s ' % (e.strerror, filename))
                continue
        else:
            warning('%s has not been found' % ff)
            missing += 1
    return (len(md5s), changed, missing)

def listpkg():
    '''return a list of all installed pkg'''
    return sorted(os.listdir('/'.join([DB, 'local'])))

def findpkgtarball(s, dbpkg):
    '''look for an installed pkg matching s and return the path of tarball in
    the cache'''
    candidates = [p for p in dbpkg if p.startswith(s)]
    found = False
    if s in candidates:
        found = True
        name = s
    else:
        for i in candidates:
            pkgname = '-'.join(i.split('-')[:-2])
            if pkgname == s:
                found = True
                name = i
                break
    if found:
        res = glob('/'.join([CACHE, name + '*']))
        if len(res) == 1:
            return res[0]
        elif len(res) >= 2:
            # we might have x86_64 and x86 copies of the package
            res = glob('/'.join([CACHE, name + '-' + ARCH + '*']))
            if len(res) == 1:
                return res[0]
    return None

def pkgmd5s(pkgfile):
    '''return a dictionary of filename and md5 of the package'''
    if pkgfile.endswith('xz'):
        with open(pkgfile, 'r') as f:
            tf = tarfile.TarFile(fileobj=StringIO(lzma.decompress(f.read())))
    else:
        tf = tarfile.open(pkgfile, 'r')
    md5s = {}
    for ti in tf:
        if ti.isfile() and ti.name not in ('.PKGINFO', '.INSTALL', '.CHANGELOG', '.MTREE'):
            f = tf.extractfile(ti)
            md5s[ti.name] = hashlib.md5(f.read()).hexdigest()
            f.close()
    tf.close()
    return md5s

if __name__ == '__main__':
    try:
        pkglist = sys.argv[1:]

        dbpkg = listpkg()
        if pkglist == []:
            pkglist = dbpkg

        for pkg in pkglist:
            if os.path.exists(pkg):
                if not ispkginstalled(pkg):
                    error('%s is not installed' % getpkgname(pkg))
                    continue
            else:
                # if we do not find the archive pkg, then look for a package with
                # that name in the db of pacman
                p = findpkgtarball(pkg, dbpkg)
                if p != None:
                    pkg = p
                else:
                    error('%s not found in the cache' % pkg)
                    continue
            # finaly check the tarball pkg
            (n,c,m) = checkpkg(pkg)
            if c != 0 or m != 0:
                print('%s: %d files, %d changed, %d missing' % (getpkgname(pkg), n, c, m))
    except KeyboardInterrupt:
        pass
