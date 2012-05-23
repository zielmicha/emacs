#!/usr/bin/python
### forward-diffs.py --- forward emacs-elpa-diffs mails to maintainers

## Copyright (C) 2012 Free Software Foundation, Inc.

## Author: Glenn Morris <rgm@gnu.org>

## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.

### Commentary:

## Forward emails from the emacs-elpa-diffs mailing list to the
## maintainer(s) of the modified files.

## Two modes of operation:

## 1) Create the maintfile (really this is just an optimization):
## forward-diffs.py --create -p packagesdir -m maintfile

## You can start with an empty maintfile and normal operation in 2)
## will append information as needed.

## 2) Call from eg procmail to forward diffs.  Example usage:

## :0c
## * ^TO_emacs-elpa-diffs@gnu\.org
## | forward-diffs.py -p packagedir -m maintfile -l logfile \
## -o overmaint -s sender

## where 

## packagedir = /path/to/packages
## sender = your email address
## logfile = file to write log to (you might want to rotate/compress/examine it)
## maintfile = file listing files and their maintainers, with format:
##
## package1/file1   email1
## package2/file2   email2,email3
##
## Use "nomail" for the email field to not send a mail.
##
## overmaint = like maintfile, but takes precedence over it.

### Code:

import optparse
import sys
import re
import email
import smtplib
import datetime
import os


## Scan FILE for Author or Maintainer (preferred) headers.
## Return a list of all email addresses found in MAINTS.
def scan_file(file, maints):

    try:
        fd = open( file, 'r')
    except Exception as err:
        lfile.write('Error opening file %s: %s\n' % (file, str(err)))
        return 1

    ## Max number of lines to scan looking for a maintainer.
    ## (20 seems to be the highest at present).
    max_lines = 50
    nline = 0
    cont = 0
    type = ""

    for line in fd:

        nline += 1

        if ( nline > max_lines ): break

        ## Try and de-obfuscate.  Worth it?
        line = re.sub( '(?i) AT ', '@', line )
        line = re.sub( '(?i) DOT ', '.', line )

        if cont:           # continued header?
            reg = re.match( ('%s[ \t]+[^:]*?<?([\w.-]+@[\w.-]+)>?' % prefix), line, re.I )
            if not reg:         # not a continued header
                cont = 0
                prefix = ""
                if ( type == "maint" ): break
                type = ""

        ## Check for one header immediately after another.
        if not cont:
            reg = re.match( '([^ ]+)? *(Author|Maintainer)s?: .*?<?([\w.-]+@[\w.-]+)>?', line, re.I )
            

        if not reg: continue

        if cont:
            email = reg.group(1)
            maints.append(email)
        else:
            cont = 1
            prefix = reg.group(1) or ""
            type = reg.group(2)
            email = reg.group(3)
            type = "maint" if re.search( 'Maintainer', type, re.I ) else "auth"
            ## maints = [] does the wrong thing.
            if type == "maint": del maints[:]
            maints.append(email)

    fd.close()


## Scan all the files under dir for maintainer information.
## Write to stdout, or optional argument outfile (which is overwritten).
def scan_dir(dir, outfile=None):

    dir = re.sub( '/+$', '', dir) + '/' # ensure trailing /

    if not os.path.isdir(dir):
        sys.stderr.write('No such directory: %s\n' % dir)
        sys.exit(1)

    fd = 0
    if outfile:
        try:
            fd = open( outfile, 'w' )
        except Exception as err:
            sys.stderr.write("Error opening `%s': %s\n" % (outfile, str(err)))
            sys.exit(1)


    for dirpath, dirnames, filenames in os.walk(dir):
        for file in filenames:
            path = os.path.join(dirpath, file)
            maints = []
            scan_file(path, maints)
            ## This would skip printing empty maints.
            ## That would mean we would scan the file each time for no reason.
            ## But empty maintainers are an error at present.
            if not maints: continue
            path = re.sub( '^%s' % dir, '', path )
            string = "%-50s %s\n" % (path, ",".join(maints))
            if fd:
                fd.write(string)
            else:
                print string,

    if fd: fd.close()


usage="""usage: %prog <-p /path/to/packages> <-m maintfile>
   <-l logfile -s sender|--create> [-o overmaintfile] [--sendmail] [--debug]
Take a GNU ELPA diff on stdin, and forward it to the maintainer(s)."""

parser = optparse.OptionParser()
parser.set_usage ( usage )
parser.add_option( "-m", dest="maintfile", default=None,
                   help="file listing packages and maintainers")
parser.add_option( "-l", dest="logfile", default=None,
                   help="file to append output to")
parser.add_option( "-o", dest="overmaintfile", default=None,
                   help="override file listing packages and maintainers")
parser.add_option( "-p", dest="packagedir", default=None,
                   help="path to packages directory")
parser.add_option( "-s", dest="sender", default=None,
                   help="sender address for forwards")
parser.add_option( "--create", dest="create", default=False,
                   action="store_true", help="create maintfile")
parser.add_option( "--sendmail", dest="sendmail", default=False,
                   action="store_true", help="use sendmail rather than smtp")
parser.add_option( "--debug", dest="debug", default=False,
                   action="store_true", help="debug only, do not send mail")


( opts, args ) = parser.parse_args()


if not opts.maintfile:
    parser.error('No maintfile specified')

if not opts.packagedir:
    parser.error('No packagedir specified')

if not os.path.isdir(opts.packagedir):
    sys.stderr.write('No such directory: %s\n' % opts.packagedir)
    sys.exit(1)


if not opts.create:
    if not opts.logfile:
        parser.error('No logfile specified')

    if not opts.sender:
        parser.error('No sender specified')


## Create the maintfile.
if opts.create:
    scan_dir( opts.packagedir, opts.maintfile )
    sys.exit()


try:
    lfile = open( opts.logfile, 'a' )
except Exception as err:
    sys.stderr.write('Error opening logfile: %s\n' % str(err))
    sys.exit(1)


try:
    mfile = open( opts.maintfile, 'r' )
except Exception as err:
    lfile.write('Error opening maintfile: %s\n' % str(err))
    sys.exit(1)

## Each element is package/file: maint1, maint2, ...
maints = {}

for line in mfile:
    if re.match( '#| *$', line ): continue
    ## FIXME error here if empty maintainer.
    (pfile, maint) = line.split()
    maints[pfile] = maint.split(',')

mfile.close()


if opts.overmaintfile:
    try:
        ofile = open( opts.overmaintfile, 'r' )
    except Exception as err:
        lfile.write('Error opening overmaintfile: %s\n' % str(err))
        sys.exit(1)

    for line in ofile:
        if re.match( '#| *$', line ): continue
        (pfile, maint) = line.split()
        maints[pfile] = maint.split(',')

    ofile.close()


stdin = sys.stdin

text = stdin.read()


resent_via = 'GNU ELPA diff forwarder'

message = email.message_from_string( text )

(msg_name, msg_from) = email.utils.parseaddr( message['from'] )

lfile.write('\nDate: %s\n' % str(datetime.datetime.now()))
lfile.write('Message-ID: %s\n' % message['message-id'])
lfile.write('From: %s\n' % msg_from)

if resent_via == message['x-resent-via']:
    lfile.write('Mail loop; aborting\n')
    sys.exit(1)


start = False
pfiles_seen = []
maints_seen = []

for line in text.splitlines():

    if re.match( 'modified:$', line ):
        start = True
        continue

    if not start: continue

    if re.match( ' *$', line ): break


    reg = re.match( 'packages/([^ ]+)', line.strip() )
    if not reg: break


    pfile = reg.group(1)

    lfile.write('File: %s\n' % pfile)

    ## Should not be possible for files (rather than packages)...
    if pfile in pfiles_seen:
        lfile.write('Already seen this file\n')
        continue

    pfiles_seen.append(pfile)


    if not pfile in maints:

        lfile.write('Unknown maintainer, scanning file...\n')

        thismaint = []
        thisfile = os.path.join( opts.packagedir, pfile )

        scan_file( thisfile, thismaint )

        if not thismaint: continue

        maints[pfile] = thismaint

        ## Append maintainer to file.
        try:
            mfile = open( opts.maintfile, 'a' )
            string = "%-50s %s\n" % (pfile, ",".join(thismaint))
            mfile.write(string)
            mfile.close()
            lfile.write('Appended to maintfile\n')
        except Exception as err:
            lfile.write('Error appending to maintfile: %s\n' % str(err))


    for maint in maints[pfile]:

        lfile.write('Maint: %s\n' % maint)


        if maint in maints_seen:
            lfile.write('Already seen this maintainer\n')
            continue

        maints_seen.append(maint)


        if maint == "nomail":
            lfile.write('Not resending, no mail is requested\n')
            continue


        if maint == msg_from:
            lfile.write('Not resending, since maintainer = committer\n')
            continue


        forward = message
        forward.add_header('X-Resent-Via', resent_via)
        forward.add_header('Resent-To', maint)
        forward.add_header('Resent-From', opts.sender)

        lfile.write('Resending via %s...\n' % ('sendmail'
                    if opts.sendmail else 'smtp') )


        if opts.debug: continue


        if opts.sendmail:
             s = os.popen("/usr/sbin/sendmail -i -f %s %s" %
                          (opts.sender, maint), "w")
             s.write(forward.as_string())
             status = s.close()
             if status:
                 lfile.write('Sendmail exit status: %s\n' % status)

        else:

            try:
                s = smtplib.SMTP('localhost')
            except Exception as err:
                lfile.write('Error opening smtp: %s\n' % str(err))
                sys.exit(1)

            try:
                s.sendmail(opts.sender, maint, forward.as_string())
            except Exception as err:
                lfile.write('Error sending smtp: %s\n' % str(err))

            s.quit()

### forward-diffs.py ends here
