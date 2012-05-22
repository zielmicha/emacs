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

## Example usage from procmail (all arguments are compulsory):

## :0c
## * ^TO_emacs-elpa-diffs@gnu\.org
## | forward-diffs.py -m maintfile -l logfile -s sender

## where 

## sender = your email address
## logfile = file to write log to (you might want to rotate/compress/examine it)
## maintfile = file listing packages and their maintainers, with format:
##
## package1   email1
## package2   email2,email3
##
## Use "nomail" for the email field to not send a mail.
##
## overmaintfile = like maintfile, but takes precedence over it.

### Code:

import optparse
import sys
import re
import email
import smtplib
import datetime
import os

usage="""usage: %prog <-m maintfile> <-l logfile> <-s sender>
   [-o overmaintfile] [--sendmail]
Take a GNU ELPA diff on stdin, and forward it to the maintainer(s)."""

parser = optparse.OptionParser()
parser.set_usage ( usage )
parser.add_option( "-m", dest="maintfile", default=None,
                   help="file listing packages and maintainers")
parser.add_option( "-l", dest="logfile", default=None,
                   help="file to append output to")
parser.add_option( "-o", dest="overmaintfile", default=None,
                   help="override file listing packages and maintainers")
parser.add_option( "-s", dest="sender", default=None,
                   help="sender address for forwards")
parser.add_option( "--sendmail", dest="sendmail", default=False,
                   action="store_true", help="use sendmail rather than smtp")

( opts, args ) = parser.parse_args()

if not opts.logfile:
    parser.error('No logfile specified')

if not opts.maintfile:
    parser.error('No maintfile specified')

if not opts.sender:
    parser.error('No sender specified')


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

## Each element is package: maint1, maint2, ...
maints = {}

for line in mfile:
    if re.match( '^#|^ *$', line ): continue
    (pack, maint) = line.split()
    maints[pack] = maint.split(',')

mfile.close()


if opts.overmaintfile:
    try:
        ofile = open( opts.overmaintfile, 'r' )
    except Exception as err:
        lfile.write('Error opening overmaintfile: %s\n' % str(err))
        sys.exit(1)

    for line in ofile:
        if re.match( '^#|^ *$', line ): continue
        (pack, maint) = line.split()
        maints[pack] = maint.split(',')

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
packs_seen = []
maints_seen = []

for line in text.splitlines():

    if re.match( '^modified:$', line ):
        start = True
        continue

    if not start: continue

    if re.match( '^ *$', line ): break


    reg = re.match( '^packages/([^/]+)', line.strip() )
    if not reg: break


    pack = reg.group(1)

    lfile.write('Package: %s\n' % pack)

    if pack in packs_seen:
        lfile.write('Already seen this package\n')
        continue

    packs_seen.append(pack)


    if not pack in maints:
        lfile.write('Unknown maintainer\n')
        continue


    for maint in maints[pack]:

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
