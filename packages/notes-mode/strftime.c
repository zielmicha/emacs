
/*
 * strftime.c
 * $Id: strftime.c,v 1.4 1999/01/28 19:26:29 johnh Exp $
 *
 * Copyright (C) 1996 by John Heidemann.
 * Comments to <johnh@isi.edu>.
 *
 * This file is under the Gnu Public License, version 2.
 * For details see the COPYING which accompanies this distribution.
 *
 */

#include <time.h>

void
usage()
{
	printf ("usage: strftime format epoch_time\n");
	printf ("\tThis program exists because Perl's POSIX loads in 880ms\n");
	printf ("\twhile this program can run in ~20ms,\n");
	printf ("\tand because some NT users find Perl's POSIX lacks strftime.\n");
	exit (1);
}

main(argc, argv)
	int argc;
	char **argv;
{
#define SLEN 1024
	char s[SLEN];
	struct tm *tm;
	time_t time;

	if (argc != 3)
		usage();
	time = atol(argv[2]);   /* sigh.  Hope int==time_t */
	tm = localtime(&time);
	strftime(s, SLEN, argv[1], tm);
	printf ("%s\n", s);
}
