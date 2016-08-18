/* fsck.fat.c - User interface

   Copyright (C) 1993 Werner Almesberger <werner.almesberger@lrc.di.epfl.ch>
   Copyright (C) 1998 Roman Hodek <Roman.Hodek@informatik.uni-erlangen.de>
   Copyright (C) 2008-2014 Daniel Baumann <mail@daniel-baumann.ch>

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.

   The complete text of the GNU General Public License
   can be found in /usr/share/common-licenses/GPL-3 file.
*/

/* FAT32, VFAT, Atari format support, and various fixes additions May 1998
 * by Roman Hodek <Roman.Hodek@informatik.uni-erlangen.de> */

#include "version.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>

#include "common.h"
#include "fsck.fat.h"
#include "io.h"
#include "boot.h"
#include "fat.h"
#include "file.h"
#include "check.h"
#include "charconv.h"

int interactive = 0, rw = 0, list = 0, test = 0, verbose = 0, write_immed = 0;
int boot_only = 0;
unsigned n_files = 0;
void *mem_queue = NULL;

static void usage(char *name)
{
    fprintf(stderr, "usage: %s [-aAbflrtvVwy] [-d path -d ...] "
	    "[-u path -u ...]\n%15sdevice\n", name, "");
    fprintf(stderr, "  -a       automatically repair the filesystem\n");
    fprintf(stderr, "  -A       toggle Atari filesystem format\n");
    fprintf(stderr, "  -b       make read-only boot sector check\n");
    fprintf(stderr,
	    "  -c N     use DOS codepage N to decode short file names (default: %d)\n",
	    DEFAULT_DOS_CODEPAGE);
    fprintf(stderr, "  -d path  drop that file\n");
    fprintf(stderr, "  -f       salvage unused chains to files\n");
    fprintf(stderr, "  -l       list path names\n");
    fprintf(stderr,
	    "  -n       no-op, check non-interactively without changing\n");
    fprintf(stderr, "  -p       same as -a, for compat with other *fsck\n");
    fprintf(stderr, "  -r       interactively repair the filesystem (default)\n");
    fprintf(stderr, "  -t       test for bad clusters\n");
    fprintf(stderr, "  -u path  try to undelete that (non-directory) file\n");
    fprintf(stderr, "  -v       verbose mode\n");
    fprintf(stderr, "  -V       perform a verification pass\n");
    fprintf(stderr, "  -w       write changes to disk immediately\n");
    fprintf(stderr, "  -y       same as -a, for compat with other *fsck\n");
    exit(2);
}
