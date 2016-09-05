/* fatlabel.c - User interface

   Copyright (C) 1993 Werner Almesberger <werner.almesberger@lrc.di.epfl.ch>
   Copyright (C) 1998 Roman Hodek <Roman.Hodek@informatik.uni-erlangen.de>
   Copyright (C) 2007 Red Hat, Inc.
   Copyright (C) 2008-2014 Daniel Baumann <mail@daniel-baumann.ch>
   Copyright (C) 2015 Andreas Bombe <aeb@debian.org>

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

#include "version.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include <ctype.h>

#include "common.h"
#include "fsck.befs.h"
#include "io.h"
#include "boot.h"
#include "fat.h"

int interactive = 0, rw = 0, list = 0, test = 0, verbose = 0, write_immed =
    0;
unsigned n_files = 0;
void *mem_queue = NULL;

static void usage(int error)
{
    FILE *f = error ? stderr : stdout;
    int status = error ? 1 : 0;

    fprintf(f, "usage: fatlabel device [label]\n");
    exit(status);
}

int main(int argc, char *argv[])
{
    rw = 0;

    int i;

    char *device = NULL;
    char label[B_OS_NAME_LENGTH] = { 0 };

    if (argc < 2 || argc > 3)
        usage(1);

    if (!strcmp(argv[1], "-h") || !strcmp(argv[1], "--help"))
        usage(0);
    else if (!strcmp(argv[1], "-V") || !strcmp(argv[1], "--version")) {
        printf("befslabel " VERSION " (" VERSION_DATE ")\n");
        exit(0);
    }

    device = argv[1];
    if (argc == 3) {
        strncpy(label, argv[2], B_OS_NAME_LENGTH);
        if (strlen(argv[2]) > B_OS_NAME_LENGTH - 1) {
            fprintf(stderr,
                    "befslabel: labels can be no longer than %d characters\n",
                    B_OS_NAME_LENGTH);
            exit(1);
        }
        rw = 1;
    }

    fs_open(device, rw);
    if (!rw) {
        fs_read(SECTOR_SIZE, B_OS_NAME_LENGTH, label);
        fprintf(stdout, "%s\n", label);
        exit(0);
    }

    fs_write(SECTOR_SIZE, B_OS_NAME_LENGTH, label);
    fs_close(rw);
    return 0;
}
