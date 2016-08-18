/* check.c - Check and repair a PC/MS-DOS filesystem

   Copyright (C) 1993 Werner Almesberger <werner.almesberger@lrc.di.epfl.ch>
   Copyright (C) 1998 Roman Hodek <Roman.Hodek@informatik.uni-erlangen.de>
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

/* FAT32, VFAT, Atari format support, and various fixes additions May 1998
 * by Roman Hodek <Roman.Hodek@informatik.uni-erlangen.de> */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "common.h"
#include "fsck.befs.h"
#include "io.h"
#include "fat.h"
#include "check.h"


off_t alloc_rootdir_entry(DOS_FS * fs, DIR_ENT * de, const char *pattern)
{
    static int curr_num = 0;
    off_t offset = 0;

    if (fs->root_cluster) {
        DIR_ENT d2;
        int i = 0;
        uint32_t clu_num;
        off_t offset2;

        clu_num = fs->root_cluster;
        offset = cluster_start(fs, clu_num);
        while (clu_num > 0 && clu_num != -1) {
            fs_read(offset, sizeof(DIR_ENT), &d2);
            if (IS_FREE(d2.name) && d2.attr != VFAT_LN_ATTR) {
                break;
            }
            i += sizeof(DIR_ENT);
            offset += sizeof(DIR_ENT);
            if ((i % fs->cluster_size) == 0) {
                if ((clu_num = next_cluster(fs, clu_num)) == 0
                    || clu_num == -1)
                    break;
                offset = cluster_start(fs, clu_num);
            }
        }
        memset(de, 0, sizeof(DIR_ENT));
        while (1) {
            char expanded[12];
            sprintf(expanded, pattern, curr_num);
            memcpy(de->name, expanded, MSDOS_NAME);
            clu_num = fs->root_cluster;
            i = 0;
            offset2 = cluster_start(fs, clu_num);
            while (clu_num > 0 && clu_num != -1) {
                fs_read(offset2, sizeof(DIR_ENT), &d2);
                if (offset2 != offset &&
                    !strncmp((const char *) d2.name,
                             (const char *) de->name, MSDOS_NAME))
                    break;
                i += sizeof(DIR_ENT);
                offset2 += sizeof(DIR_ENT);
                if ((i % fs->cluster_size) == 0) {
                    if ((clu_num = next_cluster(fs, clu_num)) == 0 ||
                        clu_num == -1)
                        break;
                    offset2 = cluster_start(fs, clu_num);
                }
            }
            if (clu_num == 0 || clu_num == -1)
                break;
            if (++curr_num >= 10000)
                die("Unable to create unique name");
        }
    }
    ++n_files;

    return offset;
}
