/* fat.h - Read/write access to the FAT

   Copyright (C) 1993 Werner Almesberger <werner.almesberger@lrc.di.epfl.ch>
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

   THe complete text of the GNU General Public License
   can be found in /usr/share/common-licenses/GPL-3 file.
*/

#ifndef _FAT_H
#define _FAT_H

void read_fat(DOS_FS * fs);

/* Loads the FAT of the filesystem described by FS. Initializes the FAT,
   replaces broken FATs and rejects invalid cluster entries. */

void get_fat(FAT_ENTRY * entry, void *fat, uint32_t cluster, DOS_FS * fs);

/* Retrieve the FAT entry (next chained cluster) for CLUSTER. */

uint32_t next_cluster(DOS_FS * fs, uint32_t cluster);

/* Returns the number of the cluster following CLUSTER, or -1 if this is the
   last cluster of the respective cluster chain. CLUSTER must not be a bad
   cluster. */

off_t cluster_start(DOS_FS * fs, uint32_t cluster);

/* Returns the byte offset of CLUSTER, relative to the respective device. */

DOS_FILE *get_owner(DOS_FS * fs, uint32_t cluster);

/* Returns the owner of the repective cluster or NULL if the cluster has no
   owner. */

#endif
