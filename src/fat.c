/* fat.c - Read/write access to the FAT

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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "common.h"
#include "fsck.befs.h"
#include "io.h"
#include "fat.h"

/**
 * Fetch the FAT entry for a specified cluster.
 *
 * @param[out]  entry	    Cluster to which cluster of interest is linked
 * @param[in]	fat	    FAT table for the partition
 * @param[in]	cluster     Cluster of interest
 * @param[in]	fs          Information from the FAT boot sectors (bits per FAT entry)
 */
void get_fat(FAT_ENTRY * entry, void *fat, uint32_t cluster, DOS_FS * fs)
{
    unsigned char *ptr;

    if (cluster > fs->data_clusters + 1) {
        die("Internal error: cluster out of range in get_fat() (%lu > %lu).", (unsigned long) cluster, (unsigned long) (fs->data_clusters + 1));
    }

    switch (fs->fat_bits) {
    case 12:
        ptr = &((unsigned char *) fat)[cluster * 3 / 2];
        entry->value = 0xfff & (cluster & 1 ? (ptr[0] >> 4) | (ptr[1] << 4)
                                : (ptr[0] | ptr[1] << 8));
        break;
    case 16:
        entry->value = le16toh(((unsigned short *) fat)[cluster]);
        break;
    case 32:
        /* According to M$, the high 4 bits of a FAT32 entry are reserved and
         * are not part of the cluster number. So we cut them off. */
        {
            uint32_t e = le32toh(((unsigned int *) fat)[cluster]);
            entry->value = e & 0xfffffff;
            entry->reserved = e >> 28;
        }
        break;
    default:
        die("Bad FAT entry size: %d bits.", fs->fat_bits);
    }
}

/**
 * Build a bookkeeping structure from the partition's FAT table.
 * If the partition has multiple FATs and they don't agree, try to pick a winner,
 * and queue a command to overwrite the loser.
 * One error that is fixed here is a cluster that links to something out of range.
 *
 * @param[inout]    fs      Information about the filesystem
 */
void read_fat(DOS_FS * fs)
{
    int eff_size, alloc_size;
    void *first, *second = NULL;
    int first_ok, second_ok;
    uint32_t total_num_clusters;

    /* Clean up from previous pass */
    if (fs->fat)
        free(fs->fat);
    if (fs->cluster_owner)
        free(fs->cluster_owner);
    fs->fat = NULL;
    fs->cluster_owner = NULL;

    total_num_clusters = fs->data_clusters + 2;
    eff_size = (total_num_clusters * fs->fat_bits + 7) / 8ULL;

    if (fs->fat_bits != 12)
        alloc_size = eff_size;
    else
        /* round up to an even number of FAT entries to avoid special
         * casing the last entry in get_fat() */
        alloc_size = (total_num_clusters * 12 + 23) / 24 * 3;

    first = alloc(alloc_size);
    fs_read(fs->fat_start, eff_size, first);
    if (fs->nfats > 1) {
        second = alloc(alloc_size);
        fs_read(fs->fat_start + fs->fat_size, eff_size, second);
    }
    if (second && memcmp(first, second, eff_size) != 0) {
        FAT_ENTRY first_media, second_media;
        get_fat(&first_media, first, 0, fs);
        get_fat(&second_media, second, 0, fs);
        first_ok = (first_media.value & FAT_EXTD(fs)) == FAT_EXTD(fs);
        second_ok = (second_media.value & FAT_EXTD(fs)) == FAT_EXTD(fs);
        if (first_ok && !second_ok) {
            printf("FATs differ - using first FAT.\n");
            fs_write(fs->fat_start + fs->fat_size, eff_size, first);
        }
        if (!first_ok && second_ok) {
            printf("FATs differ - using second FAT.\n");
            fs_write(fs->fat_start, eff_size, second);
            memcpy(first, second, eff_size);
        }
        if (first_ok && second_ok) {
            if (interactive) {
                printf
                    ("FATs differ but appear to be intact. Use which FAT ?\n"
                     "1) Use first FAT\n2) Use second FAT\n");
                if (get_key("12", "?") == '1') {
                    fs_write(fs->fat_start + fs->fat_size, eff_size,
                             first);
                } else {
                    fs_write(fs->fat_start, eff_size, second);
                    memcpy(first, second, eff_size);
                }
            } else {
                printf("FATs differ but appear to be intact. Using first "
                       "FAT.\n");
                fs_write(fs->fat_start + fs->fat_size, eff_size, first);
            }
        }
        if (!first_ok && !second_ok) {
            printf("Both FATs appear to be corrupt. Giving up.\n");
            exit(1);
        }
    }
    if (second) {
        free(second);
    }
    fs->fat = (unsigned char *) first;

    fs->cluster_owner = alloc(total_num_clusters * sizeof(DOS_FILE *));
    memset(fs->cluster_owner, 0,
           (total_num_clusters * sizeof(DOS_FILE *)));
}

/**
 * Get the cluster to which the specified cluster is linked.
 * If the linked cluster is marked bad, abort.
 *
 * @param[in]   fs          Information about the filesystem
 * @param[in]	cluster     Cluster to follow
 *
 * @return  -1              'cluster' is at the end of the chain
 * @return  Other values    Next cluster in this chain
 */
uint32_t next_cluster(DOS_FS * fs, uint32_t cluster)
{
    uint32_t value;
    FAT_ENTRY curEntry;

    get_fat(&curEntry, fs->fat, cluster, fs);

    value = curEntry.value;
    if (FAT_IS_BAD(fs, value))
        die("Internal error: next_cluster on bad cluster");
    return FAT_IS_EOF(fs, value) ? -1 : value;
}

off_t cluster_start(DOS_FS * fs, uint32_t cluster)
{
    return fs->data_start + ((off_t) cluster -
                             2) * (uint64_t) fs->cluster_size;
}

DOS_FILE *get_owner(DOS_FS * fs, uint32_t cluster)
{
    if (fs->cluster_owner == NULL)
        return NULL;
    else
        return fs->cluster_owner[cluster];
}
