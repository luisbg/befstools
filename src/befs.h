/* befs.h - BeOS filesystem constants/structures

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

#ifndef _BEFS_H
#define _BEFS_H

#include <stdint.h>

#define SECTOR_SIZE 512         /* sector size (bytes) */
#define MSDOS_DIR_BITS 5        /* log2(sizeof(struct msdos_dir_entry)) */

#define ATTR_NONE 0             /* no attribute bits */
#define ATTR_RO 1               /* read-only */
#define ATTR_HIDDEN 2           /* hidden */
#define ATTR_SYS 4              /* system */
#define ATTR_VOLUME 8           /* volume label */
#define ATTR_DIR 16             /* directory */
#define ATTR_ARCH 32            /* archived */

/* attribute bits that are copied "as is" */
#define ATTR_UNUSED (ATTR_VOLUME | ATTR_ARCH | ATTR_SYS | ATTR_HIDDEN)

#define DELETED_FLAG 0xe5       /* marks file as deleted when in name[0] */
#define IS_FREE(n) (!*(n) || *(n) == DELETED_FLAG)

#define MSDOS_NAME 11           /* maximum name length */
#define MSDOS_DOT ".          " /* ".", padded to MSDOS_NAME chars */
#define MSDOS_DOTDOT "..         "      /* "..", padded to MSDOS_NAME chars */

#define NO_NAME "befs"

#define BEFS_SUPER_MAGIC1 0x42465331	/* BFS1 */
#define BEFS_SUPER_MAGIC2 0xdd121031
#define BEFS_SUPER_MAGIC3 0x15b6830e

#define BEFS_INODE_MAGIC1 0x3bbe0ad9

#define BEFS_BTREE_MAGIC 0x69f6c2e8

#define BEFS_BYTEORDER_NATIVE 0x42494745
#define BEFS_CLEAN  0x434C454E

#define B_OS_NAME_LENGTH 32

#define BEFS_SYMLINK_LEN 144
#define BEFS_NUM_DIRECT_BLOCKS 12

/* Block runs */
typedef struct {
	uint32_t allocation_group;
	uint16_t start;
	uint16_t len;
} __attribute__ ((__packed__)) befs_disk_block_run;

typedef befs_disk_block_run befs_disk_inode_addr;
typedef uint64_t befs_off_t;
typedef uint64_t befs_time_t;

/*
 * The Superblock structure
 *
 * For an explanation of the elements of the structure read the
 * write_superblock() function in src/mkfs.befs.c
 */
typedef struct {
    char name[B_OS_NAME_LENGTH];
    uint32_t magic1;
    uint32_t fs_byte_order;

    uint32_t block_size;
    uint32_t block_shift;

    uint64_t num_blocks;
    uint64_t used_blocks;

    uint32_t inode_size;

    uint32_t magic2;
    uint32_t blocks_per_ag;
    uint32_t ag_shift;
    uint32_t num_ags;

    uint32_t flags;

    befs_disk_block_run log_blocks;
    uint64_t log_start;
    uint64_t log_end;

    uint32_t magic3;
    befs_disk_inode_addr root_dir;
    befs_disk_inode_addr indices;

} __attribute__ ((packed)) befs_super_block;

typedef struct {
    befs_disk_block_run direct[BEFS_NUM_DIRECT_BLOCKS];
    befs_off_t max_direct_range;
    befs_disk_block_run indirect;
    befs_off_t max_indirect_range;
    befs_disk_block_run double_indirect;
    befs_off_t max_double_indirect_range;
    befs_off_t size;
} __attribute__ ((packed)) befs_data_stream;

typedef struct {
    uint32_t type;
    uint16_t name_size;
    uint16_t data_size;
    char name[1];
} __attribute__ ((packed)) befs_small_data;

/* Inode structure */
typedef struct {
    uint32_t magic1;
    befs_disk_inode_addr inode_num;
    uint32_t uid;
    uint32_t gid;
    uint32_t mode;
    uint32_t flags;
    befs_time_t create_time;
    befs_time_t last_modified_time;
    befs_disk_inode_addr parent;
    befs_disk_inode_addr attributes;
    uint32_t type;

    uint32_t inode_size;
    uint32_t etc;		/* not use */

    union {
        befs_data_stream datastream;
        char symlink[BEFS_SYMLINK_LEN];
    } data;

    uint32_t pad[4];		/* not use */
    befs_small_data small_data[1];
} __attribute__ ((packed)) befs_inode;

/* B+tree superblock */
typedef struct {
    uint32_t magic;
    uint32_t node_size;
    uint32_t max_depth;
    uint32_t data_type;
    uint64_t root_node_ptr;
    uint64_t free_node_ptr;
    uint64_t max_size;
} __attribute__ ((packed)) befs_btree_super;

/*
 * Header structure of each btree node
 */
typedef struct {
    uint64_t left;
    uint64_t right;
    uint64_t overflow;
    uint16_t all_key_count;
    uint16_t all_key_length;
} __attribute__ ((packed)) befs_btree_nodehead;
#endif                          /* _BEFS_H */
