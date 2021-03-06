/* mkfs.befs.c - utility to create BeOS filesystems

   Copyright (C) 1991 Linus Torvalds <torvalds@klaava.helsinki.fi>
   Copyright (C) 1992-1993 Remy Card <card@masi.ibp.fr>
   Copyright (C) 1993-1994 David Hudson <dave@humbug.demon.co.uk>
   Copyright (C) 1998 H. Peter Anvin <hpa@zytor.com>
   Copyright (C) 1998-2005 Roman Hodek <Roman.Hodek@informatik.uni-erlangen.de>
   Copyright (C) 2008-2014 Daniel Baumann <mail@daniel-baumann.ch>
   Copyright (C) 2015-2016 Andreas Bombe <aeb@debian.org>
   Copyright (C) 2016 Luis de Bethencourt <luisbg@osg.samsung.com>

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

/* Description: Utility to allow a BeOS filesystem to be created
   under Linux.  A lot of the basic structure of this program has been
   borrowed from Remy Card's "mke2fs" code.

   As far as possible the aim here is to make the "mkfs.befs" command
   look almost identical to the other Linux filesystem make utilties.
*/

#include "version.h"

#include <fcntl.h>
#include <signal.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>
#include <time.h>
#include <errno.h>
#include <ctype.h>
#include <stdint.h>
#include <getopt.h>
#include "endian_compat.h"

#include "befs.h"
#include "device_info.h"


/* Constant definitions */

#define BLOCK_SIZE         2048
#define INODE_SIZE         2048
#define HARD_SECTOR_SIZE   512
#define SECTORS_PER_BLOCK ( BLOCK_SIZE / HARD_SECTOR_SIZE )

/* Macro definitions */

/* Report a failure message and return a failure error code */

#define die( str ) fatal_error( "%s: " str "\n" )

#define BOOT_SIGN 		0xAA55  /* Boot sector magic number */
#define BOOTCODE_SIZE		448
#define BOOTCODE_FAT32_SIZE	420

/* __attribute__ ((packed)) is used on all structures to make gcc ignore any
 * alignments */

struct msdos_volume_info {
    uint8_t drive_number;       /* BIOS drive number */
    uint8_t RESERVED;           /* Unused */
    uint8_t ext_boot_sign;      /* 0x29 if fields below exist (DOS 3.3+) */
    uint8_t volume_id[4];       /* Volume ID number */
    uint8_t volume_label[11];   /* Volume label */
    uint8_t fs_type[8];         /* Typically FAT12 or FAT16 */
} __attribute__ ((packed));

/* Use own definition of boot sector structure -- the kernel headers'
 * name for it is msdos_boot_sector in 2.0 and fat_boot_sector in 2.1 ... */
struct boot_sector {
    uint8_t ignored[3];         /* Boot strap short or near jump */
    uint8_t system_id[8];       /* Name - can be used to special case
                                   partition manager volumes */
    uint8_t sector_size[2];     /* bytes per logical sector */
    uint8_t cluster_size;       /* sectors/cluster */
    uint16_t reserved;          /* reserved sectors */
    uint8_t fats;               /* number of FATs */
    uint8_t dir_entries[2];     /* root directory entries */
    uint8_t sectors[2];         /* number of sectors */
    uint8_t media;              /* media code (unused) */
    uint16_t fat_length;        /* sectors/FAT */
    uint16_t secs_track;        /* sectors per track */
    uint16_t heads;             /* number of heads */
    uint32_t hidden;            /* hidden sectors (unused) */
    uint32_t total_sect;        /* number of sectors (if sectors == 0) */

    /* The following fields are only used by FAT32 */
    uint32_t fat32_length;      /* sectors/FAT */
    uint16_t flags;             /* bit 8: fat mirroring, low 4: active fat */
    uint8_t version[2];         /* major, minor filesystem version */
    uint32_t root_cluster;      /* first cluster in root directory */
    uint16_t info_sector;       /* filesystem info sector */
    uint16_t backup_boot;       /* backup boot sector */
    uint8_t reserved2[12];      /* Unused */

    uint8_t drive_number;       /* Logical Drive Number */
    uint8_t reserved3;          /* Unused */

    uint8_t extended_sig;       /* Extended Signature (0x29) */
    uint32_t serial;            /* Serial number */
    uint8_t label[11];          /* FS label */
    uint8_t fs_type[8];         /* FS Type */

    /* fill up to 512 bytes */
    uint8_t junk[422];
} __attribute__ ((packed));

/* The "boot code" we put into the filesystem... it writes a message and
   tells the user to try again */

unsigned char dummy_boot_jump[3] = { 0xeb, 0x3c, 0x90 };
unsigned char dummy_boot_jump_m68k[2] = { 0x60, 0x1c };

#define MSG_OFFSET_OFFSET 3
char dummy_boot_code[BOOTCODE_SIZE] = "\x0e"    /* push cs */
    "\x1f"                      /* pop ds */
    "\xbe\x5b\x7c"              /* mov si, offset message_txt */
    /* write_msg: */
    "\xac"                      /* lodsb */
    "\x22\xc0"                  /* and al, al */
    "\x74\x0b"                  /* jz key_press */
    "\x56"                      /* push si */
    "\xb4\x0e"                  /* mov ah, 0eh */
    "\xbb\x07\x00"              /* mov bx, 0007h */
    "\xcd\x10"                  /* int 10h */
    "\x5e"                      /* pop si */
    "\xeb\xf0"                  /* jmp write_msg */
    /* key_press: */
    "\x32\xe4"                  /* xor ah, ah */
    "\xcd\x16"                  /* int 16h */
    "\xcd\x19"                  /* int 19h */
    "\xeb\xfe"                  /* foo: jmp foo */
    /* message_txt: */
    "This is not a bootable disk.  Please insert a bootable floppy and\r\n"
    "press any key to try again ... \r\n";

#define MESSAGE_OFFSET 29       /* Offset of message in above code */

/* Global variables - the root of all evil :-) - see these and weep! */

static const char *program_name = "mkfs.befs";  /* Name of the program */
static char *device_name = NULL;        /* Name of the device on which to create the filesystem */
static int verbose = 0;         /* Default to verbose mode off */
static long volume_id;          /* Volume ID number */
static time_t create_time;      /* Creation time */
static char volume_name[] = NO_NAME;    /* Volume name */
static uint64_t blocks;         /* Number of blocks in filesystem */
static int sector_size = 512;   /* Size of a logical sector */
static uint32_t block_size = BLOCK_SIZE;        /* Number of sectors per disk cluster */
static uint32_t blocks_per_ag = 0x4000; /* Number of blocks per allocation group */
static int dev = -1;            /* FS block device file handle */
static struct boot_sector bs;   /* Boot sector data */
static int root_dir_entries = 0;        /* Number of root directory entries */
static int hidden_sectors = 0;  /* Number of hidden sectors */
static int orphaned_sectors = 0;        /* Sectors that exist in the last block of filesystem */
static uint64_t current_pos = 0;        /* current seek position */

static const size_t BIOS_drive_offset = 0x1e;
static const size_t partition_data_offset = 0xab;

static int bootable = 1;

// Note: this boot block is taken directly from a Haiku generated disk
static uint8_t boot_block_data[512] = {
    0xb8, 0x00, 0x90, 0x8e, 0xd0, 0x89, 0xc4, 0xfc, 0x68, 0xc0, 0x07, 0x1f,
    0x1e, 0x07, 0x80, 0xfa, 0x02, 0x72, 0x0c, 0x80, 0xfa, 0x80, 0x72, 0x05,
    0x80, 0xfa, 0x90, 0x72, 0x02, 0xb2, 0x80, 0x66, 0x31, 0xc0, 0x40, 0xbb,
    0x00, 0x02, 0xe8, 0x7e, 0x00, 0x81, 0x7f, 0x20, 0x31, 0x53, 0x75, 0x75,
    0x80, 0xbf, 0xff, 0x01, 0x20, 0x75, 0x6e, 0xbe, 0x74, 0x02, 0xe8, 0x82,
    0x01, 0xe8, 0xa3, 0x02, 0xbf, 0x07, 0x03, 0xbb, 0x00, 0x06, 0x57, 0xe8,
    0xf5, 0x00, 0x72, 0x59, 0x5f, 0x53, 0xbb, 0x00, 0x02, 0x59, 0x51, 0x81,
    0xc3, 0x00, 0x04, 0x39, 0xcb, 0x73, 0xe8, 0x66, 0xff, 0x47, 0x10, 0x75,
    0xf0, 0x66, 0xff, 0x47, 0x14, 0x75, 0xea, 0xb0, 0x2f, 0x89, 0xd9, 0x57,
    0xf2, 0xae, 0x75, 0x35, 0x29, 0xd9, 0xf7, 0xd1, 0x5f, 0x60, 0xe8, 0x2b,
    0x02, 0x61, 0x72, 0xd5, 0x8c, 0xd5, 0x01, 0xcf, 0x47, 0x38, 0x05, 0x75,
    0xbe, 0xb8, 0x00, 0x10, 0x8e, 0xc0, 0x31, 0xdb, 0xc1, 0xeb, 0x04, 0x8c,
    0xc0, 0x01, 0xd8, 0x8e, 0xc0, 0x31, 0xdb, 0xe8, 0xa5, 0x00, 0x73, 0xf0,
    0x66, 0xa1, 0xab, 0x00, 0xea, 0x00, 0x02, 0x00, 0x10, 0xeb, 0x7f, 0x66,
    0x60, 0x66, 0x05, 0x5b, 0x63, 0x40, 0x01, 0x06, 0x53, 0x52, 0x66, 0x50,
    0x66, 0x31, 0xf6, 0x66, 0xc7, 0x04, 0x10, 0x00, 0x01, 0x00, 0x89, 0x5c,
    0x04, 0x8c, 0x44, 0x06, 0x66, 0x89, 0x44, 0x08, 0x66, 0x89, 0x74, 0x0c,
    0xb4, 0x08, 0xcd, 0x13, 0x88, 0xc8, 0x86, 0xe9, 0xc0, 0xc5, 0x02, 0x80,
    0xe5, 0x03, 0x89, 0xcd, 0x25, 0x3f, 0x00, 0x89, 0xc3, 0xb2, 0x00, 0x86,
    0xd6, 0x42, 0xf7, 0xe2, 0x91, 0x58, 0x5a, 0xe3, 0x39, 0x39, 0xca, 0x73,
    0x2a, 0xf7, 0xf1, 0x39, 0xc5, 0x76, 0x24, 0x91, 0x92, 0xf6, 0xf3, 0x86,
    0xcd, 0xc0, 0xc9, 0x02, 0x08, 0xe1, 0x41, 0x5a, 0x88, 0xc6, 0x5b, 0x07,
    0xbf, 0x05, 0x00, 0xb8, 0x01, 0x02, 0xcd, 0x13, 0x73, 0x27, 0x31, 0xc0,
    0xcd, 0x13, 0x4f, 0x75, 0xf2, 0xeb, 0x03, 0x5a, 0x5b, 0x07, 0x31, 0xf6,
    0xb4, 0x42, 0xcd, 0x13, 0x73, 0x13, 0xbe, 0xd4, 0x01, 0x31, 0xdb, 0xb4,
    0x0e, 0xac, 0xcd, 0x10, 0x3c, 0x2e, 0x75, 0xf9, 0x93, 0xcd, 0x16, 0xcd,
    0x19, 0x66, 0x61, 0xc3, 0x01, 0x46, 0x00, 0x8b, 0x76, 0x00, 0xe8, 0x7a,
    0x00, 0x83, 0x3c, 0x00, 0x75, 0x27, 0xb8, 0x08, 0x00, 0x01, 0x46, 0x00,
    0xff, 0x4e, 0x02, 0x74, 0x08, 0x83, 0x4c, 0x08, 0x00, 0x75, 0xe4, 0xf9,
    0xc3, 0xff, 0x46, 0x02, 0xfe, 0x0e, 0x10, 0x00, 0x7c, 0xf5, 0x81, 0xfd,
    0x00, 0x90, 0x74, 0xd0, 0x83, 0xed, 0x04, 0xeb, 0xce, 0xff, 0x44, 0xfe,
    0xff, 0x0c, 0x80, 0x3e, 0x10, 0x00, 0x02, 0x74, 0x24, 0x06, 0x53, 0x1e,
    0x07, 0x8d, 0x5e, 0x08, 0xc1, 0xe3, 0x0b, 0xe8, 0x17, 0x00, 0x93, 0x5b,
    0x07, 0x83, 0xc5, 0x04, 0x89, 0x7e, 0x00, 0x29, 0xf8, 0xc1, 0xe8, 0x03,
    0x89, 0x46, 0x02, 0xfe, 0x06, 0x10, 0x00, 0xeb, 0x9e, 0xbe, 0x00, 0x02,
    0x89, 0xdf, 0x8a, 0x4c, 0x2c, 0x80, 0xe9, 0x09, 0x66, 0xd3, 0xe0, 0x8b,
    0x4c, 0x28, 0xc1, 0xe9, 0x09, 0xe8, 0xef, 0xfe, 0x01, 0xf3, 0x66, 0x40,
    0xe2, 0xf7, 0xc3, 0x51, 0x66, 0xad, 0x8a, 0x0e, 0x4c, 0x02, 0x66, 0xd3,
    0xe0, 0x66, 0x91, 0x66, 0x31, 0xc0, 0xad, 0x66, 0x01, 0xc8, 0x59, 0xc3,
    0x45, 0x72, 0x72, 0x6f, 0x72, 0x20, 0x6c, 0x6f, 0x61, 0x64, 0x69, 0x6e,
    0x67, 0x20, 0x4f, 0x53, 0x3b, 0x20, 0x70, 0x72, 0x65, 0x73, 0x73, 0x20,
    0x61, 0x6e, 0x79, 0x20, 0x6b, 0x65, 0x79, 0x20, 0x74, 0x6f, 0x20, 0x72,
    0x65, 0x62, 0x6f, 0x6f, 0x74, 0x2e, 0x55, 0xaa,
};

/* Function prototype definitions */

static void fatal_error(const char *fmt_string) __attribute__ ((noreturn));
static void establish_params(struct device_info *info);
static void erase_boot_block(void);
static befs_super_block write_superblock(void);
static befs_inode write_root_dir(befs_super_block superblock);
static uint64_t write_btree_super(befs_super_block superblock,
                                  befs_inode root_dir);
static void write_btree_root(befs_super_block superblock, uint64_t pos);

/* The function implementations */

/* Handle the reporting of fatal errors.
 * Volatile to let gcc know that this doesn't return */

static void fatal_error(const char *fmt_string)
{
    fprintf(stderr, fmt_string, program_name, device_name);
    exit(1);                    /* The error exit code is 1! */
}

/* Establish the geometry and media parameters for the device */

static void establish_params(struct device_info *info)
{
    unsigned int sec_per_track = 63;
    unsigned int heads = 255;
    unsigned int media = 0xf8;
    unsigned int cluster_size = 4;      /* starting point for FAT12 and FAT16 */
    int def_root_dir_entries = 512;

    /*
     * For FAT32, try to do the same as M$'s format command
     * (see http://www.win.tue.nl/~aeb/linux/fs/fat/fatgen103.pdf p. 20):
     * fs size <= 260M: 0.5k clusters
     * fs size <=   8G:   4k clusters
     * fs size <=  16G:   8k clusters
     * fs size <=  32G:  16k clusters
     * fs size >   32G:  32k clusters
     *
     * This only works correctly for 512 byte sectors!
     */
    uint32_t sz_mb = info->size / (1024 * 1024);
    cluster_size =
        sz_mb > 32 * 1024 ? 64 : sz_mb > 16 * 1024 ? 32 : sz_mb >
        8 * 1024 ? 16 : sz_mb > 260 ? 8 : 1;

    if (info->geom_heads > 0) {
        heads = info->geom_heads;
        sec_per_track = info->geom_sectors;
    }

    if (info->geom_start >= 0)
        hidden_sectors = htole32(info->geom_start);

    if (!root_dir_entries)
        root_dir_entries = def_root_dir_entries;

    bs.secs_track = htole16(sec_per_track);
    bs.heads = htole16(heads);
    bs.media = media;
    bs.cluster_size = cluster_size;
}

#define error(str)				\
  do {						\
    die (str);					\
  } while(0)

#define seekto(pos,errstr)						\
  do {									\
    off_t __pos = (pos);						\
    current_pos = pos;							\
    if (lseek (dev, __pos, SEEK_SET) != __pos)				\
	error ("seek to " errstr " failed whilst writing tables");	\
  } while(0)

#define writebuf(buf,size,errstr)			\
  do {							\
    int __size = (size);				\
    if (write (dev, buf, __size) != __size)		\
	error ("failed whilst writing " errstr);	\
  } while(0)

/* Erase the boot block, as we don't use it and there might be leftovers from
 * other file systems. It could confuse the identification of the partition if
 * not erased.
 */
static void erase_boot_block(void)
{
    const int boot_block_size = 512;
    char empty_sector[boot_block_size];
    uint32_t *offset;

    seekto(0, "start of device");
    memset(empty_sector, 0, boot_block_size);
    writebuf(empty_sector, boot_block_size, "Empty boot sector block");

    if (bootable) {
        // TODO: this will only work for images only
        boot_block_data[BIOS_drive_offset] = 0x80;
        // TODO: this should be replaced by the real thing
        offset = (uint32_t *) & boot_block_data[partition_data_offset];
        *offset = 0;

        seekto(0, "start of device");
        writebuf(boot_block_data, boot_block_size, "Bootable boot sector");
    }
}

static befs_super_block write_superblock(void)
{
    befs_super_block superblock;
    befs_disk_block_run log_blocks, root_dir, indices;

    /*
     * A great explanation of the Superblock structure can be found in page 48
     * of 'Practical File System Design with the Be File System'
     * by Dominic Giampaolo
     */

    /* seek to start of superblock and write them all */
    seekto(SECTOR_SIZE, "first sector");
    memset(superblock.name, 0, B_OS_NAME_LENGTH);

    if (verbose || memcmp(volume_name, NO_NAME, strlen(volume_name)) != 0)
        printf("Using name: %s\n", volume_name);
    memcpy((char *) superblock.name, volume_name, strlen(volume_name));

    superblock.magic1 = BEFS_SUPER_MAGIC1;
    superblock.fs_byte_order = BEFS_BYTEORDER_NATIVE;

    superblock.block_size = block_size; /* Default block of 2048 bytes, unless changed  */
    superblock.block_shift = ffs(block_size) - 1;       /* Matching left shift of 11 */

    /* size of disk = num_blocks * block_size */
    superblock.num_blocks = blocks;
    superblock.used_blocks = 0x88F;     /* of which 2,191 are currently in use */

    superblock.inode_size = INODE_SIZE; /* Inode size of 2048 */
    if (superblock.inode_size < superblock.block_size)
        superblock.inode_size = superblock.block_size;
    else if (superblock.inode_size % superblock.block_size != 0)
        error("Inode size needs to be a multiple of block size.");

    if (verbose)
        printf("Using block size: %d\n  and inode size: %d\n",
               superblock.block_size, superblock.inode_size);

    superblock.magic2 = BEFS_SUPER_MAGIC2;
    superblock.blocks_per_ag = blocks_per_ag;   /* Default 16,384 blocks per allocation group */
    superblock.ag_shift = ffs(superblock.blocks_per_ag) - 1;    /* Matching left shift of 14 */
    superblock.num_ags = 0x4;   /* 4 allocation groups in this file system */

    superblock.flags = BEFS_CLEAN;      /* Journal transaction state is clean */

    log_blocks.allocation_group = 0;    /* Where to find the journal */
    log_blocks.start = 5;
    log_blocks.len = superblock.inode_size;
    superblock.log_blocks = log_blocks;
    superblock.log_start = 0x13;        /* Start index of ring buffer for the log */
    superblock.log_end = 0x13;  /* In clean state the end matches */

    superblock.magic3 = BEFS_SUPER_MAGIC3;

    root_dir.allocation_group = 0;      /* Where to find root directory */
    root_dir.start = 0x805;     /* Beginning of disk is for allocation indices */
    root_dir.len = 1;
    superblock.root_dir = root_dir;

    indices.allocation_group = 0;       /* Where to find the index directory */
    indices.start = 0x808;
    indices.len = 1;
    superblock.indices = indices;

    writebuf((char *) &superblock, sizeof(befs_super_block), "Superblock");

    return superblock;
}

static befs_inode write_root_dir(befs_super_block superblock)
{
    uint64_t start;
    befs_inode root_inode;
    befs_disk_inode_addr inode_num, parent, attributes;
    befs_disk_block_run direct[BEFS_NUM_DIRECT_BLOCKS];
    befs_disk_block_run indirect, double_indirect;
    time_t current_time;
    int c;

    start = superblock.block_size * superblock.root_dir.start;
    if (verbose)
        printf("Writing root dir at: 0x%lx \t\t(Start block: 0x%x)\n",
               start, superblock.root_dir.start);

    seekto(start, "first sector");
    root_inode.magic1 = BEFS_INODE_MAGIC1;

    inode_num.allocation_group = 0;
    inode_num.start = superblock.root_dir.start;        /* this will be checked against vfs inode number */
    inode_num.len = 1;
    root_inode.inode_num = inode_num;

    root_inode.uid = 0;
    root_inode.gid = 0;
    root_inode.mode = S_IRWXU | /* owner: rwx */
        S_IRGRP | S_IXGRP |     /* group: r-x */
        S_IROTH | S_IXOTH |     /* other: r-x */
        S_IFDIR;                /* Directory */
    root_inode.flags = BEFS_INODE_IN_USE | BEFS_INODE_WAS_WRITTEN;

    /* befs_time_t is POSIX time_t + 16 bits of randomness to avoid duplicates */
    current_time = time(NULL);
    root_inode.create_time = current_time << 16;
    srand(time(NULL));
    root_inode.create_time += rand() % 0xFFFF;
    root_inode.last_modified_time = root_inode.create_time;

    /* Parent is same as inode_num, it point to iself */
    parent.allocation_group = 0;
    parent.start = superblock.root_dir.start;
    parent.len = 1;
    root_inode.parent = parent;

    attributes.allocation_group = 0;
    attributes.start = 0;
    attributes.len = 0;
    root_inode.attributes = attributes;

    root_inode.type = 0;

    root_inode.inode_size = superblock.inode_size;
    root_inode.etc = 0;

    /* root inode only uses one direct block */
    direct[0].allocation_group = superblock.root_dir.allocation_group;
    direct[0].start = superblock.root_dir.start + 1;
    printf("root inode direct block: 0x%x\n", direct[0].start);
    direct[0].len = 2;
    root_inode.data.datastream.direct[0] = direct[0];

    /* set the rest of direct blocks to 0 */
    for (c = 1; c < BEFS_NUM_DIRECT_BLOCKS; c++) {
        direct[c].allocation_group = 0;
        direct[c].start = 0;
        direct[c].len = 0;
        root_inode.data.datastream.direct[c] = direct[c];
    }
    root_inode.data.datastream.max_direct_range = 0x100;

    indirect.allocation_group = 0;
    indirect.start = 0;
    indirect.len = 0;
    root_inode.data.datastream.indirect = indirect;
    root_inode.data.datastream.max_indirect_range = 0;

    double_indirect.allocation_group = 0;
    double_indirect.start = 0;
    double_indirect.len = 0;
    root_inode.data.datastream.double_indirect = double_indirect;
    root_inode.data.datastream.max_double_indirect_range = 0;

    root_inode.data.datastream.size = 0;

    root_inode.pad[0] = 0;
    root_inode.pad[1] = 0;
    root_inode.pad[2] = 0;
    root_inode.pad[3] = 0;

    root_inode.small_data[0].type = 0;
    root_inode.small_data[0].name_size = 0;
    root_inode.small_data[0].data_size = 0;
    root_inode.small_data[0].name[0] = 0x99;

    writebuf((char *) &root_inode, sizeof(befs_inode), "Root Inode");

    return root_inode;
}

static uint64_t write_btree_super(befs_super_block superblock,
                                  befs_inode root_dir)
{
    befs_btree_super bt_super;
    befs_disk_block_run first_direct;
    uint64_t start;

    first_direct = root_dir.data.datastream.direct[0];
    start = superblock.block_size * first_direct.start;
    if (verbose)
        printf("Writing btree super at: 0x%lx\n", start);

    seekto(start, "btree super");
    bt_super.magic = BEFS_BTREE_MAGIC;

    bt_super.node_size = superblock.inode_size;
    bt_super.max_depth = 1;
    bt_super.data_type = 0;

    bt_super.root_node_ptr = superblock.block_size / 2;
    bt_super.free_node_ptr = ~(0);
    bt_super.max_size = superblock.inode_size;

    writebuf((char *) &bt_super, sizeof(befs_btree_super), "Btree Super");

    return start + bt_super.root_node_ptr;
}

static void write_btree_root(befs_super_block superblock, uint64_t pos)
{
    befs_btree_nodehead root_node;
    const int keylen_align = 8;
    unsigned long int index_off;
    ulong align;
    uint64_t keylen_index;
    uint64_t first_key_pos, second_key_pos;
    uint64_t key;
    int c;

    if (verbose)
        printf("Writing btree root node at: 0x%lx\n", pos);

    seekto(pos, "btree root");
    root_node.left = ~(0);
    root_node.right = ~(0);
    root_node.overflow = ~(0);
    root_node.all_key_count = 2;
    root_node.all_key_length = 3;

    writebuf((char *) &root_node, sizeof(befs_btree_nodehead),
             "Btree Root");

    // Seek to end of node head, where the keys are concatenated
    seekto(pos + sizeof(befs_btree_nodehead), "Start of key index");
    writebuf("..", 2, "First key is '..'");
    seekto(current_pos + 2, "Start of second key");
    writebuf(".", 1, "Second key is '.'");

    index_off = sizeof(befs_btree_nodehead) + root_node.all_key_length;
    align = index_off % keylen_align;
    if (align)
        index_off += keylen_align - align;
    keylen_index = pos + index_off;

    // '..'
    seekto(keylen_index, "Start of the keylen index");
    first_key_pos = 1;          /* first key is at 1 */
    writebuf((char *) &first_key_pos, sizeof(uint16_t), "First key pos");

    // '.'
    seekto(current_pos + 2, "Second keylen index");
    second_key_pos = 3;         /* since first key is of size 2, second is at 3 */
    writebuf((char *) &second_key_pos, sizeof(uint16_t), "Second key pos");

    /* Both keys point to the root_dir start */
    seekto(current_pos + 2, "Next key");
    for (c = 0; c < 2; c++) {
        key = superblock.root_dir.start;
        writebuf((char *) &key, sizeof(uint64_t), "First key");
        seekto(current_pos + 8, "Next key");
    }
}

/* Report the command usage and exit with the given error code */

static void usage(int exitval)
{
    fprintf(stderr, "\
Usage: mkfs.befs [-v] [-n volume-name]\n\
       [-b block-size]\n\
       [-a block-per-ag]\n\
       [--help]\n\
       /dev/name [blocks]\n");
    exit(exitval);
}

/* The "main" entry point into the utility */

int main(int argc, char **argv)
{
    int c;
    struct device_info devinfo;
    struct timeval create_timeval;
    befs_super_block superblock;
    befs_inode root_dir;
    uint64_t root_node_pos;
    char *tmp;

    enum { OPT_HELP = 1000, };
    const struct option long_options[] = {
        {"help", no_argument, NULL, OPT_HELP},
        {0,}
    };

    if (argc && *argv) {        /* What's the program name? */
        char *p;
        program_name = *argv;
        if ((p = strrchr(program_name, '/')))
            program_name = p + 1;
    }

    /* Default volume ID = creation time, fudged for more uniqueness */
    gettimeofday(&create_timeval, NULL);
    create_time = create_timeval.tv_sec;
    volume_id = (uint32_t) ((create_timeval.tv_sec << 20) |
                            create_timeval.tv_usec);

    printf("mkfs.befs " VERSION " (" VERSION_DATE ")\n");

    while ((c =
            getopt_long(argc, argv, "n:b:a:v", long_options,
                        NULL)) != -1) {
        /* Scan the command line for options */
        switch (c) {
        case 'n': {             /* n : Volume name */
            int c, len = strlen(optarg);

            if (len >= B_OS_NAME_LENGTH) {
                printf("Volume name is too long. Trimming to 32 chars\n");
                len = B_OS_NAME_LENGTH;
            }
            sprintf(volume_name, "%-.32s", optarg);

            for (c = 0; c < len; c++)
                if (volume_name[c] == '/')
                    volume_name[c] = '-';

            break;
        }
        case 'b':              /* b : Block size */
            block_size = strtol(optarg, &tmp, 0);
            if (*tmp || (block_size != 1024 && block_size != 2048
                         && block_size != 4096 && block_size != 8192)) {
                printf("Bad number for block size: %s\n\n", optarg);
                usage(1);
            }
            break;

        case 'a':
            blocks_per_ag = strtol(optarg, &tmp, 0);
            if (*tmp || blocks_per_ag < 1 || blocks_per_ag > 0x10000) {
                printf("Bad number of blocks per allocation group: %s\n",
                       optarg);
                usage(1);
            }
            break;

        case 'v':              /* v : Verbose execution */
            ++verbose;
            break;

        case OPT_HELP:
            usage(0);
            break;

        default:
            printf("Unknown option: %c\n", c);
            usage(1);
        }
    }

    if (optind == argc || !argv[optind]) {
        printf("No device specified.\n");
        usage(1);
    }

    device_name = argv[optind++];

    if (optind != argc) {
        fprintf(stderr, "Excess arguments on command line\n");
        usage(1);
    }

    /* Is the device already mounted? */
    if (is_device_mounted(device_name))
        die("%s contains a mounted filesystem.");

    /* Is it a suitable device to build the FS on? */
    dev = open(device_name, O_EXCL | O_RDWR);
    if (dev < 0) {
        fprintf(stderr, "%s: unable to open %s: %s\n", program_name,
                device_name, strerror(errno));
        exit(1);                /* The error exit code is 1! */
    }

    if (get_device_info(dev, &devinfo) < 0)
        die("error collecting information about %s");

    if (devinfo.size <= 0)
        die("unable to discover size of %s");

    if (devinfo.sector_size > 0)
        sector_size = devinfo.sector_size;

    blocks = devinfo.size / block_size;
    orphaned_sectors = (devinfo.size % block_size) / sector_size;

    if (devinfo.type == TYPE_FIXED && devinfo.partition == 0)
        die("Device partition expected,"
            " not making filesystem on entire device '%s'");

    if (devinfo.has_children > 0)
        die("Partitions or virtual mappings on device '%s',"
            " not making filesystem");

    if (sector_size > 4096)
        fprintf(stderr,
                "Warning: sector size %d > 4096 is non-standard,"
                " filesystem may not be usable\n", sector_size);

    establish_params(&devinfo);
    /* Establish the media parameters */

    erase_boot_block();
    superblock = write_superblock();    /* Write the Superblock */
    root_dir = write_root_dir(superblock);
    root_node_pos = write_btree_super(superblock, root_dir);
    write_btree_root(superblock, root_node_pos);

    exit(0);                    /* Terminate with no errors! */
}
