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

#define TRUE 1                  /* Boolean constants */
#define FALSE 0

#define TEST_BUFFER_BLOCKS 16
#define BLOCK_SIZE         1024
#define HARD_SECTOR_SIZE   512
#define SECTORS_PER_BLOCK ( BLOCK_SIZE / HARD_SECTOR_SIZE )

#define NO_NAME "befs"

#define BEFS_SUPER_MAGIC1 0x42465331	/* BFS1 */
#define BEFS_SUPER_MAGIC2 0xdd121031
#define BEFS_SUPER_MAGIC3 0x15b6830e

#define BEFS_BYTEORDER_NATIVE 0x42494745
#define BEFS_CLEAN  0x434C454E

/* Macro definitions */

/* Report a failure message and return a failure error code */

#define die( str ) fatal_error( "%s: " str "\n" )

/* Compute ceil(a/b) */

static inline int cdiv(int a, int b)
{
    return (a + b - 1) / b;
}

/* FAT values */
#define FAT_EOF      (0x0ffffff8)
#define FAT_BAD      0x0ffffff7

#define MSDOS_EXT_SIGN 0x29     /* extended boot sector signature */
#define MSDOS_FAT32_SIGN "FAT32   "     /* FAT32 filesystem signature */

#define BOOT_SIGN 0xAA55        /* Boot sector magic number */

#define MAX_CLUST_12	((1 << 12) - 16)
#define MAX_CLUST_16	((1 << 16) - 16)
#define MIN_CLUST_32    65529
/* M$ says the high 4 bits of a FAT32 FAT entry are reserved and don't belong
 * to the cluster number. So the max. cluster# is based on 2^28 */
#define MAX_CLUST_32	((1 << 28) - 16)

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

struct msdos_boot_sector {
    uint8_t boot_jump[3];       /* Boot strap short or near jump */
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
    struct {
        uint32_t fat32_length;  /* sectors/FAT */
        uint16_t flags;         /* bit 8: fat mirroring, low 4: active fat */
        uint8_t version[2];     /* major, minor filesystem version */
        uint32_t root_cluster;  /* first cluster in root directory */
        uint16_t info_sector;   /* filesystem info sector */
        uint16_t backup_boot;   /* backup boot sector */
        uint16_t reserved2[6];  /* Unused */
        struct msdos_volume_info vi;
        uint8_t boot_code[BOOTCODE_FAT32_SIZE];
    } __attribute__ ((packed)) fstype;
    uint16_t boot_sign;
} __attribute__ ((packed));

struct fat32_fsinfo {
    uint32_t reserved1;         /* Nothing as far as I can tell */
    uint32_t signature;         /* 0x61417272L */
    uint32_t free_clusters;     /* Free cluster count.  -1 if unknown */
    uint32_t next_cluster;      /* Most recently allocated cluster.
                                 * Unused under Linux. */
    uint32_t reserved2[4];
};

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
static int backup_boot = 0;     /* Sector# of backup boot sector */
static int reserved_sectors = 0;        /* Number of reserved sectors */
static int nr_fats = 2;         /* Default number of FATs to produce */
static int size_fat = 32;       /* Size in bits of FAT entries */
static int dev = -1;            /* FS block device file handle */
static struct msdos_boot_sector bs;     /* Boot sector data */
static int start_data_sector;   /* Sector number for the start of the data area */
static int start_data_block;    /* Block number for the start of the data area */
static int size_root_dir;       /* Size of the root directory in bytes */
static uint32_t num_sectors;    /* Total number of sectors in device */
static int root_dir_entries = 0;        /* Number of root directory entries */
static char *blank_sector;      /* Blank sector - all zeros */
static int hidden_sectors = 0;  /* Number of hidden sectors */
static int align_structures = TRUE;     /* Whether to enforce alignment */
static int orphaned_sectors = 0;        /* Sectors that exist in the last block of filesystem */
static int invariant = 0;       /* Whether to set normally randomized or
                                   current time based values to
                                   constants */

/* Function prototype definitions */

static void fatal_error(const char *fmt_string) __attribute__ ((noreturn));
static void establish_params(struct device_info *info);
static void setup_tables(void);
static void write_tables(void);

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

/*
 * If alignment is enabled, round the first argument up to the second; the
 * latter must be a power of two.
 */
static unsigned int align_object(unsigned int sectors,
                                 unsigned int clustsize)
{
    if (align_structures)
        return (sectors + clustsize - 1) & ~(clustsize - 1);
    else
        return sectors;
}

/* Create the filesystem data tables */

static void setup_tables(void)
{
    unsigned fatdata32;         /* Sectors for FATs + data area (FAT32) */
    unsigned fatlength32;
    unsigned maxclust32;
    unsigned clust32;
    int maxclustsize;
    unsigned root_dir_sectors = cdiv(root_dir_entries * 32, sector_size);

    unsigned cluster_count = 0;
    struct msdos_volume_info *vi = &bs.fstype.vi;

    memcpy((char *) bs.system_id, "mkfsbefs", strlen("mkfsbefs"));

    if (bs.media == 0xf8)
        vi->drive_number = 0x80;
    else
        vi->drive_number = 0x00;

    /* Under FAT32, the root dir is in a cluster chain, and this is
     * signalled by bs.dir_entries being 0. */
    root_dir_entries = 0;

    vi->volume_id[0] = (unsigned char) (volume_id & 0x000000ff);
    vi->volume_id[1] = (unsigned char) ((volume_id & 0x0000ff00) >> 8);
    vi->volume_id[2] = (unsigned char) ((volume_id & 0x00ff0000) >> 16);
    vi->volume_id[3] = (unsigned char) (volume_id >> 24);

    memcpy(vi->volume_label, volume_name, 11);
    memcpy(bs.boot_jump, dummy_boot_jump, 3);

    /* Patch in the correct offset to the boot code */
    bs.boot_jump[1] = ((char *) &bs.fstype.boot_code - (char *) &bs) - 2;

    int offset = (char *) &bs.fstype.boot_code -
        (char *) &bs + MESSAGE_OFFSET + 0x7c00;
    if (dummy_boot_code[BOOTCODE_FAT32_SIZE - 1])
        printf("Warning: message too long; truncated\n");
    dummy_boot_code[BOOTCODE_FAT32_SIZE - 1] = 0;
    memcpy(bs.fstype.boot_code, dummy_boot_code, BOOTCODE_FAT32_SIZE);
    bs.fstype.boot_code[MSG_OFFSET_OFFSET] = offset & 0xff;
    bs.fstype.boot_code[MSG_OFFSET_OFFSET + 1] = offset >> 8;
    bs.boot_sign = htole16(BOOT_SIGN);

    if (verbose >= 2)
        printf("Boot jump code is %02x %02x\n",
               bs.boot_jump[0], bs.boot_jump[1]);

    if (!reserved_sectors)
        reserved_sectors = 32;
    else {
        if (reserved_sectors < 2)
            die("On FAT32 at least 2 reserved sectors are needed.");
    }
    bs.reserved = htole16(reserved_sectors);
    if (verbose >= 2)
        printf("Using %d reserved sectors\n", reserved_sectors);
    bs.fats = (char) nr_fats;
    bs.hidden = htole32(hidden_sectors);

    if ((long long) (blocks * BLOCK_SIZE / sector_size) +
        orphaned_sectors > UINT32_MAX) {
        printf
            ("Warning: target too large, space at end will be left unused\n");
        num_sectors = UINT32_MAX;
        blocks = (uint64_t) UINT32_MAX *sector_size / BLOCK_SIZE;
    } else {
        num_sectors =
            (long long) (blocks * BLOCK_SIZE / sector_size) +
            orphaned_sectors;
    }

    /*
     * If the filesystem is 8192 sectors or less (4 MB with 512-byte
     * sectors, i.e. floppy size), don't align the data structures.
     */
    if (num_sectors <= 8192) {
        if (align_structures && verbose >= 2)
            printf("Disabling alignment due to tiny filesystem\n");

        align_structures = FALSE;
    }

    /* An initial guess for bs.cluster_size should already be set */
    maxclustsize = 128;

    do {
        fatdata32 = num_sectors
            - align_object(reserved_sectors, bs.cluster_size);

        if (verbose >= 2)
            printf("Trying with %d sectors/cluster:\n", bs.cluster_size);

        clust32 = ((long long) fatdata32 * sector_size + nr_fats * 8) /
            ((int) bs.cluster_size * sector_size + nr_fats * 4);
        fatlength32 = cdiv((clust32 + 2) * 4, sector_size);
        fatlength32 = align_object(fatlength32, bs.cluster_size);
        /* Need to recalculate number of clusters, since the unused parts of the
         * FATS and data area together could make up space for an additional,
         * not really present cluster. */
        clust32 = (fatdata32 - nr_fats * fatlength32) / bs.cluster_size;
        maxclust32 = (fatlength32 * sector_size) / 4;
        if (maxclust32 > MAX_CLUST_32)
            maxclust32 = MAX_CLUST_32;
        if (clust32 && clust32 < MIN_CLUST_32) {
            clust32 = 0;
            if (verbose >= 2)
                printf("FAT32: not enough clusters (%d)\n", MIN_CLUST_32);
        }
        if (verbose >= 2)
            printf("FAT32: #clu=%u, fatlen=%u, maxclu=%u, limit=%u\n",
                   clust32, fatlength32, maxclust32, MAX_CLUST_32);
        if (clust32 > maxclust32) {
            clust32 = 0;
            if (verbose >= 2)
                printf("FAT32: too much clusters\n");
        }

        if (clust32)
            break;

        bs.cluster_size <<= 1;
    } while (bs.cluster_size && bs.cluster_size <= maxclustsize);

    if (clust32 < MIN_CLUST_32)
        fprintf(stderr,
                "WARNING: Not enough clusters for a 32 bit FAT!\n");
    cluster_count = clust32;
    bs.fat_length = htole16(0);
    bs.fstype.fat32_length = htole32(fatlength32);
    root_dir_entries = 0;

    /* Adjust the reserved number of sectors for alignment */
    reserved_sectors = align_object(reserved_sectors, bs.cluster_size);
    bs.reserved = htole16(reserved_sectors);

    /* Adjust the number of root directory entries to help enforce alignment */
    if (align_structures) {
        root_dir_entries = align_object(root_dir_sectors, bs.cluster_size)
            * (sector_size >> 5);
    }

    bs.sector_size[0] = (char) (sector_size & 0x00ff);
    bs.sector_size[1] = (char) ((sector_size & 0xff00) >> 8);

    bs.dir_entries[0] = (char) (root_dir_entries & 0x00ff);
    bs.dir_entries[1] = (char) ((root_dir_entries & 0xff00) >> 8);

    /* set up additional FAT32 fields */
    bs.fstype.flags = htole16(0);
    bs.fstype.version[0] = 0;
    bs.fstype.version[1] = 0;
    bs.fstype.root_cluster = htole32(2);
    bs.fstype.info_sector = htole16(1);
    bs.fstype.backup_boot = htole16(backup_boot);
    memset(&bs.fstype.reserved2, 0, sizeof(bs.fstype.reserved2));

    if (num_sectors >= 65536) {
        bs.sectors[0] = (char) 0;
        bs.sectors[1] = (char) 0;
        bs.total_sect = htole32(num_sectors);
    } else {
        bs.sectors[0] = (char) (num_sectors & 0x00ff);
        bs.sectors[1] = (char) ((num_sectors & 0xff00) >> 8);
        bs.total_sect = htole32(0);
    }

    vi->ext_boot_sign = MSDOS_EXT_SIGN;

    if (!cluster_count) {
        die("Attempting to create a too large filesystem");
    }

    /* The two following vars are in hard sectors, i.e. 512 byte sectors! */
    start_data_sector = (reserved_sectors + nr_fats * fatlength32 +
                         cdiv(root_dir_entries * 32, sector_size)) *
        (sector_size / HARD_SECTOR_SIZE);
    start_data_block = (start_data_sector + SECTORS_PER_BLOCK - 1) /
        SECTORS_PER_BLOCK;

    if (blocks < start_data_block + 32) /* Arbitrary undersize filesystem! */
        die("Too few blocks for viable filesystem");

    if (verbose) {
        printf("%s has %d head%s and %d sector%s per track,\n",
               device_name, le16toh(bs.heads),
               (le16toh(bs.heads) != 1) ? "s" : "", le16toh(bs.secs_track),
               (le16toh(bs.secs_track) != 1) ? "s" : "");
        printf("hidden sectors 0x%04x;\n", hidden_sectors);
        printf("logical sector size is %d,\n", sector_size);
        printf("using 0x%02x media descriptor, with %d sectors;\n",
               (int) (bs.media), num_sectors);
        printf("drive number 0x%02x;\n", (int) (vi->drive_number));
        printf
            ("filesystem has %d %d-bit FAT%s and %d sector%s per cluster.\n",
             (int) (bs.fats), size_fat, (bs.fats != 1) ? "s" : "",
             (int) (bs.cluster_size), (bs.cluster_size != 1) ? "s" : "");
        printf("FAT size is %d sector%s, and provides %d cluster%s.\n",
               fatlength32, (fatlength32 != 1) ? "s" : "", cluster_count,
               (cluster_count != 1) ? "s" : "");
        printf("There %s %u reserved sector%s.\n",
               (reserved_sectors != 1) ? "are" : "is", reserved_sectors,
               (reserved_sectors != 1) ? "s" : "");

        printf("Volume ID is %08lx, ", volume_id & (0xffffffff));
        if (strcmp(volume_name, NO_NAME))
            printf("volume label %s.\n", volume_name);
        else
            printf("no volume label.\n");
    }

    size_root_dir = bs.cluster_size * sector_size;

    if (!(blank_sector = malloc(sector_size)))
        die("Out of memory");
    memset(blank_sector, 0, sector_size);
}

/* Write the new filesystem's data tables to wherever they're going to end up */

#define error(str)				\
  do {						\
    die (str);					\
  } while(0)

#define seekto(pos,errstr)						\
  do {									\
    off_t __pos = (pos);						\
    if (lseek (dev, __pos, SEEK_SET) != __pos)				\
	error ("seek to " errstr " failed whilst writing tables");	\
  } while(0)

#define writebuf(buf,size,errstr)			\
  do {							\
    int __size = (size);				\
    if (write (dev, buf, __size) != __size)		\
	error ("failed whilst writing " errstr);	\
  } while(0)

static void write_tables(void)
{
    int x;
    befs_super_block superblock;
    befs_disk_block_run log_blocks, root_dir, indices;

    seekto(0, "start of device");
    /* clear all reserved sectors */
    for (x = 0; x < reserved_sectors; ++x)
        writebuf(blank_sector, sector_size, "reserved sector");

    /* seek to start of superblock and write them all */
    seekto(SECTOR_SIZE, "first sector");
    memset(superblock.name, 0, B_OS_NAME_LENGTH);

    if (memcmp(volume_name, NO_NAME, strlen(volume_name)) != 0)
      printf("Using name: %s\n", volume_name);
    memcpy((char *) superblock.name, volume_name, strlen(volume_name));

    superblock.magic1 = BEFS_SUPER_MAGIC1;
    superblock.fs_byte_order = BEFS_BYTEORDER_NATIVE;

    superblock.block_size = 0x800;
    superblock.block_shift = 0xB;

    superblock.num_blocks = 0x10000;
    superblock.used_blocks = 0x88F;

    superblock.inode_size = 0x800;

    superblock.magic2 = BEFS_SUPER_MAGIC2;
    superblock.blocks_per_ag = 1;
    superblock.ag_shift = 0xE;
    superblock.num_ags = 0x4;

    superblock.flags = BEFS_CLEAN;

    log_blocks.allocation_group = 0;
    log_blocks.start = 5;
    log_blocks.len = 0x800;
    superblock.log_blocks = log_blocks;
    superblock.log_start = 0x13;
    superblock.log_end = 0x13;

    superblock.magic3 = BEFS_SUPER_MAGIC3;

    root_dir.allocation_group = 0;
    root_dir.start = 0x805;
    root_dir.len = 1;
    superblock.root_dir = root_dir;

    indices.allocation_group = 0;
    indices.start = 0x808;
    indices.len = 1;
    superblock.indices = indices;

    writebuf((char *) &superblock, sizeof(befs_super_block), "Superblock");
}

/* Report the command usage and exit with the given error code */

static void usage(int exitval)
{
    fprintf(stderr, "\
Usage: mkfs.befs [-n volume-name] [--help] /dev/name [blocks]\n");
    exit(exitval);
}

/* The "main" entry point into the utility */

int main(int argc, char **argv)
{
    int c;
    struct device_info devinfo;
    struct timeval create_timeval;

    enum { OPT_HELP = 1000, OPT_INVARIANT, };
    const struct option long_options[] = {
        {"help", no_argument, NULL, OPT_HELP},
        {"invariant", no_argument, NULL, OPT_INVARIANT},
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

    while ((c = getopt_long(argc, argv, "n:v", long_options, NULL)) != -1) {
        /* Scan the command line for options */
        switch (c) {
        case 'n':              /* n : Volume name */
            /* TODO: cut names longer than 32 characters */
            sprintf(volume_name, "%-s", optarg);
            break;

        case 'v':              /* v : Verbose execution */
            ++verbose;
            break;

        case OPT_HELP:
            usage(0);
            break;

        case OPT_INVARIANT:
            invariant = 1;
            volume_id = 0x1234abcd;
            create_time = 1426325213;
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

    blocks = devinfo.size / BLOCK_SIZE;
    orphaned_sectors = (devinfo.size % BLOCK_SIZE) / sector_size;

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

    setup_tables();             /* Establish the filesystem tables */
    write_tables();             /* Write the filesystem tables away! */

    exit(0);                    /* Terminate with no errors! */
}
