/*
 * No copyright is claimed.  This code is in the public domain; do with
 * it what you wish.
 *
 * Written by Karel Zak <kzak@redhat.com>
 */
#ifndef BLKDEV_H
#define BLKDEV_H

#include <sys/types.h>
#include <sys/ioctl.h>
#ifdef HAVE_SYS_IOCCOM_H
#include <sys/ioccom.h>         /* for _IO macro on e.g. Solaris */
#endif
#include <fcntl.h>
#include <unistd.h>

#ifdef HAVE_SYS_MKDEV_H
#include <sys/mkdev.h>          /* major and minor on Solaris */
#endif

#define DEFAULT_SECTOR_SIZE       512

#ifdef __linux__
/* very basic ioctls, should be available everywhere */
#ifndef BLKROSET
#define BLKROSET   _IO(0x12,93) /* set device read-only (0 = read-write) */
#define BLKROGET   _IO(0x12,94) /* get read-only status (0 = read_write) */
#define BLKRRPART  _IO(0x12,95) /* re-read partition table */
#define BLKGETSIZE _IO(0x12,96) /* return device size /512 (long *arg) */
#define BLKFLSBUF  _IO(0x12,97) /* flush buffer cache */
#define BLKRASET   _IO(0x12,98) /* set read ahead for block device */
#define BLKRAGET   _IO(0x12,99) /* get current read ahead setting */
#define BLKFRASET  _IO(0x12,100)        /* set filesystem (mm/filemap.c) read-ahead */
#define BLKFRAGET  _IO(0x12,101)        /* get filesystem (mm/filemap.c) read-ahead */
#define BLKSECTSET _IO(0x12,102)        /* set max sectors per request (ll_rw_blk.c) */
#define BLKSECTGET _IO(0x12,103)        /* get max sectors per request (ll_rw_blk.c) */
#define BLKSSZGET  _IO(0x12,104)        /* get block device sector size */
#endif                          /* !BLKROSET */

#ifndef BLKGETSIZE64
#define BLKGETSIZE64 _IOR(0x12,114,size_t)      /* return device size in bytes (u64 *arg) */
#endif

/* block device topology ioctls, introduced in 2.6.32 (commit ac481c20) */
#ifndef BLKIOMIN
#define BLKIOMIN   _IO(0x12,120)
#define BLKIOOPT   _IO(0x12,121)
#define BLKALIGNOFF _IO(0x12,122)
#define BLKPBSZGET _IO(0x12,123)
#endif

/* discard zeroes support, introduced in 2.6.33 (commit 98262f27) */
#ifndef BLKDISCARDZEROES
#define BLKDISCARDZEROES _IO(0x12,124)
#endif

/* filesystem freeze, introduced in 2.6.29 (commit fcccf502) */
#ifndef FIFREEZE
#define FIFREEZE   _IOWR('X', 119, int) /* Freeze */
#define FITHAW     _IOWR('X', 120, int) /* Thaw */
#endif

#endif                          /* __linux */

#ifndef HDIO_GETGEO
#ifdef __linux__
#define HDIO_GETGEO 0x0301
#endif

struct hd_geometry {
    unsigned char heads;
    unsigned char sectors;
    unsigned short cylinders;   /* truncated */
    unsigned long start;
};
#endif                          /* HDIO_GETGEO */


/* Determine size in bytes */
off_t blkdev_find_size(int fd);

/* get size in bytes */
int blkdev_get_size(int fd, unsigned long long *bytes);

/* get hardware sector size */
int blkdev_get_sector_size(int fd, int *sector_size);

/* specifies whether or not the device is misaligned */
int blkdev_is_misaligned(int fd);

/* get device's geometry - legacy */
int blkdev_get_geometry(int fd, unsigned int *h, unsigned int *s);

/* get partition devices start offset */
int blkdev_get_start(int fd, unsigned int *s);

#endif                          /* BLKDEV_H */
