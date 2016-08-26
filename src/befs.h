/* msdos_fs.h - MS-DOS filesystem constants/structures

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

#ifndef _MSDOS_FS_H
#define _MSDOS_FS_H

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

#endif                          /* _MSDOS_FS_H */
