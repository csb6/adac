/*****************************************************************************/
/*                                                                           */
/*                                 G N A T                                   */
/*                                                                           */
/*                           COMPILER COMPONENTS                             */
/*                                                                           */
/*                               A D A I N T                                 */
/*                                                                           */
/*                              Specification                                */
/*                                                                           */
/*                 Copyright (c) 1992, All Rights Reserved                   */
/*                  under standard GNU Public License. For                   */
/*                     details, see GNAT documentation.                      */
/*                                                                           */
/*                  Written at New York University by the                    */
/*                        GNU/Ada development team.                          */
/*                                                                           */
/*                             $Revision: 1.12 $                             */
/*                                                                           */
/*****************************************************************************/

/* This file contains thos routines named by "pragma interface_name"         */
/*  statements in the package unix_Library, which are not simply system      */
/*  or standard library calls.                                               */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "config.h"  /* this may define OS2 */

#ifndef O_BINARY
#define O_BINARY 0
#endif

int open_read(char *path)
{
    return open(path, O_RDONLY | O_BINARY);
}

int open_create(char *path)
{
    return open(path, O_WRONLY | O_CREAT | O_TRUNC,
                S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
}

long file_length(int fd)
{
    /* Return the number of bytes in the named file. */
    int ret;
    struct stat statbuf;

    ret = fstat(fd, &statbuf);
    if (ret || !S_ISREG(statbuf.st_mode)) return 0L;
    return (statbuf.st_size);
}

void file_time (int fd, char *stamp)
{
    int ret;
    struct stat statbuf;
    char buf[13];

#ifndef OS2
    ret = fstat (fd, &statbuf);
    /* Not doing anything with errors here! */
    strftime (buf, 13, "%Y%m%d%H%M%S",localtime (&statbuf.st_mtime));
#else
    unsigned long Dos32QueryFileInfo () asm ("Dos32QueryFileInfo");

    typedef struct _FDATE {
	       unsigned short day   : 5;
	       unsigned short month : 4;
	       unsigned short year  : 7;
			  } FDATE;

    typedef struct _FTIME {
	       unsigned short twosecs : 5;
	       unsigned short minutes : 6;
	       unsigned short hours   : 5;
			  } FTIME;

    struct FILESTATUS {
	       FDATE fdateCreation;
	       FTIME ftimeCreation;
	       FDATE fdateLastAccess;
	       FTIME ftimeLastAccess;
	       FDATE fdateLastWrite;
	       FTIME ftimeLastWrite;
	       unsigned long cbFile;
	       unsigned long cbFileAlloc;
	       unsigned short attrFile;
		      } fs;

    ret = Dos32QueryFileInfo (fd, 1, (unsigned char *) &fs,
		 sizeof (struct FILESTATUS));

    sprintf (buf, "%4d%02d%02d%02d%02d",
		fs.fdateLastWrite.year + 1980,
		fs.fdateLastWrite.month,
		fs.fdateLastWrite.day,
		fs.ftimeLastWrite.hours,
		fs.ftimeLastWrite.minutes);
#endif

    strncpy (stamp, buf, 12);
}
