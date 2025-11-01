/****************************************************************************/
/*                                                                          */
/*                         GNAT COMPILER COMPONENTS                         */
/*									    */
/*		               Make TTYPES.ADS 				    */
/*									    */
/*				    Body				    */
/*                                                                          */
/*                            $Revision: 1.3 $                            */
/*                                                                          */
/*             Copyright (c) 1992,1993, NYU, All Rights Reserved            */
/*                                                                          */
/* GNAT is free software;  you can  redistribute it  and/or modify it under */
/* terms  of the GNU  General  Public  License  as  published  by the  Free */
/* Software  Foundation;  either version 2,  or (at your option)  any later */
/* version.  GNAT is distributed  in the hope  that it will be useful,  but */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANT- */
/* ABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public */
/* License  for  more details.  You should have received  a copy of the GNU */
/* General Public License along with GNAT;  see file COPYING. If not, write */
/* to the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */
/*                                                                          */
/****************************************************************************/

#include "hconfig.h"
#include "machmode.h"
#include <stdio.h>

/* Provide default values for all of the type widths
   (copied from c-decl.c).  */

#ifndef CHAR_TYPE_SIZE
#define CHAR_TYPE_SIZE BITS_PER_UNIT
#endif

#ifndef SHORT_TYPE_SIZE
#define SHORT_TYPE_SIZE (BITS_PER_UNIT * MIN ((UNITS_PER_WORD + 1) / 2, 2))
#endif

#ifndef INT_TYPE_SIZE
#define INT_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef LONG_LONG_TYPE_SIZE
#define LONG_LONG_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

#ifndef FLOAT_TYPE_SIZE
#define FLOAT_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef DOUBLE_TYPE_SIZE
#define DOUBLE_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

#ifndef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

/* Maps integer bit sizes to the value of the 'Width attribute for a type
   of that many bits.  */

struct {int size; int width;} widths[] = {{8, 4}, {16, 6}, {32, 11}, {64, 21}};
#define NUM_WIDTHS sizeof widths / sizeof widths[0]

/* Maps float bit sizes to the value of the 'Digits for a type of that many
   bits.  */

struct {int size; int digits;} digits[] = {{32, 6}, {64, 15},
					   {96, 18}, {128, 18}};
#define NUM_DIGITS sizeof digits / sizeof digits[0]

/* Map character in template into value.  */

struct {char c; int bits; int suffix;} charmap[]
  = {{'c', CHAR_TYPE_SIZE, 1}, {'s', SHORT_TYPE_SIZE, 1},
     {'i', INT_TYPE_SIZE, 1}, {'l', LONG_TYPE_SIZE, 1},
     {'L', LONG_LONG_TYPE_SIZE, 1},
     {'F', FLOAT_TYPE_SIZE, 1}, {'f', FLOAT_TYPE_SIZE, 1},
     {'d', DOUBLE_TYPE_SIZE, 1}, {'D', LONG_DOUBLE_TYPE_SIZE, 1},
     {'U', BITS_PER_UNIT, 0}, {'W', BITS_PER_WORD, 0}};
#define NUM_CHARS sizeof charmap / sizeof charmap[0]

int
main (argc, argv)
     int argc;
     char **argv;
{
  FILE *infile;
  int c;

  infile = fopen (argv[1], "r");
  if (infile == 0)
    {
      perror (argv[1]);
      exit (FATAL_EXIT_CODE);
    }

  /* Read the entire file doing something special when we hit a "%".  */
  while ((c = getc (infile)) != EOF)
    {
      if (c != '%')
	putchar (c);
      else
	{
	  int bit_size = -1;
	  int i;
	  int value = -1;
	  int suffix;

	  c = getc (infile);

	  /* Scan the character map to get the size for this object.  */
	  for (i = 0; i < NUM_CHARS; i++)
	    if (charmap[i].c == c)
	      bit_size = charmap[i].bits, suffix = charmap[i].suffix;

	  if (bit_size == -1)
	    {
	      fprintf (stderr, "Unknown map character `%c'\n", c);
	      exit (FATAL_EXIT_CODE);
	    }

	  /* See if we have a suffix and handle it if so.  */
	  if (suffix)
	    {
	      c = getc (infile);
	      switch (c)
		{
		case 'S':
		  value = bit_size;
		  break;

		case 'W':
		  for (i = 0; i < NUM_WIDTHS; i++)
		    if (widths[i].size == bit_size)
		      value = widths[i].width;

		  break;

		case 'D':
		  for (i = 0 ; i < NUM_DIGITS; i++)
		    if (digits[i].size == bit_size)
		      value = digits[i].digits;

		  break;

		default:
		  fprintf (stderr, "Unknown suffix character '%c'\n", c);
		  exit (FATAL_EXIT_CODE);
		}
	    }
	  else
	    value = bit_size;

	  if (value == -1)
	    {
	      fprintf (stderr, "Cannot find needed value\n");
	      exit (FATAL_EXIT_CODE);
	    }

	  printf ("%d", value);
	}
    }

  fflush (stdout);
  exit (ferror (stdout) != 0 ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE);
  /* NOTREACHED */
  return 0;
}
