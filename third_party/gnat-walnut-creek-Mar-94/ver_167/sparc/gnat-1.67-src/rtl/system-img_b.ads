------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                         S Y S T E M . I M G _ B                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1 $                              --
--                                                                          --
--             Copyright (c) 1992,1993, NYU, All Rights Reserved            --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms  of the GNU  General  Public  License  as  published  by the  Free --
-- Software  Foundation;  either version 2,  or (at your option)  any later --
-- version.  GNAT is distributed  in the hope  that it will be useful,  but --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANT- --
-- ABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public --
-- License  for  more details.  You should have received  a copy of the GNU --
-- General Public License along with GNAT;  see file COPYING. If not, write --
-- to the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. --
--                                                                          --
------------------------------------------------------------------------------

--  Boolean'Image 
 
pragma Ada_9X;
function System.Img_B (V : Boolean; B : Address) return Natural;
pragma Inline (System.Img_B);
--  Computes Boolean'Image (V) and stores the resulting string into the 
--  buffer B, followed by a possible NUL character (the NUL character may
--  or may not be present depending on the implementation of the body, but
--  the caller must allow for its possible presence). The returned result is
--  the number of characters stored, excluding the possible NUL character.
--  The caller must ensure that the buffer provided is long enough to hold
--  the longest possible result.
