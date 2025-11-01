------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E R R C O U N T                              --
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

with Types; use Types;
package Errcount is

--  This package contains global variables which hold the number of errors
--  and warnings detected so far. It is separated off from errout so that
--  it can be with'ed without bringing in all the stuff that Errout drags
--  in, which is important because it allows the binder, which uses modules
--  that with Errcount (notably Comperr), to avoid dragging in this stuff.
--  In the case of the binder, these error counts are maintained by Binderr.

   Errors_Detected : Int;
   --  Number of errors detected so far

   Warnings_Detected : Int;
   --  Number of warnings detected

end Errcount;
