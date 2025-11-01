------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            D E B U G _ S E M                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.2 $                              --
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

--  This package contains data and subprograms to support the A debug switch
--  that is used to generate output showing what node is being analyzed,
--  resolved, evaluated, or expanded.

with Types; use Types;

package Debug_Sem is

   procedure Debug_A_Entry (S : Str; N : Node_Id);
   --  Generates a message prefixed by a sequence of bars showing the nesting
   --  depth (depth increases by 1 for a Debug_A_Entry call and is decreased
   --  by the corresponding Debug_A_Exit call). Then the string is output
   --  (analyzing, expanding etc), followed by the node number and its kind.
   --  This output is generated only if the debug A flag is set. If the debug
   --  A flag is not set, then the call has no effect.

   procedure Debug_A_Exit (S : Str; N : Node_Id; Comment : Str);
   --  Generates the corresponding termination message. The message is preceded
   --  by a sequence of bars, followed by the string S, the node number, and
   --  a trailing comment (e.g. " (already evaluated)"). This output is
   --  generated only if the debug A flag is set. If the debug A flag is not
   --  set, then the call has no effect.

end Debug_Sem;
