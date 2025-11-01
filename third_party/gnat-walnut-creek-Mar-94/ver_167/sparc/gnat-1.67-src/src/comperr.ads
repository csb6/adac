------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              C O M P E R R                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.3 $                              --
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

--  This package contains routines called when a fatal internal compiler
--  error is detected. Calls to these routines cause termination of the
--  current compilation with appropriate error output.

with Types; use Types;
package Comperr is

   procedure Compiler_Abort;
   --  Signals a compiler error unconditionally. Never returns control. Used
   --  in a case where the traceback alone is sufficient diagnostic information
   --  in the case where an error traceback is generated.

   procedure Compiler_Abort (N : Node_Id);
   --  This routine first calls Compiler_Error, then prints the given tree
   --  node, uising Print_Tree_Node, and then calls Compiler_Abort with no
   --  parameter to signal the compiler error.

   procedure Compiler_Error;
   --  Signals a compiler error unconditionally. Returns control if a traceback
   --  is to be generated. Used in a case where the traceback is to be preceded
   --  by additional information. The protocol is to call Compiler_Error, then
   --  generate the additional information required, then call Compiler_Abort.
   --  The additional information will always go to Standard_Error.

end Comperr;
