------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                E X C E P                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.8 $                              --
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

package Excep is

--  This package contains declarations for all exceptions handled
--  or raised explicitly (with raise statements) by the compiler.

   Error_Resync : exception;
   --  This exception is raised and handled by the parser to control
   --  syntactic error recovery. For details on the use of this exception,
   --  see the description of the error recovery approach in Parse.

   Unrecoverable_Error : exception;
   --  This exception is raised to immediately terminate the compilation
   --  of the current source program. Uses at the moment are to catch the
   --  maximum errors exceeded situation, and to deal with failure to locate
   --  a required runtime entity.

   Compile_Time_Constraint_Error : exception;
   --  This exception is raised by the compiler during constant folding of
   --  expressions in Sem.Eval when the result of such expression evaluation
   --  would result in a raise of Constraint_Error during run-time. An example
   --  is division, rem or mod where the right operand is zero. At the point
   --  where the operation is attempted the exception is raised and is then
   --  handled in a higher level procedure where the node containing the
   --  result would normally be created.

   --  Note: Constraint_Error should never be raised by the compiler. It if
   --  is raised, it indicates a logic error in the compiler. The compiler
   --  does not have handlers for this, or any other predefined exception.

end Excep;
