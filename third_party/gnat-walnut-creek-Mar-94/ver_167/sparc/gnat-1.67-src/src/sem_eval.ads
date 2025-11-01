------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ E V A L                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.4 $                              --
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

--  This package contains various subprograms involved in compile time
--  evaluation of expressions and checks for staticness of expressions
--  and types.

with Types; use Types;
package Sem_Eval is
   procedure Static_Evaluation (N : Node_Id);
   --  Performs compile time evaluation of a given expression.

   function Is_Static_Expression (N : Node_Id) return Boolean;
   --  Determines whether an expression fits the definition of an Ada static
   --  expression as given in LRM 4.9.

   function Is_Static_Subtype (Typ : Entity_Id) return Boolean;
   --  Determines whether a subtype fits the definition of an Ada static
   --  subtype as given in LRM 4.9.

   function Expr_Value (N : Node_Id) return Uint;
   --  Returns the folded value of the expression. This function is called
   --  in instances which it has already been determined that the expression
   --  has been constant folded.
end Sem_Eval;
