------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         S I N F O . C H A N G E                          --
--                                                                          --
--                                 B o d y                                  --
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

--  This child package of Sinfo contains some routines that permit in place
--  alteration of existing tree nodes by changing the value in the Nkind
--  field. Since Nkind functions logically in a manner similart to a variant
--  record discriminant part, such alterations cannot be permitted in a
--  general manner, but in some specific cases, the fields of related nodes
--  have been deliberately layed out in a manner that permits such alteration.
--  that determin

with Atree; use Atree;

package body Sinfo.Change is

   use Atree.Unchecked_Access;
   --  This package is one of the few packages which is allowed to make direct
   --  references to tree nodes (since it is in the business of providing a
   --  higher level of tree access which other clients are expected to use and
   --  which implements checks).

   ----------------------------------------------
   -- Change_Identifier_To_Defining_Identifier --
   ----------------------------------------------

   procedure Change_Identifier_To_Defining_Identifier (N : in out Node_Id) is
   begin
      Set_Nkind (N, N_Defining_Identifier);
      N := Extend_Node (N);
   end Change_Identifier_To_Defining_Identifier;

   ------------------------------------------------------------
   -- Change_Character_Literal_To_Defining_Character_Literal --
   ------------------------------------------------------------

   procedure Change_Character_Literal_To_Defining_Character_Literal
     (N : in out Node_Id) is
   begin
      Set_Nkind (N, N_Defining_Character_Literal);
      N := Extend_Node (N);
   end Change_Character_Literal_To_Defining_Character_Literal;

   --------------------------------------------------------
   -- Change_Operator_Symbol_To_Defining_Operator_Symbol --
   --------------------------------------------------------

   procedure Change_Operator_Symbol_To_Defining_Operator_Symbol
     (N : in out Node_Id) is
   begin
      Set_Nkind (N, N_Defining_Operator_Symbol);
      Set_Node2 (N, Empty); -- Clear unused Str2 field
      N := Extend_Node (N);
   end Change_Operator_Symbol_To_Defining_Operator_Symbol;

   ----------------------------------------------
   -- Change_Operator_Symbol_To_String_Literal --
   ----------------------------------------------

   procedure Change_Operator_Symbol_To_String_Literal (N : Node_Id) is
   begin
      Set_Nkind (N, N_String_Literal);
      Set_Node1 (N, Empty); -- clear Name1 field
   end Change_Operator_Symbol_To_String_Literal;

   ------------------------------------------------
   -- Change_Selected_Component_To_Expanded_Name --
   ------------------------------------------------

   procedure Change_Selected_Component_To_Expanded_Name (N : Node_Id) is
   begin
      Set_Nkind (N, N_Expanded_Name);
      Set_Chars (N, Chars (Selector_Name (N)));
   end Change_Selected_Component_To_Expanded_Name;

end Sinfo.Change;
