------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P A R . L A B L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.6 $                              --
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

separate (Par)
procedure Labl is
   Parent_Node : Node_Id;
   --  Used to climb up parents of label node

   Label_Decl_Node : Node_Id;
   --  Implicit label declaration node

   Defining_Ident_Node : Node_Id;
   --  Defining identifier node for implicit label declaration

   Next_Label_Elmt : Elmt_Id;
   --  Next element on label element list

   Label_Node : Node_Id;
   --  Next label node to process

begin
   Next_Label_Elmt := First_Elmt (Label_List);

   while Next_Label_Elmt /= No_Elmt loop
      Label_Node := Id_Of (Next_Label_Elmt);

      --  Climb parents until we find the closest enclosing body or block
      --  containing a declarative region

      Parent_Node := Parent (Label_Node);

      while Present (Parent_Node)
        and then Nkind (Parent_Node) /= N_Entry_Body
        and then Nkind (Parent_Node) /= N_Task_Body
        and then Nkind (Parent_Node) /= N_Package_Body
        and then Nkind (Parent_Node) /= N_Subprogram_Body
        and then Nkind (Parent_Node) /= N_Block_Statement
      loop
         Parent_Node := Parent (Parent_Node);
      end loop;

      --  If we didn't find a parent, then the label in question never got
      --  hooked into a reasonable declarative part. This happens only in
      --  error situations, and we simply ignore the entry (we aren't going
      --  to get into the semantics in any case given the error).

      if Present (Parent_Node) then

         --  Now create the implicit label declaration node and its
         --  corresponding defining identifier. Note that the defining
         --  occurrence of a label is the implicit label declaration that
         --  we are creating. The label itself is an applied occurrence.

         Label_Decl_Node
           := New_Node (N_Implicit_Label_Declaration, Sloc (Label_Node));
         Set_Label (Label_Decl_Node, Label_Node);
         Defining_Ident_Node :=
           Extend_Node (New_Node
             (N_Defining_Identifier, Sloc (Identifier (Label_Node))));
         Set_Chars (Defining_Ident_Node, Chars (Identifier (Label_Node)));
         Set_Defining_Identifier (Label_Decl_Node, Defining_Ident_Node);

         --  Now attach the implicit label declaration to the appropriate
         --  declarative region, creating a declaration list if none exists

         if not List_Present (Declarations (Parent_Node)) then
            Set_Declarations (Parent_Node, New_List);
         end if;

         Append (Label_Decl_Node, Declarations (Parent_Node));
      end if;

      Next_Label_Elmt := Next_Elmt (Next_Label_Elmt);
   end loop;

end Labl;
