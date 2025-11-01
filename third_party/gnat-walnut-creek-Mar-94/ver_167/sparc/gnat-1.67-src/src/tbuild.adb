------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               T B U I L D                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.45 $                             --
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

with Atree;   use Atree;
with Einfo;   use Einfo;
with Errout;  use Errout;
with Namet;   use Namet;
with Nmake;   use Nmake;
with Sinfo;   use Sinfo;
with Stand;   use Stand;

package body Tbuild is

   Serial : Nat := 0;
   --  A serial number used to make sure all created names are unique

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Add_Nat_To_Name_Buffer (V : Nat);
   --  Add decimal representation of given value to the end of the string
   --  currently stored in Name_Buffer, incrementing Name_Len as required.

   ----------------------------
   -- Add_Nat_To_Name_Buffer --
   ----------------------------

   procedure Add_Nat_To_Name_Buffer (V : Nat) is
   begin
      if V >= 10 then
         Add_Nat_To_Name_Buffer (V / 10);
      end if;

      Name_Len := Name_Len + 1;
      Name_Buffer (Name_Len) := Char'Val (Char'Pos ('0') + V rem 10);
   end Add_Nat_To_Name_Buffer;

   -----------------------------
   -- Create_Raise_Expression --
   -----------------------------

   procedure Create_Raise_Expression (N : Node_Id; Excep_Id : Entity_Id) is
      Original_Node  : Node_Id;
      Raise_Node     : Node_Id;
      Ident_Node     : Node_Id;
      Statement_List : List_Id;

   begin
      Original_Node := New_Copy (N);
      Change_Node (N, N_Expression_Actions);
      Set_Etype (N, Etype (Original_Node));
      Set_Expression (N, Original_Node);
      Ident_Node := New_Node (N_Identifier, Sloc (N));
      Set_Chars (Ident_Node, Chars (Excep_Id));
      Set_Entity (Ident_Node, Excep_Id);
      Raise_Node := New_Node (N_Raise_Statement, Sloc (N));
      Set_Name (Raise_Node, Ident_Node);
      Statement_List := New_List_1 (Raise_Node);
      Set_Actions (N, Statement_List);
   end Create_Raise_Expression;

   --------------------------
   -- New_Constraint_Error --
   --------------------------

   function New_Constraint_Error (Loc : Source_Ptr) return Node_Id is
      Ident_Node : Node_Id;
      Raise_Node : Node_Id;

   begin
      Ident_Node := New_Node (N_Identifier, Loc);
      Set_Chars (Ident_Node, Chars (Standard_Entity (S_Constraint_Error)));
      Set_Entity (Ident_Node, Standard_Entity (S_Constraint_Error));
      Raise_Node := New_Node (N_Raise_Statement, Loc);
      Set_Name (Raise_Node, Ident_Node);
      return Raise_Node;
   end New_Constraint_Error;

   -----------------------
   -- Make_DT_Component --
   -----------------------

   function Make_DT_Component (Loc : Source_Ptr;
                               Typ : Entity_Id;
                               I   : Positive) return Node_Id is
      X : Node_Id;

   begin
      X := 
        First_Component (Designated_Type (Etype (Access_Disp_Table (Typ))));

      for J in 2 .. I loop
         X := Next_Component (X);
      end loop;

      return New_Reference_To (X, Loc);
   end Make_DT_Component;

   --------------------
   -- Make_DT_Access --
   --------------------

   function Make_DT_Access
     (Loc : Source_Ptr; Rec : Node_Id; Typ : Entity_Id) return Node_Id is
   begin
      return
        Make_Unchecked_Type_Conversion (Loc,
          Subtype_Mark =>
            New_Occurrence_Of (
              Etype (Access_Disp_Table (Typ)), Loc),
          Expression =>
            Make_Selected_Component (Loc,
              Prefix => New_Copy (Rec),
              Selector_Name =>
                New_Reference_To (Tag_Component (Typ), Loc)));
   end Make_DT_Access;

   -------------------------------
   -- Make_Unchecked_Conversion --
   -------------------------------

   function Make_Unchecked_Type_Conversion (Sloc : Source_Ptr;
      Subtype_Mark                 : Node_Id;
      Expression                   : Node_Id;
      Parens                       : Boolean := False) return Node_Id is

      N : constant Node_Id :=
            Make_Type_Conversion (Sloc, Subtype_Mark, Expression, Parens);

   begin
      Set_Unchecked_Conversion (N, True);
      return N;
   end Make_Unchecked_Type_Conversion;

   -----------------------
   -- New_External_Name --
   -----------------------

   function New_External_Name
     (Related_Id : Name_Id; Suffix : Str; Index : Nat := 0; Prefix : Str := "")
     return Name_Id
   is
      PL : constant Int := Prefix'Length;

   begin
      Get_Name_String (Related_Id);

      if PL > 0 then
         Name_Buffer (PL + 3 .. PL + 2 + Name_Len) :=
           Name_Buffer (1 .. Name_Len);
         Name_Buffer (1 .. PL) := Prefix;
         Name_Buffer (PL + 1) := '_';
         Name_Buffer (PL + 2) := '_';
         Name_Len := Name_Len + PL + 2;
      end if;

      Name_Buffer (Name_Len + 1) := '_';
      Name_Buffer (Name_Len + 2) := '_';
      Name_Buffer (Name_Len + 3) := '_';
      Name_Len := Name_Len + 3;
      Name_Buffer (Name_Len + 1 .. Name_Len + Suffix'Length) := Suffix;
      Name_Len := Name_Len + Suffix'Length;

      if Index /= 0 then
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := '_';
         Add_Nat_To_Name_Buffer (Index);
      end if;

      return Name_Find;
   end New_External_Name;

   -----------------------
   -- New_Internal_Name --
   -----------------------

   function New_Internal_Name (Id_Str : Str) return Name_Id is
      Id_Len : constant Nat := Id_Str'Length;

   begin
      Name_Buffer (1 .. Id_Len) := Id_Str;
      Name_Buffer (Id_Len + 1) := '_';
      Name_Buffer (Id_Len + 2) := '_';
      Name_Len := Id_Len + 2;
      Serial := Serial + 1;
      Add_Nat_To_Name_Buffer (Serial);
      return Name_Enter;
   end New_Internal_Name;

   -----------------------
   -- New_Occurrence_Of --
   -----------------------

   function New_Occurrence_Of (Def_Id : Entity_Id;
                               Loc : Source_Ptr) return Node_Id is
      Occurrence : Node_Id;

   begin
      Occurrence := New_Node (N_Identifier, Loc);
      Set_Chars (Occurrence, Chars (Def_Id));
      Set_Entity (Occurrence, Def_Id);
      Set_Etype (Occurrence, Etype (Def_Id));
      return Occurrence;
   end New_Occurrence_Of;

   ----------------------
   -- New_Reference_To --
   ----------------------

   function New_Reference_To (Def_Id : Entity_Id;
                              Loc : Source_Ptr) return Node_Id is
      Occurrence : Node_Id;

   begin
      Occurrence := New_Node (N_Identifier, Loc);
      Set_Chars (Occurrence, Chars (Def_Id));
      Set_Entity (Occurrence, Def_Id);
      return Occurrence;
   end New_Reference_To;

   -------------------
   -- Raise_Warning --
   -------------------

   procedure Raise_Warning (N : Node_Id; Excep_Id : Entity_Id; Reason : Str) is
   begin
      Error_Msg_N (Reason, N);
      Error_Msg_NE ("& will be raised at runtime?!", N, Excep_Id);
   end Raise_Warning;

   -------------------------
   -- Rewrite_Int_Literal --
   -------------------------

   procedure Rewrite_Int_Literal (N : Node_Id; Val : Uint) is
      Int_Literal_Node : Node_Id;

   begin
      Int_Literal_Node := New_Node (N_Integer_Literal, Sloc (N));
      Set_Intval (Int_Literal_Node, Val);
      Set_Etype (Int_Literal_Node, Etype (N));
      Set_Is_Evaluated (Int_Literal_Node, True);
      Set_Is_Static (Int_Literal_Node, Is_Static (N));
      Rewrite_Substitute_Tree (N, Int_Literal_Node);
   end Rewrite_Int_Literal;

   -------------------------
   -- Rewrite_Real_Literal --
   -------------------------

   procedure Rewrite_Real_Literal (N : Node_Id;
                                   Num, Den : Uint;
                                   Decimal_Flag : Boolean) is
      Real_Literal_Node : Node_Id;

   begin
      Real_Literal_Node := New_Node (N_Real_Literal, Sloc (N));
      Set_Numerator (Real_Literal_Node, Num);
      Set_Denominator (Real_Literal_Node, Den);
      Set_Etype (Real_Literal_Node, Etype (N));
      Set_Is_Evaluated (Real_Literal_Node, True);
      Set_Is_Static (Real_Literal_Node, Is_Static (N));
      Set_Decimal (Real_Literal_Node, Decimal_Flag);
      Rewrite_Substitute_Tree (N, Real_Literal_Node);
   end Rewrite_Real_Literal;

   --------------------------
   -- Rewrite_Enum_Literal --
   --------------------------

   procedure Rewrite_Enum_Literal (N : Node_Id; Val : Entity_Id) is
      Id_Node : Node_Id;

   begin
      Id_Node := New_Node (N_Identifier, Sloc (N));
      Set_Entity (Id_Node, Val);
      Set_Chars (Id_Node, Chars (Val));
      Set_Etype (Id_Node, Etype (N));
      Set_Is_Evaluated (Id_Node, True);
      Set_Is_Static (Id_Node, Is_Static (N));
      Rewrite_Substitute_Tree (N, Id_Node);
   end Rewrite_Enum_Literal;

end Tbuild;
