------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ A T T R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.16 $                             --
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
with Exp_Ch9; use Exp_Ch9;
with Nmake;   use Nmake;
with Rtsfind; use Rtsfind;
with Sem;     use Sem;
with Sem_Res; use Sem_Res;
with Sinfo;   use Sinfo;
with Snames;  use Snames;
with Stand;   use Stand;
with Tbuild;  use Tbuild;
with Ttypes;  use Ttypes;
with Uintp;   use Uintp;

package body Exp_Attr is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Expand_Attribute_Callable          (N : Node_Id);
   procedure Expand_Attribute_Image             (N : Node_Id);
   procedure Expand_Attribute_Size              (N : Node_Id);
   procedure Expand_Attribute_Terminated        (N : Node_Id);

   ---------------------------------
   -- Expand_Attribute_Callable --
   ---------------------------------

   --  Transforms 'Callable attribute into a call to the Callable function.

   procedure Expand_Attribute_Callable (N : Node_Id) is
   begin
      Rewrite_Substitute_Tree (N,
        Build_Call_With_Task (Prefix (N), RTE (RE_Callable)));
      Analyze (N);
      Resolve_Subexpr (N, Standard_Boolean);
   end Expand_Attribute_Callable;

   -----------------------------
   --  Expand_Attribute_Image --
   -----------------------------

   --  For Boolean, Character, and numeric types, typ'Image (X) expands to:

   --    [B : string (1 .. max); 
   --     B (1 .. System.Image.Image_styp (styp (X), B'Address)) ]

   --  where styp is one of the standard supported types. For user defined
   --  enumeration types, the transformation is to:

   --    Table (Enum'Pos (X)).all

   --  where table is the special table declared in the front end and
   --  constructed by special code in Gigi.

   procedure Expand_Attribute_Image (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

      Typ : constant Entity_Id := Entity (Prefix (N));
      --  Prefix type of image attribute

      Btyp : constant Entity_Id := Base_Type (Typ);
      --  The base type of the prefix type

      Maxb : Nat;
      --  Maximum length required for buffer (including trailing NUL) used
      --  in call to function in System.Image package.

      Ient : Entity_Id;
      --  Entity for routine to call in Image package

      Ityp : Entity_Id := Btyp;
      --  Type used for first argument to image routine (styp above).
      --  Usually, but not always the same as the base type.

      B : Entity_Id;
      --  The entity used for the buffer in the expansion

   begin
      if Btyp = Standard_Boolean then
         Maxb := 6;
         Ient := RTE (RE_Img_B);

      --  In the case of Integer, we don't bother to have separate routines
      --  for all integer lengths, it's good enough to just handle the Integer
      --  and Long_Long_Integer cases, and use the smaller of these that fits.

      elsif Is_Integer_Type (Btyp) then
         if Esize (Btyp) <= Esize (Standard_Integer) then
            Ityp := Standard_Integer;
            Maxb := Standard_Integer_Width + 1;
            Ient := RTE (RE_Img_I);
         else
            Ityp := Standard_Long_Long_Integer;
            Maxb := Standard_Long_Long_Integer_Width + 1;
            Ient := RTE (RE_Img_LLI);
         end if;

      elsif Btyp = Standard_Short_Float then
         Maxb := Standard_Short_Float_Digits + 10;
         Ient := RTE (RE_Img_SF);

      elsif Btyp = Standard_Float then
         Maxb := Standard_Float_Digits + 10;
         Ient := RTE (RE_Img_F);

      elsif Btyp = Standard_Long_Float then
         Maxb := Standard_Long_Float_Digits + 10;
         Ient := RTE (RE_Img_LF);

      elsif Btyp = Standard_Long_Long_Float then
         Maxb := Standard_Long_Float_Digits + 10;
         Ient := RTE (RE_Img_LLF);

      elsif Btyp = Standard_Character then
         Maxb := 10;
         Ient := RTE (RE_Img_C);

      --  Only other possibility that is currently supported is the user
      --  defined enumeration type case (other unsupported cases, notably
      --  the real cases, should have been caught as errors earlier on)

      else
         Rewrite_Substitute_Tree (N,
           Make_Explicit_Dereference (Loc,
             Prefix =>
               Make_Indexed_Component (Loc,
                 Prefix =>
                   New_Reference_To
                     (Lit_Name_Table (Entity (Prefix (N))), Loc),
                 Expressions => New_List_1 (
                   Make_Attribute_Reference (Loc,
                     Prefix     => Prefix (N),
                     Identifier => Make_Identifier (Loc, Name_Pos),
                     Expression => Expression (N))))));

         Analyze (N);
         Resolve_Subexpr (N, Standard_String);
         return;
      end if;

      --  If we fall through, we have one of the cases that is handled by
      --  calling one of the routines in the System.Image package

      B := Make_Defining_Identifier (Loc, New_Internal_Name ("temp"));

      Rewrite_Substitute_Tree (N,
        Make_Expression_Actions (Loc,
           Actions => New_List_1 (
             Make_Object_Declaration (Loc,
               Defining_Identifier => B,
               Object_Definition   =>
                 Make_Subtype_Indication (Loc,
                   Subtype_Mark => New_Reference_To (Standard_String, Loc),
                   Constraint   =>
                     Make_Index_Or_Discriminant_Constraint (Loc,
                       Constraints => New_List_1 (
                         Make_Range (Loc,
                           Low_Bound  => Make_Integer_Literal (Loc, Uint_1),
                           High_Bound =>
                             Make_Integer_Literal (Loc,
                               Intval => UI_From_Int (Maxb)))))))),
           Expression =>
             Make_Slice (Loc,
               Prefix => New_Reference_To (B, Loc),
               Discrete_Range =>
                 Make_Range (Loc,
                   Low_Bound  => Make_Integer_Literal (Loc, Uint_1),
                   High_Bound =>
                     Make_Function_Call (Loc,
                       Name => New_Reference_To (Ient, Loc),
                       Parameter_Associations => New_List_2 (
                         Make_Type_Conversion (Loc,
                           Subtype_Mark => New_Reference_To (Ityp, Loc),
                           Expression => Expression (N)),
                         Make_Attribute_Reference (Loc,
                           Prefix => New_Reference_To (B, Loc),
                           Identifier => 
                             Make_Identifier (Loc, Name_Address))))))));

      Analyze (N);
      Resolve_Subexpr (N, Standard_String);

      --  All implicit subtypes that were generated by the expression part
      --  should be become attached to the actions part so they will be
      --  processed before the expression. The implicit type for the slice
      --  is generated in Resolve_Subexpr and thus is not caught by the 
      --  processing of Analyze_Expression_Actions. 

      if Is_Non_Empty_List (Implicit_Type_List) then
         Append_List (Implicit_Type_List, Actions (N));
         Implicit_Type_List := New_List;
      end if;

   end Expand_Attribute_Image;

   ---------------------------
   -- Expand_Attribute_Size --
   ---------------------------

   --  Transforms X'Size into a call to the first dispatching operation
   --  contained  in the Dispatch Table pointed by X._tag. This first operation
   --  happens to be  the implicit _Size function giving the size of a tagged 
   --  object. We can't just expand a call to this function, and rely on
   --  further expansion to transform it into a dispatch call, because _size
   --  may not be visible at this point. 

   procedure Expand_Attribute_Size (N : Node_Id) is
      Loc         : constant Source_Ptr := Sloc (N);
      Pref        : constant Node_Id := Prefix (N);
      Typ         : constant Entity_Id := Etype (Pref);
      Context_Typ : constant Entity_Id := Etype (N);
      New_Node    : Node_Id;

   begin
      if Is_Class_Type (Typ) then 

         New_Node := 
           Make_Indexed_Component (Loc,
             Prefix =>
               Make_Selected_Component (Loc,
                 Prefix => Make_DT_Access (Loc, Pref, Etype (Typ)),
                 Selector_Name => Make_DT_Component (Loc, Etype (Typ), 3)),
             Expressions => New_List_1 (New_Copy (Pref)));

         if Context_Typ /= Universal_Integer then

            New_Node := 
               Make_Type_Conversion (Loc,
                 Subtype_Mark => New_Reference_To (Context_Typ, Loc),
                 Expression => New_Node);
         end if;

         Rewrite_Substitute_Tree (N, New_Node);
         Analyze (N);
         Resolve_Subexpr (N, Context_Typ);
      end if;
   end Expand_Attribute_Size;

   ---------------------------------
   -- Expand_Attribute_Terminated --
   ---------------------------------

   --  Transforms 'Terminated attribute into a call to the Terminated function.

   procedure Expand_Attribute_Terminated (N : Node_Id) is
   begin
      Rewrite_Substitute_Tree (N, 
        Build_Call_With_Task (Prefix (N), RTE (RE_Terminated)));
      Analyze (N);
      Resolve_Subexpr (N, Standard_Boolean);
   end Expand_Attribute_Terminated;

   ----------------------------------
   -- Expand_N_Attribute_Reference --
   ----------------------------------

   procedure Expand_N_Attribute_Reference (N : Node_Id) is
   begin
      case Get_Attribute_Id (Chars (Identifier (N))) is

         --  Attributes requiring special expander action

         when Attribute_Callable =>
            Expand_Attribute_Callable (N);

         when Attribute_Image =>
            Expand_Attribute_Image (N);

         when Attribute_Size =>
            Expand_Attribute_Size (N);

         when Attribute_Terminated =>
            Expand_Attribute_Terminated (N);

         --  All other attributes need no expander action

         when others => null;
      end case;

   end Expand_N_Attribute_Reference;

end Exp_Attr;
