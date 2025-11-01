------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ C H 1 3                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.48 $                             --
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

with Atree;    use Atree;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Excep;    use Excep;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Ttypes;   use Ttypes;
with Uintp;    use Uintp;

package body Sem_Ch13 is

   -----------------------------------------
   -- Analyze_Attribute_Definition_Clause --
   -----------------------------------------

   procedure Analyze_Attribute_Definition_Clause (N : Node_Id) is
      Nam  : Node_Id := Name (N);
      Attr : Name_Id := Chars (Identifier (N));
      Expr : Node_Id := Expression (N);
      Id   : Attribute_Id := Get_Attribute_Id (Attr);
      Typ  : Node_Id;
      Ent  : Entity_Id;

   begin
      Find_Name (Nam);
      Ent := Entity (Nam);

      --  Ignore rep clauses for junk entities

      if Etype (Nam) = Any_Type then
         return;

      --  Case of setting Address attribute

      elsif Id = Attribute_Address then

         if Has_Address_Clause (Ent) then
            Error_Msg_N ("address already given for &", Nam);
            return;

         elsif Ekind (Ent) not in Subprogram_Kind
           and then Ekind (Ent) /= E_Variable
           and then Ekind (Ent) /= E_Constant
         then
            Error_Msg_N ("address cannot be given for &", Nam);
         end if;

         Analyze (Expr);

         Typ := RTE (RE_Address);
         Resolve_Complete_Context (Expr, Typ);
         Set_Has_Address_Clause (Ent, True);
         Set_Is_Delayed (Ent);

      --  Case of setting Size attribute

      elsif Id = Attribute_Size then

         if Has_Size_Clause (Ent) then
            Error_Msg_N ("size already given for &", Nam);
            return;

         elsif not Is_Type (Ent)
           and then Ekind (Ent) /= E_Variable
           and then Ekind (Ent) /= E_Constant
         then
            Error_Msg_N ("size cannot be given for &", Nam);
         end if;

         Analyze (Expr);
         Resolve_Complete_Context (Expr, Any_Integer);

         if not Is_Static_Expression (Expr) then
            Error_Msg_N ("expect static value for size ", Expr);

         else
            --  Note that Gigi is in charge of checking that the size we
            --  are assigning is acceptable, and will generate the error
            --  message if the size is inappropriate.

            Set_Esize (Ent, Intval (Expr));
            Set_Has_Size_Clause (Ent, True);
         end if;

      --  Case of setting Storage_Size attribute

      elsif Id = Attribute_Storage_Size then

         if Has_Storage_Size_Clause (Ent) then
            Error_Msg_N ("storage size already given for &", Nam);
            return;

         elsif not Is_Access_Type (Ent) 
           and then Ekind (Ent) /= E_Task_Type
         then
            Error_Msg_N ("size cannot be given for &", Nam);
         end if;

         Analyze (Expr);
         Resolve_Complete_Context (Expr, Any_Integer);

         --  We don't yet implement Storage_Size for access types

         if Is_Access_Type (Ent) then
            Unimplemented (N, "storage size for collections");
         end if;

      --  All other attribute definition clauses are rejected

      else
         Unimplemented (N, "attribute definition?");
      end if;
   end Analyze_Attribute_Definition_Clause;

   -----------------------------------------------
   -- Analyze_Enumeration_Representation_Clause --
   -----------------------------------------------

   procedure Analyze_Enumeration_Representation_Clause (N : Node_Id) is
   begin
      Unimplemented (N, "representation clause?");
   end Analyze_Enumeration_Representation_Clause;

   ------------------------------------------
   -- Analyze_Record_Representation_Clause --
   ------------------------------------------

   --  For now, the only acceptable record representation clauses are those
   --  that confirm a pragma Packed, i.e. the fields must be strictly in
   --  order, each field must take up the number of bits indicated by its
   --  size, and there must be no gaps. Any other record representation
   --  clauses are rejected.

   procedure Analyze_Record_Representation_Clause (N : Node_Id) is
      Mod_Val    : Uint;
      Next_Pos   : Uint;
      Rectype    : Entity_Id;
      Next_CC    : Node_Id;
      Typedecl   : Node_Id;
      Recdef     : Node_Id;

      --  Local procedure used to check the bit position of the next field.
      --  CC is a component clause node. Check_Bit_Pos checks that the
      --  bit position specified by the component clause matches Next_Pos,
      --  and then updates Next_Pos from the Esize field of the type
      --  of the field entity given.

      procedure Check_Bit_Pos (CC : Node_Id; Field : Entity_Id) is
         Bpos   : Uint;
         Posit  : constant Uint := Static_Integer (Position (CC));
         Fbit   : constant Uint := Static_Integer (First_Bit (CC));
         Lbit   : constant Uint := Static_Integer (Last_Bit (CC));
         Oldpos : constant Uint := Next_Pos;
         Cnam   : constant Node_Id := Component_Name (CC);
         Maxsiz : constant Uint := UI_From_Int (System_Address_Size);

      begin
         if Esize (Etype (Field)) = Uint_0 then
            Error_Msg_N
              ("type of component& has unknown size", Cnam);
            raise Error_Resync;
            return;
         else
            Next_Pos := UI_Sum (Next_Pos, Esize (Etype (Field)));
         end if;

         if Posit = No_Uint or else Fbit = No_Uint or else Lbit = No_Uint then
            raise Error_Resync;

         elsif Esize (Etype (Field)) = Uint_0 then
            Error_Msg_N
              ("type of component& has unknown size", Cnam);
            raise Error_Resync;

         elsif UI_Gt (Esize (Etype (Field)), Maxsiz) then
            Error_Msg_Uint_1 := Maxsiz;
            Error_Msg_N
              ("size of component& exceeds maximum permitted (^)", Cnam);
            raise Error_Resync;

         elsif UI_Ne (
            UI_Quotient (Oldpos, Maxsiz),
            UI_Quotient (UI_Difference (Next_Pos, Uint_1), Maxsiz))
         then
            Error_Msg_N
              ("component& crosses word boundary", Cnam);
            raise Error_Resync;

         elsif Chars (Cnam) /= Chars (Field) then
            Error_Msg_Node_2 := Field;
            Error_Msg_N
              ("wrong component, found&, expecting&", Cnam);
            raise Error_Resync;

         elsif UI_Is_Negative (Posit) then
            Error_Msg_N ("position cannot be negative", Position (CC));
            raise Error_Resync;

         elsif UI_Is_Negative (Fbit) then
            Error_Msg_N ("first bit cannot be negative", First_Bit (CC));
            raise Error_Resync;

         elsif not UI_Eq (
                     Esize (Etype (Field)),
                     UI_Sum (Uint_1, UI_Difference (Lbit, Fbit)))
         then
            Error_Msg_N ("wrong number of bits for component&", Cnam);
            raise Error_Resync;

         else
            Bpos := UI_Sum (
                      UI_Product 
                        (UI_From_Int (System_Storage_Unit), Posit), Fbit);

            if UI_Ne (Bpos, Oldpos) then
               Error_Msg_N ("component& not in required position", Cnam);
               raise Error_Resync;
            end if;
         end if;
      end Check_Bit_Pos;

      --  Local procedure used to check a sequence of fields. The single
      --  argument is a list of nodes, and each node in the list has a
      --  Defining_Identifier field.

      procedure Check_Fields (Lst : List_Id) is
         Fld : Entity_Id;
         Nod : Node_Id;

      begin
         if Lst = No_List then 
            return;
         end if;

         Nod := First (Lst);

         while Present (Nod) loop
            Fld := Defining_Identifier (Nod);

            if Next_CC = Empty then
               Error_Msg_NE ("missing component clause for field&", N, Fld);
               raise Error_Resync;

            else
               Check_Bit_Pos (Next_CC, Fld);
               Next_CC := Next (Next_CC);
            end if;

            Nod := Next (Nod);
         end loop;
      end Check_Fields;

      --  Local procedure to process a component list. This is a recursive
      --  procedure that calls itself to process nested variant parts

      procedure Check_Component_List (CL : Node_Id) is
         Save_Start : Uint;
         Max_At_End : Uint;
         Variant    : Node_Id;

      begin
         Check_Fields (Component_Declarations (CL));

         --  Now we process the variants. We record the current bit position
         --  and reset it for each variant, and when we are done we reset the
         --  bit position to be the maximum that it reached for any variant.

         if Present (Variant_Part (CL)) then
            Max_At_End := Next_Pos;
            Save_Start := Next_Pos;

            Variant := First (Variants (Variant_Part (CL)));

            while Present (Variant) loop
               Next_Pos := Save_Start;
               Check_Component_List (Component_List (Variant));

               if UI_Gt (Next_Pos, Max_At_End) then
                  Max_At_End := Next_Pos;
               end if;

               Variant := Next (Variant);
            end loop;

            Next_Pos := Max_At_End;
         end if;
      end Check_Component_List;

   --  Start of processing for Analyze_Record_Representation_Clause

   begin
      if Present (Mod_Clause (N)) then
         Mod_Val := Static_Integer (Expression (Mod_Clause (N)));
         Unimplemented (N, "mod clause");
      end if;

      Analyze (Identifier (N));

      Rectype := Entity (Identifier (N));

      if Ekind (Rectype) /= E_Record_Type then
         Error_Msg_N ("& is not a record type", Identifier (N));
         return;
      end if;

      Typedecl := Parent (Rectype);
      Recdef   := Type_Definition (Typedecl);
      Next_Pos := Uint_0;
      Next_CC := First (Component_Clauses (N));

      --  First check out the discriminants then call the recursive internal
      --  procedure to check out the component list (this call includes the
      --  processing for the common components, and for all the variants)

      Check_Fields (Discriminant_Specifications (Typedecl));
      Check_Component_List (Component_List (Recdef));

      --  Make sure all component declarations are used up

      if Next_CC /= Empty then
         Error_Msg_N
           ("component& does not exist in record", Component_Name (Next_CC));

      --  If everything has checked out (we would not be here if that were
      --  not the case, since anyone finding an error raised Error_Resync to
      --  bypass further processing), then we simply set the Is_Packed flag
      --  on the type, since we have checked that the layout is exactly
      --  equivalent to the default behavior of pragma packed.

      else
         Set_Is_Packed (Rectype, True);
      end if;

   --  We use the Error_Resync exception to get out if we detect any
   --  errors, since otherwise we can get bad cases of cascaded errors

   exception
      when Error_Resync =>
         return;

   end Analyze_Record_Representation_Clause;

   ------------------------
   -- Analyze_Mod_Clause --
   ------------------------

   procedure Analyze_Mod_Clause (N : Node_Id) is
   begin
      Unimplemented (N, "representation clause?");
   end Analyze_Mod_Clause;

   ------------------------------
   -- Analyze_Component_Clause --
   ------------------------------

   procedure Analyze_Component_Clause (N : Node_Id) is
   begin
      Unimplemented (N, "representation clause?");
   end Analyze_Component_Clause;

   -----------------------
   -- Analyze_At_Clause --
   -----------------------

   procedure Analyze_At_Clause (N : Node_Id) is
   begin
      Rewrite_Substitute_Tree (N,
        Make_Attribute_Definition_Clause (Sloc (N),
          Name => Identifier (N),
          Identifier => Make_Identifier (Sloc (N), Name_Address),
          Expression => Expression (N)));
      Analyze (N);
   end Analyze_At_Clause;

   ----------------------------
   -- Analyze_Code_Statement --
   ----------------------------

   procedure Analyze_Code_Statement (N : Node_Id) is
   begin
      Unimplemented (N, "code statement");
   end Analyze_Code_Statement;

   -------------------
   -- Freeze_Entity --
   -------------------

   function Freeze_Entity (E : Entity_Id) return List_Id is
      Comp    : Entity_Id;
      Elmt    : Elmt_Id;
      F_Node  : Node_Id;
      Op_List : Elist_Id;
      Result  : List_Id;
      Subp    : Entity_Id;

      procedure Freeze_It is
      begin
         F_Node := New_Node (N_Freeze_Entity, Sloc (E));
         Set_Entity (F_Node, E);
         Append (F_Node, Result);
      end Freeze_It; 

   begin
      if not Is_Frozen (E) 
        and then Scope (E) = Current_Scope 
        and then Present (Parent (E))
      then
         Result := New_List;
         Set_Is_Frozen (E);

         if Is_Array_Type (E) then
            Append_List (Freeze_Entity (Component_Type (E)), Result);
            Freeze_It;

         elsif Is_Record_Type (E) then
            Comp := First_Entity (E);

            while Present (Comp) loop
               Append_List (Freeze_Entity (Etype (Comp)), Result);
               Comp := Next_Entity (Comp);
            end loop;

            Freeze_It;

            if Is_Tagged_Type (E)
              and then Ekind (E) = E_Record_Type
              and then not Is_Abstract (E)
            then
               Op_List := Primitive_Operations (E); 

               --  Verify that no primitive operation of the type is abstract

               Elmt := First_Elmt (Op_List);

                  while Elmt  /= No_Elmt loop
                     Subp := Id_Of (Elmt);

                     if Is_Abstract (Subp) then
                        Error_Msg_Name_1 := Chars (Subp);
                        Error_Msg_N (
                           "non-abstract type has abstract subprogram%", E);
                     end if;
                     Elmt := Next_Elmt (Elmt);
                  end loop;
            end if;

         elsif Is_Task_Type (E) 
           and then Present (Task_Value_Type (E))
         then
            Append_List (Freeze_Entity (Task_Value_Type (E)), Result);

         elsif Is_Delayed (E) 
           and then Ekind (E) not in Incomplete_Kind 
         then
            --  Subprogram whose profile includes a private type,
            --  or subtype of private type,  or full declaration of
            --  private type. Note that the entity in the full declaration
            --  is currently a copy of the original private entity (it has
            --  not been swapped back yet) and does not receive a freeze node.

            Freeze_It;
         else
            null;
         end if;

         return Result;

      else
         return Empty_List;
      end if;

   end Freeze_Entity;

   -----------------
   --  Freeze_All --
   -----------------

   function Freeze_All return List_Id is
      E      : Entity_Id;
      Result : List_Id := New_List;

   begin
      E := First_Entity (Current_Scope);

      while Present (E) loop

         if not Is_Frozen (E) then
            Append_List (Freeze_Entity (E), Result);
         end if;

         E := Next_Entity (E);
      end loop;

      return Result;
   end Freeze_All;

   --------------------------------------
   -- Validate_Unchecked_Conversion --
   --------------------------------------

   procedure Validate_Unchecked_Conversion (N : Node_Id; Act_Unit : Entity_Id)
   is
      Source : Entity_Id;
      Target : Entity_Id;

   begin
      --  If we are dealing with private types, then do the check on their
      --  fully declared counterparts if the full declarations have been
      --  encountered (they don't have to be visible, but they must exist!)

      Source := Etype (First_Formal (Act_Unit));

      if Ekind (Source) in Private_Kind and then
        Present (Full_Declaration (Source))
      then
         Source := Full_Declaration (Source);
      end if;

      Target := Etype (Act_Unit);

      if Ekind (Target) in Private_Kind and then
        Present (Full_Declaration (Target))
      then
         Target := Full_Declaration (Target);
      end if;

      if Is_Unconstrained_Type (Source) then
         Error_Msg_NE
           ("unconstrained type& not allowed in unchecked conversion",
            N, Source);
         return;
      end if;

      if Is_Unconstrained_Type (Target) then
         Error_Msg_NE
           ("unconstrained type& not allowed in unchecked conversion",
            N, Target);
         return;
      end if;

      if Esize (Source) /= Uint_0
        and then Esize (Target) /= Uint_0
        and then UI_Ne (Esize (Source), Esize (Target))
      then
         Error_Msg_N
           ("types for unchecked conversion have different sizes", N);
      end if;
   end Validate_Unchecked_Conversion;

end Sem_Ch13;
