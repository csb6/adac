------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ U T I L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.46 $                             --
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
with Comperr;  use Comperr;
with Csets;    use Csets;
with Debug;    use Debug;
with Errout;   use Errout;
with Lib;      use Lib;
with Namet;    use Namet;
with Nmake;    use Nmake;
with Output;   use Output;
with Opt;      use Opt;
with Sem;      use Sem;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Stand;    use Stand;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;
package body Sem_Util is

   ------------------------------
   -- Access_Checks_Suppressed --
   ------------------------------

   function Access_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Access_Checks
        or else Suppress_Access_Checks (E);
   end Access_Checks_Suppressed;

   -------------------------------------
   -- Accessibility_Checks_Suppressed --
   -------------------------------------

   function Accessibility_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Accessibility_Checks
        or else Suppress_Accessibility_Checks (E);
   end Accessibility_Checks_Suppressed;

   --------------------
   -- Current_Entity --
   --------------------

   --  The currently visible definition for a given identifier is the
   --  one most chained at the start of the visibility chain, i.e. the
   --  one that is referenced by the Node_Id value of the name of the
   --  given identifier.

   function Current_Entity (N : Node_Id) return Entity_Id is
   begin
      return Get_Name_Entity_Id (Chars (N));
   end Current_Entity;

   -------------------
   -- Current_Scope --
   -------------------

   function Current_Scope return Entity_Id is
      C : constant Entity_Id := Scope_Stack.Table (Scope_Stack.last).Entity;

   begin
      if Present (C) then
         return C;
      else
         return Standard_Standard;
      end if;
   end Current_Scope;

   ------------------------------------
   -- Discriminant_Checks_Suppressed --
   ------------------------------------

   function Discriminant_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Discriminant_Checks
        or else Suppress_Discriminant_Checks (E);
   end Discriminant_Checks_Suppressed;

   --------------------------------
   -- Division_Checks_Suppressed --
   --------------------------------

   function Division_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Division_Checks
        or else Suppress_Division_Checks (E);
   end Division_Checks_Suppressed;

   -----------------------------------
   -- Elaboration_Checks_Suppressed --
   -----------------------------------

   function Elaboration_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Elaboration_Checks
        or else Suppress_Elaboration_Checks (E);
   end Elaboration_Checks_Suppressed;

   ----------------
   -- Enter_Name --
   ----------------

   procedure Enter_Name (Def_Id : Node_Id) is
      E : constant Entity_Id := Current_Entity (Def_Id);
      S : constant Entity_Id := Current_Scope;

   begin
      --  Add new name to current scope declarations

      if Present (E) and then Scope (E) = S then
         if Etype (E) = Any_Type then
            --  Previous entity was entered because of a missing declaration
            --  or else a bad subtype indication. Best is to use the new
            --  entity, and make the previous one invisible.
            Set_Is_Directly_Visible (E, False);

         else
            --  Genuine duplicate declaration. Keep previous one visible,
            --  but give some usable attributes to new one.
            Set_Ekind (Def_Id, E_Variable);
            Set_Etype (Def_Id, Any_Type);
            Set_Scope (Def_Id,  S);

            Error_Msg_Sloc_1 := Sloc (E);
            Error_Msg_N
              ("declaration of& conflicts with line #", Def_Id);
            return;
         end if;
      end if;

      Set_Ekind (Def_Id, E_Void);
      Set_Etype (Def_Id, Any_Type);

      --  The kind E_Void insures that premature uses of the entity will be
      --  detected. Any_Type insures that no cascaded errors will occur.

      Set_Is_Directly_Visible (Def_Id);
      Set_Current_Entity (Def_Id);
      Set_Homonym (Def_Id, E);
      Append_Entity (Def_Id, S);
      Set_Public_Status (Def_Id);

   end Enter_Name;

   --------------------------
   -- Get_Declaration_Node --
   --------------------------

   function Get_Declaration_Node (Unit_Id : Entity_Id) return Node_Id is
      N : Node_Id := Parent (Unit_Id);
   begin
      if Ekind (Unit_Id) = E_Operator then
         --  Predefined operators do not have a full function declaration.
         return N;
      end if;

      while Nkind (N) /= N_Generic_Package_Declaration
        and then Nkind (N) /= N_Generic_Subprogram_Declaration
        and then Nkind (N) /= N_Package_Declaration
        and then Nkind (N) /= N_Subprogram_Declaration
        and then Nkind (N) /= N_Subprogram_Body
        and then Nkind (N) /= N_Package_Renaming_Declaration
        and then Nkind (N) /= N_Subprogram_Renaming_Declaration
      loop
         N := Parent (N);
      end loop;

      return N;
   end Get_Declaration_Node;

   ---------------------------
   -- Has_Private_Component --
   ---------------------------

   function Has_Private_Component (Type_Id : Entity_Id) return Boolean is
      T : Entity_Id := Base_Type (Type_Id);
      Component : Entity_Id;

   begin
      if Ekind (T) in Private_Kind then
         return No (Full_Declaration (T)) and not Is_Generic_Type (T);

      elsif Ekind (T) in Array_Kind then
         return Has_Private_Component (Component_Type (T));

      elsif Ekind (T) in Access_Kind then
         return Is_Private_Type (Designated_Type (T));

      elsif Ekind (T) in Record_Kind then
         Component := First_Entity (T);
         while Present (Component) loop
            if Has_Private_Component (Etype (Component)) then
               return True;
            end if;
            Component := Next_Entity (Component);
         end loop;
         return False;
      else
         return False;
      end if;
   end Has_Private_Component;

   --------------------------
   -- Has_Tagged_Component --
   --------------------------

   function Has_Tagged_Component (Typ : Entity_Id) return Boolean is 
      Comp : Entity_Id;

   begin 
      if Is_Array_Type (Typ) then 
         return Is_Tagged_Type (Component_Type (Typ));
      elsif Is_Tagged_Type (Typ) then
         return True;
      elsif Is_Record_Type (Typ) then
         Comp := First_Component (Typ);

         while Present (Comp) loop
            if Has_Tagged_Component (Etype (Comp)) then
               return True;
            end if;

            Comp := Next_Component (Typ);
         end loop;

         return False;
      else 
         return False;
      end if;
   end Has_Tagged_Component;

   -----------------------------
   -- Index_Checks_Suppressed --
   -----------------------------

   function Index_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Index_Checks
        or else Suppress_Index_Checks (E);
   end Index_Checks_Suppressed;

   -------------
   -- Is_Name --
   -------------

   function Is_Name (N : Node_Id) return Boolean is
   begin
      return Nkind (N) in N_Entity_Name;
   end Is_Name;

   ------------------------------
   -- Length_Checks_Suppressed --
   ------------------------------

   function Length_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Length_Checks
        or else Suppress_Length_Checks (E);
   end Length_Checks_Suppressed;

   -------------------------
   -- New_External_Entity --
   -------------------------

   function New_External_Entity (Kind       : Entity_Kind;
                                 Scope_Id   : Entity_Id;
                                 Sloc_Value : Source_Ptr;
                                 Related_Id : Entity_Id;
                                 Suffix     : Str;
                                 Index      : Nat := 0;
                                 Prefix     : Str := "") return Entity_Id is
      N : constant Entity_Id :=
            Make_Defining_Identifier (Sloc_Value,
              New_External_Name (Chars (Related_Id), Suffix, Index, Prefix));

   begin
      Set_Ekind (N, Kind);
      Set_Is_Internal (N);
      Append_Entity (N, Scope_Id);
      Set_Current_Entity (N);
      return N;
   end New_External_Entity;

   -------------------------
   -- New_Internal_Entity --
   -------------------------

   function New_Internal_Entity (Kind       : Entity_Kind;
                                 Scope_Id   : Entity_Id;
                                 Sloc_Value : Source_Ptr;
                                 Id_Str     : Str) return Entity_Id
   is
      N : constant Entity_Id :=
            Make_Defining_Identifier (Sloc_Value, New_Internal_Name (Id_Str));

   begin
      Set_Ekind (N, Kind);
      Set_Is_Internal (N);
      Append_Entity (N, Scope_Id);
      Set_Current_Entity (N);
      return N;
   end New_Internal_Entity;

   -----------------------
   -- New_Implicit_Type --
   -----------------------

   function New_Implicit_Type (Sloc_Value : Source_Ptr; 
                               Related_Id : Entity_Id := Empty;
                               Suffix     : Str := "";
                               Index      : Nat := 0;
                               Scope_Id   : Entity_Id := Current_Scope) 
     return Entity_Id
   is
      N : Entity_Id;

   begin
      if Related_Id = Empty then
         N := New_Internal_Entity (E_Void, Scope_Id, Sloc_Value, "ityp");

      else
         N := New_External_Entity (E_Void, Scope_Id, Sloc_Value,
                                   Related_Id, Suffix, Index, "ityp");
      end if;

      Set_Etype (N, Any_Type);
      Set_Public_Status (N);
      Append_To (Implicit_Type_List, Make_Implicit_Type (Sloc_Value, N));
      return N;
   end New_Implicit_Type;

   -----------------------
   -- Normalize_Actuals --
   -----------------------

   --  Chain actuals according to formals of subprogram. If there are
   --  no named associations, the chain is simply the list of Parameter
   --  Associations, since the order is the same as the declaration order.
   --  If there are named associations, then the First_Named_Actual field
   --  in the N_Procedure_Call_Statement node or N_Function_Call node
   --  points to the Parameter_Association node for the parameter that
   --  comes first in declaration order. The remaining named parameters
   --  are then chained in declaration order using Next_Named_Actual.

   --  This routine also verifies that the number of actuals is compatible
   --  with the number and default values of formals, but performs no type
   --  checking (type checking is done by the caller).

   --  If the matching succeeds, the function returns True, and the caller
   --  proceeds with type-checking. If the match is unsuccessful, then the
   --  function returns False, and the caller attempts a different inter-
   --  pretation, if there is one.

   --  If the flag Report is on, the call is not overloaded, and a failure
   --  to match can be reported here, rather than in the caller.

   function Normalize_Actuals (N : Node_Id; S : Entity_Id; Report : Boolean)
     return Boolean
   is
      Actuals  : constant List_Id := Parameter_Associations (N);
      Actual   : Node_Id := Empty;
      Formal   : Entity_Id;
      Last     : Entity_Id := Empty;
      First_Named : Entity_Id := Empty;
      Found    : Boolean;
      Formals_To_Match : Integer := 0;
      Actuals_To_Match : Integer := 0;

      procedure Chain (A : Node_Id) is
      begin
         if No (Last) then

            --  Call node points to first actual in list.

            Set_First_Named_Actual (N, Actual_Parameter (A));

         else
            Set_Next_Named_Actual (Last, Actual_Parameter (A));
         end if;

         Last := A;
      end Chain;

   --  Start of processing for Normalize_Actuals

   begin
      Formal := First_Formal (S);

      while Present (Formal) loop
         Formals_To_Match := Formals_To_Match + 1;
         Formal := Next_Formal (Formal);
      end loop;

      --  Find if there is a named association, and verify that no positional
      --  associations appear after named ones.

      if List_Present (Actuals) then
         Actual := First (Actuals);
      end if;

      while Present (Actual)
        and then Nkind (Actual) /= N_Parameter_Association
      loop
         Actuals_To_Match := Actuals_To_Match + 1;
         Actual := Next (Actual);
      end loop;

      if No (Actual) and Actuals_To_Match = Formals_To_Match then

         --  Most common case: positional notation, no defaults

         return True;

      elsif Actuals_To_Match > Formals_To_Match then

         --  Too many actuals: will not work.

         if Report then
            Error_Msg_N ("too many arguments in call", N);
         end if;

         return False;
      end if;

      First_Named := Actual;

      while Present (Actual) loop
         if Nkind (Actual) /= N_Parameter_Association then
            Error_Msg_N
              ("positional parameters not allowed after named ones", Actual);
            return False;

         else
            Actuals_To_Match := Actuals_To_Match + 1;
         end if;

         Actual := Next (Actual);
      end loop;

      if List_Present (Actuals) then
         Actual := First (Actuals);
      end if;

      Formal := First_Formal (S);

      while Present (Formal) loop

         --  Match the formals in order. If the corresponding actual
         --  is positional,  nothing to do. Else scan the list of named
         --  actuals to find the one with the right name.

         if Present (Actual)
           and then Nkind (Actual) /= N_Parameter_Association
         then
            Actual := Next (Actual);
            Actuals_To_Match := Actuals_To_Match - 1;
            Formals_To_Match := Formals_To_Match - 1;

         else
            --  For named parameters, search the list of actuals to find
            --  one that matches the next formal name.

            Actual := First_Named;
            Found  := False;

            while Present (Actual) loop
               if Chars (Selector_Name (Actual)) = Chars (Formal) then
                  Found := True;
                  Chain (Actual);
                  Actuals_To_Match := Actuals_To_Match - 1;
                  Formals_To_Match := Formals_To_Match - 1;
                  exit;
               end if;

               Actual := Next (Actual);
            end loop;

            if not Found then
               if Ekind (Formal) /= E_In_Parameter
                 or else No (Default_Value (Formal))
               then
                  if Report then
                     Error_Msg_NE ("missing argument in call:&", N, Formal);
                  end if;

                  return False;

               else
                  Formals_To_Match := Formals_To_Match - 1;
                  null; -- Chain_Default_Node;
               end if;
            end if;
         end if;

         Formal := Next_Formal (Formal);
      end loop;

      if  Formals_To_Match = 0 and then Actuals_To_Match = 0 then
         return True;
      else
         if Report then
            Error_Msg_N ("too many arguments in call", N);
         end if;

         return False;
      end if;
   end Normalize_Actuals;

   --------------------------------
   -- Overflow_Checks_Suppressed --
   --------------------------------

   function Overflow_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Overflow_Checks
        or else Suppress_Overflow_Checks (E);
   end Overflow_Checks_Suppressed;

   -----------------------------
   -- Range_Checks_Suppressed --
   -----------------------------

   function Range_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Range_Checks
        or else Suppress_Range_Checks (E);
   end Range_Checks_Suppressed;

   ------------------------
   -- Set_Current_Entity --
   ------------------------

   --  The given entity is to be set as the currently visible definition
   --  of its associated name (i.e. the Node_Id associated with its name).
   --  All we have to do is to get the name from the identifier, and
   --  then set the associated Node_Id to point to the given entity.

   procedure Set_Current_Entity (E : Entity_Id) is
   begin
      Set_Name_Entity_Id (Chars (E), E);
   end Set_Current_Entity;

   -----------------------
   -- Set_Public_Status --
   -----------------------

   procedure Set_Public_Status (Id : Entity_Id) is
      S : Entity_Id := Current_Scope;

   begin
      if S = Standard_Standard
        or else (Is_Public (S)
                   and then
                     ((Ekind (S) = E_Package and then not Is_Package_Body (S))
                        or else Is_Record_Type (S)
                        or else Ekind (S) = E_Void))
      then
         Set_Is_Public (Id);
      end if;
   end Set_Public_Status;

   -------------------------------
   -- Storage_Checks_Suppressed --
   -------------------------------

   function Storage_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Storage_Checks
        or else Suppress_Storage_Checks (E);
   end Storage_Checks_Suppressed;

   -----------------
   -- Trace_Scope --
   -----------------

   procedure Trace_Scope (N : Node_Id; E : Entity_Id; Msg : Str) is
   begin
      if Debug_Flag_C then
         for I in 0 .. Scope_Stack.Last loop
            Write_Str ("  ");
         end loop;

         Write_Str (Msg);
         Write_Name (Chars (E));
         Write_Str ("   line ");
         Write_Int (Int (Get_Line_Number (Sloc (N))));
         Write_Eol;
      end if;
   end Trace_Scope;

   ---------------------------
   -- Tag_Checks_Suppressed --
   ---------------------------

   function Tag_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Tag_Checks
        or else Suppress_Tag_Checks (E);
   end Tag_Checks_Suppressed;

   ------------------------
   -- Apply_Access_Check --
   ------------------------

   procedure Apply_Access_Check (N : Node_Id; Typ : Entity_Id) is
   begin
      if not Access_Checks_Suppressed (Typ) then
         Set_Do_Access_Check (N, True);
      end if;
   end Apply_Access_Check;

   ------------------------------
   -- Apply_Discriminant_Check --
   ------------------------------

   procedure Apply_Discriminant_Check (N : Node_Id; Typ : Entity_Id) is
   begin
      if not Discriminant_Checks_Suppressed (Typ) then
         Set_Do_Discriminant_Check (N, True);
      end if;
   end Apply_Discriminant_Check;

   -----------------------
   -- Apply_Range_Check --
   -----------------------

   --  A range constraint may be applied in some of the following contexts:
   --  object declaration, subtype declaration, derived declaration
   --  assignment, function/procedure/entry call, type conversion

   procedure Apply_Range_Check (N : Node_Id; Target_Type : Entity_Id) is
      Source_Type : constant Entity_Id := Etype (N);

   begin

      if Range_Checks_Suppressed (Target_Type)
        or else Index_Checks_Suppressed (Target_Type) 
      then
         return;

      --  Confine the range checks currently to only signed integer types and
      --  enumeration types since support for floating point and fixed point
      --  types is too limited to do useful checks at this time and checks
      --  for modular types need to be better understood by us.

      elsif not (Is_Discrete_Type (Source_Type)
        and then Ekind (Source_Type) not in Modular_Kind)
      then
         return;

      --  ???
      --  Currently the Etype of literals are given the subtype of LHS
      --  rather than the base type. When this is corrected this test can
      --  be removed.

      elsif Nkind (N) = N_Integer_Literal then
         if Is_Static_Subtype (Target_Type)
           and then UI_Le (Expr_Value (Type_Low_Bound (Target_Type)),
                           Intval (N))
           and then UI_Ge (Expr_Value (Type_High_Bound (Target_Type)),
                           Intval (N)) then
            return;
         else
            null;
         end if;

      --  There is no need to have a range check at run-time if the value
      --  considered is guaranteed to be in the range of the target type
      --  even if it might be a dynamic value.
      elsif In_Subrange_Of (Source_Type, Target_Type) then
         return;
      end if;

      Set_Do_Range_Check (N, True);
   end Apply_Range_Check;

   --------------------------
   -- Rewrite_Named_Number --
   --------------------------

   procedure Rewrite_Named_Number (N : Node_Id; Typ : Entity_Id) is
      E    : constant Entity_Id := Entity (N);
      Expr : Node_Id;

   begin
      Expr := Expression (Parent (E));

      --  The Nkind of Expr which is the value given in the original
      --  number declaration should be N_Interal_Literal or N_Real_Literal
      --  at this point since it has already been constant folded.

      Set_Is_Static (N, True);

      if Ekind (E) = E_Named_Integer then
         Rewrite_Int_Literal (N, Intval (Expr));
      else
         Rewrite_Real_Literal (N    => N,
                               Num  => Numerator (Expr),
                               Den  => Denominator (Expr),
                               Decimal_Flag => Decimal (Expr));
      end if;

      Set_Etype (N, Typ);
   end Rewrite_Named_Number;

   -------------------------------
   -- Defining_Unit_Simple_Name --
   -------------------------------

   function Defining_Unit_Simple_Name (N : Node_Id) return Entity_Id is
      Nam : Node_Id := Defining_Unit_Name (N);
   begin
      if Nkind (Nam) in N_Entity then
         return Nam;
      else
         return Defining_Identifier (Nam);
      end if;
   end Defining_Unit_Simple_Name;

   ----------------------
   -- Get_Index_Bounds --
   ----------------------

   procedure Get_Index_Bounds (I : Node_Id; L, H : out Node_Id) is
      Kind : Node_Kind := Nkind (I);

   begin
      if Kind = N_Range then
         L := Low_Bound (I);
         H := High_Bound (I);

      elsif Kind = N_Subtype_Indication then
         L := Low_Bound (Range_Expression (Constraint (I)));
         H := High_Bound (Range_Expression (Constraint (I)));

      elsif Kind in N_Entity_Name and then Is_Type (Entity (I)) then
         L := Low_Bound (Scalar_Range (Entity (I)));
         H := High_Bound (Scalar_Range (Entity (I)));

      else
         Compiler_Abort;
      end if;
   end Get_Index_Bounds;

   --------------------
   -- In_Subrange_Of --
   --------------------

   function In_Subrange_Of (T1 : Entity_Id; T2 : Entity_Id) return Boolean is
   begin
      if T1 = T2 or else Is_Subtype_Of (T1, T2) then
         return True;

      elsif not (Is_Static_Subtype (T1) and then Is_Static_Subtype (T2)) then
         return False;

      elsif Is_Discrete_Type (T1) then
         return UI_Le (Expr_Value (Type_Low_Bound (T2)),
                       Expr_Value (Type_Low_Bound (T1)))
           and then
                UI_Ge (Expr_Value (Type_High_Bound (T2)),
                       Expr_Value (Type_High_Bound (T1)));
      else
         return False;
      end if;
   end In_Subrange_Of;

   ---------------
   -- Same_Name --
   ---------------

   function Same_Name (N1, N2 : Node_Id) return Boolean is
      K1 : constant Node_Kind := Nkind (N1);
      K2 : constant Node_Kind := Nkind (N2);

   begin
      if (K1 = N_Identifier or else K1 = N_Defining_Identifier)
        and then (K2 = N_Identifier or else K2 = N_Defining_Identifier)
      then
         return Chars (N1) = Chars (N2);

      elsif (K1 = N_Selected_Component or else K1 = N_Expanded_Name)
        and then (K2 = N_Selected_Component or else K2 = N_Expanded_Name)
      then
         return Same_Name (Selector_Name (N1), Selector_Name (N2))
           and then Same_Name (Prefix (N1), Prefix (N2));

      else
         return False;
      end if;
   end Same_Name;

   ---------------------------------
   -- Set_Entity_With_Style_Check --
   ---------------------------------

   procedure Set_Entity_With_Style_Check (N : Node_Id; Val : Node_Id) is
      SN : Source_Ptr := Sloc (N);
      SV : Source_Ptr := Sloc (Val);
      TN : Source_Buffer_Ptr;
      TV : Source_Buffer_Ptr;

   begin
      if GNAT_Style_Check
        and then Nkind (N) = N_Identifier
        and then Nkind (Val) = N_Defining_Identifier
        and then SN /= No_Location
        and then SN /= Standard_Location
        and then SV /= No_Location
        and then SV /= Standard_Location
        and then Length_Of_Name (Chars (N)) = Length_Of_Name (Chars (Val))
      then
         TN := File.Table (Get_Sloc_Unit_Number (SN)).Source;
         TV := File.Table (Get_Sloc_Unit_Number (SV)).Source;

         --  A defensive check, if the two Sloc's do not point to instances
         --  of identical names, then we have some internal node with a strange
         --  Sloc, and we should not carry out the rest of the check.

         for I in 1 .. Length_Of_Name (Chars (N)) loop
            if Fold_Lower (TN (SN)) /= Fold_Lower (TV (SV)) then
               goto Done;
            end if;

            SN := SN + 1;
            SV := SV + 1;
         end loop;

         --  And both identifiers should now be ended!

         if Identifier_Char (TN (SN)) or else Identifier_Char (TV (SV)) then
            goto Done;
         end if;

         --  OK, this looks like a safe case to carry out the check

         SN := Sloc (N);
         SV := Sloc (Val);

         for I in 1 .. Length_Of_Name (Chars (N)) loop
            if TN (SN) /= TV (SV) then
               Error_Msg_Node_1 := Val;
               Error_Msg ("(style) bad identifier spelling, should be&", SN);
               goto Done;
            end if;

            SN := SN + 1;
            SV := SV + 1;
         end loop;
      end if;

      --  Come here when error check is completed

      <<Done>>
         Set_Entity (N, Val);
   end Set_Entity_With_Style_Check;

   ------------------------------
   -- Static_Universal_Integer --
   ------------------------------

   function Static_Integer (N : Node_Id) return Uint is
   begin
      Analyze (N);
      Resolve_Complete_Context (N, Any_Integer);

      if Is_Static_Expression (N) then
         return Expr_Value (N);

      else
         Error_Msg_N ("static expression required here", N);
         return No_Uint;
      end if;
   end Static_Integer;

   -------------------
   -- Unimplemented --
   -------------------

   procedure Unimplemented (N : Node_Id; Feature : Str) is
      Nstring : Str (1 .. Feature'Length + 21);

   begin
      --  Note: this is one place where we really would prefer to use &
      --  Error_Msg_N (Msg & " not implemented yet", N), but unfortunately
      --  dynamic concatenation is still a client of this procedure ???

      Nstring (1 .. Feature'Length) := Feature;
      Nstring (Feature'Length + 1 .. Nstring'Length) :=
        " not implemented yet!";
      Error_Msg_N (Nstring, N);
   end Unimplemented;

end Sem_Util;
