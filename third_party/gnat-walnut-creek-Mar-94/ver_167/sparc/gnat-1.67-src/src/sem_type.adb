------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ T Y P E                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.25 $                              --
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
with Debug;    use Debug;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Output;   use Output;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Util; use Sem_Util;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Snames;   use Snames;

package body Sem_Type is

   -------------------------------------
   -- Handling of Overload Resolution --
   -------------------------------------

   --  Overload resolution uses two passes over the syntax tree of a complete
   --  context. In the first, bottom-up pass, the types of actuals in calls
   --  are used to resolve possibly overloaded subprogram and operator names.
   --  In the second top-down pass, the type of the context (for example the
   --  condition in a while statement) is used to resolve a possibly ambiguous
   --  call, and the unique subprogram name in turn imposes a specific context
   --  on each of its actuals.

   --  Most expressions are in fact unambiguous, and the bottom-up pass is
   --  sufficient  to resolve most everything. To simplify the common case,
   --  names and expressions carry a flag Is_Overloaded to indicate whether
   --  they have more than one interpretation. If the flag is off, then each
   --  name has already a unique meaning and type, and the bottom-up pass is
   --  sufficient (and much simpler).

   --------------------------
   -- Operator Overloading --
   --------------------------

   --  The visibility of operators is handled differently from that of
   --  other entities. We do not introduce explicit versions of primitive
   --  operators for each type definition. As a result, there is only one
   --  entity corresponding to predefined addition on all numeric types, etc.
   --  The back-end resolves predefined operators according to their type.
   --  The visibility of primitive operations then reduces to the visibility
   --  of the resulting type:  (a + b) is a legal interpretation of some
   --  primitive operator + if the type of the result (which must also be
   --  the type of a and b) is directly visible or use-visible.
   --  User-defined operators are treated like other functions, but the
   --  visibility of these user-defined operations must be special-cased
   --  to determine whether they hide or are hidden by predefined operators.
   --  The form P."+" (x, y) requires additional handling.
   --
   --  Concatenation is treated more conventionally: for every one-dimensional
   --  array type we introduce a explicit concatenation operator. This is
   --  necessary to handle the case of (element & element => array) which
   --  cannot be handled conveniently if there is no explicit instance of
   --  resulting type of the operation.

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Is_Ancestor (T1, T2 : Entity_Id) return Boolean;
   --  T1 is a class type. Verify that it is one of the ancestors of
   --  type T2.

   -----------------
   -- Init_Interp --
   -----------------

   procedure Init_Interp is
   begin
      Interp_Map.Init;
      All_Interp.Init;
   end Init_Interp;

   -----------------
   -- New_Interps --
   -----------------

   procedure New_Interps (N : Node_Id)  is
   begin
      Interp_Map.Increment_Last;
      All_Interp.Increment_Last;
      Interp_Map.Table (Interp_Map.Last) := (N, All_Interp.Last);
      All_Interp.Table (All_Interp.Last) := No_Interp;
      Set_Is_Overloaded (N, True);
   end New_Interps;

   ---------------------
   -- Collect_Interps --
   ---------------------

   procedure Collect_Interps (N : Node_Id) is
      H : Entity_Id;
      First_Interp  : Interp_Index;

   begin
      New_Interps (N);
      First_Interp := All_Interp.Last;
      Add_One_Interp (N, Entity (N), Etype (N));
      H := Homonym (Entity (N));

      if Nkind (N) = N_Expanded_Name then
         while Present (H) and then Scope (H) = Scope (Entity (N)) loop
            Add_One_Interp (N, H, Etype (H));
            H := Homonym (H);
         end loop;

      else
         while Present (H) loop
            exit when not Is_Overloadable (H);

            if Is_Directly_Visible (H) or else Is_Use_Visible (H) then

               --  Only add interpretation if not hidden by an inner
               --  directly-visible one.

               for I in First_Interp .. All_Interp.Last - 1 loop

                  --  Current homograph is not hidden. Add to overloads.

                  if not Is_Directly_Visible (All_Interp.Table (I).Nam) then
                     exit;

                  --  Homograph is hidden

                  elsif Type_Conformant (All_Interp.Table (I).Nam, H) then
                     goto next_homograph;
                  end if;
               end loop;

               --  On exit, we know that current homograph is not hidden.

               Add_One_Interp (N, H, Etype (H));

               if Debug_Flag_E then
                  Write_Str ("Add overloaded Interpretation ");
                  Write_Int (Int (H));
                  Write_Eol;
               end if;
            end if;

            <<next_homograph>>
               H := Homonym (H);
         end loop;
      end if;

         --  If more than one interpretation exists, then mark the end
         --  of the list of interpretations.

      if Is_Overloaded (N) then
         End_Interp_List;
      end if;

   end Collect_Interps;

   --------------------
   -- Add_One_Interp --
   --------------------

   procedure Add_One_Interp (N : Node_Id; E : Entity_Id; T : Entity_Id) is

      procedure Add_Entry (Name :  Entity_Id; Typ : Entity_Id) is
         Index : Interp_Index;
         It    : Interp;

      begin
         Get_First_Interp (N, Index, It);

         while Present (It.Nam) loop

            --  A user-defined function hides another declared at an outer
            --  level, or one that is use-visible.

            if Ekind (Name) = E_Function
              and then Ekind (It.Nam) = E_Function
              and then Is_Directly_Visible (It.Nam)
              and then Type_Conformant (Name, It.Nam)
            then
               --  Previous definition hides new one (which is either in outer
               --  scope or use-visible).
               return;
            end if;

            Get_Next_Interp (Index, It);
         end loop;

         --  On exit, enter new interpretation. The context, or a preference
         --  rule, will resolve the ambiguity on the second pass.

         All_Interp.Table (All_Interp.Last) := (Name, Typ);
         All_Interp.Increment_Last;
         All_Interp.Table (All_Interp.Last) := No_Interp;
      end Add_Entry;

   --  Processing for Add_One_Interp starts here

   begin

      --  If the interpretation is a predefined operator, verify that the
      --  result type is visible.

      if Ekind (E) = E_Operator
        and then not (In_Open_Scopes (Scope (T)) or else Is_Use_Visible (T))
      then
         return;
      end if;

      --  If this is the first interpretation of N, N has type Any_Type.
      --  In that case place the new type on the node. If one interpretation
      --  already exists, indicate that the node is overloaded, and store
      --  both the previous and the new interpretation in All_Interp. If
      --  this is a later interpretation, just add it to the set.

      if Etype (N) = Any_Type then
         if Is_Type (E) then
            Set_Etype (N, T);
         else
            --  Record both the operator or subprogram name, and its type.

            if Nkind (N) in N_Op or else Is_Name (N) then
               Set_Entity (N, E);
            end if;

            Set_Etype (N, T);
         end if;

      --  Either there is no current interpretation in the table for any
      --  node or the interpretation that is present is for a different
      --  node. In both cases add a new interpretation to the table.

      elsif Interp_Map.Last < Interp_Map.First
        or else Interp_Map.Table (Interp_Map.Last).Node /= N
      then
         New_Interps (N);

         if (Nkind (N) in N_Op or else Is_Name (N))
           and then Present (Entity (N))
         then
            Add_Entry (Entity (N), Etype (N));

         elsif Nkind (N) = N_Function_Call
           or else Nkind (N) = N_Procedure_Call_Statement
         then
            Add_Entry (Entity (Name (N)), Etype (N));

         else
            --  Overloaded prefix in indexed or selected component, etc.

            Add_Entry (Etype (N), Etype (N));
         end if;

         Add_Entry (E, T);

      else
         Add_Entry (E, T);
      end if;
   end Add_One_Interp;

   ---------------------
   -- End_Interp_List --
   ---------------------

   procedure End_Interp_List is
   begin
      All_Interp.Table (All_Interp.Last) := No_Interp;
      All_Interp.Increment_Last;
   end End_Interp_List;

   ----------------------
   -- Get_First_Interp --
   ----------------------

   procedure Get_First_Interp (N  : Node_Id;
                               I  : out Interp_Index;
                               It : out Interp) is
      Int_Ind : Interp_Index;

   begin
      for Index in 0 .. Interp_Map.Last loop
         if Interp_Map.Table (Index).Node = N then
            Int_Ind := Interp_Map.Table (Index).Index;
            It := All_Interp.Table (Int_Ind);
            I := Int_Ind;
            return;
         end if;
      end loop;

      --  Procedure should never be called if the node has no interpretations

      Compiler_Abort;
   end Get_First_Interp;

   ----------------------
   --  Get_Next_Interp --
   ----------------------

   procedure Get_Next_Interp (I : in out Interp_Index; It : out Interp) is
   begin
      I  := I + 1;
      It := All_Interp.Table (I);
   end Get_Next_Interp;

   -------------------
   -- Remove_Interp --
   -------------------

   procedure Remove_Interp (I : in out Interp_Index) is
      II : Interp_Index;

   begin

      --  Find end of Interp list and copy downward to erase the discarded one

      II := I + 1;

      while Present (All_Interp.Table (II).Typ) loop
         II := II + 1;
      end loop;

      for J in I + 1 .. II loop
         All_Interp.Table (J - 1) := All_Interp.Table (J);
      end loop;

      --  Back up interp. index to insure that iterator will pick up next
      --  available interpretation.
      I := I - 1;
   end Remove_Interp;

   ------------
   -- Covers --
   ------------

   function Covers (T1, T2 : Entity_Id) return Boolean is
      Typ1 : Entity_Id := T1;
      Typ2 : Entity_Id := T2;

   begin
      pragma Assert (Present (T1) and Present (T2));

      --  Result is returned as a giant logical expression covering the cases

      return

         --  Simplest case: same types

         Base_Type (Typ1) = Base_Type (Typ2)

         --  Literals are compatible with types in  a given "class"

         or else (Typ2 = Universal_Integer and then Is_Integer_Type (Typ1))
         or else (Typ2 = Universal_Real    and then Is_Real_Type (Typ1))
         or else (Typ2 = Any_String        and then Is_String_Type (Typ1))
         or else (Typ2 = Any_Character     and then Is_Character_Type (Typ1))
         or else (Typ2 = Any_Access        and then Is_Access_Type (Typ1))

         --  The context and/or the actual type may be class types

         or else ( Is_Class_Type (Typ1)
                    and then Is_Ancestor (Etype (Typ1), Typ2))
         or else ( Is_Class_Type (Typ2) and then Etype (Typ2) = Typ1)

         --  Some contexts require a class of types rather than a specific type

         or else (Typ1 = Any_Integer and then Is_Integer_Type (Typ2))
         or else (Typ1 = Any_Boolean and then Is_Boolean_Type (Typ2))

         --  An aggregate is compatible with an array or record type

         or else (Typ2 = Any_Composite
                   and then Ekind (Typ1) in E_Array_Type .. E_Record_Subtype)

         --  If the expected type is an anonymous access, the designated
         --  type must cover that of the expression. 

         or else (Ekind (Typ1) = E_Anonymous_Access_Type
            and then Covers (Designated_Type (Typ1), Designated_Type (Typ2)))

         or else (Ekind (Typ1) = E_Access_Subprogram_Type
            and then Is_Access_Type (Typ2)
            and then Is_Overloadable (Designated_Type (Typ2))
            and then Type_Conformant (Designated_Type (Typ1), 
                                      Designated_Type (Typ2))) 

         or else (Ekind (Typ2) = E_Allocator_Type
           and then Covers (Designated_Type (Typ1), Designated_Type (Typ2))) 

         --  the actual type may be the result of a previous error

         or else (Typ2 = Any_Type);
   end Covers;

   function Disambiguate (Nam1, Nam2 : Entity_Id; Typ : Entity_Id)
                                                      return Entity_Id is
      Predef_Subp : Entity_Id;
      User_Subp   : Entity_Id;
   begin
      if Ekind (Nam1) = E_Operator then
         Predef_Subp := Nam1;
         User_Subp   := Nam2;
      elsif Ekind (Nam2) = E_Operator then
         Predef_Subp := Nam2;
         User_Subp   := Nam1;
      else
         --  if two user defined subprograms are visible,  there is a true
         --  ambiguity.
         return Any_Id;
      end if;

      --  If the context is universal, the predefined operator is preferred.
      if Typ = Universal_Integer 
        or else Typ = Universal_Real 
      then
         return Predef_Subp;

      --  If the user-defined operator is in  an open scope,  or in the scope
      --  of the resulting type,  it hides the predefined operator for the
      --  type.
      elsif Hides_Op (User_Subp, Predef_Subp) then
         return User_Subp;

      --  otherwise,  the predefined operator has precedence.
      else return Predef_Subp;
      end if;

   end Disambiguate;


   -------------------------
   -- Has_Compatible_Type --
   -------------------------

   function Has_Compatible_Type (N : Node_Id; Typ : Entity_Id)
     return Boolean
   is
      I  : Interp_Index;
      It : Interp;

   begin
      if not Is_Overloaded (N) then
         return Covers (Typ, Etype (N))
          or else (not Is_Tagged_Type (Typ) and then Covers (Etype (N), Typ));

      else
         Get_First_Interp (N, I, It);

         while Present (It.Typ) loop
            if Covers (Typ, It.Typ)
              or else (not Is_Tagged_Type (Typ) and then Covers (It.Typ, Typ))
            then
               return True;
            end if;

            Get_Next_Interp (I, It);
         end loop;

         return False;
      end if;
   end Has_Compatible_Type;

   --------------
   -- Hides_Op --
   --------------

   function Hides_Op (F : Entity_Id; Op : Entity_Id) return boolean is
   begin
      --  TBSL: if result type is universal, prefer universal (predefined)
      --   interpretation.
      return Operator_Matches_Spec (Op, F)
        and then (In_Open_Scopes (Scope (F))
                    or else Scope (F) = Scope (Etype (First_Formal (F))));
   end Hides_Op;

   -----------------
   -- Is_Ancestor --
   -----------------

   function Is_Ancestor (T1, T2 : Entity_Id) return Boolean is
      Par : Entity_Id;
   begin
      if T1 = T2 then 
         return True;
      else
         Par := Etype (T2);
         loop 
            if T1 = Par then 
               return True;
            elsif Etype (Par) /= Par then
               Par := Etype (Par);
            else
               return False;
            end if;
         end loop;
      end if;
   end Is_Ancestor;

   -------------------
   -- Is_Subtype_Of --
   -------------------

   function Is_Subtype_Of (T1 : Entity_Id; T2 : Entity_Id) return Boolean is
      S : Entity_Id;

   begin
      S := Parent_Subtype (T1);
      while Present (S) loop
         if S = T2 then
            return True;
         else
            S := Parent_Subtype (S);
         end if;
      end loop;
      return False;
   end Is_Subtype_Of;

   ----------------------
   -- Find_Unique_Type --
   ----------------------

   function Find_Unique_Type (L : Node_Id; R : Node_Id) return Entity_Id is
      I  : Interp_Index;
      It : Interp;
      T  : Entity_Id := Etype (L);
   begin

      if Is_Overloaded (R) then
         Get_First_Interp (R, I, It);

         while Present (It.Typ) loop
            if Covers (T, It.Typ) or else Covers (It.Typ, T) then
               Set_Etype (R, It.Typ);
               exit;
            end if;

            Get_Next_Interp (I, It);
         end loop;
      else
         --  Etype of R is correct.
         null;
      end if;

      return Intersect_Types (L, R);
   end Find_Unique_Type;

   ---------------------
   -- Intersect_Types --
   ---------------------

   function Intersect_Types (L, R : Node_Id) return Entity_Id is
      T1 : Entity_Id := Etype (L);
      T2 : Entity_Id := Etype (R);

   begin
      if (T1 = Any_Type or else T2 = Any_Type) then
         return Any_Type;
      end if;

      if Base_Type (T1) = Base_Type (T2) then
         return Base_Type (T1);

      elsif (T1 = Universal_Integer and then Is_Integer_Type (T2))
        or else (T1 = Universal_Real and then Is_Real_Type (T2))
      then
         return Base_Type (T2);

      elsif (T2 = Universal_Integer and then Is_Integer_Type (T1))
        or else (T2 = Universal_Real and then Is_Real_Type (T1))
      then
         return Base_Type (T1);

      elsif (T2 = Any_String and then Is_String_Type (T1)) then
         return Base_Type (T1);

      elsif (T1 = Any_String and then Is_String_Type (T2)) then
         return Base_Type (T2);

      elsif (T2 = Any_Character and then Is_Character_Type (T1)) then
         return Base_Type (T1);

      elsif (T1 = Any_Character and then Is_Character_Type (T2)) then
         return Base_Type (T2);

      elsif (T1 = Any_Access and then Is_Access_Type (T2)) then
         return T2;

      elsif (T2 = Any_Access and then Is_Access_Type (T1)) then
         return T1;

      elsif (T2 = Any_Composite
         and then Ekind (T1) in E_Array_Type .. E_Record_Subtype)
      then
         return T1;

      elsif (T1 = Any_Composite
         and then Ekind (T2) in E_Array_Type .. E_Record_Subtype)
      then
         return T2;

      elsif  Is_Class_Type (T1)
        and then Is_Ancestor (Etype (T1), T2) 
      then
         --  Can only happen for equality operators (the other predefined
         --  operators cannot apply to tagged types).
         return T1;

      elsif Is_Class_Type (T2)
        and then Is_Ancestor (Etype (T2), T1)
      then
         return T2;

      else
         Error_Msg_N ("incompatible types for operator", Parent (L));
         return Any_Type;
      end if;
   end Intersect_Types;

   ---------------------------
   -- Operator_Matches_Spec --
   ---------------------------

   function Operator_Matches_Spec (Op, New_S : Entity_Id) return Boolean is
      New_F,  Old_F : Entity_Id;
      Num : Int := 0;
      Op_Name : Name_Id := Chars (Op);
      T1, T2 : Entity_Id;
      T : constant Entity_Id := Etype (New_S);

   begin
      --  To verify that a predefined operator matches a given signature,
      --  do a case analysis of the operator classes. Function can have one
      --  or two formals and must have the proper result type.

      New_F := First_Formal (New_S);
      Old_F := First_Formal (Op);

      while Present (New_F) and then Present (Old_F) loop
         Num := Num + 1;
         New_F := Next_Formal (New_F);
         Old_F := Next_Formal (Old_F);
      end loop;

      if Present (Old_F) or else Present (New_F) then
         return False; -- different number of parameters

      elsif Num = 1 then
         T1 := Etype (First_Formal (New_S));

         if Op_Name = Name_Op_Subtract
           or else Op_Name = Name_Op_Add
           or else Op_Name = Name_Op_Abs
         then
            return Base_Type (T1) = Base_Type (T)
              and then Is_Numeric_Type (T);

         elsif Op_Name = Name_Op_Not then
            return Base_Type (T1) = Base_Type (T)
              and then Valid_Boolean_Arg (Base_Type (T));

         else
            return False;
         end if;

      else  --  Binary operators.
         T1 := Etype (First_Formal (New_S));
         T2 := Etype (Next_Formal (First_Formal (New_S)));

         if Op_Name =  Name_Op_And or else Op_Name = Name_Op_Or
           or else Op_Name = Name_Op_Xor
         then
            return Base_Type (T1) = Base_Type (T2)
              and then Base_Type (T1) = Base_Type (T)
              and then Valid_Boolean_Arg (Base_Type (T));

         elsif Op_Name = Name_Op_Eq or else Op_Name = Name_Op_Ne then
            return Base_Type (T1) = Base_Type (T2)
              and then Is_Boolean_Type (T);

         elsif Op_Name = Name_Op_Lt or else Op_Name = Name_Op_Le
           or else Op_Name = Name_Op_Gt or else Op_Name = Name_Op_Ge
         then
            return Base_Type (T1) = Base_Type (T2)
              and then Valid_Comparison_Arg (T1)
              and then Is_Boolean_Type (T);

         elsif Op_Name = Name_Op_Add or else Op_Name = Name_Op_Subtract then
            return Base_Type (T1) = Base_Type (T2)
              and then Base_Type (T1) = Base_Type (T)
              and then Is_Numeric_Type (T);

         elsif Op_Name = Name_Op_Multiply
           or else Op_Name = Name_Op_Divide
         then
            --  TBSL : mixed-mode operators.
            return Base_Type (T1) = Base_Type (T2)
              and then Base_Type (T1) = Base_Type (T)
              and then Is_Numeric_Type (T);

         elsif Op_Name = Name_Op_Mod or else Op_Name = Name_Op_Rem then
            return Base_Type (T1) = Base_Type (T2)
              and then Base_Type (T1) = Base_Type (T)
              and then Is_Integer_Type (T);

         elsif Op_Name = Name_Op_Expon then
            return Base_Type (T1) = Base_Type (T)
              and then Is_Numeric_Type (T)
              and then Is_Integer_Type (T2);

         elsif Op_Name = Name_Op_Concat then
            return Is_Array_Type (T)
              and then (Base_Type (T1) = Base_Type (T)
               or else (Base_Type (T1) = Base_Type (Component_Type (T))))
              and then (Base_Type (T2) = Base_Type (T)
               or else (Base_Type (T2) = Base_Type (Component_Type (T))));

         else
            return False;
         end if;
      end if;
   end Operator_Matches_Spec;

   ---------------------------
   --  Valid_Comparison_Arg --
   ---------------------------

   function Valid_Comparison_Arg (T : Entity_Id) return Boolean is
   begin
      return Is_Discrete_Type (T)
        or else Is_Real_Type (T)
        or else (Is_Array_Type (T) and then Number_Dimensions (T) = 1
                  and then Is_Discrete_Type (Component_Type (T)));
   end Valid_Comparison_Arg;

   -----------------------
   -- Valid_Boolean_Arg --
   -----------------------

   function Valid_Boolean_Arg (T : Entity_Id) return Boolean is
   begin
      return Is_Boolean_Type (T)
        or else (Is_Array_Type (T) and then Number_Dimensions (T) = 1
                 and then Is_Boolean_Type (Component_Type (T)))
        or else Ekind (T) in Modular_Kind
        or else T = Universal_Integer;
   end Valid_Boolean_Arg;

   ---------------------
   -- Write_Overloads --
   ---------------------

   procedure Write_Overloads (N : Node_Id) is
      I   : Interp_Index;
      It  : Interp;
      Nam : Entity_Id;

   begin
      if not Is_Overloaded (N) then
         Write_Str ("Non-overloaded entity ");
         Write_Eol;
         Write_Entity_Info (Entity (N), " ");

      else
         Get_First_Interp (N, I, It);
         Write_Str ("Overloaded entity ");
         Write_Eol;
         Nam := It.Nam;

         while Present (Nam) loop
            Write_Entity_Info (Nam,  "      ");
            Write_Str ("=================");
            Write_Eol;
            Get_Next_Interp (I, It);
            Nam := It.Nam;
         end loop;
      end if;
   end Write_Overloads;

end Sem_Type;
