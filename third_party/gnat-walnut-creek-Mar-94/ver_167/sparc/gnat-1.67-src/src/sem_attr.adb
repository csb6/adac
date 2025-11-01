------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ A T T R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.62 $                             --
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
with Nmake;    use Nmake;
with Opt;      use Opt;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Table;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

package body Sem_Attr is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Analyze_Access                        (N : Node_Id);
   procedure Analyze_Address                       (N : Node_Id);
   procedure Analyze_Adjacent                      (N : Node_Id);
   procedure Analyze_Aft                           (N : Node_Id);
   procedure Analyze_Alignment                     (N : Node_Id);
   procedure Analyze_Base                          (N : Node_Id);
   procedure Analyze_Bit_Order                     (N : Node_Id);
   procedure Analyze_Body_Version                  (N : Node_Id);
   procedure Analyze_Callable                      (N : Node_Id);
   procedure Analyze_Caller                        (N : Node_Id);
   procedure Analyze_Ceiling                       (N : Node_Id);
   procedure Analyze_Class                         (N : Node_Id);
   procedure Analyze_Component_Size                (N : Node_Id);
   procedure Analyze_Compose                       (N : Node_Id);
   procedure Analyze_Constrained                   (N : Node_Id);
   procedure Analyze_Copy_Sign                     (N : Node_Id);
   procedure Analyze_Count                         (N : Node_Id);
   procedure Analyze_Delta                         (N : Node_Id);
   procedure Analyze_Denorm                        (N : Node_Id);
   procedure Analyze_Digits                        (N : Node_Id);
   procedure Analyze_Emax                          (N : Node_Id);
   procedure Analyze_Epsilon                       (N : Node_Id);
   procedure Analyze_Exponent                      (N : Node_Id);
   procedure Analyze_External_Tag                  (N : Node_Id);
   procedure Analyze_First                         (N : Node_Id);
   procedure Analyze_First_Bit                     (N : Node_Id);
   procedure Analyze_Floor                         (N : Node_Id);
   procedure Analyze_Fore                          (N : Node_Id);
   procedure Analyze_Fraction                      (N : Node_Id);
   procedure Analyze_Identity                      (N : Node_Id);
   procedure Analyze_Image                         (N : Node_Id);
   procedure Analyze_Input                         (N : Node_Id);
   procedure Analyze_Large                         (N : Node_Id);
   procedure Analyze_Last                          (N : Node_Id);
   procedure Analyze_Last_Bit                      (N : Node_Id);
   procedure Analyze_Leading_Part                  (N : Node_Id);
   procedure Analyze_Length                        (N : Node_Id);
   procedure Analyze_Machine                       (N : Node_Id);
   procedure Analyze_Machine_Emax                  (N : Node_Id);
   procedure Analyze_Machine_Emin                  (N : Node_Id);
   procedure Analyze_Machine_Mantissa              (N : Node_Id);
   procedure Analyze_Machine_Overflows             (N : Node_Id);
   procedure Analyze_Machine_Radix                 (N : Node_Id);
   procedure Analyze_Machine_Rounds                (N : Node_Id);
   procedure Analyze_Mantissa                      (N : Node_Id);
   procedure Analyze_Max                           (N : Node_Id);
   procedure Analyze_Max_Size_In_Storage_Elements  (N : Node_Id);
   procedure Analyze_Min                           (N : Node_Id);
   procedure Analyze_Model                         (N : Node_Id);
   procedure Analyze_Model_Emax                    (N : Node_Id);
   procedure Analyze_Model_Emin                    (N : Node_Id);
   procedure Analyze_Model_Epsilon                 (N : Node_Id);
   procedure Analyze_Model_Large                   (N : Node_Id);
   procedure Analyze_Model_Mantissa                (N : Node_Id);
   procedure Analyze_Model_Small                   (N : Node_Id);
   procedure Analyze_Output                        (N : Node_Id);
   procedure Analyze_Pos                           (N : Node_Id);
   procedure Analyze_Position                      (N : Node_Id);
   procedure Analyze_Pred                          (N : Node_Id);
   procedure Analyze_Range_Attribute               (N : Node_Id);
   procedure Analyze_Read                          (N : Node_Id);
   procedure Analyze_Remainder                     (N : Node_Id);
   procedure Analyze_Round                         (N : Node_Id);
   procedure Analyze_Rounding                      (N : Node_Id);
   procedure Analyze_Safe_Emax                     (N : Node_Id);
   procedure Analyze_Safe_First                    (N : Node_Id);
   procedure Analyze_Safe_Large                    (N : Node_Id);
   procedure Analyze_Safe_Last                     (N : Node_Id);
   procedure Analyze_Safe_Small                    (N : Node_Id);
   procedure Analyze_Scale                         (N : Node_Id);
   procedure Analyze_Signed_Zeros                  (N : Node_Id);
   procedure Analyze_Size                          (N : Node_Id);
   procedure Analyze_Small                         (N : Node_Id);
   procedure Analyze_Standard_Access               (N : Node_Id);
   procedure Analyze_Storage_Pool                  (N : Node_Id);
   procedure Analyze_Storage_Size                  (N : Node_Id);
   procedure Analyze_Succ                          (N : Node_Id);
   procedure Analyze_Tag                           (N : Node_Id);
   procedure Analyze_Terminated                    (N : Node_Id);
   procedure Analyze_Truncation                    (N : Node_Id);
   procedure Analyze_Unbiased_Rounding             (N : Node_Id);
   procedure Analyze_Unchecked_Access              (N : Node_Id);
   procedure Analyze_Val                           (N : Node_Id);
   procedure Analyze_Valid                         (N : Node_Id);
   procedure Analyze_Value                         (N : Node_Id);
   procedure Analyze_Version                       (N : Node_Id);
   procedure Analyze_Wide_Image                    (N : Node_Id);
   procedure Analyze_Wide_Value                    (N : Node_Id);
   procedure Analyze_Width                         (N : Node_Id);
   procedure Analyze_Write                         (N : Node_Id);

   function Check_Array_Type (N : Node_Id; T : Entity_Id) return Boolean;
   --  Common semantic checks for all array attributes

   procedure Check_Array_Or_Scalar_Type (N : Node_Id; T : Entity_Id);
   --  Common procedure used by First, Last, Range

   procedure Check_Discrete_Type (N : Node_Id; T : Entity_Id);
   --  Verify that type is discrete type

   procedure Check_Float_Type (N : Node_Id; T : Entity_Id);
   --  Verify that type is float type

   procedure Check_Fixed_Type (N : Node_Id; T : Entity_Id);
   --  Verify that type is fixed type

   procedure Check_Real_Type (N : Node_Id; T : Entity_Id);
   --  Verify that type is fixed or float type

   procedure Check_Discrete_Attribute (N : Node_Id);
   --  Common processing for attributes operating on discrete types

   procedure Unimplemented_Attribute (N : Node_Id);
   --  Give error message for unimplemented attribute

   ----------------------
   -- Check_Array_Type --
   ----------------------

   function Check_Array_Type (N : Node_Id; T : Entity_Id) return Boolean is
      P : constant Node_Id := Prefix (N);
      E : constant Node_Id := Expression (N);

      D : Int;
      --  Dimension number for array attributes.

   begin
      if Is_Array_Type (T) then
         if (not Is_Constrained (T))
           and then Is_Name (P) and then Is_Type (Entity (P))
         then
            Error_Msg_N ("expect constrained array prefix for attribute", N);
            return False;
         end if;

         D := Number_Dimensions (T);

      elsif Is_Access_Type (T)
        and then Is_Array_Type (Designated_Type (T))
      then
         if Is_Name (P) and then Is_Type (Entity (P)) then
            Error_Msg_N ("prefix of attribute cannot be access type", N);
            return False;
         end if;

         D := Number_Dimensions (Designated_Type (T));

      else
         Error_Msg_N ("expect array prefix for attribute", N);
         return False;
      end if;

      if Present (E) then
         Resolve_Subexpr (E, Any_Integer);
         Set_Etype (E, Standard_Integer);

         if not Is_Static_Expression (E) then
            Error_Msg_N ("expect static expression for dimension", E);
            return False;

         elsif  UI_To_Int (Intval (E)) > D
           or else UI_To_Int (Intval (E)) < 1
         then
            Error_Msg_N ("Invalid dimension number for array type", E);
            return False;
         end if;
      end if;

      return True;
   end Check_Array_Type;

   --------------------------------
   -- Check_Array_Or_Scalar_Type --
   --------------------------------

   procedure Check_Array_Or_Scalar_Type (N : Node_Id; T : Entity_Id) is
      E : constant Node_Id := Expression (N);
      Index_Type : Entity_Id;

      D : Int;
      --  Dimension number for array attributes.

   begin
      Set_Etype (N, Any_Type);

      if Is_Scalar_Type (T) then
         if Present (E) then
            Error_Msg_N ("invalid second argument in scalar attribute", E);
         else
            Set_Etype (N, Base_Type (T));
            return;
         end if;

      elsif Check_Array_Type (N, T) then

         --  We know prefix is appropriate for an array, and the expression
         --  is static and within range of the dimensions of the type.

         if Is_Array_Type (T) then
            Index_Type := First_Index (T);

         elsif Is_Access_Type (T) then
            Index_Type := First_Index (Designated_Type (T));
         end if;

         if No (E) then

            --  First dimension assumed

            Set_Etype (N, Etype (Index_Type));

         else
            D := UI_To_Int (Intval (E));

            for I in 1 .. D - 1 loop
               Index_Type := Next_Index (Index_Type);
            end loop;

            Set_Etype (N, Etype (Index_Type));
            Set_Etype (E, Standard_Integer);
         end if;
      end if;
   end Check_Array_Or_Scalar_Type;

   -------------------------
   -- Check_Discrete_Type --
   -------------------------

   procedure Check_Discrete_Type (N : Node_Id; T : Entity_Id) is
   begin
      if not Is_Discrete_Type (T) then
         Error_Msg_N ("prefix of attribute must be discrete type", N);
      end if;
   end Check_Discrete_Type;

   ----------------------
   -- Check_Float_Type --
   ----------------------

   procedure Check_Float_Type (N : Node_Id; T : Entity_Id) is
   begin
      if not Is_Float_Type (T) then
         Error_Msg_N ("prefix of attribute must be floating type", N);
      end if;
   end Check_Float_Type;

   ----------------------
   -- Check_Fixed_Type --
   ----------------------

   procedure Check_Fixed_Type (N : Node_Id; T : Entity_Id) is
   begin
      if not Is_Fixed_Type (T) then
         Error_Msg_N ("prefix of attribute must be fixed type", N);
      end if;
   end Check_Fixed_Type;

   ---------------------
   -- Check_Real_Type --
   ---------------------

   procedure Check_Real_Type (N : Node_Id; T : Entity_Id) is
   begin
      if not Is_Real_Type (T) then
         Error_Msg_N ("prefix of attribute must be real type", N);
      end if;
   end Check_Real_Type;

   ------------------------------
   -- Check_Discrete_Attribute --
   ------------------------------

   procedure Check_Discrete_Attribute (N : Node_Id) is
      T : Entity_Id := Etype (Prefix (N));
      E : Node_Id   := Expression (N);

   begin
      Check_Discrete_Type (N, T);

      if No (E) then
         Error_Msg_N ("missing second argument of discrete attribute", N);
      else
         Resolve_Subexpr (E, T);
      end if;
   end Check_Discrete_Attribute;

   -----------------------------
   -- Unimplemented_Attribute --
   -----------------------------

   procedure Unimplemented_Attribute (N : Node_Id) is
   begin
      Unimplemented (N, "attribute");
   end Unimplemented_Attribute;

   -----------------------
   -- Resolve_Attribute --
   -----------------------

   procedure Resolve_Attribute (N : Node_Id; Typ : Entity_Id) is
      Index : Interp_Index;
      It    : Interp;
      P     : Node_Id := Prefix (N);

   begin
      if Etype (N) = Universal_Integer or else Etype (N) = Universal_Real then
         if Typ = Universal_Integer then
            Set_Etype (N, Standard_Longest_Runtime_Integer);

         elsif Typ = Universal_Real then
            Set_Etype (N, Standard_Longest_Runtime_Real);

         else
            Set_Etype (N, Typ);
         end if;

      else
         case Get_Attribute_Id (Chars (Identifier (N))) is

            --  For these attributes, if the prefix denotes an entity, it is
            --  interpreted as a name, never as a call. It may be overloaded,
            --  in which case resolution uses the profile of the context type.
            --  Otherwise prefix must be resolved.

            when Attribute_Access 
               | Attribute_Address 
               | Attribute_Unchecked_Access =>

               if Is_Name (P) then
                  if Is_Overloaded (P) then
                     Get_First_Interp (P, Index, It);

                     while Present (It.Nam) loop

                        if Type_Conformant (It.Nam, Designated_Type (Typ)) then
                           Set_Entity (P, It.Nam);
                           exit;
                        end if;

                        Get_Next_Interp (Index, It);
                     end loop;
                  end if;

                  if Is_Abstract (Entity (P)) then
                     Error_Msg_N 
                       ("prefix of attribute cannot be abstract subprogram", 
                            N);
                  end if;

               else 
                  Resolve_Subexpr (P, Etype (P));
               end if;

               Set_Etype (N, Typ);

            when Attribute_Range =>

               if not Is_Name (P)
                 or else not Is_Type (Entity (P)) then
                  Resolve_Subexpr (P, Etype (P));
               end if;

               --  We now replace the Range attribute node with a range 
               --  expression whose bounds are the 'First and 'Last attributes
               --  applied to the same prefix. The reason that we do this 
               --  transformation here instead of in the expander is that it
               --  simplifies other parts of the semantic analysis (note that 
               --  the RM specifically mentions this equivalence, we take care
               --  that the prefix is only evaluated once).

               Set_Evaluate_Once (P, True);
               Rewrite_Substitute_Tree (N,
                 Make_Range (Sloc (N),
                   Low_Bound => Make_Attribute_Reference (Sloc (N),
                     Prefix     => P,
                     Identifier => Make_Identifier (Sloc (N), Name_First),
                     Expression => Expression (N)),
                   High_Bound => Make_Attribute_Reference (Sloc (N),
                     Prefix     => P,
                     Identifier => Make_Identifier (Sloc (N), Name_Last),
                     Expression => Expression (N))));
               Analyze (N);

            --  For other attributes, resolve prefix if it is not a type.

            when others =>
               if not Is_Name (P)
                 or else not Is_Type (Entity (P)) then
                  Resolve_Subexpr (P, Etype (P));
               end if;


         end case;
      end if;
   end Resolve_Attribute;

   -----------------------
   -- Analyze_Attribute --
   -----------------------

   procedure Analyze_Attribute (N : Node_Id) is
      Id     : constant Attribute_Id :=
                 Get_Attribute_Id (Chars (Identifier (N)));
      P      : constant Node_Id := Prefix (N);
      E      : constant Node_Id := Expression (N);
      P_Type : Entity_Id;

   begin
      --  Analyze prefix and expression,  and dispatch to specific proc

      Set_Etype (N, Any_Type);
      Analyze (P);
      P_Type := Etype (P);

      --  Immediate exit if prefix had previous error

      if P_Type = Any_Type then
         return;
      end if;

      if Present (E) then
         Analyze (E);

         --  Immediate exit if expression had previous error

         if Etype (E) = Any_Type then
            return;
         end if;
      end if;

      if Is_Overloaded (P) and then Id /= Attribute_Access then
         Error_Msg_N ("ambiguous prefix for attribute", N);
         return;
      end if;

      case Id is
         when Attribute_Access            => Analyze_Access            (N);
         when Attribute_Address           => Analyze_Address           (N);
         when Attribute_Adjacent          => Analyze_Adjacent          (N);
         when Attribute_Aft               => Analyze_Aft               (N);
         when Attribute_Alignment         => Analyze_Alignment         (N);
         when Attribute_Base              => Analyze_Base              (N);
         when Attribute_Bit_Order         => Analyze_Bit_Order         (N);
         when Attribute_Body_Version      => Analyze_Body_Version      (N);
         when Attribute_Caller            => Analyze_Caller            (N);
         when Attribute_Callable          => Analyze_Callable          (N);
         when Attribute_Ceiling           => Analyze_Ceiling           (N);
         when Attribute_Class             => Analyze_Class             (N);
         when Attribute_Component_Size    => Analyze_Component_Size    (N);
         when Attribute_Compose           => Analyze_Compose           (N);
         when Attribute_Constrained       => Analyze_Constrained       (N);
         when Attribute_Copy_Sign         => Analyze_Copy_Sign         (N);
         when Attribute_Count             => Analyze_Count             (N);
         when Attribute_Delta             => Analyze_Delta             (N);
         when Attribute_Denorm            => Analyze_Denorm            (N);
         when Attribute_Digits            => Analyze_Digits            (N);
         when Attribute_Emax              => Analyze_Emax              (N);
         when Attribute_Epsilon           => Analyze_Epsilon           (N);
         when Attribute_Exponent          => Analyze_Exponent          (N);
         when Attribute_External_Tag      => Analyze_External_Tag      (N);
         when Attribute_First             => Analyze_First             (N);
         when Attribute_First_Bit         => Analyze_First_Bit         (N);
         when Attribute_Floor             => Analyze_Floor             (N);
         when Attribute_Fore              => Analyze_Fore              (N);
         when Attribute_Fraction          => Analyze_Fraction          (N);
         when Attribute_Identity          => Analyze_Identity          (N);
         when Attribute_Image             => Analyze_Image             (N);
         when Attribute_Input             => Analyze_Input             (N);
         when Attribute_Large             => Analyze_Large             (N);
         when Attribute_Last              => Analyze_Last              (N);
         when Attribute_Last_Bit          => Analyze_Last_Bit          (N);
         when Attribute_Leading_Part      => Analyze_Leading_Part      (N);
         when Attribute_Length            => Analyze_Length            (N);
         when Attribute_Machine           => Analyze_Machine           (N);
         when Attribute_Machine_Emax      => Analyze_Machine_Emax      (N);
         when Attribute_Machine_Emin      => Analyze_Machine_Emin      (N);
         when Attribute_Machine_Mantissa  => Analyze_Machine_Mantissa  (N);
         when Attribute_Machine_Overflows => Analyze_Machine_Overflows (N);
         when Attribute_Machine_Radix     => Analyze_Machine_Radix     (N);
         when Attribute_Machine_Rounds    => Analyze_Machine_Rounds    (N);
         when Attribute_Mantissa          => Analyze_Mantissa          (N);
         when Attribute_Max               => Analyze_Max               (N);
         when Attribute_Max_Size_In_Storage_Elements =>
            Analyze_Max_Size_In_Storage_Elements (N);
         when Attribute_Min               => Analyze_Min               (N);
         when Attribute_Model             => Analyze_Model             (N);
         when Attribute_Model_Emax        => Analyze_Model_Emax        (N);
         when Attribute_Model_Emin        => Analyze_Model_Emin        (N);
         when Attribute_Model_Epsilon     => Analyze_Model_Epsilon     (N);
         when Attribute_Model_Large       => Analyze_Model_Large       (N);
         when Attribute_Model_Mantissa    => Analyze_Model_Mantissa    (N);
         when Attribute_Model_Small       => Analyze_Model_Small       (N);
         when Attribute_Output            => Analyze_Output            (N);
         when Attribute_Pos               => Analyze_Pos               (N);
         when Attribute_Position          => Analyze_Position          (N);
         when Attribute_Pred              => Analyze_Pred              (N);
         when Attribute_Range             => Analyze_Range_Attribute   (N);
         when Attribute_Read              => Analyze_Read              (N);
         when Attribute_Remainder         => Analyze_Remainder         (N);
         when Attribute_Round             => Analyze_Round             (N);
         when Attribute_Rounding          => Analyze_Rounding          (N);
         when Attribute_Safe_Emax         => Analyze_Safe_Emax         (N);
         when Attribute_Safe_First        => Analyze_Safe_First        (N);
         when Attribute_Safe_Large        => Analyze_Safe_Large        (N);
         when Attribute_Safe_Last         => Analyze_Safe_Last         (N);
         when Attribute_Safe_Small        => Analyze_Safe_Small        (N);
         when Attribute_Scale             => Analyze_Scale             (N);
         when Attribute_Signed_Zeros      => Analyze_Signed_Zeros      (N);
         when Attribute_Size              => Analyze_Size              (N);
         when Attribute_Small             => Analyze_Small             (N);
         when Attribute_Storage_Pool      => Analyze_Storage_Pool      (N);
         when Attribute_Standard_Access   => Analyze_Standard_Access   (N);
         when Attribute_Storage_Size      => Analyze_Storage_Size      (N);
         when Attribute_Succ              => Analyze_Succ              (N);
         when Attribute_Tag               => Analyze_Tag               (N);
         when Attribute_Terminated        => Analyze_Terminated        (N);
         when Attribute_Truncation        => Analyze_Truncation        (N);
         when Attribute_Unbiased_Rounding => Analyze_Unbiased_Rounding (N);
         when Attribute_Unchecked_Access  => Analyze_Unchecked_Access  (N);
         when Attribute_Val               => Analyze_Val               (N);
         when Attribute_Valid             => Analyze_Valid             (N);
         when Attribute_Value             => Analyze_Value             (N);
         when Attribute_Version           => Analyze_Version           (N);
         when Attribute_Wide_Image        => Analyze_Wide_Image        (N);
         when Attribute_Wide_Value        => Analyze_Wide_Value        (N);
         when Attribute_Width             => Analyze_Width             (N);
         when Attribute_Write             => Analyze_Write             (N);
      end case;
   end Analyze_Attribute;

   --------------------
   -- Analyze_Access --
   --------------------

   procedure Analyze_Access (N : Node_Id) is
      P        : constant Node_Id   := Prefix (N);
      T        : constant Entity_Id := Etype (P);
      Index    : Interp_Index;
      It       : Interp;
      Acc_Type : Entity_Id;

      function Valid_Aliased_View (Obj : Node_Id) return Boolean is
         E : Entity_Id;

      begin
         if Is_Name (Obj) then
            E := Entity (Obj);
            return (Is_Aliased (E)
                 or else (Present (Renamed_Object (E)) 
                     and then Valid_Aliased_View (Renamed_Object (E)))

                 or else (Ekind (E) = E_In_Out_Parameter
                      and then Is_Tagged_Type (Etype (E)))

                 or else (Ekind (E) = E_Generic_In_Out_Parameter
                      and then Is_Tagged_Type (Etype (E)))

                 or else ((Ekind (E) = E_Task_Type
                          or else Ekind (E) = E_Protected_Type)
                      and then In_Open_Scopes (E)));

         elsif Nkind (Obj) = N_Selected_Component then
            return Is_Aliased (Entity (Selector_Name (Obj)));

         elsif Nkind (Obj) = N_Indexed_Component then
            return Is_Aliased (Etype (Prefix (Obj)));

         elsif Nkind (Obj) = N_Type_Conversion then
            return Is_Tagged_Type (Etype (Obj));

         elsif Nkind (Obj) = N_Explicit_Dereference then
            return True;  --  more precise test needed???

         else
            return False;
         end if;
      end Valid_Aliased_View;

   --  Start of processing for Analyze_Access

   begin
      --  In the case of an access to subprogram, use the name of the
      --  subprogram itself as the designated type. Type-checking in
      --  this case compares the signatures of the designated types.

      if Is_Name (P) 
        and then Is_Overloadable (Entity (P))
      then
         if not Is_Overloaded (P) then
            Acc_Type := New_Internal_Entity (E_Access_Subprogram_Type,
               Current_Scope, Sloc (N), "access");
            Set_Etype (Acc_Type,  Acc_Type);
            Set_Directly_Designated_Type (Acc_Type, Entity (P));
            Set_Etype (N, Acc_Type);

         else
            Get_First_Interp (P, Index, It);

            while Present (It.Nam) loop
               Acc_Type := New_Internal_Entity (E_Access_Subprogram_Type,
                  Current_Scope, Sloc (N), "access");
               Set_Etype (Acc_Type,  Acc_Type);
               Set_Directly_Designated_Type (Acc_Type, It.Nam);
               Add_One_Interp (N,  Acc_Type,  Acc_Type);
               Get_Next_Interp (Index, It);
            end loop;
         end if;

      --  Case of access to object

      else
         Acc_Type := New_Internal_Entity (E_Allocator_Type,
            Current_Scope, Sloc (N), "access");
         Set_Etype (Acc_Type,  Acc_Type);
         Set_Directly_Designated_Type (Acc_Type, T);
         Set_Etype (N, Acc_Type);

         if not Valid_Aliased_View (P) then
            Error_Msg_N ("prefix of 'ACCESS must be aliased view", N);
         end if;
      end if;

   end Analyze_Access;

   ---------------------
   -- Analyze_Address --
   ---------------------

   procedure Analyze_Address (N : Node_Id) is
   begin
      Set_Etype (N, RTE (RE_Address));
   end Analyze_Address;

   ----------------------
   -- Analyze_Adjacent --
   ----------------------

   procedure Analyze_Adjacent (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Adjacent;

   -----------------
   -- Analyze_Aft --
   -----------------

   procedure Analyze_Aft (N : Node_Id) is
   begin
      Check_Fixed_Type (N, Etype (Prefix (N)));
      Set_Etype (N, Universal_Integer);
      Unimplemented (N, "attribute");
   end Analyze_Aft;

   -----------------------
   -- Analyze_Alignment --
   -----------------------

   procedure Analyze_Alignment (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Alignment;

   ------------------
   -- Analyze_Base --
   ------------------

   procedure Analyze_Base (N : Node_Id) is
   begin
      Find_Type (Prefix (N));
      Set_Etype (N, Base_Type (Entity (Prefix (N))));
   end Analyze_Base;

   -----------------------
   -- Analyze_Bit_Order --
   -----------------------

   procedure Analyze_Bit_Order (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Bit_Order;

   --------------------------
   -- Analyze_Body_Version --
   --------------------------

   procedure Analyze_Body_Version (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Body_Version;

   ----------------------
   -- Analyze_Callable --
   ----------------------


   procedure Analyze_Callable (N : Node_Id) is
   begin
      --  The semantic processing for 'Callable is identical to 'Terminated

      Analyze_Terminated (N);
   end Analyze_Callable;

   --------------------
   -- Analyze_Caller --
   --------------------

   procedure Analyze_Caller (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Caller;

   ---------------------
   -- Analyze_Ceiling --
   ---------------------

   procedure Analyze_Ceiling (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Ceiling;

   -------------------
   -- Analyze_Class --
   -------------------

   procedure Analyze_Class (N : Node_Id) is
   begin
      Find_Type (N);

      if Present (Expression (N)) then

         --  in fact, this is a conversion not an attribute : T'Class (X)

         Rewrite_Substitute_Tree (N, Make_Type_Conversion (Sloc (N),
           Subtype_Mark => New_Occurrence_Of (Etype (N), Sloc (N)),
           Expression => New_Copy (Expression (N))));

         Analyze (N);
      end if;

   end Analyze_Class;

   ----------------------------
   -- Analyze_Component_Size --
   ----------------------------

   procedure Analyze_Component_Size (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Component_Size;

   ---------------------
   -- Analyze_Compose --
   ---------------------

   procedure Analyze_Compose (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Compose;

   -------------------------
   -- Analyze_Constrained --
   -------------------------

   procedure Analyze_Constrained (N : Node_Id) is
   begin
      if Ada_9X then
         Error_Msg_N ("obsolescent feature: 'Constrained attribute?", N);
      end if;

      Set_Etype (N, Standard_Boolean);
   end Analyze_Constrained;

   -----------------------
   -- Analyze_Copy_Sign --
   -----------------------

   procedure Analyze_Copy_Sign (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Copy_Sign;

   -------------------
   -- Analyze_Count --
   -------------------

   procedure Analyze_Count (N : Node_Id) is
   begin
      Set_Etype (N, Universal_Integer);
      Unimplemented_Attribute (N);
   end Analyze_Count;

   -------------------
   -- Analyze_Delta --
   -------------------

   procedure Analyze_Delta (N : Node_Id) is
   begin
      Check_Fixed_Type (N, Etype (Prefix (N)));
      Set_Etype (N, Universal_Real);
      Unimplemented_Attribute (N);
   end Analyze_Delta;

   --------------------
   -- Analyze_Denorm --
   --------------------

   procedure Analyze_Denorm (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Denorm;

   --------------------
   -- Analyze_Digits --
   --------------------

   procedure Analyze_Digits (N : Node_Id) is
   begin
      Check_Float_Type (N, Etype (Prefix (N)));
      Set_Etype (N, Universal_Integer);
      Unimplemented_Attribute (N);
   end Analyze_Digits;

   ------------------
   -- Analyze_Emax --
   ------------------

   procedure Analyze_Emax (N : Node_Id) is
   begin
      Check_Float_Type (N, Etype (Prefix (N)));
      Set_Etype (N, Universal_Integer);
      Unimplemented_Attribute (N);
   end Analyze_Emax;

   ---------------------
   -- Analyze_Epsilon --
   ---------------------

   procedure Analyze_Epsilon (N : Node_Id) is
   begin
      Check_Float_Type (N, Etype (Prefix (N)));
      Set_Etype (N, Universal_Real);
      Unimplemented_Attribute (N);
   end Analyze_Epsilon;

   ----------------------
   -- Analyze_Exponent --
   ----------------------

   procedure Analyze_Exponent (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Exponent;

   --------------------------
   -- Analyze_External_Tag --
   --------------------------

   procedure Analyze_External_Tag (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_External_Tag;

   -------------------
   -- Analyze_First --
   -------------------

   procedure Analyze_First (N : Node_Id) is
   begin
      Check_Array_Or_Scalar_Type (N, Etype (Prefix (N)));
   end Analyze_First;

   -----------------------
   -- Analyze_First_Bit --
   -----------------------

   procedure Analyze_First_Bit (N : Node_Id) is
   begin
      Set_Etype (N, Universal_Integer);
      Unimplemented_Attribute (N);
   end Analyze_First_Bit;

   -------------------
   -- Analyze_Floor --
   -------------------

   procedure Analyze_Floor (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Floor;

   ------------------
   -- Analyze_Fore --
   ------------------

   procedure Analyze_Fore (N : Node_Id) is
   begin
      Check_Fixed_Type (N, Etype (Prefix (N)));
      Set_Etype (N, Universal_Integer);
      Unimplemented_Attribute (N);
   end Analyze_Fore;

   ----------------------
   -- Analyze_Fraction --
   ----------------------

   procedure Analyze_Fraction (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Fraction;

   ----------------------
   -- Analyze_Identity --
   ----------------------

   procedure Analyze_Identity (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Identity;

   -------------------
   -- Analyze_Image --
   -------------------

   procedure Analyze_Image (N : Node_Id) is
   begin
      Set_Etype (N, Standard_String);

      if Is_Real_Type (Etype (Prefix (N))) then
         if Ada_83 then
            Error_Msg_N
              ("Image attribute not allowed for real types in Ada 83", N);
         end if;
      else
         Check_Discrete_Attribute (N);
      end if;
   end Analyze_Image;

   -------------------
   -- Analyze_Input --
   -------------------

   procedure Analyze_Input (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Input;

   -------------------
   -- Analyze_Large --
   -------------------

   procedure Analyze_Large (N : Node_Id) is
   begin
      Check_Real_Type (N, Etype (Prefix (N)));
      Set_Etype (N, Universal_Real);
      Unimplemented_Attribute (N);
   end Analyze_Large;

   ------------------
   -- Analyze_Last --
   ------------------

   procedure Analyze_Last (N : Node_Id) is
   begin
      Check_Array_Or_Scalar_Type (N, Etype (Prefix (N)));
   end Analyze_Last;

   ----------------------
   -- Analyze_Last_Bit --
   ----------------------

   procedure Analyze_Last_Bit (N : Node_Id) is
   begin
      Set_Etype (N, Universal_Integer);
      Unimplemented_Attribute (N);
   end Analyze_Last_Bit;

   --------------------------
   -- Analyze_Leading_Part --
   --------------------------

   procedure Analyze_Leading_Part (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Leading_Part;

   --------------------
   -- Analyze_Length --
   --------------------

   procedure Analyze_Length (N : Node_Id) is
      ok : boolean := Check_Array_Type (N, Etype (Prefix (N)));
   begin
      Set_Etype (N, Universal_Integer);
   end Analyze_Length;

   ---------------------
   -- Analyze_Machine --
   ---------------------

   procedure Analyze_Machine (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Machine;

   --------------------------
   -- Analyze_Machine_Emax --
   --------------------------

   procedure Analyze_Machine_Emax (N : Node_Id) is
   begin
      Check_Float_Type (N, Etype (Prefix (N)));
      Set_Etype (N, Universal_Integer);
      Unimplemented_Attribute (N);
   end Analyze_Machine_Emax;

   --------------------------
   -- Analyze_Machine_Emin --
   --------------------------

   procedure Analyze_Machine_Emin (N : Node_Id) is
   begin
      Check_Float_Type (N, Etype (Prefix (N)));
      Set_Etype (N, Universal_Integer);
      Unimplemented_Attribute (N);
   end Analyze_Machine_Emin;

   ------------------------------
   -- Analyze_Machine_Mantissa --
   ------------------------------

   procedure Analyze_Machine_Mantissa (N : Node_Id) is
   begin
      Check_Float_Type (N, Etype (Prefix (N)));
      Set_Etype (N, Universal_Integer);
      Unimplemented_Attribute (N);
   end Analyze_Machine_Mantissa;

   -------------------------------
   -- Analyze_Machine_Overflows --
   -------------------------------

   procedure Analyze_Machine_Overflows (N : Node_Id) is
   begin
      Check_Real_Type (N, Etype (Prefix (N)));
      Set_Etype (N, Standard_Boolean);
      Unimplemented_Attribute (N);
   end Analyze_Machine_Overflows;

   ---------------------------
   -- Analyze_Machine_Radix --
   ---------------------------

   procedure Analyze_Machine_Radix (N : Node_Id) is
   begin
      Check_Float_Type (N, Etype (Prefix (N)));
      Set_Etype (N, Universal_Integer);
      Unimplemented_Attribute (N);
   end Analyze_Machine_Radix;

   ----------------------------
   -- Analyze_Machine_Rounds --
   ----------------------------

   procedure Analyze_Machine_Rounds (N : Node_Id) is
   begin
      Check_Real_Type (N, Etype (Prefix (N)));
      Set_Etype (N, Standard_Boolean);
      Unimplemented_Attribute (N);
   end Analyze_Machine_Rounds;

   ----------------------
   -- Analyze_Mantissa --
   ----------------------

   procedure Analyze_Mantissa (N : Node_Id) is
   begin
      Check_Real_Type (N, Etype (Prefix (N)));
      Set_Etype (N, Universal_Integer);
      Unimplemented_Attribute (N);
   end Analyze_Mantissa;

   -----------------
   -- Analyze_Max --
   -----------------

   procedure Analyze_Max (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Max;

   ------------------------------------------
   -- Analyze_Max_Size_In_Storage_Elements --
   ------------------------------------------

   procedure Analyze_Max_Size_In_Storage_Elements (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Max_Size_In_Storage_Elements;

   -----------------
   -- Analyze_Min --
   -----------------

   procedure Analyze_Min (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Min;

   -------------------
   -- Analyze_Model --
   -------------------

   procedure Analyze_Model (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Model;

   ------------------------
   -- Analyze_Model_Emax --
   ------------------------

   procedure Analyze_Model_Emax (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Model_Emax;

   ------------------------
   -- Analyze_Model_Emin --
   ------------------------

   procedure Analyze_Model_Emin (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Model_Emin;

   ---------------------------
   -- Analyze_Model_Epsilon --
   ---------------------------

   procedure Analyze_Model_Epsilon (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Model_Epsilon;

   -------------------------
   -- Analyze_Model_Large --
   -------------------------

   procedure Analyze_Model_Large (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Model_Large;

   ----------------------------
   -- Analyze_Model_Mantissa --
   ----------------------------

   procedure Analyze_Model_Mantissa (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Model_Mantissa;

   -------------------------
   -- Analyze_Model_Small --
   -------------------------

   procedure Analyze_Model_Small (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Model_Small;

   --------------------
   -- Analyze_Output --
   --------------------

   procedure Analyze_Output (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Output;

   -----------------
   -- Analyze_Pos --
   -----------------

   procedure Analyze_Pos (N : Node_Id) is
   begin
      Check_Discrete_Attribute (N);
      Set_Etype (N, Universal_Integer);
   end Analyze_Pos;

   ----------------------
   -- Analyze_Position --
   ----------------------

   procedure Analyze_Position (N : Node_Id) is
   begin
      Set_Etype (N, Universal_Integer);
   end Analyze_Position;

   ------------------
   -- Analyze_Pred --
   ------------------

   procedure Analyze_Pred (N : Node_Id) is
   begin
      Check_Discrete_Attribute (N);
      Set_Etype (N, Etype (Prefix (N)));
   end Analyze_Pred;

   -----------------------------
   -- Analyze_Range_Attribute --
   -----------------------------

   procedure Analyze_Range_Attribute (N : Node_Id) is
   begin
      Check_Array_Or_Scalar_Type (N, Etype (Prefix (N)));
   end Analyze_Range_Attribute;

   ------------------
   -- Analyze_Read --
   ------------------

   procedure Analyze_Read (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Read;

   -----------------------
   -- Analyze_Remainder --
   -----------------------

   procedure Analyze_Remainder (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Remainder;

   -------------------
   -- Analyze_Round --
   -------------------

   procedure Analyze_Round (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Round;

   ----------------------
   -- Analyze_Rounding --
   ----------------------

   procedure Analyze_Rounding (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Rounding;

   -----------------------
   -- Analyze_Safe_Emax --
   -----------------------

   procedure Analyze_Safe_Emax (N : Node_Id) is
   begin
      Check_Float_Type (N, Etype (Prefix (N)));
      Set_Etype (N, Universal_Integer);
      Unimplemented_Attribute (N);
   end Analyze_Safe_Emax;

   ------------------------
   -- Analyze_Safe_First --
   ------------------------

   procedure Analyze_Safe_First (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Safe_First;

   ------------------------
   -- Analyze_Safe_Large --
   ------------------------

   procedure Analyze_Safe_Large (N : Node_Id) is
   begin
      Check_Real_Type (N, Etype (Prefix (N)));
      Set_Etype (N, Universal_Real);
      Unimplemented_Attribute (N);
   end Analyze_Safe_Large;

   -----------------------
   -- Analyze_Safe_Last --
   -----------------------

   procedure Analyze_Safe_Last (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Safe_Last;

   ------------------------
   -- Analyze_Safe_Small --
   ------------------------

   procedure Analyze_Safe_Small (N : Node_Id) is
   begin
      Check_Real_Type (N, Etype (Prefix (N)));
      Set_Etype (N, Universal_Real);
      Unimplemented_Attribute (N);
   end Analyze_Safe_Small;

   -------------------
   -- Analyze_Scale --
   -------------------

   procedure Analyze_Scale (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Scale;

   --------------------------
   -- Analyze_Signed_Zeros --
   --------------------------

   procedure Analyze_Signed_Zeros (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Signed_Zeros;

   ------------------
   -- Analyze_Size --
   ------------------

   procedure Analyze_Size (N : Node_Id) is
   begin
      Set_Etype (N, Universal_Integer);
   end Analyze_Size;

   -------------------
   -- Analyze_Small --
   -------------------

   procedure Analyze_Small (N : Node_Id) is
   begin
      Check_Real_Type (N, Etype (Prefix (N)));
      Set_Etype (N, Universal_Real);
      Unimplemented_Attribute (N);
   end Analyze_Small;

   -----------------------------
   -- Analyze_Standard_Access --
   -----------------------------

   procedure Analyze_Standard_Access (N : Node_Id) is
   begin
      Find_Type (Prefix (N));

      if Base_Type (Entity (Prefix (N))) /= Standard_String then
         Error_Msg_N ("only permitted prefix is `String`", Prefix (N));
         Set_Etype (N, Any_Type);

      else
         Set_Etype (N, Standard_A_String);
      end if;
   end Analyze_Standard_Access;

   --------------------------
   -- Analyze_Storage_Pool --
   --------------------------

   procedure Analyze_Storage_Pool (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Storage_Pool;

   --------------------------
   -- Analyze_Storage_Size --
   --------------------------

   procedure Analyze_Storage_Size (N : Node_Id) is
   begin
      Set_Etype (N, Universal_Integer);
      Unimplemented_Attribute (N);
   end Analyze_Storage_Size;

   ------------------
   -- Analyze_Succ --
   ------------------

   procedure Analyze_Succ (N : Node_Id) is
   begin
      Check_Discrete_Attribute (N);
      Set_Etype (N, Etype (Prefix (N)));
   end Analyze_Succ;

   -----------------
   -- Analyze_Tag --
   -----------------

   procedure Analyze_Tag (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
      Set_Etype (N, RTE (RE_Tag));
   end Analyze_Tag;

   ------------------------
   -- Analyze_Terminated --
   ------------------------

   procedure Analyze_Terminated (N : Node_Id) is
      P : constant Node_Id := Prefix (N);

   begin
      Set_Etype (N, Standard_Boolean);
      Analyze (P);

      if Is_Task_Type (Etype (P))
        or else (Is_Access_Type (Etype (P))
           and then Is_Task_Type (Designated_Type (Etype (P))))
      then
         Resolve_Subexpr (P, Etype (P));
      else
         Error_Msg_N ("prefix of attribute must be a task", N);
      end if;
   end Analyze_Terminated;

   ------------------------
   -- Analyze_Truncation --
   ------------------------

   procedure Analyze_Truncation (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Truncation;

   -------------------------------
   -- Analyze_Unbiased_Rounding --
   -------------------------------

   procedure Analyze_Unbiased_Rounding (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Unbiased_Rounding;

   ------------------------------
   -- Analyze_Unchecked_Access --
   ------------------------------

   procedure Analyze_Unchecked_Access (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Unchecked_Access;

   -----------------
   -- Analyze_Val --
   -----------------

   procedure Analyze_Val (N : Node_Id) is
      T : constant Entity_Id := Etype (Prefix (N));
      E : constant Node_Id   := Expression (N);

   begin
      Check_Discrete_Type (N, T);

      if No (E) or else not Is_Integer_Type (Etype (E)) then
         Error_Msg_N ("expect integer value as argument of attribute", N);
      else
         Resolve_Subexpr (E, Etype (E));
      end if;

      Set_Etype (N, T);
   end Analyze_Val;

   -------------------
   -- Analyze_Valid --
   -------------------

   procedure Analyze_Valid (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Valid;

   -------------------
   -- Analyze_Value --
   -------------------

   procedure Analyze_Value (N : Node_Id) is
      T : constant Entity_Id :=  Etype (Prefix (N));
      E : constant Node_Id   := Expression (N);

   begin
      Check_Discrete_Type (N, T);
      if No (E) then
         Error_Msg_N ("expect string value as argument of attribute", N);
      else
         Resolve_Subexpr (E, Standard_String);
      end if;

      Set_Etype (N, T);
   end Analyze_Value;

   ---------------------
   -- Analyze_Version --
   ---------------------

   procedure Analyze_Version (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Version;

   ------------------------
   -- Analyze_Wide_Image --
   ------------------------

   procedure Analyze_Wide_Image (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Wide_Image;

   ------------------------
   -- Analyze_Wide_Value --
   ------------------------

   procedure Analyze_Wide_Value (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Wide_Value;

   -------------------
   -- Analyze_Width --
   -------------------

   procedure Analyze_Width (N : Node_Id) is
      T : constant Entity_Id := Etype (Prefix (N));

   begin
      Check_Discrete_Type (N, T);
      Set_Etype (N, Universal_Integer);
   end Analyze_Width;

   -------------------
   -- Analyze_Write --
   -------------------

   procedure Analyze_Write (N : Node_Id) is
   begin
      Unimplemented_Attribute (N);
   end Analyze_Write;

end Sem_Attr;
