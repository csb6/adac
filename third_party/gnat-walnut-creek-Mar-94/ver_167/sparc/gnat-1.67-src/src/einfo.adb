------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                E I N F O                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.146 $                            --
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
with Comperr; use Comperr;
with Debug;   use Debug;
with Namet;   use Namet;
with Sinfo;   use Sinfo;
with Snames;  use Snames;
with Output;  use Output;

package body Einfo is

   use Atree.Unchecked_Access;
   --  This is one of the packages that is allowed direct untyped access to
   --  the fields in a node, since it provides the next level abstraction
   --  which incorporates appropriate checks.

   ----------------------------------------------
   -- Usage of Fields in Defining Entity Nodes --
   ----------------------------------------------

   --  Chars                         Name1 

   --  Discriminant_Constraint       Elist2

   --  Scope                         Node3

   --  Homonym                       Node4

   --  Etype                         Node5 

   --  Next_Entity                   Node6

   --  Alias                         Node7
   --  Corresponding_Task_Type       Node7
   --  Equivalent_Type               Node7
   --  Lit_Name_Table                Node7
   --  Primitive_Operations          Elist7
   --  Renamed_Object                Node7
   --  Task_Value_Type               Node7

   --  Init_Proc                     Node8
   --  Original_Record_Component     Node8

   --  Discriminal                   Node9
   --  First_Entity                  Node9
   --  First_Index                   Node9
   --  First_Literal                 Node9
   --  Modulus                       Uint9
   --  Master_Id                     Node9

   --  Component_Type                Node10
   --  Default_Value                 Node10
   --  Discriminant_Checking_Func    Node10
   --  Discriminant_Default_Value    Node10
   --  Directly_Designated_Type      Node10
   --  Last_Entity                   Node10
   --  Scalar_Range                  Node10

   --  Internal_Access_Disp_Table    Node11
   --  Enumeration_Pos               Uint11
   --  Entry_Index                   Uint11
   --  First_Private_Entity          Node11
   --  Full_Declaration              Node11
   --  Parent_Subtype                Node11
   --  Slice_Range                   Node11
   --  String_Literal_Length         Uint11
   --  Table_High_Bound              Node11

   --  Esize                         Uint12
   --  Enumeration_Rep               Uint12
   --  Interface_Name                Node12

   ---------------------------------------------
   -- Usage of Flags in Defining Entity Nodes --
   ---------------------------------------------

   --  (unused)                      Flag1

   --  Is_Internal                   Flag2

   --  Is_Constrained                Flag3
   --  Is_Pure                       Flag3

   --  In_Private_Part               Flag4
   --  Is_Frozen                     Flag4

   --  Has_Discriminants             Flag5
   --  Has_Subprogram_Body           Flag5
   --  Has_Exit                      Flag5
   --  Is_Package_Body               Flag5
   --  Reachable                     Flag5
   --  Needs_Discr_Check             Flag5

   --  Is_Dispatching_Operation      Flag6
   --  Is_Generic_Type               Flag6

   --  Is_Directly_Visible           Flag7

   --  In_Use                        Flag8
   --  Is_Tagged_Type                Flag8
   --  Needs_No_Actuals              Flag8

   --  Is_Use_Visible                Flag9

   --  Is_Public                     Flag10

   --  Is_Inlined                    Flag11
   --  Is_Packed                     Flag11

   --  Has_Homonym                   Flag12

   --  Is_Private                    Flag13

   --  Is_Private_Descendant         Flag14
   --  Is_Private_Type               Flag14

   --  Is_Aliased                    Flag15

   --  Is_Volatile                   Flag16

   --  (unused)                      Flag17

   --  Is_Delayed                    Flag18

   --  Is_Abstract                   Flag19

   --  Is_Task_Record_Type           Flag20

   --  Has_Master_Entity             Flag21

   --  (unused)                      Flag22

   --  Has_Storage_Size_Clause       Flag23

   --  Is_Imported                   Flag24

   --  Is_Limited_Type               Flag25

   --  Has_Completion                Flag26

   --  Is_Intrinsic                  Flag27

   --  Has_Address_Clause            Flag28

   --  Has_Size_Clause               Flag29

   --  Has_Tasks                     Flag30

   --  Suppress_Access_Checks        Flag31

   --  Suppress_Accessibility_Checks Flag32

   --  Suppress_Discriminant_Checks  Flag33

   --  Suppress_Division_Checks      Flag34

   --  Suppress_Elaboration_Checks   Flag35

   --  Suppress_Index_Checks         Flag36

   --  Suppress_Length_Checks        Flag37

   --  Suppress_Overflow_Checks      Flag38

   --  Suppress_Range_Checks         Flag39

   --  Suppress_Storage_Checks       Flag40

   --  Suppress_Tag_Checks           Flag41

   --------------------------
   -- Debugging Procedures --
   --------------------------

   --  These procedures are called only if assertions are enabled

   procedure Check_Defining_Occurrence (Id : Entity_Id);
   --  Debug procedure used to check that a given node denotes the defining
   --  occurrence of an entity.

   procedure Check_Type (Id : Entity_Id);
   --  Debug procedure used to check that a given node denotes the defining
   --  occurrence of an entity for a type or subtype.

   procedure Check_Discriminated_Type (Id : Entity_Id);
   --  Debug procedure used to check that an entity denotes a type that
   --  can have discriminants (composite type that is not an array type,
   --  or incomplete type).

   procedure Check_Entity_Kind (Id : Entity_Id; Kind : Entity_Kind);
   --  Debug procedure used to check that a given node denotes a defining
   --  occurrence of a particular Entity_Kind

   procedure Check_Record_Or_Incomplete_Kind (Id : Entity_Id);
   --  Debug procedure used to check that a given node denotes a defining
   --  occurrence which is a record type or an incomplete type.

   -------------------------------
   -- Check_Defining_Occurrence --
   -------------------------------

   procedure Check_Defining_Occurrence (Id : Entity_Id) is
   begin
      if Nkind (Id) not in N_Entity then
         Compiler_Error;
         Write_Eol;
         Write_String ("Not a defining occurence:  Node_Id = ");
         Write_Int (Int (Id));
         Write_Eol;
         Compiler_Abort;
      end if;
   end Check_Defining_Occurrence;

   -----------------------
   -- Check_Entity_Kind --
   -----------------------

   procedure Check_Entity_Kind (Id : Entity_Id; Kind : Entity_Kind) is
   begin
      Check_Defining_Occurrence (Id);

      if Ekind (Id) /= Kind then
         Compiler_Error;
         Write_Eol;
         Write_String ("Occurence of ");
         Write_String (Entity_Kind'Image (Ekind (Id)));
         Write_String (" instead of ");
         Write_String (Entity_Kind'Image (Kind));
         Write_Eol;
         Compiler_Abort;
      end if;
   end Check_Entity_Kind;

   ----------------
   -- Check_Type --
   ----------------

   procedure Check_Type (Id : Entity_Id) is
   begin
      Check_Defining_Occurrence (Id);

      if Ekind (Id) not in Type_Kind then
         Compiler_Error;
         Write_Eol;
         Write_String ("not a type ");
         Write_String (Entity_Kind'Image (Ekind (Id)));
         Write_Eol;
         Compiler_Abort;
      end if;
   end Check_Type;

   ------------------------------
   -- Check_Discriminated_Type --
   ------------------------------

   procedure Check_Discriminated_Type (Id : Entity_Id) is
   begin
      Check_Defining_Occurrence (Id);
      if not Is_Composite_Type (Id) or else Is_Array_Type (Id) then
         Compiler_Error;
         Write_Eol;
         Write_String ("not a discriminated type ");
         Write_String (Entity_Kind'Image (Ekind (Id)));
         Write_Eol;
         Compiler_Abort;
      end if;
   end Check_Discriminated_Type;

   -------------------------------------
   -- Check_Record_Or_Incomplete_Kind --
   -------------------------------------

   procedure Check_Record_Or_Incomplete_Kind (Id : Entity_Id) is
   begin
      Check_Defining_Occurrence (Id);

      if not (Is_Record_Type (Id) or else Is_Incomplete_Type (Id)) then
         Compiler_Error;
         Write_Eol;
         Write_String ("Not a record or incomplete occurence:  ");
         Write_String (Entity_Kind'Image (Ekind (Id)));
         Write_Eol;
         Compiler_Abort;
      end if;
   end Check_Record_Or_Incomplete_Kind;

   ---------------------------
   -- Check_Overloaded_Kind --
   ---------------------------

   procedure Check_Overloaded_Kind (Id : Entity_Id) is
   begin
      Check_Defining_Occurrence (Id);

      if not Is_Overloadable (Id)
        and then Ekind (Id) /= E_Subprogram_Type
      then
         Compiler_Error;
         Write_Eol;
         Write_String ("Not a subprogram or enumeration literal :  ");
         Write_String (Entity_Kind'Image (Ekind (Id)));
         Write_Eol;
         Compiler_Abort;
      end if;
   end Check_Overloaded_Kind;

   ----------------------
   -- Access Functions --
   ----------------------

      -----------------------------
      -- Corresponding_Task_Type --
      -----------------------------

      function Corresponding_Task_Type (Id : Entity_Id) return Entity_Id is
      begin
         pragma Assert (Is_Record_Type (Id) and then not Is_Tagged_Type (Id),
                        Compiler_Abort (Id));
         return Node7 (Id);
      end Corresponding_Task_Type;

      -------------
      -- Homonym --
      -------------

      function Homonym (Id : Entity_Id) return Entity_Id is
      begin
         pragma Debug (Check_Defining_Occurrence (Id));
         return Node4 (Id);
      end Homonym;

      --------------------
      -- Has_Completion --
      --------------------

      function Has_Completion (Id : Entity_Id) return Boolean is
      begin
         pragma Debug (Check_Defining_Occurrence (Id));
         return Flag26 (Id);
      end Has_Completion;

      ---------------
      -- Has_Tasks --
      ---------------

      function Has_Tasks (Id : Entity_Id) return Boolean is
      begin
         pragma Debug (Check_Defining_Occurrence (Id));
         return Flag30 (Id);
      end Has_Tasks;

      -----------------------
      -- Has_Master_Entity --
      -----------------------

      function Has_Master_Entity (Id : Entity_Id) return Boolean is
      begin
         pragma Debug (Check_Defining_Occurrence (Id));
         return Flag21 (Id);
      end Has_Master_Entity;

      ------------------------
      -- Has_Address_Clause --
      ------------------------

      function Has_Address_Clause (Id : Entity_Id) return Boolean is
      begin
         pragma Debug (Check_Defining_Occurrence (Id));
         return Flag28 (Id);
      end Has_Address_Clause;

      -----------------
      -- Has_Homonym --
      -----------------

      function Has_Homonym (Id : Entity_Id) return Boolean is
      begin
         pragma Debug (Check_Defining_Occurrence (Id));
         return Flag12 (Id);
      end Has_Homonym;

      ---------------------
      -- Has_Size_Clause --
      ---------------------

      function Has_Size_Clause (Id : Entity_Id) return Boolean is
      begin
         pragma Debug (Check_Defining_Occurrence (Id));
         return Flag29 (Id);
      end Has_Size_Clause;

      ----------------------------
      -- Has_Storage_Size_Clause --
      ----------------------------

      function Has_Storage_Size_Clause (Id : Entity_Id) return Boolean is
      begin
         pragma Debug (Check_Defining_Occurrence (Id));
         return Flag23 (Id);
      end Has_Storage_Size_Clause;

      -----------------
      -- Next_Entity --
      -----------------

      function Next_Entity (Id : Entity_Id) return Entity_Id is
      begin
         pragma Debug (Check_Defining_Occurrence (Id));
         return Node6 (Id);
      end Next_Entity;

      ------------------
      -- First_Entity --
      ------------------

      function First_Entity (Scope_Id : Entity_Id) return Entity_Id is
      begin
         pragma Debug (Check_Defining_Occurrence (Scope_Id));
         return Node9 (Scope_Id);
      end First_Entity;

      -----------------
      -- Last_Entity --
      -----------------

      function Last_Entity (Scope_Id : Entity_Id) return Entity_Id is
      begin
         pragma Debug (Check_Defining_Occurrence (Scope_Id));
         return Node10 (Scope_Id);
      end Last_Entity;

      ---------------
      -- Master_Id --
      ---------------

      function Master_Id (Access_Id : Entity_Id) return Entity_Id is
      begin
         pragma Debug (Check_Defining_Occurrence (Access_Id));
         return Node9 (Access_Id);
      end Master_Id;

      -----------
      -- Scope --
      -----------

      function Scope (Id : Entity_Id) return Entity_Id is
      begin
         pragma Debug (Check_Defining_Occurrence (Id));
         return Node3 (Id);
      end Scope;

      -----------
      -- Esize --
      -----------

      function Esize (Type_Id : Entity_Id) return Uint is
      begin
         pragma Debug (Check_Defining_Occurrence (Type_Id));
         return Uint12 (Type_Id);
      end Esize;

      ---------------
      -- Root_Type --
      ---------------

      function Root_Type (Type_Id : Entity_Id) return Entity_Id is
         T : Entity_Id;

      begin
         pragma Debug (Check_Defining_Occurrence (Type_Id));

         T := Type_Id;

         while T /= Etype (T) loop
            T := Etype (T);
         end loop;

         return T;
      end Root_Type;

      ----------------
      -- Is_Delayed --
      ----------------

      function Is_Delayed (E : Entity_Id) return Boolean is
      begin
         pragma Debug (Check_Defining_Occurrence (E));
         return Flag18 (E);
      end Is_Delayed;

      ---------------------
      -- Is_Generic_Type --
      ---------------------

      function Is_Generic_Type (Type_Id : Entity_Id) return Boolean is
      begin
         pragma Debug (Check_Defining_Occurrence (Type_Id));
         return Flag6 (Type_Id);
      end Is_Generic_Type;

      ---------------
      -- Is_Frozen --
      ---------------

      function Is_Frozen (Type_Id : Entity_Id) return Boolean is
      begin
         pragma Debug (Check_Defining_Occurrence (Type_Id));
         return Flag4 (Type_Id);
      end Is_Frozen;

      -------------------
      -- First_Literal --
      -------------------

      function First_Literal (Type_Id : Entity_Id) return Entity_Id is
      begin
         pragma Debug (Check_Defining_Occurrence (Type_Id));
         return Node9 (Type_Id);
      end First_Literal;

      ------------------
      -- Next_literal --
      ------------------

      function Next_Literal (Id : Entity_Id) return Entity_Id is
      begin
         pragma Debug (Check_Defining_Occurrence (Id));
         return Next (Id);
      end Next_Literal;

      --------------------
      -- Lit_Name_Table --
      --------------------

      function Lit_Name_Table (Id : Entity_Id) return Entity_Id is
      begin
         pragma Debug (Check_Defining_Occurrence (Id));
         return Node7 (Id);
      end Lit_Name_Table;

      -------------------------
      -- Has_Subprogram_Body --
      -------------------------

      function Has_Subprogram_Body (Subprogram_Id : Entity_Id)
                                                      return Boolean is
      begin
         return Flag5 (Subprogram_Id);
      end Has_Subprogram_Body;

      ------------------
      -- Scalar_Range --
      ------------------

      function Scalar_Range (Type_Id : Entity_Id) return Node_Id is
      begin
         pragma Debug (Check_Defining_Occurrence (Type_Id));
         return Node10 (Type_Id);
      end Scalar_Range;

      -------------
      -- Modulus --
      -------------

      function Modulus (Type_Id : Entity_Id) return Uint is
      begin
         pragma Debug (Check_Defining_Occurrence (Type_Id));
         return Uint9 (Type_Id);
      end Modulus;

      --------------------
      -- Type_Low_Bound --
      --------------------

      function Type_Low_Bound (Type_Id : Entity_Id) return Node_Id is
      begin
         return Low_Bound (Scalar_Range (Type_Id));
      end Type_Low_Bound;

      ---------------------
      -- Type_High_Bound --
      ---------------------

      function Type_High_Bound (Type_Id : Entity_Id) return Node_Id is
      begin
         return High_Bound (Scalar_Range (Type_Id));
      end Type_High_Bound;

      -----------------------
      -- Number_Dimensions --
      -----------------------

      function Number_Dimensions (Array_Id : Entity_Id) return Pos is
         N : Int;
         T : Node_Id;

      begin
         pragma Debug (Check_Defining_Occurrence (Array_Id));

         N := 0;
         T := First_Index (Array_Id);

         while Present (T) loop
            N := N + 1;
            T := Next (T);
         end loop;

         return N;
      end Number_Dimensions;

      --------------------
      -- Component_type --
      --------------------

      function Component_Type (Array_Id : Entity_Id) return Entity_Id is
      begin
         pragma Debug (Check_Defining_Occurrence (Array_Id));
         return Node10 (Array_Id);
      end Component_Type;

      -----------------
      -- First_Index --
      -----------------

      function First_Index (Array_Id : Entity_Id) return Node_Id is
      begin
         pragma Debug (Check_Defining_Occurrence (Array_Id));
         return Node9 (Array_Id);
      end First_Index;

      ----------------
      -- Next_Index --
      ----------------

      function Next_Index (Index_Id : Entity_Id) return Node_Id is
      begin
         return Next (Index_Id);
      end Next_Index;

      --------------------
      -- Is_Constrained --
      --------------------

      function Is_Constrained (Type_Id : Entity_Id) return Boolean is
      begin
         return Flag3 (Type_Id);
      end Is_Constrained;

      ------------------------
      -- First_Discriminant --
      ------------------------

      function First_Discriminant (Type_Id : Entity_Id) return Entity_Id is
         Id : Entity_Id;

      begin
         pragma Assert (Has_Discriminants (Type_Id), Compiler_Abort (Type_Id));

         Id := First_Entity (Type_Id);

         if Chars (Id) = Name_uTag then
            pragma Assert (Is_Tagged_Type (Type_Id), Compiler_Abort (Type_Id));
            return Next_Entity (Id);
         else
            return Id;
         end if;
      end First_Discriminant;

      -----------------------
      -- Next_Discriminant --
      -----------------------

      function Next_Discriminant (Id : Entity_Id) return Entity_Id is
         D : constant Entity_Id := Next_Entity (Id);

      begin
         pragma Debug (Check_Entity_Kind (Id, E_Discriminant));

         if Present (D) and then Ekind (D) = E_Discriminant then
            return D;
         else
            return Empty;
         end if;
      end Next_Discriminant;

      -----------------------------
      -- Discriminant_Constraint --
      -----------------------------

      function Discriminant_Constraint (Type_Id : Entity_Id) return Elist_Id is
      begin
         pragma Debug (Check_Discriminated_Type (Type_Id));
         return Elist2 (Type_Id);
      end Discriminant_Constraint;

      ---------------
      -- Init_Proc --
      ---------------

      function Init_Proc (Type_Id : Entity_Id) return Entity_Id is
      begin
         pragma Debug (Check_Type (Type_Id));
         return Node8 (Type_Id);
      end Init_Proc;

      --------------------
      -- Base_Init_Proc --
      --------------------

      function Base_Init_Proc (Type_Id : Entity_Id) return Entity_Id is
      begin
         pragma Debug (Check_Type (Type_Id));

         if Is_Task_Type (Type_Id) then
            return Init_Proc (Task_Value_Type (Base_Type (Type_Id)));
         else
            return Init_Proc (Base_Type (Type_Id));
         end if;
      end Base_Init_Proc;

      --------------------------
      -- Primitive_Operations --
      --------------------------

      function Primitive_Operations (Id : Entity_Id) return Elist_Id is
      begin
         pragma Assert (Is_Tagged_Type (Id), Compiler_Abort (Id));
         return Elist7 (Id);
      end Primitive_Operations;

      --------------------------------
      -- Internal_Access_Disp_Table --
      --------------------------------

      function Internal_Access_Disp_Table (Component_Id : Entity_Id) 
        return Entity_Id is
      begin
         pragma Debug (Check_Entity_Kind (Component_Id, E_Component));
         return Node11 (Component_Id);
      end Internal_Access_Disp_Table;

      -----------------------
      -- Access_Disp_Table --
      -----------------------

      function Access_Disp_Table (Rec_Id : Entity_Id) return Entity_Id is
      begin
         pragma Assert (Is_Tagged_Type (Rec_Id), Compiler_Abort (Rec_Id));
         return Internal_Access_Disp_Table (Tag_Component (Rec_Id));
      end Access_Disp_Table;

      ------------------------------
      -- Is_Dispatching_Operation --
      ------------------------------

      function Is_Dispatching_Operation
        (Overloaded_Id : Entity_Id) return Boolean is
      begin
         pragma Debug (Check_Overloaded_Kind (Overloaded_Id));
         return Flag6 (Overloaded_Id);
      end Is_Dispatching_Operation;

      -------------------
      -- Tag_Component --
      -------------------

      function Tag_Component (Rec_Id : Entity_Id) return Entity_Id is
         E : Entity_Id;

      begin
         pragma Assert (Is_Tagged_Type (Rec_Id), Compiler_Abort (Rec_Id));

         E := First_Entity (Rec_Id);
         while Present (E) loop
            if Chars (E) = Name_uTag then
               return E;
            end if;

            E := Next_Entity (E);
         end loop;

         --  Here if no _Tag component found (some kind of compiler error!)

         Compiler_Abort;
      end Tag_Component;

      -----------------------
      -- Has_Discriminants --
      -----------------------

      function Has_Discriminants (Type_Id : Entity_Id) return Boolean is
      begin
         pragma Debug (Check_Defining_Occurrence (Type_Id));
         return Flag5 (Type_Id);
      end Has_Discriminants;

      ---------------------
      -- First_Component --
      ---------------------

      function First_Component (Rec_Id : Entity_Id) return Entity_Id is
         Comp_Id : Entity_Id;

      begin
         pragma Debug (Check_Record_Or_Incomplete_Kind (Rec_Id));

         Comp_Id := First_Entity (Rec_Id);

         while Present (Comp_Id) loop
            exit when Ekind (Comp_Id) = E_Component;
            Comp_Id := Next_Entity (Comp_Id);
         end loop;

         return Comp_Id;
      end First_Component;

      --------------------
      -- Next_Component --
      --------------------

      function Next_Component (Id : Entity_Id) return Entity_Id is
         Comp_Id : Entity_Id;

      begin
         pragma Debug (Check_Defining_Occurrence (Id));

         Comp_Id := Next_Entity (Id);

         while Present (Comp_Id) loop
            exit when Ekind (Comp_Id) = E_Component;
            Comp_Id := Next_Entity (Comp_Id);
         end loop;

         return Comp_Id;
      end Next_Component;

      ------------------------------
      -- Directly_Designated_Type --
      ------------------------------

      function Directly_Designated_Type (Access_Id : Entity_Id)
        return Entity_Id is
      begin
         pragma Debug (Check_Defining_Occurrence (Access_Id));
         return Node10 (Access_Id);
      end Directly_Designated_Type;

      ---------------------
      -- Designated_Type --
      ---------------------

      function Designated_Type (Access_Id : Entity_Id) return Entity_Id is
         Desig_Type : Entity_Id;

      begin
         pragma Debug (Check_Defining_Occurrence (Access_Id));

         Desig_Type := Node10 (Access_Id);

         if (Ekind (Desig_Type) = E_Incomplete_Type
             and then Present (Full_Declaration (Desig_Type)))
         then
            return Full_Declaration (Desig_Type);

         elsif Is_Class_Type (Desig_Type) 
           and then Ekind (Etype (Desig_Type)) = E_Incomplete_Type 
           and then Present (Full_Declaration (Etype (Desig_Type))) 
         then
            return Classwide_Type (Full_Declaration (Etype (Desig_Type)));

         else
            return Desig_Type;
         end if;
      end Designated_Type;

      ----------------------
      -- Full_Declaration --
      ----------------------

      function Full_Declaration (Private_Id : Entity_Id) return Entity_Id is
         Decl : Entity_Id;
      begin
         pragma Debug (Check_Defining_Occurrence (Private_Id));
         Decl := Node11 (Private_Id);
         --  If a type DT is derived in package CP from a private type T 
         --  defined in an ancestor of CP, then the full declaration of DT
         --  may point to the private declaration of T rather than to the
         --  full declaration. In that case,  follow the link to the current
         --  location of the full declaration.  
         if Present (Decl)
           and then (Ekind (Decl) = E_Private_Type
           or else Ekind (Decl) = E_Limited_Private_Type)
         then
            Decl := Node11 (Decl);
         end if;
         return Decl;

      end Full_Declaration;

      --------------------------------
      -- Discriminant_Checking_Func --
      --------------------------------

      function Discriminant_Checking_Func (Id : Entity_Id) return Entity_Id is
      begin
         pragma Debug (Check_Entity_Kind (Id, E_Component));
         return Node10 (Id);
      end Discriminant_Checking_Func;

      -------------------------
      -- Is_Directly_Visible --
      -------------------------

      function Is_Directly_Visible (Id : Entity_Id) return Boolean is
      begin
         return Flag7 (Id);
      end Is_Directly_Visible;

      --------------------
      -- Is_Use_Visible --
      --------------------

      function Is_Use_Visible (Id : Entity_Id) return Boolean is
      begin
         return Flag9 (Id);
      end Is_Use_Visible;

      --------------------
      -- Constant_Value --
      --------------------

      function Constant_Value (Constant_Id : Entity_Id) return Node_Id is
      begin
         if Nkind (Parent (Constant_Id)) = N_Object_Renaming_Declaration then
            return Renamed_Object (Constant_Id);
         else
            if Present (Expression (Parent (Constant_Id))) then
               return (Expression (Parent (Constant_Id)));
            elsif Present (Full_Declaration (Constant_Id)) then
               return (Expression (Parent (Full_Declaration (Constant_Id))));
            end if;
         end if;
      end Constant_Value;


      --------------------
      -- Renamed_Object --
      --------------------

      function Renamed_Object (Id : Entity_Id) return Node_Id is
      begin
         return Node7 (Id);
      end Renamed_Object;

      --------------------------------
      -- Discriminant_Default_Value --
      --------------------------------

      function Discriminant_Default_Value (Id : Entity_Id) return Node_Id is
      begin
         pragma Debug (Check_Entity_Kind (Id, E_Discriminant));
         return Node10 (Id);
      end Discriminant_Default_Value;

      -----------------
      -- Discriminal --
      -----------------

      function Discriminal (Id : Entity_Id) return Node_Id is
      begin
         pragma Debug (Check_Entity_Kind (Id, E_Discriminant));
         return Node9 (Id);
      end Discriminal;

      -------------
      -- Is_Type --
      -------------

      function Is_Type (Id : Entity_Id) return Boolean is
      begin
         pragma Debug (Check_Defining_Occurrence (Id));
         return Ekind (Id) in Type_Kind;
      end Is_Type;

      --------------------
      -- Parent_Subtype --
      --------------------

      function Parent_Subtype (Id : Entity_Id) return Entity_Id is
      begin
         pragma Assert (Ekind (Id) in Scalar_Kind, Compiler_Abort (Id));
         return Node11 (Id);
      end Parent_Subtype;

      ------------------------
      -- Is_Elementary_Type --
      ------------------------

      function Is_Elementary_Type (Type_Id : Entity_Id) return Boolean is
      begin
         return Ekind (Type_Id) in Scalar_Kind
           or else Ekind (Type_Id) in Access_Kind;
      end Is_Elementary_Type;

      --------------------
      -- Is_Scalar_Type --
      --------------------

      function Is_Scalar_Type (Type_Id : Entity_Id) return Boolean is
      begin
         return Ekind (Type_Id) in Scalar_Kind;
      end Is_Scalar_Type;

      ----------------------
      -- Is_Discrete_Type --
      ----------------------

      function Is_Discrete_Type (Type_Id : Entity_Id) return Boolean is
      begin
         return Ekind (Type_Id) in Discrete_Kind;
      end Is_Discrete_Type;

      -------------------------
      -- Is_Enumeration_Type --
      -------------------------

      function Is_Enumeration_Type (Type_Id : Entity_Id) return Boolean is
      begin
         return Ekind (Type_Id) in Enumeration_Kind;
      end Is_Enumeration_Type;

      ---------------------
      -- Is_Boolean_Type --
      ---------------------

      function Is_Boolean_Type (Type_Id : Entity_Id) return Boolean is
      begin
         return Ekind (Base_Type (Type_Id)) = E_Boolean_Type;
      end Is_Boolean_Type;

      -----------------------
      -- Is_Character_Type --
      -----------------------

      function Is_Character_Type (Type_Id : Entity_Id) return Boolean is
      begin
         return Ekind (Base_Type (Type_Id)) = E_Character_Type;
      end Is_Character_Type;

      ---------------------
      -- Is_Integer_Type --
      ---------------------

      function Is_Integer_Type (Type_Id : Entity_Id) return Boolean is
      begin
         return Ekind (Type_Id) in Integer_Kind;
      end Is_Integer_Type;

      ------------------
      -- Is_Real_Type --
      ------------------

      function Is_Real_Type (Type_Id : Entity_Id) return Boolean is
      begin
         return Ekind (Type_Id) in Real_Kind;
      end Is_Real_Type;

      -------------------
      -- Is_Fixed_Type --
      -------------------

      function Is_Fixed_Type (Type_Id : Entity_Id) return Boolean is
      begin
         return Ekind (Type_Id) in Fixed_Kind;
      end Is_Fixed_Type;

      -------------------
      -- Is_Float_Type --
      -------------------

      function Is_Float_Type (Type_Id : Entity_Id) return Boolean is
      begin
         return Ekind (Type_Id) in Float_Kind;
      end Is_Float_Type;

      ---------------------
      -- Is_Numeric_Type --
      ---------------------

      function Is_Numeric_Type (Type_Id : Entity_Id) return Boolean is
      begin
         return Ekind (Type_Id) in Numeric_Kind;
      end Is_Numeric_Type;

      -----------------------
      -- Is_Composite_Type --
      -----------------------

      function Is_Composite_Type (Type_Id : Entity_Id) return Boolean is
      begin
         return Ekind (Type_Id) in Composite_Kind;
      end Is_Composite_Type;

      -------------------
      -- Is_Array_Type --
      -------------------

      function Is_Array_Type (Type_Id : Entity_Id) return Boolean is
      begin
         return Ekind (Type_Id) in Array_Kind;
      end Is_Array_Type;

      --------------------
      -- Is_String_Type --
      --------------------

      function Is_String_Type (Type_Id : Entity_Id) return Boolean is
      begin
         return Ekind (Type_Id) in String_Kind
           or else (Ekind (Type_Id) = E_Slice_Subtype
                     and then Is_String_Type (Base_Type (Type_Id)))
           or else (Is_Array_Type (Type_Id)
                     and then Number_Dimensions (Type_Id) = 1
                     and then Is_Character_Type (Component_Type (Type_Id)));
      end Is_String_Type;

      --------------------
      -- Is_Record_Type --
      --------------------

      function Is_Record_Type (Type_Id : Entity_Id) return Boolean is
      begin
         return Ekind (Type_Id) in Record_Kind;
      end Is_Record_Type;

      --------------------
      -- Is_Access_Type --
      --------------------

      function Is_Access_Type (Type_Id : Entity_Id) return Boolean is
      begin
         return Ekind (Type_Id) in Access_Kind;
      end Is_Access_Type;

      -----------------
      -- Is_Abstract --
      -----------------

      function Is_Abstract (Id : Entity_Id) return Boolean is
      begin
         pragma Debug (Check_Defining_Occurrence (Id));
         return Flag19 (Id);
      end Is_Abstract;

      ----------------
      -- Is_Aliased --
      ----------------

      function Is_Aliased (Id : Entity_Id) return Boolean is
      begin
         pragma Debug (Check_Defining_Occurrence (Id));
         return Flag15 (Id);
      end Is_Aliased;

      -------------------
      -- Is_Subprogram --
      -------------------

      function Is_Subprogram (Type_Id : Entity_Id) return Boolean is
      begin
         return Ekind (Type_Id) in Subprogram_Kind;
      end Is_Subprogram;

      ---------------------
      -- Is_Named_Number --
      ---------------------

      function Is_Named_Number (Type_Id : Entity_Id) return Boolean is
      begin
         return Ekind (Type_Id) in Named_Kind;
      end Is_Named_Number;

      ------------------
      -- Is_Task_Type --
      ------------------

      function Is_Task_Type (Type_Id : Entity_Id) return Boolean is
      begin
         return Ekind (Type_Id) in Task_Kind;
      end Is_Task_Type;

      -----------------------
      -- Is_Protected_Type --
      -----------------------

      function Is_Protected_Type (Type_Id : Entity_Id) return Boolean is
      begin
         return Ekind (Type_Id) in Protected_Kind;
      end Is_Protected_Type;

      --------------------
      -- Is_Tagged_Type --
      --------------------

      function Is_Tagged_Type (Type_Id : Entity_Id) return Boolean is
      begin
         return Flag8 (Type_Id);
      end Is_Tagged_Type;

      -------------------------
      -- Is_Task_Record_Type --
      -------------------------

      function Is_Task_Record_Type (Type_Id : Entity_Id) return Boolean is
      begin
         return Flag20 (Type_Id);
      end Is_Task_Record_Type;

      ---------------------
      -- Is_Limited_Type --
      ---------------------

      function Is_Limited_Type (Type_Id : Entity_Id) return Boolean is
      begin
         return Flag25 (Type_Id);
      end Is_Limited_Type;

      ------------------------
      -- Is_Incomplete_Type --
      ------------------------

      function Is_Incomplete_Type (Type_Id : Entity_Id) return Boolean is
      begin
         return Ekind (Type_Id) in Incomplete_Kind;
      end Is_Incomplete_Type;

      -----------------
      -- Is_Internal --
      -----------------

      function Is_Internal (Type_Id : Entity_Id) return Boolean is
      begin
         return Flag2 (Type_Id);
      end Is_Internal;

      ---------------
      -- Is_Packed --
      ---------------

      function Is_Packed (Type_Id : Entity_Id) return Boolean is
      begin
         return Flag11 (Type_Id);
      end Is_Packed;

      ----------------
      -- Is_Private --
      ----------------

      function Is_Private (Id : Entity_Id) return Boolean is
      begin
         return Flag13 (Id);
      end Is_Private;

      ---------------------
      -- Is_Private_Type --
      ---------------------

      function Is_Private_Type (Type_Id : Entity_Id) return Boolean is
      begin
         return Flag14 (Type_Id);
      end Is_Private_Type;

      ---------------
      -- Is_Public --
      ---------------

      function Is_Public (Id : Entity_Id) return Boolean is
      begin
         return Flag10 (Id);
      end Is_Public;

      -------------
      -- Is_Pure --
      -------------

      function Is_Pure (Proc_Id : Entity_Id) return Boolean is
      begin
         return Flag3 (Proc_Id);
      end Is_Pure;

      -----------------
      -- Is_Imported --
      -----------------

      function Is_Imported (Id : Entity_Id) return Boolean is
      begin
         pragma Debug (Check_Defining_Occurrence (Id));
         return Flag24 (Id);
      end Is_Imported;

      ----------------
      -- Is_Inlined --
      ----------------

      function Is_Inlined (Id : Entity_Id) return Boolean is
      begin
         pragma Debug (Check_Overloaded_Kind (Id));
         return Flag11 (Id);
      end Is_Inlined;

      -----------------
      -- Is_Volatile --
      -----------------

      function Is_Volatile (Id : Entity_Id) return Boolean is
      begin
         pragma Debug (Check_Defining_Occurrence (Id));
         return Flag16 (Id);
      end Is_Volatile;

      -------------------------------
      -- Original_Record_Component --
      -------------------------------

      function Original_Record_Component (Id : Entity_Id) return Entity_Id is
      begin
         --  needs double check E_Component or E_Discriminant ???
         --  pragma Debug (Check_Entity_Kind (Id, E_Component));
         return Node8 (Id);
      end Original_Record_Component;

      ---------------------
      -- Task_Value_Type --
      ---------------------

      function Task_Value_Type (Id : Entity_Id) return Entity_Id is
      begin
         pragma Debug (Check_Defining_Occurrence (Id));
         return Node7 (Id);
      end Task_Value_Type;

      --------------
      -- Reachable --
      ---------------

      function Reachable (Label_Id : Entity_Id) return Boolean is
      begin
         return Flag5 (Label_Id);
      end Reachable;

      -----------------
      -- Slice_Range --
      -----------------

      function Slice_Range (Id : Entity_Id) return Node_Id is
      begin
         return Node11 (Id);
      end Slice_Range;

      ---------------------------
      -- String_Literal_Length --
      ---------------------------

      function String_Literal_Length (Id : Entity_Id) return Uint is
      begin
         return Uint11 (Id);
      end String_Literal_Length;

      ------------------
      -- Subtype_Kind --
      ------------------

      function Subtype_Kind (K : Entity_Kind) return Entity_Kind is
         Kind : Entity_Kind;

      begin
         case K is
            when Access_Kind          => Kind := E_Access_Subtype;

            when E_Array_Type |
                 E_Array_Subtype      => Kind := E_Array_Subtype;

            when E_Class_Type |
                 E_Class_Subtype      => Kind := E_Class_Subtype;

            when E_Decimal_Type |
                 E_Decimal_Subtype    => Kind := E_Decimal_Subtype;

            when E_Fixed_Type |
                 E_Fixed_Subtype      => Kind := E_Fixed_Subtype;

            when E_Record_Type |
                 E_Record_Subtype     => Kind := E_Record_Subtype;

            when E_String_Type |
                 E_String_Subtype     => Kind := E_String_Subtype;

            when Enumeration_Kind     => Kind := E_Enumeration_Subtype;
            when Float_Kind           => Kind := E_Float_Subtype;
            when Signed_Integer_Kind  => Kind := E_Integer_Subtype;
            when Modular_Kind         => Kind := E_Modular_Subtype;
            when Protected_Kind       => Kind := E_Protected_Subtype;
            when Task_Kind            => Kind := E_Task_Subtype;

            when others =>
               Compiler_Error;
               Write_String (Entity_Kind'Image (K));
               Write_String (" has no appropriate subtype");
               Write_Eol;
               Compiler_Abort;
         end case;

         return Kind;
      end Subtype_Kind;

      -----------------------
      -- Table_High_Bounds --
      -----------------------

      function Table_High_Bound (Id : Entity_Id) return Node_Id is
      begin
         return Node11 (Id);
      end Table_High_Bound;

      -----------------------
      -- Needs_Discr_Check --
      -----------------------

      function Needs_Discr_Check (Id : Entity_Id) return Boolean is
      begin
         return Flag5 (Id);
      end Needs_Discr_Check;

      --------------
      -- Has_Exit --
      --------------

      function Has_Exit (Loop_Id : Entity_Id) return Boolean is
      begin
         return Flag5 (Loop_Id);
      end Has_Exit;

      ------------------
      -- First_Formal --
      ------------------

      function First_Formal (Overloaded_Id : Entity_Id) return Entity_Id is
         Formal : Entity_Id;

      begin
         pragma Debug (Check_Overloaded_Kind (Overloaded_Id));

         if Ekind (Overloaded_Id) = E_Enumeration_Literal then
            return Empty;

         else
            Formal := First_Entity (Overloaded_Id);

            if Present (Formal) and then
              Ekind (Formal) in E_In_Parameter .. E_In_Out_Parameter
            then
               return Formal;
            else
               return Empty;
            end if;
         end if;
      end First_Formal;

      -----------------
      -- Next_Formal --
      -----------------

      function Next_Formal (Formal_Id : Entity_Id) return Entity_Id is
         P : Entity_Id;

      begin
         pragma Debug (Check_Defining_Occurrence (Formal_Id));

         --  Follow the chain of declared entities as long as the kind of
         --  the entity corresponds to a formal parameter. Skip internal
         --  entities that may have been created for implicit subtypes,
         --  in the process of analyzing default expressions.

         P := Next_Entity (Formal_Id);

         if Present (P) and then
              Ekind (P) in E_In_Parameter .. E_In_Out_Parameter
         then
            return P;
         elsif Is_Internal (P) then
            while Present (P) and then Is_Internal (P)
            loop
               P := Next_Entity (P);
            end loop;
            if Present (P)
              and then Ekind (P) in E_In_Parameter .. E_In_Out_Parameter
            then
               return P;
            else
               return Empty;
            end if;
         else
            return Empty;
         end if;
      end Next_Formal;

      --------------------
      -- Parameter_Mode --
      --------------------

      function Parameter_Mode (Formal_Id : Entity_Id) return Formal_Kind is
      begin
         pragma Debug (Check_Defining_Occurrence (Formal_Id));
         return Ekind (Formal_Id);
      end Parameter_Mode;

      ------------------
      -- First_Actual --
      ------------------

      function First_Actual (Node : Node_Id) return Node_Id is
         N : Node_Id;

      begin
         if Parameter_Associations (Node) = No_List then
            return Empty;
         end if;

         N := First (Parameter_Associations (Node));

         if Nkind (N) = N_Parameter_Association then
            return First_Named_Actual (Node);
         else
            return N;
         end if;
      end First_Actual;

      -----------------
      -- Next_Actual --
      -----------------

      function Next_Actual (Actual_Id : Node_Id) return Node_Id is
         N  : Node_Id;

      begin
         --  If we are pointing at a positional parameter, it is a member of
         --  a node list (the list of parameters), and the next parameter
         --  is the next node on the list, unless we hit a parameter
         --  association, in which case we shift to using the chain whose
         --  head is the First_Named_Actual in the parent, and then is
         --  threaded using the Next_Named_Actual of the Parameter_Association.
         --  All this fiddling is because the original node list is in the
         --  textual call order, and what we need is the declaration order.

         if Is_List_Member (Actual_Id) then
            N := Next (Actual_Id);

            if Nkind (N) = N_Parameter_Association then
               return First_Named_Actual (Parent (Actual_Id));
            else
               return N;
            end if;

         else
            return Next_Named_Actual (Parent (Actual_Id));
         end if;
      end Next_Actual;

      --------------------
      -- Next_Overloads --
      --------------------

      function Next_Overloads (Overloaded_Id : Entity_Id) return Entity_Id is
      begin
         pragma Debug (Check_Overloaded_Kind (Overloaded_Id));
         return Homonym (Overloaded_Id);
      end Next_Overloads;

      -----------
      -- Alias --
      -----------

      function Alias  (Overloaded_Id : Entity_Id) return Entity_Id is
      begin
         pragma Debug (Check_Overloaded_Kind (Overloaded_Id));
         return Node7 (Overloaded_Id);
      end Alias;

      ----------------------
      -- Needs_No_Actuals --
      ----------------------

      function Needs_No_Actuals (Overloaded_Id : Entity_Id) return Boolean is
      begin
         pragma Debug (Check_Overloaded_Kind (Overloaded_Id));
         return Flag8 (Overloaded_Id);
      end Needs_No_Actuals;

      ------------------
      -- Is_Intrinsic --
      ------------------

      function Is_Intrinsic (Proc_Id : Entity_Id) return Boolean is
      begin
         pragma Debug (Check_Defining_Occurrence (Proc_Id));
         return Flag27 (Proc_Id);
      end Is_Intrinsic;

      --------------------
      -- Interface_Name --
      --------------------

      function Interface_Name (Proc_Id : Entity_Id) return Node_Id is
      begin
         pragma Debug (Check_Defining_Occurrence (Proc_Id));
         return Node12 (Proc_Id);
      end Interface_Name;

      ---------------------
      -- Is_Overloadable --
      ---------------------

      function Is_Overloadable (Id : Entity_Id) return Boolean is
      begin
         return Ekind (Id) in Overloaded_Kind;
      end Is_Overloadable;

      -----------------
      -- Entry_Index --
      -----------------

      function Entry_Index (Id : Entity_Id) return Uint is
      begin
         return Uint11 (Id);
      end Entry_Index;

      ---------------------
      -- Enumeration_Pos --
      ---------------------

      function Enumeration_Pos (Enum_Id : Entity_Id) return Uint is
      begin
         return Uint11 (Enum_Id);
      end Enumeration_Pos;

      ---------------------
      -- Enumeration_Rep --
      ---------------------

      function Enumeration_Rep (Enum_Id : Entity_Id) return Uint is
      begin
         return Uint12 (Enum_Id);
      end Enumeration_Rep;

      -------------------
      -- Default_Value --
      -------------------

      function Default_Value (In_Id : Entity_Id) return Node_Id is
      begin
         pragma Debug (Check_Entity_Kind (In_Id, E_In_Parameter));
         return Node10 (In_Id);
      end Default_Value;

      ---------------------
      -- Is_Package_Body --
      ---------------------

      function Is_Package_Body (Package_Id : Entity_Id) return Boolean is
      begin
         pragma Debug (Check_Defining_Occurrence (Package_Id));
         return Flag5 (Package_Id);
      end Is_Package_Body;

      ---------------------
      -- In_Private_Part --
      ---------------------

      function In_Private_Part (Package_Id : Entity_Id) return Boolean is
      begin
         pragma Debug (Check_Defining_Occurrence (Package_Id));
         return Flag4 (Package_Id);
      end In_Private_Part;

      ------------
      -- In_Use --
      ------------

      function In_Use (Package_Id : Entity_Id) return Boolean is
      begin
         pragma Debug (Check_Defining_Occurrence (Package_Id));
         return Flag8 (Package_Id);
      end In_Use;

      ---------------------------
      -- Is_Private_Descendant --
      ---------------------------

      function Is_Private_Descendant (Id : Entity_Id) return Boolean is
      begin
         pragma Debug (Check_Defining_Occurrence (Id));
         return Flag14 (Id);
      end Is_Private_Descendant;

      ---------------
      -- Base_Type --
      ---------------

      function Base_Type (Type_Id : Entity_Id) return Entity_Id is
      begin
         pragma Debug (Check_Defining_Occurrence (Type_Id));

         case Ekind (Type_Id) is
            when E_Enumeration_Subtype |
                 E_Integer_Subtype     |
                 E_Modular_Subtype     |
                 E_Float_Subtype       |
                 E_Fixed_Subtype       |
                 E_Decimal_Subtype     |
                 E_Array_Subtype       |
                 E_String_Subtype      |
                 E_Record_Subtype      |
                 E_Access_Subtype      |
                 E_Protected_Subtype   |
                 E_Task_Subtype        |
                 E_String_Literal_Subtype =>
               return Etype (Type_Id);

            when E_Slice_Subtype =>
               return Base_Type (Etype (Type_Id));

            when others =>
               return Type_Id;
         end case;
      end Base_Type;

      --------------------------
      -- First_Private_Entity --
      --------------------------

      function First_Private_Entity (Package_Id : Entity_Id)
                                                return Entity_Id is
      begin
         pragma Debug (Check_Defining_Occurrence (Package_Id));
         return Node11 (Package_Id);
      end First_Private_Entity;

      ----------------------------
      -- Suppress_Access_Checks --
      ----------------------------

      function Suppress_Access_Checks (E : Entity_Id) return Boolean is
      begin
         return Flag31 (E);
      end Suppress_Access_Checks;

      -----------------------------------
      -- Suppress_Accessibility_Checks --
      -----------------------------------

      function Suppress_Accessibility_Checks (E : Entity_Id) return Boolean is
      begin
         return Flag32 (E);
      end Suppress_Accessibility_Checks;

      ----------------------------------
      -- Suppress_Discriminant_Checks --
      ----------------------------------

      function Suppress_Discriminant_Checks (E : Entity_Id) return Boolean is
      begin
         return Flag33 (E);
      end Suppress_Discriminant_Checks;

      ------------------------------
      -- Suppress_Division_Checks --
      ------------------------------

      function Suppress_Division_Checks (E : Entity_Id) return Boolean is
      begin
         return Flag34 (E);
      end Suppress_Division_Checks;

      ---------------------------------
      -- Suppress_Elaboration_Checks --
      ---------------------------------

      function Suppress_Elaboration_Checks (E : Entity_Id) return Boolean is
      begin
         return Flag35 (E);
      end Suppress_Elaboration_Checks;

      ---------------------------
      -- Suppress_Index_Checks --
      ---------------------------

      function Suppress_Index_Checks (E : Entity_Id) return Boolean is
      begin
         return Flag36 (E);
      end Suppress_Index_Checks;

      ----------------------------
      -- Suppress_Length_Checks --
      ----------------------------

      function Suppress_Length_Checks (E : Entity_Id) return Boolean is
      begin
         return Flag37 (E);
      end Suppress_Length_Checks;

      ------------------------------
      -- Suppress_Overflow_Checks --
      ------------------------------

      function Suppress_Overflow_Checks (E : Entity_Id) return Boolean is
      begin
         return Flag38 (E);
      end Suppress_Overflow_Checks;

      ---------------------------
      -- Suppress_Range_Checks --
      ---------------------------

      function Suppress_Range_Checks (E : Entity_Id) return Boolean is
      begin
         return Flag39 (E);
      end Suppress_Range_Checks;

      -----------------------------
      -- Suppress_Storage_Checks --
      -----------------------------

      function Suppress_Storage_Checks (E : Entity_Id) return Boolean is
      begin
         return Flag40 (E);
      end Suppress_Storage_Checks;

      -------------------------
      -- Suppress_Tag_Checks --
      -------------------------

      function Suppress_Tag_Checks (E : Entity_Id) return Boolean is
      begin
         return Flag41 (E);
      end Suppress_Tag_Checks;

      ---------------------
      -- Equivalent_Type --
      ---------------------

      function Equivalent_Type (Sub_Class : Entity_Id) return Entity_Id is
      begin
         pragma Assert (Ekind (Sub_Class) = E_Class_Subtype, 
                        Compiler_Abort (Sub_Class));
         return Node7 (Sub_Class);
      end Equivalent_Type;

   -----------------------------------------
   -- Procedures to Set Entity Attributes --
   -----------------------------------------

   --  There is one procedure to set each basic attribute. Attributes that
   --  are retrieved from other tree nodes (for example, the first parameter
   --  of a subprogram) are purely syntactic and do not require setting
   --  procedures.

      ---------------------------------
      -- Set_Corresponding_Task_Type --
      ---------------------------------

      procedure Set_Corresponding_Task_Type (Id : Entity_Id; T : Entity_Id) is
      begin
         pragma Assert (Is_Record_Type (Id)
                          and then not Is_Tagged_Type (Id)
                          and then Is_Task_Type (T),
                        Compiler_Abort (Id));
         Set_Node7 (Id, T);
      end Set_Corresponding_Task_Type;

      ---------------
      -- Set_Scope --
      ---------------

      procedure Set_Scope (Id : Entity_Id; S : Entity_Id) is
      begin
         pragma Debug (Check_Defining_Occurrence (Id));
         Set_Node3 (Id, S);
      end Set_Scope;

      -----------------
      -- Set_Homonym --
      -----------------

      procedure Set_Homonym (Id : Entity_Id; T : Entity_Id) is
      begin
         pragma Debug (Check_Defining_Occurrence (Id));
         Set_Node4 (Id, T);
      end Set_Homonym;

      -------------------------
      --  Set_Has_Completion --
      -------------------------

      procedure Set_Has_Completion (Id : Entity_Id;
                                    Status : Boolean := True) is
      begin
         pragma Debug (Check_Defining_Occurrence (Id));
         Set_Flag26 (Id, Status);
      end Set_Has_Completion;

      --------------------
      --  Set_Has_Tasks --
      --------------------

      procedure Set_Has_Tasks (Id : Entity_Id; Status : Boolean := True) is
      begin
         pragma Debug (Check_Defining_Occurrence (Id));
         Set_Flag30 (Id, Status);
      end Set_Has_Tasks;

      ----------------------------
      --  Set_Has_Master_Entity --
      ----------------------------

      procedure Set_Has_Master_Entity (Id : Entity_Id; 
                                       Status : Boolean := True) is
      begin
         pragma Debug (Check_Defining_Occurrence (Id));
         Set_Flag21 (Id, Status);
      end Set_Has_Master_Entity;

      -----------------------------
      --  Set_Has_Address_Clause --
      -----------------------------

      procedure Set_Has_Address_Clause (Id : Entity_Id;
                                        Status : Boolean := True) is
      begin
         pragma Debug (Check_Defining_Occurrence (Id));
         Set_Flag28 (Id, Status);
      end Set_Has_Address_Clause;

      ----------------------
      --  Set_Has_Homonym --
      ----------------------

      procedure Set_Has_Homonym (Id : Entity_Id; Status : Boolean := True) is
      begin
         pragma Debug (Check_Defining_Occurrence (Id));
         Set_Flag12 (Id, Status);
      end Set_Has_Homonym;

      --------------------------
      --  Set_Has_Size_Clause --
      --------------------------

      procedure Set_Has_Size_Clause (Id : Entity_Id;
                                     Status : Boolean := True) is
      begin
         pragma Debug (Check_Defining_Occurrence (Id));
         Set_Flag29 (Id, Status);
      end Set_Has_Size_Clause;

      ----------------------------------
      --  Set_Has_Storage_Size_Clause --
      ----------------------------------

      procedure Set_Has_Storage_Size_Clause 
        (Id : Entity_Id; Status : Boolean := True) is
      begin
         pragma Debug (Check_Defining_Occurrence (Id));
         Set_Flag23 (Id, Status);
      end Set_Has_Storage_Size_Clause;

      --------------
      -- Set_Esize --
      --------------

      procedure Set_Esize (Type_Id : Entity_Id; S : Uint) is
      begin
         pragma Debug (Check_Defining_Occurrence (Type_Id));
         Set_Uint12 (Type_Id, S);
      end Set_Esize;

      ----------------------
      -- Set_First_Entity --
      ----------------------

      procedure Set_First_Entity (Scope_Id : Entity_Id; Id : Entity_Id) is
      begin
         Set_Node9 (Scope_Id, Id);
      end Set_First_Entity;

      ---------------------
      -- Set_Next_Entity --
      ---------------------

      procedure Set_Next_Entity (Prev : Entity_Id; Id : Entity_Id) is
      begin
         Set_Node6 (Prev, Id);
      end Set_Next_Entity;

      -------------------------
      --  Set_Renamed_Object --
      -------------------------

      procedure Set_Renamed_Object (Id : Entity_Id; Object : Node_Id) is
      begin
         Set_Node7 (Id, Object);
      end Set_Renamed_Object;

      ---------------------
      -- Set_First_Index --
      ---------------------

      procedure Set_First_Index (Array_Id : Entity_Id; Index : Node_Id) is
      begin
         Set_Node9 (Array_Id, Index);
      end Set_First_Index;

      ----------------------
      -- Set_First_Literal --
      ----------------------

      procedure Set_First_Literal (Type_Id : Entity_Id; Id : Entity_Id) is
      begin
         Set_Node9  (Type_Id, Id);
      end Set_First_Literal;

      ------------------------
      -- Set_Is_Constrained --
      ------------------------

      procedure Set_Is_Constrained
        (Type_Id : Entity_Id; Status : Boolean := True) is
      begin
         Set_Flag3 (Type_Id, Status);
      end Set_Is_Constrained;

      ---------------------------
      -- Set_Has_Discriminants --
      ---------------------------

      procedure Set_Has_Discriminants
        (Type_Id : Entity_Id; Status : Boolean := True) is
      begin
         Set_Flag5 (Type_Id, Status);
      end Set_Has_Discriminants;

      --------------------
      -- Set_Is_Delayed --
      --------------------

      procedure Set_Is_Delayed (E : Entity_Id;
                                Status : Boolean := True) is
      begin
         pragma Debug (Check_Defining_Occurrence (E));
         Set_Flag18 (E, Status);
      end Set_Is_Delayed;

      -------------------------
      -- Set_Is_Generic_Type --
      -------------------------

      procedure Set_Is_Generic_Type (Type_Id : Entity_Id;
                                     Status : Boolean := True) is
      begin
         pragma Debug (Check_Defining_Occurrence (Type_Id));
         Set_Flag6 (Type_Id, Status);
      end Set_Is_Generic_Type;

      -------------------
      -- Set_Is_Frozen --
      -------------------

      procedure Set_Is_Frozen (Type_Id : Entity_Id;
                               Status : Boolean := True) is
      begin
         pragma Debug (Check_Defining_Occurrence (Type_Id));
         Set_Flag4 (Type_Id, Status);
      end Set_Is_Frozen;

      ----------------------
      -- Set_Scalar_Range --
      ----------------------

      procedure Set_Scalar_Range  (Type_Id : Entity_Id; R_Node  : Node_Id) is
      begin
         pragma Debug (Check_Defining_Occurrence (Type_Id));
         Set_Node10 (Type_Id, R_Node);
      end Set_Scalar_Range;

      -----------------
      -- Set_Modulus --
      -----------------

      procedure Set_Modulus (Type_Id : Entity_Id; Mod_Value : Uint) is
      begin
         pragma Debug (Check_Defining_Occurrence (Type_Id));
         Set_Uint9 (Type_Id, Mod_Value);
      end Set_Modulus;

      ------------------------
      -- Set_Lit_Name_Table --
      ------------------------

      procedure Set_Lit_Name_Table (Type_Id : Entity_Id; Table : Entity_Id) is
      begin
         pragma Debug (Check_Defining_Occurrence (Type_Id));
         Set_Node7 (Type_Id, Table);
      end Set_Lit_Name_Table;

      -------------------------
      -- Set_Is_Limited_Type --
      -------------------------

      procedure Set_Is_Limited_Type 
        (Type_Id : Entity_Id; Status : Boolean := True) is
      begin
         Set_Flag25 (Type_Id, Status);
      end Set_Is_Limited_Type;

      ------------------------
      -- Set_Is_Tagged_Type --
      ------------------------

      procedure Set_Is_Tagged_Type (Type_Id : Entity_Id;
                                    Status : Boolean := True) is
      begin
         Set_Flag8 (Type_Id, Status);
      end Set_Is_Tagged_Type;

      -----------------------------
      -- Set_Is_Task_Record_Type --
      -----------------------------

      procedure Set_Is_Task_Record_Type 
        (Type_Id : Entity_Id; Status : Boolean := True) is
      begin
         Set_Flag20 (Type_Id, Status);
      end Set_Is_Task_Record_Type;

      ---------------------
      -- Set_Is_Internal --
      ---------------------

      procedure Set_Is_Internal
        (Type_Id : Entity_Id; Status : Boolean := True) is
      begin
         Set_Flag2 (Type_Id, Status);
      end Set_Is_Internal;

      -------------------
      -- Set_Is_Packed --
      -------------------

      procedure Set_Is_Packed 
        (Type_Id : Entity_Id; Status : Boolean := True) is
      begin
         Set_Flag11 (Type_Id, Status);
      end Set_Is_Packed;

      --------------------
      -- Set_Is_Private --
      --------------------

      procedure Set_Is_Private
        (Id : Entity_Id; Status : Boolean := True) is
      begin
         Set_Flag13 (Id, Status);
      end Set_Is_Private;

      ---------------------
      -- Set_Is_Abstract --
      ---------------------

      procedure Set_Is_Abstract
        (Id : Entity_Id; Status : Boolean := True) is
      begin
         Set_Flag19 (Id, Status);
      end Set_Is_Abstract;

      --------------------
      -- Set_Is_Aliased --
      --------------------

      procedure Set_Is_Aliased
        (Id : Entity_Id; Status : Boolean := True) is
      begin
         Set_Flag15 (Id, Status);
      end Set_Is_Aliased;

      -------------------------
      -- Set_Is_Private_Type --
      -------------------------

      procedure Set_Is_Private_Type
        (Type_Id : Entity_Id; Status : Boolean := True) is
      begin
         Set_Flag14 (Type_Id, Status);
      end Set_Is_Private_Type;

      -------------------
      -- Set_Is_Public --
      -------------------

      procedure Set_Is_Public (Id : Entity_Id; Status : Boolean := True) is
      begin
         Set_Flag10 (Id, Status);
      end Set_Is_Public;

      -----------------
      -- Set_Is_Pure --
      -----------------

      procedure Set_Is_Pure (Proc_Id : Entity_Id; Status : Boolean := True) is
      begin
         Set_Flag3 (Proc_Id, Status);
      end Set_Is_Pure;

      ---------------------
      -- Set_Is_Imported --
      ---------------------

      procedure Set_Is_Imported (Id : Entity_Id; Status : Boolean := True) is
      begin
         pragma Debug (Check_Defining_Occurrence (Id));
         Set_Flag24 (Id, Status);
      end Set_Is_Imported;

      --------------------
      -- Set_Is_Inlined --
      --------------------

      procedure Set_Is_Inlined (Id : Entity_Id; Status : Boolean := True) is
      begin
         pragma Debug (Check_Overloaded_Kind (Id));
         Set_Flag11 (Id, Status);
      end Set_Is_Inlined;

      ---------------------
      -- Set_Is_Volatile --
      ---------------------

      procedure Set_Is_Volatile  (Id : Entity_Id;
                                 Status : Boolean := True) is
      begin
         Set_Flag16 (Id, Status);
      end Set_Is_Volatile;

      ------------------------
      -- Set_Parent_Subtype --
      ------------------------

      procedure Set_Parent_Subtype (Id : Entity_Id; Parent_Id : Entity_Id) is
      begin
         pragma Assert (Ekind (Id) in Scalar_Kind, Compiler_Abort (Id));
         Set_Node11 (Id, Parent_Id);
      end Set_Parent_Subtype;

      -------------------
      -- Set_Reachable --
      -------------------

      procedure Set_Reachable (Label_Id : Entity_Id;
                               Status : Boolean := True) is
      begin
         pragma Debug (Check_Defining_Occurrence (Label_Id));
         Set_Flag5 (Label_Id, Status);
      end Set_Reachable;

      ---------------------------
      -- Set_Needs_Discr_Check --
      ---------------------------

      procedure Set_Needs_Discr_Check
        (Id : Entity_Id; Status : Boolean := True) is
      begin
         pragma Debug (Check_Entity_Kind (Id, E_Component));
         pragma Assert 
           (Ekind (Scope (Id)) in Record_Kind, Compiler_Abort (Id));
         Set_Flag5 (Id, Status);
      end Set_Needs_Discr_Check;

      ------------------
      -- Set_Has_Exit --
      ------------------

      procedure Set_Has_Exit
        (Loop_Id : Entity_Id; Status : Boolean := True) is
      begin
         pragma Debug (Check_Defining_Occurrence (Loop_Id));
         Set_Flag5 (Loop_Id, Status);
      end Set_Has_Exit;

      ---------------------------------
      -- Set_Discriminant_Constraint --
      ---------------------------------

      procedure Set_Discriminant_Constraint
        (Type_Id : Entity_Id; Elist : Elist_Id) is
      begin
         Set_Elist2 (Type_Id, Elist);
      end Set_Discriminant_Constraint;

      -------------------
      -- Set_Init_Proc --
      -------------------

      procedure Set_Init_Proc (Type_Id : Entity_Id; P : Entity_Id) is
      begin
         pragma Debug (Check_Type (Type_Id));
         Set_Node8 (Type_Id, P);
      end Set_Init_Proc;

      ------------------------------
      -- Set_Primitive_Operations --
      ------------------------------

      procedure Set_Primitive_Operations
        (Id : Entity_Id; Elist : Elist_Id) is
      begin
         pragma Assert (Is_Tagged_Type (Id), Compiler_Abort (Id));
         Set_Elist7 (Id, Elist);
      end Set_Primitive_Operations;

      ------------------------------------
      -- Set_Internal_Access_Disp_Table --
      ------------------------------------

      procedure Set_Internal_Access_Disp_Table 
        (Component_Id : Entity_Id; Id : Entity_Id) is
      begin
         pragma Debug (Check_Entity_Kind (Component_Id, E_Component));
         Set_Node11 (Component_Id, Id);
      end Set_Internal_Access_Disp_Table;

      ---------------------------
      -- Set_Access_Disp_Table --
      ---------------------------

      procedure Set_Access_Disp_Table (Rec_Id : Entity_Id; Id : Entity_Id) is
      begin
         pragma Assert (Is_Tagged_Type (Rec_Id), Compiler_Abort (Rec_Id));
         Set_Internal_Access_Disp_Table (Tag_Component (Rec_Id), Id);
      end Set_Access_Disp_Table;

      ----------------------------------
      -- Set_Is_Dispatching_Operation --
      ----------------------------------

      procedure Set_Is_Dispatching_Operation
        (Overloaded_Id : Entity_Id; Status : Boolean := True) is
      begin
         pragma Debug (Check_Overloaded_Kind (Overloaded_Id));
         Set_Flag6 (Overloaded_Id, Status);
      end Set_Is_Dispatching_Operation;

      ------------------------
      -- Set_Component_Type --
      ------------------------

      procedure Set_Component_Type (Array_Id : Entity_Id; Ct : Entity_Id) is
      begin
         pragma Debug (Check_Defining_Occurrence (Array_Id));
         Set_Node10 (Array_Id, Ct);
      end Set_Component_Type;

      ----------------------------------
      -- Set_Directly_Designated_Type --
      ----------------------------------

      procedure Set_Directly_Designated_Type
        (Access_Id : Entity_Id; Dt : Entity_Id) is
      begin
         pragma Debug (Check_Defining_Occurrence (Access_Id));
         Set_Node10 (Access_Id, Dt);
      end Set_Directly_Designated_Type;

      --------------------------
      -- Set_Full_Declaration --
      --------------------------

      procedure Set_Full_Declaration 
        (Private_Id : Entity_Id; Fd : Entity_Id) is
      begin
         pragma Debug (Check_Defining_Occurrence (Private_Id));
         Set_Node11 (Private_Id, Fd);
      end Set_Full_Declaration;

      ---------------------
      -- Set_Discriminal --
      ---------------------

      procedure Set_Discriminal (Id : Entity_Id; Formal : Entity_Id) is
      begin
         pragma Debug (Check_Entity_Kind (Id, E_Discriminant));
         Set_Node9 (Id, Formal);
      end Set_Discriminal;

      ------------------------------------
      -- Set_Discriminant_Default_Value --
      ------------------------------------

      procedure Set_Discriminant_Default_Value
        (Id : Entity_Id; Val : Node_Id) is
      begin
         --  It is too strict to include a call to Check_Entity_Kind
         --  (E_Discriminant) here as a debugging check since at the
         --  point the default value is set the discriminant has
         --  an Ekind of E_Void to prevent its premature use. So simply
         --  check that it is a defining identifier.

         pragma Debug (Check_Defining_Occurrence (Id));

         Set_Node10 (Id, Val);
      end Set_Discriminant_Default_Value;

      ------------------------------------
      -- Set_Discriminant_Checking_Func --
      ------------------------------------

      procedure Set_Discriminant_Checking_Func
        (Id : Entity_Id; Proc_Id : Entity_Id) is
      begin
         pragma Debug (Check_Entity_Kind (Id, E_Component));
         pragma Assert
           (Ekind (Scope (Id)) in Record_Kind, Compiler_Abort (Id));
         Set_Node10 (Id, Proc_Id);
      end Set_Discriminant_Checking_Func;

      -----------------------
      -- Set_Default_Value --
      -----------------------

      procedure Set_Default_Value (In_Id : Entity_Id; Val : Node_Id) is
      begin
         pragma Debug (Check_Entity_Kind (In_Id, E_In_Parameter));
         Set_Node10 (In_Id, Val);
      end Set_Default_Value;

      -----------------------------
      -- Set_Is_Directly_Visible --
      -----------------------------

      procedure Set_Is_Directly_Visible
        (Id : Entity_Id; Status : Boolean := True) is
      begin
         Set_Flag7 (Id, Status);
      end Set_Is_Directly_Visible;

      ------------------------
      -- Set_Is_Use_Visible --
      ------------------------

      procedure Set_Is_Use_Visible
        (Id : Entity_Id; Status : Boolean := True) is
      begin
         Set_Flag9 (Id, Status);
      end Set_Is_Use_Visible;

      ---------------------
      -- Set_Next_Actual --
      ---------------------

      procedure Set_Next_Actual (Ass1_Id : Node_Id; Ass2_Id : Node_Id) is
      begin
         if Nkind (Parent (Ass1_Id)) = N_Parameter_Association then
            Set_Node4 (Parent (Ass1_Id), Ass2_Id);
         else
            null;
         end if;
      end Set_Next_Actual;

      ---------------
      -- Set_Alias --
      ---------------

      procedure Set_Alias (Overloaded_Id : Entity_Id; Id : Entity_Id) is
      begin
         pragma Debug (Check_Overloaded_Kind (Overloaded_Id));
         Set_Node7 (Overloaded_Id, Id);
      end Set_Alias;

      --------------------------
      -- Set_Needs_No_Actuals --
      --------------------------

      procedure Set_Needs_No_Actuals
        (Overloaded_Id : Entity_Id; Status : Boolean := True) is
      begin
         pragma Debug (Check_Overloaded_Kind (Overloaded_Id));
         Set_Flag8 (Overloaded_Id, Status);
      end Set_Needs_No_Actuals;

      ----------------------
      -- Set_Is_Intrinsic --
      ----------------------

      procedure Set_Is_Intrinsic (Proc_Id : Entity_Id) is
      begin
         pragma Debug (Check_Defining_Occurrence (Proc_Id));
         Set_Flag27 (Proc_Id, True);
      end Set_Is_Intrinsic;

      ------------------------
      -- Set_Interface_Name --
      ------------------------

      procedure Set_Interface_Name (Proc_Id : Entity_Id; Lit_Node : Node_Id) is
      begin
         pragma Debug (Check_Defining_Occurrence (Proc_Id));
         Set_Node12 (Proc_Id, Lit_Node);
      end Set_Interface_Name;

      ---------------------
      -- Set_Entry_Index --
      ---------------------

      procedure Set_Entry_Index (Id : Entity_Id; Ev : Uint) is
      begin
         Set_Uint11 (Id, Ev);
      end Set_Entry_Index;

      -------------------------
      -- Set_Enumeration_Pos --
      -------------------------

      procedure Set_Enumeration_Pos (Enum_Id : Entity_Id; Ev : Uint) is
      begin
         Set_Uint11 (Enum_Id, Ev);
      end Set_Enumeration_Pos;

      -------------------------
      -- Set_Enumeration_Rep --
      -------------------------

      procedure Set_Enumeration_Rep (Enum_Id : Entity_Id; Ev : Uint) is
      begin
         Set_Uint12 (Enum_Id, Ev);
      end Set_Enumeration_Rep;

      ---------------------
      -- Set_Slice_Range --
      ---------------------

      procedure Set_Slice_Range (Id : Entity_Id; R_Node : Node_Id) is
      begin
         pragma Debug (Check_Entity_Kind (Id, E_Slice_Subtype));
         Set_Node11 (Id, R_Node);
      end Set_Slice_Range;

      -------------------------------
      -- Set_String_Literal_Length --
      -------------------------------

      procedure Set_String_Literal_Length (Id : Entity_Id; Val : Uint) is
      begin
         pragma Debug (Check_Entity_Kind (Id, E_String_Literal_Subtype));
         Set_Uint11 (Id, Val);
      end Set_String_Literal_Length;

      ---------------------------
      -- Set_Table_High_Bounds --
      ---------------------------

      procedure Set_Table_High_Bound (Id : Entity_Id; Val : Node_Id) is
      begin
         pragma Debug (Check_Entity_Kind (Id, E_Enum_Table_Type));
         Set_Node11 (Id, Val);
      end Set_Table_High_Bound;

      -----------------------------
      -- Set_Has_Subprogram_Body --
      -----------------------------

      procedure Set_Has_Subprogram_Body
        (Subprogram_Id : Entity_Id; Status : Boolean := True) is
      begin
         Set_Flag5 (Subprogram_Id, Status);
      end Set_Has_Subprogram_Body;

      -------------------------
      -- Set_Is_Package_Body --
      -------------------------

      procedure Set_Is_Package_Body
        (Package_Id : Entity_Id; Status : Boolean := True) is
      begin
         Set_Flag5 (Package_Id, Status);
      end Set_Is_Package_Body;

      -------------------------
      -- Set_In_Private_Part --
      -------------------------

      procedure Set_In_Private_Part
        (Package_Id : Entity_Id; Status : Boolean := True)  is
      begin
         Set_Flag4 (Package_Id, Status);
      end Set_In_Private_Part;

      ----------------
      -- Set_In_Use --
      ----------------

      procedure Set_In_Use
        (Package_Id : Entity_Id; Status : Boolean := True) is
      begin
         Set_Flag8 (Package_Id, Status);
      end Set_In_Use;


      -------------------------------
      -- Set_Is_Private_Descendant --
      -------------------------------

      procedure Set_Is_Private_Descendant
        (Id : Entity_Id; Status : Boolean := True) is
      begin
         Set_Flag14 (Id, Status);
      end Set_Is_Private_Descendant;

      -----------------------------------
      -- Set_Original_Record_Component --
      -----------------------------------

      procedure Set_Original_Record_Component
        (Id : Entity_Id; Orig_Id : Entity_Id) is
      begin
         --  needs double check E_Component or E_Discriminant ???
         --  pragma Debug (Check_Entity_Kind (Id, E_Component));
         Set_Node8 (Id, Orig_Id);
      end Set_Original_Record_Component;

      -------------------------
      -- Set_Task_Value_Type --
      -------------------------

      procedure Set_Task_Value_Type (Id : Entity_Id; Ent : Entity_Id) is
      begin
         pragma Debug (Check_Defining_Occurrence (Id));
         pragma Debug (Check_Defining_Occurrence (Ent));
         Set_Node7 (Id, Ent);
      end Set_Task_Value_Type;

      -------------------
      -- Append_Entity --
      -------------------

      procedure Append_Entity (Id : Entity_Id; Scope_Id : Entity_Id) is
      begin
         pragma Debug (Check_Defining_Occurrence (Id));

         if Last_Entity (Scope_Id) = Empty then
            Set_First_Entity (Scope_Id, Id);
         else
            Set_Next_Entity (Last_Entity (Scope_Id), Id);
         end if;

         Set_Next_Entity (Id, Empty);
         Set_Scope (Id, Scope_Id);
         Set_Node10 (Scope_Id, Id);
      end Append_Entity;

      ---------------------
      -- Set_Last_Entity --
      ---------------------

      procedure Set_Last_Entity (Scope_Id : Entity_Id; Id : Entity_Id) is
      begin
         pragma Debug (Check_Defining_Occurrence (Scope_Id));
         Set_Node10 (Scope_Id, Id);
      end Set_Last_Entity;

      -------------------
      -- Set_Master_Id --
      -------------------

      procedure Set_Master_Id (Access_Id : Entity_Id; Id : Entity_Id) is
      begin
         pragma Debug (Check_Defining_Occurrence (Access_Id));
         Set_Node9 (Access_Id, Id);
      end Set_Master_Id;

      ------------------------------
      -- Set_First_Private_Entity --
      ------------------------------

      procedure Set_First_Private_Entity
        (Package_Id : Entity_Id; Id : Entity_Id) is
      begin
         Set_Node11 (Package_Id, Id);
      end Set_First_Private_Entity;

      --------------------------------
      -- Set_Suppress_Access_Checks --
      --------------------------------

      procedure Set_Suppress_Access_Checks 
        (E : Entity_Id; V : Boolean := True) is
      begin
         Set_Flag31 (E, V);
      end Set_Suppress_Access_Checks;

      ---------------------------------------
      -- Set_Suppress_Accessibility_Checks --
      ---------------------------------------

      procedure Set_Suppress_Accessibility_Checks
        (E : Entity_Id; V : Boolean := True) is
      begin
         Set_Flag32 (E, V);
      end Set_Suppress_Accessibility_Checks;

      --------------------------------------
      -- Set_Suppress_Discriminant_Checks --
      --------------------------------------

      procedure Set_Suppress_Discriminant_Checks
        (E : Entity_Id; V : Boolean := True) is
      begin
         Set_Flag33 (E, V);
      end Set_Suppress_Discriminant_Checks;

      ----------------------------------
      -- Set_Suppress_Division_Checks --
      ----------------------------------

      procedure Set_Suppress_Division_Checks 
        (E : Entity_Id; V : Boolean := True) is
      begin
         Set_Flag34 (E, V);
      end Set_Suppress_Division_Checks;

      -------------------------------------
      -- Set_Suppress_Elaboration_Checks --
      -------------------------------------

      procedure Set_Suppress_Elaboration_Checks 
        (E : Entity_Id; V : Boolean := True) is
      begin
         Set_Flag35 (E, V);
      end Set_Suppress_Elaboration_Checks;

      -------------------------------
      -- Set_Suppress_Index_Checks --
      -------------------------------

      procedure Set_Suppress_Index_Checks
        (E : Entity_Id; V : Boolean := True) is
      begin
         Set_Flag36 (E, V);
      end Set_Suppress_Index_Checks;

      --------------------------------
      -- Set_Suppress_Length_Checks --
      --------------------------------

      procedure Set_Suppress_Length_Checks
        (E : Entity_Id; V : Boolean := True) is
      begin
         Set_Flag37 (E, V);
      end Set_Suppress_Length_Checks;

      ----------------------------------
      -- Set_Suppress_Overflow_Checks --
      ----------------------------------

      procedure Set_Suppress_Overflow_Checks
        (E : Entity_Id; V : Boolean := True) is
      begin
         Set_Flag38 (E, V);
      end Set_Suppress_Overflow_Checks;

      -------------------------------
      -- Set_Suppress_Range_Checks --
      -------------------------------

      procedure Set_Suppress_Range_Checks
        (E : Entity_Id; V : Boolean := True) is
      begin
         Set_Flag39 (E, V);
      end Set_Suppress_Range_Checks;

      ---------------------------------
      -- Set_Suppress_Storage_Checks --
      ---------------------------------

      procedure Set_Suppress_Storage_Checks
        (E : Entity_Id; V : Boolean := True) is
      begin
         Set_Flag40 (E, V);
      end Set_Suppress_Storage_Checks;

      -----------------------------
      -- Set_Suppress_Tag_Checks --
      -----------------------------

      procedure Set_Suppress_Tag_Checks
        (E : Entity_Id; V : Boolean := True) is
      begin
         Set_Flag41 (E, V);
      end Set_Suppress_Tag_Checks;

      -------------------------
      -- Set_Equivalent_Type --
      -------------------------

      procedure Set_Equivalent_Type (Sub_Class : Entity_Id; Typ : Entity_Id) is
      begin
         pragma Assert (Ekind (Sub_Class) = E_Class_Subtype,
                        Compiler_Abort (Sub_Class));
         Set_Node7 (Sub_Class, Typ);
      end Set_Equivalent_Type;

      -------------------
      -- Is_Class_Type --
      -------------------

      function Is_Class_Type (T : Entity_Id) return Boolean is 
      begin
         pragma Debug (Check_Defining_Occurrence (T));
         return Ekind (T) = E_Class_Type or else Ekind (T) = E_Class_Subtype; 
      end Is_Class_Type;

      --------------------
      -- Classwide_Type --
      --------------------

      function Classwide_Type (T : Entity_Id) return Entity_Id is
         E : Entity_Id;
      begin
         --  The Class_Type of a type is introduced after the type declaration.
         --  Scan entity list forward from the type.

         E := Next_Entity (T);
         while not Is_Class_Type (E)
           or else Etype (E) /= T
         loop
            E := Next_Entity (E);
         end loop;

         pragma Assert (Present (E));
         return E;
      end Classwide_Type;

      -----------------------
      -- Write_Entity_Info --
      -----------------------

      procedure Write_Entity_Info (Id : Entity_Id; Prefix : Str) is

         procedure Write_Kind (Id : Entity_Id) is
            K : constant String := Entity_Kind'Image (Ekind (Id));

         begin
            Write_Str (Prefix);
            Write_Str ("   Kind    ");
            if Is_Type (Id) and then Is_Tagged_Type (Id) then
               Write_Str ("TAGGED ");
            end if;
            Write_String (K (3 .. K'Length));
            Write_Str (" ");
            if Is_Type (Id) and then Is_Private_Type (Id) then
               Write_Str ("Is_Private_Type ");
            end if;
         end Write_Kind;

         procedure Write_Attribute (Which : Str; Nam : Entity_Id) is
         begin
            Write_Str (Prefix);
            Write_Str (Which);
            Write_Int (Int (Nam));
            Write_Str (" ");
            Write_Name (Chars (Nam));
            Write_Str (" ");
         end Write_Attribute;

      begin
         Write_Eol;
         Write_Attribute ("Name ", Id);
         Write_Int (Int (Id));
         Write_Eol;
         Write_Kind (Id);
         Write_Eol;
         Write_Attribute ("   Type    ", Etype (Id));
         Write_Eol;
         Write_Attribute ("   Scope   ", Scope (Id));
         Write_Eol;

         case Ekind (Id) is

            when Discrete_Kind =>
               Write_Str ("Bounds: Id = ");

               if Present (Scalar_Range (Id)) then
                  Write_Int (Int (Type_Low_Bound (Id)));
                  Write_Str (" .. Id = ");
                  Write_Int (Int (Type_High_Bound (Id)));
               else
                  Write_Str ("Empty");
               end if;

               Write_Eol;

            when Array_Kind =>
               declare
                  Index : Entity_Id;

               begin
                  Write_Attribute ("   Component Type    ",
                                                      Component_Type (Id));
                  Write_Eol;
                  Write_Str (Prefix);
                  Write_Str ("   Indices ");

                  Index := First_Index (Id);

                  while Present (Index) loop
                     Write_Attribute (" ", Etype (Index));
                     Index := Next_Index (Index);
                  end loop;

                  Write_Eol;
               end;

            when Access_Kind =>
                  Write_Attribute
                    ("   Directly Designated Type ",
                     Directly_Designated_Type (Id));
                  Write_Eol;

            when Overloaded_Kind =>
               if Present (Homonym (Id)) then
                  Write_Str ("   Homonym   ");
                  Write_Name (Chars (Homonym (Id)));
                  Write_Str ("   ");
                  Write_Int (Int (Homonym (Id)));
                  Write_Eol;
               end if;

               Write_Eol;

            when E_Component =>
               if Ekind (Scope (Id)) in Record_Kind then
                  Write_Attribute (
                     "   Original_Record_Component   ",
                     Original_Record_Component (Id));
                  Write_Int (Int (Original_Record_Component (Id)));
                  Write_Eol;
               end if;

            when others => null;
         end case;
      end Write_Entity_Info;
end Einfo;
