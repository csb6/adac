------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                S T A N D                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.79 $                             --
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
with Csets;   use Csets;
with Einfo;   use Einfo;
with Nmake;   use Nmake;
with Opt;     use Opt;
with Ttypes;  use Ttypes;
with Stringt; use Stringt;
with Sinfo;   use Sinfo;
with Snames;  use Snames;
with Uintp;   use Uintp;

package body Stand is

--  WARNING! If any change is made to this module which would affect the format
--  of library information, then the following standard version string must be
--  updated so that previously compiled library modules will be recognized as
--  obsolete and rejected by the library manager.

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Identifier_For (S : Standard_Entity_Type) return Node_Id;
   --  Returns an identifier node with the same name as the defining
   --  identifier corresponding to the given Standard_Entity_Type value

   procedure Make_Name (Id : Entity_Id; Nam : String);
   --  Make an entry in the names table for Nam, and set as Chars field of Id

   procedure Create_Operators;
   --  Make entries for each of the predefined operators in Standard

   ----------------------
   --  Create_Standard --
   ----------------------

   procedure Create_Standard is

      --  The AST for the package Standard is prefixed to all compilations.
      --  Several entities required by semantic analysis are denoted by global
      --  variables that are initialized to point to the corresponding
      --  occurences in STANDARD. The visible entities of STANDARD are
      --  created here. The private entities defined in STANDARD are created
      --  by Initialize_Standard in the semantics module.

      Decl_S : List_Id;     -- list of declarations in Standard
      Decl_A : List_Id;     -- list of declarations in Ascii
      Decl : Node_Id;       -- node for full type declaration
      Pspec : Node_Id;      -- node for package specification node
      Tdef_Node : Node_Id;  -- node for type definition
      Ident_Node : Node_Id; -- identifier node
      Ccode : Char_Code;    -- character code value, used for values in Ascii
      E_Id   : Entity_Id;   -- temporary entity id
      R_Node : Node_Id;     -- range node to hold bounds of scalar types
      B_Node : Node_Id;     -- to hold each bound.

      Uint_31 : Uint := UI_From_Int (31);

      --  Procedure to set bounds for integer type or subtype. Id is the entity
      --  whose bounds and type are to be set. The Typ parameter is the Etype
      --  value for the entity (which will be the same as Id for all predefined
      --  integer base types. The third and fourth parameters are the bounds.

      procedure Set_Integer_Bounds
        (Id : Entity_Id; Typ : Entity_Id; Lb, Hb : Uint)
      is
         L, H : Node_Id;         -- to hold literal values of bounds
         R    : Node_Id;         -- Range specification

      begin
         L := Make_Integer_Literal (Standard_Location, Lb);
         Set_Etype (L, Typ);
         Set_Is_Evaluated (L, True);
         Set_Is_Static (L, True);

         H := Make_Integer_Literal (Standard_Location, Hb);
         Set_Etype (H, Typ);
         Set_Is_Evaluated (H, True);
         Set_Is_Static (H, True);

         R := New_Node (N_Range, Standard_Location);
         Set_Low_Bound  (R, L);
         Set_High_Bound (R, H);
         Set_Scalar_Range (Id, R);
      end Set_Integer_Bounds;

      --  Procedure to declare given entity as an exception

      procedure Build_Exception (S : Standard_Entity_Type) is
      begin
         Set_Ekind (Standard_Entity (S), E_Exception);
         Set_Etype (Standard_Entity (S), Standard_Exception_Type);
         Set_Is_Public (Standard_Entity (S));
         Decl :=
           Make_Exception_Declaration (Standard_Location,
             Defining_Identifier => Standard_Entity (S));
         Append (Decl, Decl_S);
      end Build_Exception;

      --  Procedure to build standard predefined integer base type. The first
      --  parameter is the entity for the type, and the second parameter is
      --  the size in bits.

      procedure Build_Integer_Type (E : Entity_Id; Siz : Int) is
         Lbound : Uint;
         Ubound : Uint;

      begin
         Ubound := UI_Difference (UI_Power (Siz - 1), Uint_1);

         if Ones_Complement then
            Lbound := UI_Negate (Ubound);
         else
            Lbound := UI_Negate (UI_Power (Siz - 1));
         end if;

         Set_Type_Definition (Parent (E),
           Make_Signed_Integer_Type_Definition (Standard_Location,
             Low_Bound  => Make_Integer_Literal (Standard_Location, Lbound),
             High_Bound => Make_Integer_Literal (Standard_Location, Ubound)));

         Set_Ekind (E, E_Integer_Type);
         Set_Etype (E, E);
         Set_Esize (E, UI_From_Int (Siz));
         Set_Integer_Bounds (E, E, Lbound, Ubound);
      end Build_Integer_Type;

      --  Procedure to build standard predefined float base type. The first
      --  parameter is the entity for the type, and the second parameter
      --  is the size in bits. The third parameter is the digits value.

      procedure Build_Float_Type (E : Entity_Id; Siz : Int; Digs : Int) is
      begin
         Set_Type_Definition (Parent (E),
           Make_Floating_Point_Definition (Standard_Location,
             Digits_Expression =>
               Make_Integer_Literal (Standard_Location,
                 Intval => UI_From_Int (Digs))));

         Set_Ekind (E, E_Float_Type);
         Set_Etype (E, E);
         Set_Esize (E, UI_From_Int (Siz));
      end Build_Float_Type;

   --  Start of processing for Create_Standard

   begin
      Decl_S := New_List;

      --  First step is to create defining identifiers for each entity

      for S in Standard_Entity_Type loop
         declare
            S_Name : constant String := Standard_Entity_Type'Image (S);
            --  Name of entity (note we skip S_ at the start)

            Ident_Node : Node_Id;
            --  Defining identifier node

         begin
            Ident_Node := Extend_Node (New_Node (N_Defining_Identifier,
                                                        Standard_Location));
            Make_Name (Ident_Node, S_Name (3 .. S_Name'Length));
            Standard_Entity (S) := Ident_Node;
         end;
      end loop;

      --  Create package declaration node for package Standard

      Standard_Package_Node :=
        New_Node (N_Package_Declaration, Standard_Location);

      Pspec := New_Node (N_Package_Specification, Standard_Location);
      Set_Specification (Standard_Package_Node, Pspec);

      Set_Defining_Unit_Name (Pspec, Standard_Standard);
      Set_Visible_Declarations (Pspec, Decl_S);

      Set_Ekind (Standard_Standard, E_Package);

      --  Create type declaration nodes for standard types

      for S in S_Types loop
         Decl := New_Node (N_Full_Type_Declaration, Standard_Location);
         Set_Defining_Identifier (Decl, Standard_Entity (S));
         Append (Decl, Decl_S);
      end loop;

      --  Create type definition node for type Boolean. The Size is set to
      --  1 as required by Ada/9X and current ARG interpretations for Ada/83.

      Tdef_Node := New_Node (N_Enumeration_Type_Definition, Standard_Location);
      Set_Literals (Tdef_Node, New_List);
      Append (Standard_False, Literals (Tdef_Node));
      Append (Standard_True, Literals (Tdef_Node));
      Set_Type_Definition (Parent (Standard_Boolean), Tdef_Node);

      Set_Ekind (Standard_Boolean, E_Boolean_Type);
      Set_First_Literal (Standard_Boolean, Standard_False);
      Set_Etype (Standard_Boolean, Standard_Boolean);
      Set_Esize (Standard_Boolean, UI_From_Int (1));

      Set_Ekind (Standard_True, E_Enumeration_Literal);
      Set_Etype (Standard_True, Standard_Boolean);
      Set_Enumeration_Pos (Standard_True, Uint_1);
      Set_Enumeration_Rep (Standard_True, Uint_1);

      Set_Ekind (Standard_False, E_Enumeration_Literal);
      Set_Etype (Standard_False, Standard_Boolean);
      Set_Enumeration_Pos (Standard_False, Uint_0);
      Set_Enumeration_Rep (Standard_False, Uint_0);

      --  For the bounds of Boolean, we create a range node corresponding to
      --    range False .. True
      --  where the occurrences of the literals must point to the
      --  corresponding  definition.

      R_Node := New_Node (N_Range, Standard_Location);
      B_Node := New_Node (N_Identifier, Standard_Location);
      Set_Chars (B_Node, Chars (Standard_False));
      Set_Entity (B_Node,  Standard_False);
      Set_Etype (B_Node, Standard_Boolean);
      Set_Low_Bound  (R_Node, B_Node);

      B_Node := New_Node (N_Identifier, Standard_Location);
      Set_Chars (B_Node, Chars (Standard_True));
      Set_Entity (B_Node,  Standard_True);
      Set_Etype (B_Node, Standard_Boolean);
      Set_High_Bound (R_Node, B_Node);

      Set_Scalar_Range (Standard_Boolean, R_Node);

      --  Create type definition nodes for predefined integer types

      Build_Integer_Type
        (Standard_Short_Short_Integer, Standard_Short_Short_Integer_Size);
      Build_Integer_Type
        (Standard_Short_Integer, Standard_Short_Integer_Size);
      Build_Integer_Type
        (Standard_Integer, Standard_Integer_Size);
      Build_Integer_Type
        (Standard_Long_Integer, Standard_Long_Integer_Size);
      Build_Integer_Type
        (Standard_Long_Long_Integer, Standard_Long_Long_Integer_Size);

      --  Create type definition nodes for predefined float types

      Build_Float_Type
        (Standard_Short_Float,
         Standard_Short_Float_Size,
         Standard_Short_Float_Digits);

      Build_Float_Type
        (Standard_Float,
         Standard_Float_Size,
         Standard_Float_Digits);

      Build_Float_Type
        (Standard_Long_Float,
         Standard_Long_Float_Size,
         Standard_Long_Float_Digits);

      Build_Float_Type
        (Standard_Long_Long_Float,
         Standard_Long_Long_Float_Size,
         Standard_Long_Long_Float_Digits);

      --  Create type definition node for type Character. The size is 8 bits.
      --  Note that we do not set the Literals field, since type Character is
      --  handled with special routines that do not need a literal list.

      Tdef_Node := New_Node (N_Enumeration_Type_Definition, Standard_Location);
      Set_Type_Definition (Parent (Standard_Character), Tdef_Node);
      Set_Ekind (Standard_Character, E_Character_Type);
      Set_Etype (Standard_Character, Standard_Character);
      Set_Esize (Standard_Character, UI_From_Int (Standard_Character_Size));

      --  Create the bounds for type Character.

      R_Node := New_Node (N_Range, Standard_Location);

      --  Low bound for type Character (Standard.Nul)

      B_Node := New_Node (N_Character_Literal, Standard_Location);
      Set_Chars (B_Node, No_Name);
      Set_Char_Literal_Value (B_Node, 16#00#);
      Set_Entity (B_Node,  Empty);
      Set_Etype (B_Node, Standard_Character);
      Set_Low_Bound (R_Node, B_Node);

      --  High bound for type Character

      B_Node := New_Node (N_Character_Literal, Standard_Location);
      Set_Chars (B_Node, No_Name);
      Set_Char_Literal_Value (B_Node, 16#FF#);
      Set_Entity (B_Node,  Empty);
      Set_Etype (B_Node, Standard_Character);
      Set_High_Bound (R_Node, B_Node);

      Set_Scalar_Range (Standard_Character, R_Node);

      --  Create type definition node for type String

      Tdef_Node :=
        New_Node (N_Unconstrained_Array_Definition, Standard_Location);
      Set_Subtype_Indication (Tdef_Node, Identifier_For (S_Character));
      Set_Subtype_Marks (Tdef_Node, New_List);
      Append (Identifier_For (S_Positive), Subtype_Marks (Tdef_Node));
      Set_Type_Definition (Parent (Standard_String), Tdef_Node);

      Set_Ekind (Standard_String, E_String_Type);
      Set_Etype (Standard_String, Standard_String);
      Set_Component_Type (Standard_String, Standard_Character);

      --  Set index_type of String

      E_Id := First
        (Subtype_Marks (Type_Definition (Parent (Standard_String))));
      Set_First_Index (Standard_String, E_Id);
      Set_Entity (E_Id, Standard_Positive);
      Set_Etype (E_Id, Standard_Positive);

      --  Create subtype declaration for Natural

      Decl := New_Node (N_Subtype_Declaration, Standard_Location);
      Set_Defining_Identifier (Decl, Standard_Natural);
      Append (Decl, Decl_S);

      Set_Ekind (Standard_Natural, E_Integer_Subtype);
      Set_Etype (Standard_Natural, Standard_Integer);
      Set_Esize (Standard_Natural, Esize (Standard_Integer));
      Set_Integer_Bounds
        (Id  => Standard_Natural,
         Typ => Standard_Integer,
         Lb  => Uint_0,
         Hb  => Intval (High_Bound (Scalar_Range (Standard_Integer))));

      --  Create subtype declaration for Positive

      Decl := New_Node (N_Subtype_Declaration, Standard_Location);
      Set_Defining_Identifier (Decl, Standard_Positive);
      Append (Decl, Decl_S);

      Set_Ekind (Standard_Positive, E_Integer_Subtype);
      Set_Etype (Standard_Positive, Standard_Integer);
      Set_Esize (Standard_Positive, Esize (Standard_Integer));
      Set_Integer_Bounds
        (Id  => Standard_Positive,
         Typ => Standard_Integer,
         Lb  => Uint_1,
         Hb  => Intval (High_Bound (Scalar_Range (Standard_Integer))));

      --  Create subtype declaration for Duration

      Decl := New_Node (N_Subtype_Declaration, Standard_Location);
      Set_Defining_Identifier (Decl, Standard_Duration);
      Append (Decl, Decl_S);

      Set_Ekind (Standard_Duration, E_Float_Subtype);
      Set_Etype (Standard_Duration, Standard_Float);
      Set_Esize (Standard_Duration, Esize (Standard_Float));

      --  Create declaration for package Ascii

      Decl := New_Node (N_Package_Declaration, Standard_Location);
      Append (Decl, Decl_S);

      Pspec := New_Node (N_Package_Specification, Standard_Location);
      Set_Specification (Decl, Pspec);

      Set_Defining_Unit_Name (Pspec, Standard_Entity (S_Ascii));
      Set_Ekind (Standard_Entity (S_Ascii), E_Package);
      Decl_A := New_List; -- for ASCII declarations
      Set_Visible_Declarations (Pspec, Decl_A);

      --  Create control character definitions in package ASCII. Note that
      --  the character literal entries created here correspond to literal
      --  values that are impossible in the source, but can be represented
      --  internally with no difficulties.

      Ccode := 16#00#;

      for S in S_Ascii_Names loop
         Decl := New_Node (N_Object_Declaration, Standard_Location);
         Set_Constant_Present (Decl, True);

         declare
            A_Char : Entity_Id := Standard_Entity (S);

         begin
            Set_Ekind (A_Char, E_Constant);
            Set_Etype (A_Char, Standard_Character);
            Set_Scope (A_Char, Standard_Entity (S_Ascii));
            Set_Is_Directly_Visible (A_Char, False);
            Set_Is_Public (A_Char);
            Append_Entity (A_Char, Standard_Entity (S_Ascii));
            Set_Defining_Identifier (Decl, A_Char);
         end;

         Set_Object_Definition (Decl, Identifier_For (S_Character));
         Set_Expression
           (Decl, New_Node (N_Character_Literal, Standard_Location));
         Set_Chars (Expression (Decl), No_Name);
         Set_Etype (Expression (Decl), Standard_Character);
         Set_Char_Literal_Value (Expression (Decl), Ccode);
         Append (Decl, Decl_A);

         --  Increment character code, dealing with non-contiguities

         Ccode := Ccode + 1;

         if Ccode = 16#20# then
            Ccode := 16#21#;
         elsif Ccode = 16#27# then
            Ccode := 16#3A#;
         elsif Ccode = 16#3C# then
            Ccode := 16#3F#;
         elsif Ccode = 16#41# then
            Ccode := 16#5B#;
         end if;
      end loop;

      --  Create semantic phase entities

      Standard_Void_Type := Extend_Node (New_Node (N_Defining_Identifier,
                                                   Standard_Location));
      Set_Ekind (Standard_Void_Type, E_Void);
      Set_Etype (Standard_Void_Type, Standard_Void_Type);
      Set_Esize (Standard_Void_Type, Uint_0);
      Set_Scope (Standard_Void_Type, Standard_Standard);
      Make_Name (Standard_Void_Type, "_void_type");

      --  The type field of packages is set to void

      Set_Etype (Standard_Standard, Standard_Void_Type);
      Set_Etype (Standard_Ascii, Standard_Void_Type);

      Standard_A_String :=
        Extend_Node (New_Node (N_Defining_Identifier, Standard_Location));
      Set_Ekind (Standard_A_String, E_Access_Type);
      Set_Directly_Designated_Type (Standard_A_String, Standard_String);
      Set_Scope (Standard_A_String, Standard_Standard);
      Set_Etype (Standard_A_String, Standard_A_String);
      Make_Name (Standard_A_String, "_Access_String");

      --  Note on type names. The type names for the following special types
      --  are constructed so that they will look reasonable in error messages
      --  such as "some integer type" expected here. The blanks would cause
      --  trouble in Gigi, but that's OK here, since none of these types
      --  should ever get through to Gigi!

      Any_Type :=
        Extend_Node (New_Node (N_Defining_Identifier, Standard_Location));
      Set_Ekind (Any_Type, E_Integer_Type);
      Set_Scope (Any_Type, Standard_Standard);
      Set_Etype (Any_Type, Any_Type);
      Set_Esize (Any_Type, Uint_0);
      Make_Name (Any_Type, "any type");

      Any_Id :=
        Extend_Node (New_Node (N_Defining_Identifier, Standard_Location));
      Set_Ekind (Any_Id, E_Variable);
      Set_Scope (Any_Id, Standard_Standard);
      Set_Etype (Any_Id, Any_Type);
      Make_Name (Any_Id, "any id");

      Any_Integer :=
        Extend_Node (New_Node (N_Defining_Identifier, Standard_Location));
      Set_Ekind (Any_Integer, E_Integer_Type);
      Set_Scope (Any_Integer, Standard_Standard);
      Set_Etype (Any_Integer, Standard_Longest_Runtime_Integer);
      Make_Name (Any_Integer, "some integer type");
      Set_Esize (Any_Integer, Esize (Standard_Longest_Runtime_Integer));

      Universal_Integer :=
        Extend_Node (New_Node (N_Defining_Identifier, Standard_Location));
      Set_Ekind (Universal_Integer, E_Integer_Type);
      Set_Scope (Universal_Integer, Standard_Standard);
      Set_Etype (Universal_Integer, Universal_Integer);
      Make_Name (Universal_Integer, "universal integer");

      Universal_Real   :=
        Extend_Node (New_Node (N_Defining_Identifier, Standard_Location));
      Set_Ekind (Universal_Real, E_Float_Type);
      Set_Scope (Universal_Real, Standard_Standard);
      Set_Etype (Universal_Real, Universal_Real);
      Make_Name (Universal_Real, "universal real");

      Any_Access :=
        Extend_Node (New_Node (N_Defining_Identifier, Standard_Location));
      Set_Ekind (Any_Access, E_Access_Type);
      Set_Scope (Any_Access, Standard_Standard);
      Set_Etype (Any_Access, Any_Access);
      Make_Name (Any_Access, "some access type");

      Any_Boolean :=
        Extend_Node (New_Node (N_Defining_Identifier, Standard_Location));
      Set_Ekind (Any_Boolean, E_Boolean_Type);
      Set_Scope (Any_Boolean, Standard_Standard);
      Set_Etype (Any_Boolean, Standard_Boolean);
      Make_Name (Any_Boolean, "some boolean type");

      Any_Character :=
        Extend_Node (New_Node (N_Defining_Identifier, Standard_Location));
      Set_Ekind (Any_Character, E_Character_Type);
      Set_Scope (Any_Character, Standard_Standard);
      Set_Etype (Any_Character, Any_Character);
      Make_Name (Any_Character, "some character type");

      Any_String := New_Copy (Standard_String);
      Set_Etype (Any_String, Any_String);
      Make_Name (Any_String, "some string type");

      Any_Composite :=
        Extend_Node (New_Node (N_Defining_Identifier, Standard_Location));
      Set_Ekind (Any_Composite, E_Array_Type);
      Set_Scope (Any_Composite, Standard_Standard);
      Set_Etype (Any_Composite, Any_Composite);
      Make_Name (Any_Composite, "some composite type");

      --  The name for the exception type avoids blanks, because this type
      --  name does get passed on to Gigi, and the assembler does not like
      --  blanks in the middle of names.

      Standard_Exception_Type :=
        Extend_Node (New_Node (N_Defining_Identifier, Standard_Location));
      Set_Ekind (Standard_Exception_Type, E_Exception_Type);
      Set_Etype (Standard_Exception_Type, Standard_Exception_Type);
      Set_Scope (Standard_Exception_Type, Standard_Standard);
      Make_Name (Standard_Exception_Type, "exception");

      --  Create declarations of standard exceptions

      Build_Exception (S_Constraint_Error);
      Build_Exception (S_Program_Error);
      Build_Exception (S_Storage_Error);
      Build_Exception (S_Tasking_Error);

      --  Numeric_Error is a normal exception in Ada 83, but in Ada 9X
      --  it is a renaming of Constraint_Error

      if Ada_83 then
         Build_Exception (S_Numeric_Error);
      else
         Decl := New_Node
           (N_Exception_Renaming_Declaration, Standard_Location);
         E_Id := Standard_Entity (S_Numeric_Error);
         Set_Ekind (E_Id, E_Exception);
         Set_Etype (E_Id, Standard_Exception_Type);
         Set_Is_Public (E_Id);
         Set_Defining_Identifier (Decl, E_Id);
         Append (Decl, Decl_S);
         Ident_Node := New_Node (N_Identifier, Standard_Location);
         Set_Chars (Ident_Node, Chars (Standard_Entity (S_Constraint_Error)));
         Set_Entity (Ident_Node, Standard_Entity (S_Constraint_Error));
         Set_Name (Decl, Ident_Node);
      end if;

      Create_Operators;

      --  Initialize visibility table with entities in Standard

      for E in Standard_Entity_Type loop
         Set_Name_Entity_Id (Chars (Standard_Entity (E)), Standard_Entity (E));
         Set_Homonym (Standard_Entity (E), Empty);

         if E not in S_Ascii_Names then
            Set_Scope (Standard_Entity (E), Standard_Standard);
            Set_Is_Directly_Visible (Standard_Entity (E));
         end if;
      end loop;

      --  Set global variables indicating last Id values and version

      Last_Standard_Node_Id   := Last_Node_Id;
      Last_Standard_List_Id   := Last_List_Id;
      Last_Standard_Name_Id   := Last_Name_Id;
      Last_Standard_String_Id := Last_String_Id;
      Last_Standard_Uint      := Last_Uint;

   end Create_Standard;

   ----------------------
   -- Create_Operators --
   ----------------------

   --  Each operator has an abbreviated signature. The formals have the names
   --  LEFT and RIGHT. Their types are not actually used for resolution.

   procedure Create_Operators is

      Op_Node : Entity_Id;
      type Op_Names is array (Positive range <>) of Name_Id;

      --  Following list excludes concatenation which must be treated specially

      Binary_Ops : constant Op_Names (1 .. 16) :=
       (Name_Op_And,      Name_Op_Mod,      Name_Op_Or,     Name_Op_Rem,
        Name_Op_Xor,      Name_Op_Eq,       Name_Op_Ne,     Name_Op_Lt,
        Name_Op_Le,       Name_Op_Gt,       Name_Op_Ge,     Name_Op_Add,
        Name_Op_Subtract, Name_Op_Multiply, Name_Op_Divide, Name_Op_Expon);

      Unary_Ops : constant Op_Names (1 .. 4) :=
        (Name_Op_Abs, Name_Op_Not, Name_Op_Add,  Name_Op_Subtract);

      function New_Operator (Op : Name_Id) return Entity_Id is
         Ident_Node : Entity_Id;
      begin
         Ident_Node := Make_Defining_Identifier (Standard_Location, Op);
         Set_Ekind (Ident_Node, E_Operator);
         Set_Etype (Ident_Node, Universal_Integer);
         Set_Scope (Ident_Node, Standard_Standard);
         Set_Homonym (Ident_Node, Get_Name_Entity_Id (Op));
         Set_Name_Entity_Id (Op, Ident_Node);
         Set_Is_Directly_Visible (Ident_Node);
         Set_Is_Intrinsic (Ident_Node);
         Append_Entity (Ident_Node, Standard_Standard);
         return Ident_Node;
      end New_Operator;

      function Make_Formal (Typ : Entity_Id; Formal_Name : String)
                                                return Entity_Id is
         Formal : Entity_Id;
      begin
         Formal :=
           Extend_Node (New_Node (N_Defining_Identifier, Standard_Location));
         Set_Ekind (Formal, E_In_Parameter);
         Set_Scope (Formal, Standard_Standard);
         Set_Etype (Formal, Typ);
         Make_Name (Formal, Formal_Name);
         return Formal;
      end Make_Formal;

   begin
      for I in Binary_Ops'range loop
         Op_Node := New_Operator (Binary_Ops (I));
         Append_Entity (Make_Formal (Any_Type, "LEFT"),  Op_Node);
         Append_Entity (Make_Formal (Any_Type, "RIGHT"), Op_Node);
      end loop;

      for I in Unary_Ops'range loop
         Op_Node := New_Operator (Unary_Ops (I));
         Append_Entity (Make_Formal (Any_Type, "RIGHT"), Op_Node);
      end loop;

      --  For concatenation, we create an operator for each one-dimensional
      --  array type. This simplifies the resolution of the component-
      --  component concatenation operation.

      Op_Node := New_Operator (Name_Op_Concat);
      Set_Etype (Op_Node, Standard_String);
      Append_Entity (Make_Formal (Standard_String, "LEFT"),  Op_Node);
      Append_Entity (Make_Formal (Standard_String, "RIGHT"), Op_Node);
   end Create_Operators;

   ---------------------
   --  Identifier_For --
   ---------------------

   function Identifier_For (S : Standard_Entity_Type) return Node_Id is
      Ident_Node : Node_Id;

   begin
      Ident_Node := New_Node (N_Identifier, Standard_Location);
      Set_Chars (Ident_Node, Chars (Standard_Entity (S)));
      return Ident_Node;
   end Identifier_For;

   ---------------
   -- Make_Name --
   ---------------

   procedure Make_Name (Id : Entity_Id; Nam : String) is
   begin
      for I in 1 .. Nam'Length loop
         Name_Buffer (Int (I)) :=
           Fold_Lower (To_Char (Nam (Nam'First + (I - 1))));
      end loop;

      Name_Len := Nam'Length;
      Set_Chars (Id, Name_Find);
   end Make_Name;

end Stand;
