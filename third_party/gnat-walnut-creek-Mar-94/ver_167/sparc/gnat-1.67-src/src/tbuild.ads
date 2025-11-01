------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               T B U I L D                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.36 $                              --
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

with Types; use Types;
package Tbuild is

--  This package contains various utility procedures to assist in
--  building specific types of tree nodes.

   procedure Create_Raise_Expression (N : Node_Id; Excep_Id : Entity_Id);
   --  This procedure converts an expression node into the special
   --  N_Expression_Actions node whose statments part is a single
   --  statement consisting of raise the predefined exception Excep_Id.

   function Make_DT_Component
     (Loc : Source_Ptr; Typ : Entity_Id; I : Positive) return Node_Id;
   --  Gives a reference to the Ith component of the Dispatch Table of
   --  a given Tagged Type.
   --
   --  I = 1    --> Inheritance_Depth
   --  I = 2    --> Tags (array of ancestors)
   --  I = 3, 4 --> predefined primitive 
   --            function _Size (X : Typ) return Long_Long_Integer;
   --            function _Equality (X : Typ; Y : Typ'Class) return Boolean;
   --  I >= 5   --> User-Defined Primitive Operations
   --    

   function Make_DT_Access
     (Loc : Source_Ptr; Rec : Node_Id; Typ : Entity_Id) return Node_Id;
   --  Create an access to the Dispatch Table by using the Tag field
   --  of a tagged record :
   --
   --    Acc_Dt (Rec.tag).all

   function Make_Unchecked_Type_Conversion
     (Sloc         : Source_Ptr;
      Subtype_Mark : Node_Id;
      Expression   : Node_Id;
      Parens       : Boolean := False)
     return Node_Id;
   --  Same as Make_Type_Conversion but set the Unchecked Flag to True.

   function New_Constraint_Error (Loc : Source_Ptr) return Node_Id;
   --  This function builds a tree corresponding to the Ada statement
   --  "raise Constraint_Error" and returns the root of this tree,
   --  the N_Raise_Statement node.

   function New_External_Name
     (Related_Id : Name_Id; Suffix : Str; Index : Nat := 0; Prefix : Str := "")
     return Name_Id;
   --  Builds a new entry in the names table of the form
   --
   --    [Prefix & "__" &] Related_Id & "___" & Suffix [& '_' & Index'Image]
   --
   --  where Prefix__ is prepended only if Prefix is non-null, and Index'Image
   --  is appended only if Index is non-zero. The constructed name is stored
   --  using Find_Name so that it can be located using a subsequent Find_Name
   --  operation (i.e. it is properly hashed into the names table). The three
   --  underscores separating Related_Id and Suffix ensure that the name does
   --  not clash with any Ada identifier name, or with any generated public
   --  name (public names use double underscores for periods). The generated
   --  names are permitted, but not required, to be made public by setting the
   --  the Is_Public flag in the associated entity. Note: the reason that
   --  Prefix is last is that it is almost always omitted. The notable case
   --  of Prefix being non-null is when it is "ityp" for an implicit type.

   function New_Internal_Name (Id_Str : Str) return Name_Id;
   --  Returns a new name of the form x__y where x is the given string and y is
   --  a serial number so that names created by New_Internal_Name are unique.
   --  The double underscore guarantees that the resulting name does not
   --  clash with any Ada identifier. The name is entered into the names
   --  table using Name_Enter rather than Name_Find, because there can never
   --  be a need to locate the entry using the Name_Find procedure later on.
   --  Names created by New_Internal_Name must never have the Is_Public flag
   --  set. This is because there is no guarantee that the same serial number
   --  will be used when the same unit is included in different compilations.
   --  If a public name is required, New_External_Name must be used instead.

   function New_Occurrence_Of
     (Def_Id : Entity_Id; Loc : Source_Ptr) return Node_Id;
   --  New_Occurrence_Of creates an N_Identifier node which is an
   --  occurrence of the defining identifier which is passed as its
   --  argument. The Entity and Etype of the result are set from
   --  the given defining identifier.

   function New_Reference_To
     (Def_Id : Entity_Id; Loc : Source_Ptr) return Node_Id;
   --  This is like New_Occurrence_Of, but it does not set the Etype field.
   --  It is used from the expander, where Etype fields are generally not set,
   --  since they are set when the expanded tree is reanalyzed.

   procedure Raise_Warning (N : Node_Id; Excep_Id : Entity_Id; Reason : Str);
   --  This procedure posts a warning message on node N stating that
   --  exception Excep_Id will be raised at run-time. The Reason string,
   --  (which should end with the characters ?!, since these are warning
   --  messages that should appear unconditionally) is used to provide an
   --  additional message which precedes the main message.

   procedure Rewrite_Int_Literal (N : Node_Id; Val : Uint);
   --  Rewrite_Int_Literal replaces node N, which is an expression node of
   --  some integer type, with an N_Integer_Literal node whose Intval field
   --  is set to Val. The Is_Static field of the new N_Integer_Literal node
   --  is set using the previous value of the setting for N. The replacement
   --  uses the tree rewrite mechanism described in the tree package so that
   --  the original tree for the expression can still be recoverd with the
   --  Original_Node function. This procedure can be used for such things
   --  as compile-time expression evaluation and replacement of named
   --  numbers where the original tree needs to be preserved.

   procedure Rewrite_Real_Literal
     (N : Node_Id; Num, Den : Uint; Decimal_Flag : Boolean);
   --  Rewrite_Real_Literal replaces node N, which is an expression node of
   --  some real type, with an N_Real_Literal node whose Numerator and
   --  Denominator fields are set to Num and Den. The Is_Decimal field is
   --  set to Decimal_Flag. The Is_Static field of the new N_Real_Literal
   --  node is set using the previous value of the setting for N. The
   --  replacement uses the tree rewrite mechanism described in the tree
   --  package so that the original tree for the expression can still be
   --  recoverd with the Original_Node function. This procedure can be used
   --  for such things as compile-time expression evaluation and replacement
   --  of named numbers where the original tree needs to be preserved.

   procedure Rewrite_Enum_Literal (N : Node_Id; Val : Entity_Id);
   --  Rewrite_Enum_Literal replaces node N, which is some expression node
   --  with an N_Identifier node whose Entity field is set to Val. The
   --  Is_Static field of the new N_Integer_Literal node is set using the
   --  previous value of the setting for N. Val represents some defining
   --  occurrence of an enumeration literal. The replacement uses the tree
   --  rewrite mechanism described in the tree package so that the original
   --  tree for the expression can still be recoverd with the Original_Node
   --  function. This procedure can be used for such things as compile-time
   --  expression evaluation where the original tree needs to be preserved.

end Tbuild;
