------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                T Y P E S                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.28 $                             --
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

with Unchecked_Deallocation;
package Types is

--  This package contains host independent type definitions which are used
--  in more than one unit in the compiler. They are gathered here for easy
--  reference, though in some cases the full description is found in the
--  relevant module which implements the definition. The main reason that
--  they are not in their "natural" specs is that this would cause a lot of
--  inter-spec dependencies, and in particular some awkward circular
--  dependencies would have to be dealt with.

--  WARNING: There is a C version of this package. Any changes to this
--  source file must be properly reflected in the C header file a-types.h

--  Note: the declarations in this package reflect an expectation that the
--  host machine has an efficient integer base type with a range at least
--  32 bits 1's complement. If there are any machines for which this is not
--  a correct assumption, a significant number of changes will be required!

   -------------------------------
   -- General Use Integer Types --
   -------------------------------

   type Model_Int is range -(2**31 - 1) .. +(2**31 - 1);
   --  Type used to establish appropriate bounds for reasonable sized integer

   Model_LB : constant := Model_Int'Pos (Model_Int'Base'First);
   Model_UB : constant := Model_Int'Pos (Model_Int'Base'Last);
   --  Machine bounds for reasonable sized integer. The integer type that we
   --  use is typically a 32-bit twos-complement integer, but we arrange the
   --  type declarations so that we can accomodate other possibilities, in
   --  particular 32-bit twos-complement with the most negative number missing
   --  as on the Rational implementation, or, if there exist any such beasts,
   --  1's complement 32-bit machines.

   type Int is range Model_LB .. Model_UB;
   --  Signed 32-bit integer

   subtype Nat is Int range 0 .. Model_UB;
   --  Non-negative Int values

   subtype Pos is Int range 1 .. Model_UB;
   --  Positive Int values

   type Short is range -32768 .. +32767;
   for Short'Size use 16;
   --  16-bit signed integer

   type Byte is range 0 .. 255;
   for Byte'Size use 8;
   --  8-bit unsigned integer

   --------------------------------------
   -- 8-Bit Character and String Types --
   --------------------------------------

   --  In Ada 9X, or an Ada 83 compiler which supported the 8-bit Character
   --  type, Char could be Character, and Str could be String, but we want
   --  to be able to compile on an Ada 83 compiler that has 7-bit Character.

   type Char is (
   --  8-bit character type

      NUL,         -- 16#00#;
      SOH,         -- 16#01#;
      STX,         -- 16#02#;
      ETX,         -- 16#03#;
      EOT,         -- 16#04#;
      ENQ,         -- 16#05#;
      ACK,         -- 16#06#;
      BEL,         -- 16#07#;
      BS,          -- 16#08#;
      HT,          -- 16#09#;
      LF,          -- 16#0A#;
      VT,          -- 16#0B#;
      FF,          -- 16#0C#;
      CR,          -- 16#0D#;
      SO,          -- 16#0E#;
      SI,          -- 16#0F#;

      DLE,         -- 16#10#;
      DC1,         -- 16#11#;
      DC2,         -- 16#12#;
      DC3,         -- 16#13#;
      DC4,         -- 16#14#;
      NAK,         -- 16#15#;
      SYN,         -- 16#16#;
      ETB,         -- 16#17#;
      CAN,         -- 16#18#;
      EM,          -- 16#19#;
      SUB,         -- 16#1A#;
      ESC,         -- 16#1B#;
      FS,          -- 16#1C#;
      GS,          -- 16#1D#;
      RS,          -- 16#1E#;
      US,          -- 16#1F#;

      ' ',         -- 16#20#;
      '!',         -- 16#21#;
      '"',         -- 16#22#;
      '#',         -- 16#23#;
      '$',         -- 16#24#;
      '%',         -- 16#25#;
      '&',         -- 16#26#;
      ''',         -- 16#27#;
      '(',         -- 16#28#;
      ')',         -- 16#29#;
      '*',         -- 16#2A#;
      '+',         -- 16#2B#;
      ',',         -- 16#2C#;
      '-',         -- 16#2D#;
      '.',         -- 16#2E#;
      '/',         -- 16#2F#;

      '0',         -- 16#30#;
      '1',         -- 16#31#;
      '2',         -- 16#32#;
      '3',         -- 16#33#;
      '4',         -- 16#34#;
      '5',         -- 16#35#;
      '6',         -- 16#36#;
      '7',         -- 16#37#;
      '8',         -- 16#38#;
      '9',         -- 16#39#;

      ':',         -- 16#3A#;
      ';',         -- 16#3B#;
      '<',         -- 16#3C#;
      '=',         -- 16#3D#;
      '>',         -- 16#3E#;
      '?',         -- 16#3F#;
      '@',         -- 16#40#;

      'A',         -- 16#41#;
      'B',         -- 16#42#;
      'C',         -- 16#43#;
      'D',         -- 16#44#;
      'E',         -- 16#45#;
      'F',         -- 16#46#;
      'G',         -- 16#47#;
      'H',         -- 16#48#;
      'I',         -- 16#49#;
      'J',         -- 16#4A#;
      'K',         -- 16#4B#;
      'L',         -- 16#4C#;
      'M',         -- 16#4D#;
      'N',         -- 16#4E#;
      'O',         -- 16#4F#;
      'P',         -- 16#50#;
      'Q',         -- 16#51#;
      'R',         -- 16#52#;
      'S',         -- 16#53#;
      'T',         -- 16#54#;
      'U',         -- 16#55#;
      'V',         -- 16#56#;
      'W',         -- 16#57#;
      'X',         -- 16#58#;
      'Y',         -- 16#59#;
      'Z',         -- 16#5A#;

      '[',         -- 16#5B#;
      '\',         -- 16#5C#;
      ']',         -- 16#5D#;
      '^',         -- 16#5E#;
      '_',         -- 16#5F#;
      '`',         -- 16#60#;

      'a',         -- 16#61#;
      'b',         -- 16#62#;
      'c',         -- 16#63#;
      'd',         -- 16#64#;
      'e',         -- 16#65#;
      'f',         -- 16#66#;
      'g',         -- 16#67#;
      'h',         -- 16#68#;
      'i',         -- 16#69#;
      'j',         -- 16#6A#;
      'k',         -- 16#6B#;
      'l',         -- 16#6C#;
      'm',         -- 16#6D#;
      'n',         -- 16#6E#;
      'o',         -- 16#6F#;
      'p',         -- 16#70#;
      'q',         -- 16#71#;
      'r',         -- 16#72#;
      's',         -- 16#73#;
      't',         -- 16#74#;
      'u',         -- 16#75#;
      'v',         -- 16#76#;
      'w',         -- 16#77#;
      'x',         -- 16#78#;
      'y',         -- 16#79#;
      'z',         -- 16#7A#;

      '{',         -- 16#7B#;
      '|',         -- 16#7C#;
      '}',         -- 16#7D#;
      '~',         -- 16#7E#;

      DEL,         -- 16#7F#;

      X_80, X_81, X_82, X_83, X_84, X_85, X_86, X_87,
      X_88, X_89, X_8A, X_8B, X_8C, X_8D, X_8E, X_8F,
      X_90, X_91, X_92, X_93, X_94, X_95, X_96, X_97,
      X_98, X_99, X_9A, X_9B, X_9C, X_9D, X_9E, X_9F,
      X_A0, X_A1, X_A2, X_A3, X_A4, X_A5, X_A6, X_A7,
      X_A8, X_A9, X_AA, X_AB, X_AC, X_AD, X_AE, X_AF,
      X_B0, X_B1, X_B2, X_B3, X_B4, X_B5, X_B6, X_B7,
      X_B8, X_B9, X_BA, X_BB, X_BC, X_BD, X_BE, X_BF,
      X_C0, X_C1, X_C2, X_C3, X_C4, X_C5, X_C6, X_C7,
      X_C8, X_C9, X_CA, X_CB, X_CC, X_CD, X_CE, X_CF,
      X_D0, X_D1, X_D2, X_D3, X_D4, X_D5, X_D6, X_D7,
      X_D8, X_D9, X_DA, X_DB, X_DC, X_DD, X_DE, X_DF,
      X_E0, X_E1, X_E2, X_E3, X_E4, X_E5, X_E6, X_E7,
      X_E8, X_E9, X_EA, X_EB, X_EC, X_ED, X_EE, X_EF,
      X_F0, X_F1, X_F2, X_F3, X_F4, X_F5, X_F6, X_F7,
      X_F8, X_F9, X_FA, X_FB, X_FC, X_FD, X_FE, X_FF);

   for Char'Size use 8;
   --  For a compiler supporting the Ada 9X 8-bit character type, this
   --  declaration may be replaced simply by "type Char is new Character".
   --  The size specification is merely for efficiency.

   subtype Graphic_Character is Char range ' ' .. '~';
   --  Graphic characters, as defined in ARM

   subtype Line_Terminator is Char range LF .. CR;
   --  Line terminator characters (LF, VT, FF, CR)

   subtype Upper_Half_Character is Char range X_80 .. X_FF;
   --  Characters with the upper bit set

   type Str is array (Nat range <>) of Char;
   --  String type built on Char (note that zero is an OK index)

   type Str_Ptr is access Str;
   --  Pointer to string of Chars

   procedure Free is new Unchecked_Deallocation (Str, Str_Ptr);
   --  Procedure for freeing dynamically allocated Str values

   EOF : constant Char := SUB;
   --  The character SUB (16#1A#) is appended to the file text to mark the end
   --  of file. In addition, in MS/DOS mode only, if this character appears in
   --  the file text, then it is recognized as an end of file, and no data
   --  after the EOF character is accessed.

   function To_Char (C : Character) return Char;
   pragma Inline (To_Char);
   --  Convert from Character to Char

   -----------------------------------------
   -- Types Used for Text Buffer Handling --
   -----------------------------------------

   type Text_Ptr is range Model_LB .. Model_UB;
   --  Type used for subscripts in text buffer

   type Text_Buffer is array (Text_Ptr range <>) of Char;
   --  Text buffer used to hold source file or library information file

   type Text_Buffer_Ptr is access Text_Buffer;
   --  Text buffers for input files are allocated dynamically and this type
   --  is used to reference these text buffers.

   procedure Free is new Unchecked_Deallocation (Text_Buffer, Text_Buffer_Ptr);
   --  Procedure for freeing dynamically allocated text buffers

   ------------------------------------------
   -- Types Used for Source Input Handling --
   ------------------------------------------

   type Line_Number_Type is range 1 .. Model_UB;
   --  Line number type, used for storing line numbers

   type Column_Number_Type is range 0 .. 32767;
   --  Column number (assume that 2**15 is large enough!)

   subtype Source_Buffer is Text_Buffer;
   --  Type used to store text of a source file . The buffer for the main
   --  source (the source specified on the command line) has a lower bound
   --  starting at zero. Subsequent subsidiary sources have lower bounds
   --  which are one greater than the previous upper bound.

   subtype Source_Buffer_Ptr is Text_Buffer_Ptr;
   --  Pointer to source buffer

   subtype Source_Ptr is Text_Ptr;
   --  Type used to represent a source location, which is a subscript of a
   --  character in the source buffer. As noted above, diffferent source
   --  buffers have different ranges, so it is possible to tell from a
   --  Source_Ptr value which source it refers to. Note that negative numbers
   --  are allowed to accomodate the following special values. There is no
   --  loss of safety, since we can rely on the subscript check to ensure
   --  that correct Source_Ptr values are used in the appropriate context.

   No_Location : constant Source_Ptr := -1;
   --  Value used to indicate no source position set in a node

   Standard_Location : constant Source_Ptr := -2;
   --  Used for all nodes in the representation of package Standard

   -------------------------------------
   -- Range Definitions for Tree Data --
   -------------------------------------

   --  The tree has fields that can hold any of the following types:

   --    Pointers to other tree nodes (type Node_Id)
   --    List pointers (type List_Id)
   --    Element list pointers (type Elist_Id)
   --    Element pointers (type Elmt_Id)
   --    Names (type Name_Id)
   --    Strings (type String_Id)
   --    Universal integers (type Uint)
   --    Character codes (type Char_Code stored with a bias)

   --  In most contexts, the strongly typed interface determines which of
   --  these types is present. However, there are some situations (involving
   --  untyped traversals of the tree), where it is convenient to be easily
   --  able to distinguish these values. The underlying representation in all
   --  cases is Int, and we ensure that the range of possible values for each
   --  of the above types is disjoint so that this distinction is possible.

   --  Note: it is also helpful for debugging purposes to make these ranges
   --  distinct. If a bug leads to misidentification of a value, then it will
   --  typically result in an out of range value and a Constraint_Error.

   List_Low_Bound : constant := -100_000_000;
   --  The List_Id values are subscripts into an array of list headers which
   --  has List_Low_Bound as its lower bound. This value is chosen so that all
   --  List_Id values are negative, and the value zero is in the range of both
   --  List_Id and Node_Id values (see further description below).

   List_High_Bound : constant := 0;
   --  Maximum List_Id subscript value. This allows up to 100 million list
   --  Id values, which is in practice infinite, and there is no need to
   --  check the range. The range overlaps the node range by one element
   --  (with value zero), which is used both for the Empty node, and for
   --  indicating no list. The fact that the same value is used is not
   --  crucial for the Ada code, but is useful in the conversion to C,
   --  where the type distinction disappears, and also generates better
   --  code in both cases for the test for empty.

   Node_Low_Bound : constant := 0;
   --  The tree Id values start at zero, because we use zero for Empty (to
   --  allow a zero test for Empty). Actual tree node subscripts start at 0
   --  since Empty is a legitimate node value.

   Node_High_Bound : constant := 099_999_999;
   --  Maximum number of nodes that can be allocated is 100 million, which
   --  is in practice infinite, and there is no need to check the range.

   Elist_Low_Bound : constant := 100_000_000;
   --  Low bound of element list Id values. Element list headers are actually
   --  stored using the same array as normal list values, but a bias is added
   --  to stored values, to aid in type identification, as described above.

   Elist_High_Bound : constant := 199_999_999;
   --  Upper bound of element list Id values

   Elmt_Low_Bound : constant := 200_000_000;
   --  Low bound of element Id values. Element list elements are actually
   --  stored using the same array as normal list values, but a bias is added
   --  to stored values, to aid in type identification, as described above.

   Elmt_High_Bound : constant := 299_999_999;
   --  Upper bound of element list Id values

   Names_Low_Bound : constant := 300_000_000;
   --  Low bound for name Id values

   Names_High_Bound : constant := 399_999_999;
   --  Maximum number of names that can be allocated is 100 million, which is
   --  in practice infinite and there is no need to check the range.

   Strings_Low_Bound : constant := 400_000_000;
   --  Low bound for string Id values

   Strings_High_Bound : constant := 499_999_999;
   --  Maximum number of strings that can be allocated is 100 million, which
   --  is in practice infinite and there is no need to check the range.

   Uint_Low_Bound : constant := 500_000_000;
   --  Low bound for Uint values.

   Uint_High_Bound : constant := 599_999_999;
   --  Maximum number of Uint values stored is 100_000_000 which is in
   --  practice infinite so that no check is required.

   Char_Code_Bias : constant := 600_000_000;
   --  A bias value added to character code values stored in the tree which
   --  ensures that they have different values from any of the above types.

   --  The following subtype definitions are used to provide convenient names
   --  for membership tests on Int values to see what data type range they
   --  lie in. Such tests appear only in the lowest level packages.

   subtype List_Range    is Int range List_Low_Bound .. List_High_Bound;
   subtype Node_Range    is Int range Node_Low_Bound .. Node_High_Bound;
   subtype Elist_Range   is Int range Elist_Low_Bound .. Elist_High_Bound;
   subtype Elmt_Range    is Int range Elmt_Low_Bound .. Elmt_High_Bound;
   subtype Names_Range   is Int range Names_Low_Bound .. Names_High_Bound;
   subtype Strings_Range is Int range Strings_Low_Bound .. Strings_High_Bound;
   subtype Uint_Range    is Int range Uint_Low_Bound .. Uint_High_Bound;
   subtype Char_Code_Range
                         is Int range Char_Code_Bias ..
                                                    Char_Code_Bias + 2**16 - 1;

   -----------------------------
   -- Types for Namet Package --
   -----------------------------

   --  Name_Id values are used to identify entries in the names table. Except
   --  for the special values No_Name, and Error_Name, they are subscript
   --  values for the Names table defined in package Namet.

   --  Note that with only a few exceptions, which are clearly documented, the
   --  type Name_Id should be regarded as a private type. In particular it is
   --  never appropriate to perform arithmetic operations using this type.

   type Name_Id is range Names_Low_Bound .. Names_High_Bound;
   --  Type used to identify entries in the names table

   No_Name : constant Name_Id := Names_Low_Bound;
   --  The special Name_Id value No_Name is used in the parser to indicate
   --  a situation where no name is present (e.g. on a loop or block).

   Error_Name : constant Name_Id := Names_Low_Bound +  1;
   --  The special Name_Id value Error_Name is used in the parser to
   --  indicate that some kind of error was encountered in scanning out
   --  the relevant name, so it does not have a representable label.

   First_Name_Id : constant Name_Id := Names_Low_Bound + 2;
   --  Subscript of first entry in names table

   ----------------------------
   -- Types for Tree Package --
   ----------------------------

   --  Node_Id values are used to identify nodes in the tree. They are
   --  subscripts into the Node table declared in package Tree. Note that
   --  the special values Empty and Error are subscripts into this table,
   --  are package Tree for further details.

   type Node_Id is range Node_Low_Bound .. Node_High_Bound;
   --  Type used to identify nodes in the tree

   subtype Entity_Id is Node_Id;
   --  A synonym for node types, used in the entity package to refer to
   --  nodes that are entities (i.e. nodes with an Nkind of N_Defining_xxx)

   Empty : constant Node_Id := 0;
   --  Used to indicate null node. A node is actually allocated with this
   --  Id value, so that Nkind (Empty) = N_Empty.

   Error : constant Node_Id := 1;
   --  Used to indicate that there was an error in the source program. A node
   --  is actually allocated at this address, so that Nkind (Error) = N_Error.

   Empty_Or_Error : constant Node_Id := Error;
   --  Since Empty and Error are the first two Node_Id values, the test for
   --  N <= Error tests to see if N is Empty or Error. This definition provides
   --  convenient self-documentation for such tests.

   First_Node_Id  : constant Node_Id := Empty;
   --  Subscript of first allocated node

   --  List_Id values are used to identify node lists in the tree. They are
   --  subscripts into the Lists table declared in package Tree. Note that
   --  the special value Error_List is a subscript in this table, but the
   --  value No_List is *not* a valid subscript, and any attempt to apply
   --  list operations to No_List will cause a (detected) error.

   type List_Id is range List_Low_Bound .. List_High_Bound;
   --  Type used to identify a node list

   No_List : constant List_Id := 0;
   --  Used to indicate absence of a list. Note that the value is the same
   --  as Empty, which is irrelevant for Ada (since generally we are strongly
   --  typed), but is useful in the conversion to C, where we lose the typing
   --  distinction.

   Error_List : constant List_Id := List_Low_Bound;
   --  Used to indicate that there was an error in the source program in a
   --  context which would normally require a list. This node appears to be
   --  an empty list to the list operations (a null list is actually allocated
   --  which has this Id value).

   First_List_Id : constant List_Id := Error_List;
   --  Subscript of first allocated list header

   --  Element list Id values are used to identify element lists stored in
   --  the tree (see package Tree for further details). They are formed by
   --  adding a bias (Element_List_Bias) to subscript values in the same
   --  array that is used for node list headers.

   type Elist_Id is range Elist_Low_Bound .. Elist_High_Bound;
   --  Type used to identify an element list

   No_Elist : constant Elist_Id := Elist_Low_Bound;
   --  Used to indicate absense of an element list

   Elist_Bias : constant := Elist_Low_Bound - List_Low_Bound;
   --  Bias added to subscript in list table to get Elist_Id value

   --  Element Id values are used to identify individual elements of an
   --  element list (see package Tree for further details). They are
   --  formed by adding a bias (Elmt_Bias) to subscript values in the same
   --  array that is used for node list headers.

   type Elmt_Id is range Elmt_Low_Bound .. Elmt_High_Bound;
   --  Type used to identify an element list

   Elmt_Bias : constant := Elmt_Low_Bound - List_Low_Bound;
   --  Bias added to subscript in list table to get Elmt_Id value

   No_Elmt : constant Elmt_Id := Elmt_Low_Bound;
   --  Used to represent empty element

   -------------------------------
   -- Types for Stringt Package --
   -------------------------------

   --  String_Id values are used to identify entries in the strings table.
   --  They are subscripts into the strings table defined in package Strings.

   --  Note that with only a few exceptions, which are clearly documented, the
   --  type String_Id should be regarded as a private type. In particular it is
   --  never appropriate to perform arithmetic operations using this type.

   type String_Id is range Strings_Low_Bound .. Strings_High_Bound;
   --  Type used to identify entries in the strings table

   No_String : constant String_Id := Strings_Low_Bound;
   --  Used to indicate missing string Id. Note that the value zero is used
   --  to indicate a missing data value for all the Int types in this section.

   First_String_Id : constant String_Id := No_String + 1;
   --  First subscript allocated in string table

   -----------------------------
   -- Types for Uintp Package --
   -----------------------------

   --  Uint values are used to represent universal integers

   --  Note that with only a few exceptions, which are clearly documented,
   --  the type Uint should be regarded as a private type. In particular it
   --  is never appropriate to perform arithmetic operations using this type,
   --  and the details of the representation should not be exploited.

   type Uint is range Uint_Low_Bound .. Uint_High_Bound;
   --  Type used for representation of universal integers

   No_Uint : constant Uint := Uint_Low_Bound;
   --  Used to indicate missing Uint value

   --  Uint values are represented as multiple precision integers stored in
   --  a multi-digit format using Base as the base. This value is chosen so
   --  that the product Base*Base is within the range of allowed Int values.

   Base : constant := 2 ** 15;

   --  Values in the range -(Base-1)..+(Base-1), i.e. one-digit values,
   --  are encoded directly as Uint values by adding a bias value:

   Uint_Direct_Bias  : constant Uint := Uint_Low_Bound + Base;
   Uint_Direct_First : constant Uint := Uint_Direct_Bias - (Base - 1);
   Uint_Direct_Last  : constant Uint := Uint_Direct_Bias + (Base - 1);
   --  These values define the bias used to store Uint values which are in
   --  the range -(Base-1)..+(Base-1), as well as the biased values for the
   --  first and last values in this range.

   subtype Uint_Direct is Uint range Uint_Direct_First .. Uint_Direct_Last;
   --  A subtype defining the range of Uint values used for direct
   --  representation of values using Uint_Direct_Bias.

   Uint_First_Entry : constant Uint := Uint_Direct_Last + 1;
   --  First subscript allocated in Uint table

   -------------------------
   -- Character Code Type --
   -------------------------

   --  The type Char is used for character data internally in the compiler,
   --  but character codes in the source are represented by the Char_Code
   --  type. Each character literal in the source is interpreted as being one
   --  of the 2**16 possible Wide_Character codes, and a unique integer value
   --  is assigned, corresponding to the POS value in the Wide_Character type.
   --  String literals are similarly interpreted as a sequence of such codes.

   --  Note: for the moment, until we properly implement wide character, we
   --  use the values 0-255 to represent the natural character codes in the
   --  source input. This will change later on. For Latin-1, there will be
   --  a bias to the value, representing the starting position of Latin-1 in
   --  the 10646 BMP. For other codes (e.g. Latin-2), the coding will be quite
   --  different, since in this case the codes are not contiguous.

   --  Note: when character code values are stored in the tree, they are stored
   --  by adding a bias value (Char_Code_Bias) that results in values that can
   --  be distinguished from other types of values stored in the tree.

   type Char_Code is range 0 .. 2**16 - 1;
   for Char_Code'Size use 16;

   function Get_Char_Code (C : Char) return Char_Code;
   pragma Inline (Get_Char_Code);
   --  Function to obtain internal character code from source character. For
   --  the moment, the internal character code is simply the Pos value of the
   --  input source character, but this may change in the future when we
   --  implement the full 16-bit character support.

   ---------------------------------------
   -- Types used for Library Management --
   ---------------------------------------

   type Unit_Number_Type is range Model_LB .. Model_UB;
   --  Unit number. The main source is unit 0, and subsidiary sources have
   --  non-zero numbers starting with 1. Unit numbers are used to index the
   --  file table in Lib.

   Main_Unit : constant Unit_Number_Type := 0;
   --  Unit number value for main unit

   No_Unit : constant Unit_Number_Type := -1;
   --  Special value used to signal no unit

   type Lines_Table_Type is array (Line_Number_Type range <>) of Source_Ptr;
   --  Type used for lines table. The entries are indexed by line number and
   --  the values are the starting Source_Ptr values for the start of line.

   type Lines_Table_Ptr is access Lines_Table_Type;
   --  Type used for pointers to line tables

   procedure Free_Lines is new Unchecked_Deallocation
     (Lines_Table_Type, Lines_Table_Ptr);
   --  Procedure for freeing dynamically allocated Lines_Tables

   Time_Stamp_Length : constant := 12;
   --  Length of time stamp value

   subtype Time_Stamp_Type is Str (1 .. Time_Stamp_Length);
   --  Type used to represent time stamp (see spec of Libfmt for details)

   subtype File_Name_Type is Name_Id;
   --  File names are stored in the names table and this synonym is used to
   --  indicate that a Name_Id value is being used to hold a simple file
   --  name (which does not include any directory information).

   No_File : constant File_Name_Type := File_Name_Type (No_Name);
   --  Constant used to indicate no file found

   subtype Unit_Name_Type is Name_Id;
   --  Unit names are stored in the names table and this synonym is used to
   --  indicate that a Name_Id value is being used to hold a unit name.

   -----------------------------------------------
   -- Types used for Pragma Suppress Management --
   -----------------------------------------------

   --  The following record contains an entry for each recognized check name
   --  for pragma Suppress. It is used to represent current settings of scope
   --  based suppress actions from pragma Suppress or command line settings.

   type Suppress_Record is record
      Access_Checks        : Boolean := False;
      Accessibility_Checks : Boolean := False;
      Discriminant_Checks  : Boolean := False;
      Division_Checks      : Boolean := False;
      Elaboration_Checks   : Boolean := False;
      Index_Checks         : Boolean := False;
      Length_Checks        : Boolean := False;
      Overflow_Checks      : Boolean := False;
      Range_Checks         : Boolean := False;
      Storage_Checks       : Boolean := False;
      Tag_Checks           : Boolean := False;
   end record;

   --  To add a new check type to GNAT, the following steps are required:

   --    1.  Add an appropriate entry to the above record type
   --    2.  Add an entry to Snames spec and body for the new name
   --    3.  Add an entry to the definition of Check_Id in the Snames spec
   --    4.  Add a new entity flag definition in Einfo for the check
   --    5.  Add a new function to Sem.Util to handle the new check test
   --    6.  Add appropriate processing for pragma Suppress in Sem.Prag
   --    7.  Add a branch to the case statement in Sem.Ch8.Pop_Scope
   --    8.  Add a new Do_xxx_Check flag to Sinfo (if required)
   --    9.  Add appropriate checks for the new test

end Types;
