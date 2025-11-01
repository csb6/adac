------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                S T A N D                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.32 $                             --
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

--  This package contains the declarations of entities in package Standard,
--  and the routine for creating the tree representation of Standard

with Types; use Types;
with Namet; use Namet;
pragma Elaborate (Namet);

package Stand is

   type Standard_Entity_Type is (
   --  This enumeration type contains an entry for each name in Standard

      --  Package names

      S_Standard,
      S_Ascii,

      --  Types defined in package Standard

      S_Boolean,
      S_Character,
      S_String,
      S_Duration,

      S_Short_Short_Integer,
      S_Short_Integer,
      S_Integer,
      S_Long_Integer,
      S_Long_Long_Integer,

      S_Short_Float,
      S_Float,
      S_Long_Float,
      S_Long_Long_Float,

      --  Enumeration literals for type Boolean

      S_False,
      S_True,

      --  Subtypes declared in package Standard

      S_Natural,
      S_Positive,

      --  Exceptions declared in package Standard

      S_Constraint_Error,
      S_Numeric_Error,
      S_Program_Error,
      S_Storage_Error,
      S_Tasking_Error,

      --  Constants defined in package Ascii (with value in hex).
      --  First the thirty-two C0 control characters)

      S_NUL,            -- 16#00#
      S_SOH,            -- 16#01#
      S_STX,            -- 16#02#
      S_ETX,            -- 16#03#
      S_EOT,            -- 16#04#
      S_ENQ,            -- 16#05#
      S_ACK,            -- 16#06#
      S_BEL,            -- 16#07#
      S_BS,             -- 16#08#
      S_HT,             -- 16#09#
      S_LF,             -- 16#0A#
      S_VT,             -- 16#0B#
      S_FF,             -- 16#0C#
      S_CR,             -- 16#0D#
      S_SO,             -- 16#0E#
      S_SI,             -- 16#0F#
      S_DLE,            -- 16#10#
      S_DC1,            -- 16#11#
      S_DC2,            -- 16#12#
      S_DC3,            -- 16#13#
      S_DC4,            -- 16#14#
      S_NAK,            -- 16#15#
      S_SYN,            -- 16#16#
      S_ETB,            -- 16#17#
      S_CAN,            -- 16#18#
      S_EM,             -- 16#19#
      S_SUB,            -- 16#1A#
      S_ESC,            -- 16#1B#
      S_FS,             -- 16#1C#
      S_GS,             -- 16#1D#
      S_RS,             -- 16#1E#
      S_US,             -- 16#1F#

      --  Here are the ones for Colonel Whitaker's O26 keypunch!

      S_Exclam,         -- 16#21#
      S_Quotation,      -- 16#22#
      S_Sharp,          -- 16#23#
      S_Dollar,         -- 16#24#
      S_Percent,        -- 16#25#
      S_Ampersand,      -- 16#26#

      S_Colon,          -- 16#3A#
      S_Semicolon,      -- 16#3B#

      S_Query,          -- 16#3F#
      S_At_Sign,        -- 16#40#

      S_L_Bracket,      -- 16#5B#
      S_Back_Slash,     -- 16#5C#
      S_R_Bracket,      -- 16#5D#
      S_Circumflex,     -- 16#5E#
      S_Underline,      -- 16#5F#
      S_Grave,          -- 16#60#

      S_LC_A,           -- 16#61#
      S_LC_B,           -- 16#62#
      S_LC_C,           -- 16#63#
      S_LC_D,           -- 16#64#
      S_LC_E,           -- 16#65#
      S_LC_F,           -- 16#66#
      S_LC_G,           -- 16#67#
      S_LC_H,           -- 16#68#
      S_LC_I,           -- 16#69#
      S_LC_J,           -- 16#6A#
      S_LC_K,           -- 16#6B#
      S_LC_L,           -- 16#6C#
      S_LC_M,           -- 16#6D#
      S_LC_N,           -- 16#6E#
      S_LC_O,           -- 16#6F#
      S_LC_P,           -- 16#70#
      S_LC_Q,           -- 16#71#
      S_LC_R,           -- 16#72#
      S_LC_S,           -- 16#73#
      S_LC_T,           -- 16#74#
      S_LC_U,           -- 16#75#
      S_LC_V,           -- 16#76#
      S_LC_W,           -- 16#77#
      S_LC_X,           -- 16#78#
      S_LC_Y,           -- 16#79#
      S_LC_Z,           -- 16#7A#

      S_L_BRACE,        -- 16#7B#
      S_BAR,            -- 16#7C#
      S_R_BRACE,        -- 16#7D#
      S_TILDE,          -- 16#7E#

      --  And one more control character, all on its own

      S_DEL);           -- 16#7F#

   subtype S_Types is
     Standard_Entity_Type range S_Boolean .. S_Long_Long_Float;

   subtype S_Exceptions is
     Standard_Entity_Type range S_Constraint_Error .. S_Tasking_Error;

   subtype S_Ascii_Names is
     Standard_Entity_Type range S_NUL .. S_DEL;

   type Standard_Entity_Array_Type is array (Standard_Entity_Type) of Node_Id;

   Standard_Entity : Standard_Entity_Array_Type;
   --  This array contains pointers to the Defining Identifier nodes
   --  for each of the entities defined in Standard_Entities_Type. It
   --  is initialized by the Create_Standard procedure.

   Standard_Package_Node : Node_Id;
   --  Points to the N_Package_Declaration node for standard. Also
   --  initialized by the Create_Standard procedure.

   --  The following Entities are the pointers to the Defining Identifier
   --  nodes for some visible entities defined in Standard_Entities_Type.

   SE : Standard_Entity_Array_Type renames Standard_Entity;

   Standard_Standard            : Entity_Id renames SE (S_Standard);

   Standard_Ascii               : Entity_Id renames SE (S_Ascii);
   Standard_Character           : Entity_Id renames SE (S_Character);
   Standard_String              : Entity_Id renames SE (S_String);

   Standard_Boolean             : Entity_Id renames SE (S_Boolean);
   Standard_False               : Entity_Id renames SE (S_False);
   Standard_True                : Entity_Id renames SE (S_True);

   Standard_Duration            : Entity_Id renames SE (S_Duration);

   Standard_Natural             : Entity_Id renames SE (S_Natural);
   Standard_Positive            : Entity_Id renames SE (S_Positive);

   Standard_Constraint_Error    : Entity_Id renames SE (S_Constraint_Error);
   Standard_Numeric_Error       : Entity_Id renames SE (S_Numeric_Error);
   Standard_Program_Error       : Entity_Id renames SE (S_Program_Error);
   Standard_Storage_Error       : Entity_Id renames SE (S_Storage_Error);
   Standard_Tasking_Error       : Entity_Id renames SE (S_Tasking_Error);

   Standard_Short_Float         : Entity_Id renames SE (S_Short_Float);
   Standard_Float               : Entity_Id renames SE (S_Float);
   Standard_Long_Float          : Entity_Id renames SE (S_Long_Float);
   Standard_Long_Long_Float     : Entity_Id renames SE (S_Long_Long_Float);

   Standard_Short_Integer       : Entity_Id renames SE (S_Short_Integer);
   Standard_Short_Short_Integer : Entity_Id renames SE (S_Short_Short_Integer);
   Standard_Integer             : Entity_Id renames SE (S_Integer);
   Standard_Long_Integer        : Entity_Id renames SE (S_Long_Integer);
   Standard_Long_Long_Integer   : Entity_Id renames SE (S_Long_Long_Integer);

   Standard_Longest_Runtime_Integer : Entity_Id
     renames Standard_Long_Long_Integer;

   Standard_Longest_Runtime_Real : Entity_Id
     renames Standard_Long_Long_Float;

   procedure Create_Standard;
   --  This procedure creates the tree for package standard, and initializes
   --  the Standard_Entities array and Standard_Package_Node. Only the
   --  syntactic representation is created (as though the parser had parsed
   --  a copy of the source of Standard) and then semantic information is
   --  added as it would be by the semantic phases of the compiler. The
   --  tree is in the standard format defined by Syntax_Info, except that
   --  all  Sloc values are set to Standard_Location. The semantics info is
   --  in the format given by Entity_Info. Create_Standard also initializes
   --  the following global variables:

   Last_Standard_Node_Id : Node_Id;
   --  Highest Node_Id value used by Standard

   Last_Standard_List_Id : List_Id;
   --  Highest List_Id value used by Standard (including those used by
   --  normal list headers, element list headers, and list elements)

   Last_Standard_Name_Id : Name_Id;
   --  Highest Name_Id value used by Standard (includes all the predefined
   --  names for pragmas, attributes etc defined in Snames package).

   Last_Standard_String_Id : String_Id;
   --  Highest String_Id value used by Standard

   Last_Standard_Uint : Uint;
   --  Highest Uint table entry value used by Standard

   -------------------------------------
   -- Semantic Phase Special Entities --
   -------------------------------------

   --  The semantic phase needs a number of entities for internal processing
   --  that are logically at the level of Standard, and hence defined in this
   --  package. However, they are never directly visible to a program, and
   --  are not chained on to the Decls list of Standard. The names of all
   --  these types are relevant only in certain debugging and error message
   --  situations. They are all of the form "<Name_In_Mixed_Case>" to clearly
   --  distinguish them from any program identifiers.

   Standard_Void_Type  : Entity_Id;
   --  This is a type used to represent the return type of procedures

   Standard_Exception_Type  : Entity_Id;
   --  This is a type used to represent the Etype of exceptions.

   Standard_A_String   : Entity_Id;
   --  An access to String type used for building elements of tables
   --  carrying the enumeration literal names.

   Any_Id : Entity_Id;
   --  Used to represent some unknown identifier. Avoids cascaded errors

   Any_Type : Entity_Id;
   --  Used to represent some unknown type. Avoids cascaded errors

   Any_Access : Entity_Id;
   --  Used to resolve the overloaded literal NULL.

   Any_Boolean : Entity_Id;
   --  The context type of conditions in IF and WHILE statements.

   Any_Character : Entity_Id;
   --  Any_Character is used to label character literals, which in general
   --  will not have an explicit declaration (this is true of the predefined
   --  character types).

   Any_Integer : Entity_Id;
   --  Used to represent some unknown integer type. Avoids cascaded errors

   Any_String : Entity_Id;
   --  The type Any_String is used for string literals before type
   --  resolution. It corresponds to array (Positive range <>) of Char
   --  but the component type is compatible with any character type,
   --  not just Standard_Character.

   Any_Composite : Entity_Id;
   --  The type Any_Composite is used for aggregates before type resolution.
   --  It is compatible with any array or non-limited record type.

   Universal_Integer : Entity_Id;
   --  Entity for universal integer type

   Universal_Real : Entity_Id;
   --  Entity for universal real type

end Stand;
