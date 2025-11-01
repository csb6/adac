------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               S N A M E S                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.37 $                             --
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

package Snames is

--  This package contains definitions of standard names (i.e. entries in the
--  Names table) that are used throughout the GNAT compiler). It also contains
--  the definitions of some enumeration types whose definitions are tied to
--  the order of these preset names.

   ------------------
   -- Preset Names --
   ------------------

   --  The following are preset entries in the names table, which are
   --  entered at the start of every compilation for easy access. Note
   --  that the order of initialization of these names in the body must
   --  be coordinated with the order of names in this table.

   --  Note: a name may not appear more than once in the following list.
   --  If additional pragmas or attributes are introduced which might
   --  otherwise cause a duplicate, then list it only once in this table,
   --  and adjust the definition of the functions for testing for pragma
   --  names and attribute names, and returning their ID values. Of course
   --  everything is simpler if no such duplications occur!

   --  Note: the following table is read by the utility program XSNAMES and
   --  should not be changed without coordinating with this program. The
   --  entry given on the comment line is for use by XSNAMES, and represents
   --  a name to be enclosed in double quotes in the names table. Note that
   --  the list of constant values here is also updated by XSNAMES.

   N : constant Name_Id := First_Name_Id;
   --  Synonym used in standard name definitions

   --  First we have the one character names used to optimize the lookup
   --  process for one character identifiers (avoid the hashing in this case)

   Name_A                              : constant Name_Id := N + 000;
   Name_B                              : constant Name_Id := N + 001;
   Name_C                              : constant Name_Id := N + 002;
   Name_D                              : constant Name_Id := N + 003;
   Name_E                              : constant Name_Id := N + 004;
   Name_F                              : constant Name_Id := N + 005;
   Name_G                              : constant Name_Id := N + 006;
   Name_H                              : constant Name_Id := N + 007;
   Name_I                              : constant Name_Id := N + 008;
   Name_J                              : constant Name_Id := N + 009;
   Name_K                              : constant Name_Id := N + 010;
   Name_L                              : constant Name_Id := N + 011;
   Name_M                              : constant Name_Id := N + 012;
   Name_N                              : constant Name_Id := N + 013;
   Name_O                              : constant Name_Id := N + 014;
   Name_P                              : constant Name_Id := N + 015;
   Name_Q                              : constant Name_Id := N + 016;
   Name_R                              : constant Name_Id := N + 017;
   Name_S                              : constant Name_Id := N + 018;
   Name_T                              : constant Name_Id := N + 019;
   Name_U                              : constant Name_Id := N + 020;
   Name_V                              : constant Name_Id := N + 021;
   Name_W                              : constant Name_Id := N + 022;
   Name_X                              : constant Name_Id := N + 023;
   Name_Y                              : constant Name_Id := N + 024;
   Name_Z                              : constant Name_Id := N + 025;

   --  Some special names used by the expander. Note that the lower case u's
   --  at the start of these names get translated to extra underscores. These
   --  names are only referenced internally by expander generated code. Note
   --  that we put these early in the list so that in particular Name_uParent
   --  and Name_uSize will be at fixed Name_Id values which will not change
   --  with the addition of other entries to Snames. This makes things easier
   --  because Gigi references these particular entries.

   Name_uParent                        : constant Name_Id := N + 026;
   Name_uSize                          : constant Name_Id := N + 027;

   --  Remaining names are not referenced by Gigi

   Name_uChain                         : constant Name_Id := N + 028;
   Name_uEquality                      : constant Name_Id := N + 029;
   Name_uExpunge                       : constant Name_Id := N + 030;
   Name_uIdepth                        : constant Name_Id := N + 031;
   Name_uInit                          : constant Name_Id := N + 032;
   Name_uMaster                        : constant Name_Id := N + 033;
   Name_uPriority                      : constant Name_Id := N + 034;
   Name_uTask                          : constant Name_Id := N + 035;
   Name_uTB_Snam                       : constant Name_Id := N + 036;
   Name_uTrace_Sp                      : constant Name_Id := N + 037;
   Name_uTag                           : constant Name_Id := N + 038;
   Name_uTags                          : constant Name_Id := N + 039;
   Name_uTask_Id                       : constant Name_Id := N + 040;
   Name_Version_B                      : constant Name_Id := N + 041;
   Name_Version_S                      : constant Name_Id := N + 042;

   --  Some miscellaneous names used by the parser for error detection/recovery

   Name_Error                          : constant Name_Id := N + 043;
   Name_Go                             : constant Name_Id := N + 044;
   Name_To                             : constant Name_Id := N + 045;

   --  Names for packages that are treated specially by the compiler

   Name_System                         : constant Name_Id := N + 046;
   Name_Unchecked_Conversion           : constant Name_Id := N + 047;

   --  Operator Symbol entries

   First_Operator_Name                 : constant Name_Id := N + 048;
   Name_Op_Abs                         : constant Name_Id := N + 048; -- "ABS"
   Name_Op_And                         : constant Name_Id := N + 049; -- "AND"
   Name_Op_Mod                         : constant Name_Id := N + 050; -- "MOD"
   Name_Op_Not                         : constant Name_Id := N + 051; -- "NOT"
   Name_Op_Or                          : constant Name_Id := N + 052; -- "OR"
   Name_Op_Rem                         : constant Name_Id := N + 053; -- "REM"
   Name_Op_Xor                         : constant Name_Id := N + 054; -- "XOR"
   Name_Op_Eq                          : constant Name_Id := N + 055; -- "="
   Name_Op_Ne                          : constant Name_Id := N + 056; -- "/="
   Name_Op_Lt                          : constant Name_Id := N + 057; -- "<"
   Name_Op_Le                          : constant Name_Id := N + 058; -- "<="
   Name_Op_Gt                          : constant Name_Id := N + 059; -- ">"
   Name_Op_Ge                          : constant Name_Id := N + 060; -- ">="
   Name_Op_Add                         : constant Name_Id := N + 061; -- "+"
   Name_Op_Subtract                    : constant Name_Id := N + 062; -- "-"
   Name_Op_Concat                      : constant Name_Id := N + 063; -- "&"
   Name_Op_Multiply                    : constant Name_Id := N + 064; -- "*"
   Name_Op_Divide                      : constant Name_Id := N + 065; -- "/"
   Name_Op_Expon                       : constant Name_Id := N + 066; -- "**"
   Last_Operator_Name                  : constant Name_Id := N + 066;

   --  Additional names for operations which we syntatically consider to be
   --  operators but are not operator symbols, but we still want the names in
   --  the name list for use in error messages.

   Name_Op_In                     : constant Name_Id := N + 067; -- "IN"
   Name_Op_Not_In                 : constant Name_Id := N + 068; -- "NOT IN"
   Name_Op_And_Then               : constant Name_Id := N + 069; -- "AND THEN"
   Name_Op_Or_Else                : constant Name_Id := N + 070; -- "OR ELSE"

   --  Names for all pragmas recognized by GNAT. The entries with the comment
   --  "Ada 83" are pragmas that are defined in Ada 83, but not in Ada 9X.
   --  These pragmas are fully implemented in both Ada 83 and Ada 9X modes
   --  in GNAT. The entries marked GNAT are pragmas that are defined by GNAT
   --  and implemented in both Ada 83 and Ada 9X modes.

   First_Pragma_Name                   : constant Name_Id := N + 071;
   Name_Abort_Defer                    : constant Name_Id := N + 071; -- GNAT
   Name_Ada_83                         : constant Name_Id := N + 072; -- GNAT
   Name_Ada_9X                         : constant Name_Id := N + 073; -- GNAT
   Name_All_Calls_Remote               : constant Name_Id := N + 074;
   Name_Assert                         : constant Name_Id := N + 075; -- GNAT
   Name_Asynchronous                   : constant Name_Id := N + 076;
   Name_Atomic                         : constant Name_Id := N + 077;
   Name_Atomic_Components              : constant Name_Id := N + 078;
   Name_Attach_Handler                 : constant Name_Id := N + 079;
   Name_Controlled                     : constant Name_Id := N + 080;
   Name_Convention                     : constant Name_Id := N + 081;
   Name_Debug                          : constant Name_Id := N + 082; -- GNAT
   Name_Elaborate                      : constant Name_Id := N + 083; -- Ada 83
   Name_Elaborate_All                  : constant Name_Id := N + 084;
   Name_Elaborate_Body                 : constant Name_Id := N + 085;
   Name_Export                         : constant Name_Id := N + 086;
   Name_Import                         : constant Name_Id := N + 087;
   Name_Improve                        : constant Name_Id := N + 088; -- GNAT
   Name_Inline                         : constant Name_Id := N + 089;
   Name_Inspection_Point               : constant Name_Id := N + 090;
   Name_Interface                      : constant Name_Id := N + 091; -- Ada 83
   Name_Interface_Name                 : constant Name_Id := N + 092; -- GNAT
   Name_Interrupt_Handler              : constant Name_Id := N + 093;
   Name_Interrupt_Priority             : constant Name_Id := N + 094;
   Name_List                           : constant Name_Id := N + 095;
   Name_Locking_Policy                 : constant Name_Id := N + 096;
   Name_Memory_Size                    : constant Name_Id := N + 097; -- Ada 83
   Name_Normalize_Scalars              : constant Name_Id := N + 098;
   Name_Optimize                       : constant Name_Id := N + 099;
   Name_Pack                           : constant Name_Id := N + 100;
   Name_Page                           : constant Name_Id := N + 101;
   Name_Preelaborate                   : constant Name_Id := N + 102;
   Name_Priority                       : constant Name_Id := N + 103;
   Name_Pure                           : constant Name_Id := N + 104;
   Name_Queuing_Policy                 : constant Name_Id := N + 105;
   Name_Remote_Call_Interface          : constant Name_Id := N + 106;
   Name_Remote_Types                   : constant Name_Id := N + 107;
   Name_Restrictions                   : constant Name_Id := N + 108;
   Name_Reviewable                     : constant Name_Id := N + 109;
   Name_Shared                         : constant Name_Id := N + 110; -- Ada 83
   Name_Shared_Passive                 : constant Name_Id := N + 111;
   Name_Storage_Unit                   : constant Name_Id := N + 112; -- Ada 83
   Name_Suppress                       : constant Name_Id := N + 113;
   Name_System_Name                    : constant Name_Id := N + 114; -- Ada 83
   Name_Task_Dispatching_Policy        : constant Name_Id := N + 115;
   Name_Task_Stack_Size                : constant Name_Id := N + 116; -- GNAT
   Name_Volatile                       : constant Name_Id := N + 117;
   Name_Volatile_Components            : constant Name_Id := N + 118;
   Last_Pragma_Name                    : constant Name_Id := N + 118;

   --  Language convention names for pragma Convention/Export/Import/Interface
   --  Note that Name_C is not included in this list, since it was already
   --  declared earlier in the context of one-character identifier names
   --  (where the order is critical to the fast look up process).

   First_Convention_Name               : constant Name_Id := N + 119;
   Name_Ada                            : constant Name_Id := N + 119;
   Name_Asm                            : constant Name_Id := N + 120;
   Name_Assembler                      : constant Name_Id := N + 121;
   Name_Intrinsic                      : constant Name_Id := N + 122;
   Last_Convention_Name                : constant Name_Id := N + 122;

   --  Other special names used in processing pragma arguments

   Name_Entity                         : constant Name_Id := N + 123;
   Name_Gcc                            : constant Name_Id := N + 124;
   Name_Gnat                           : constant Name_Id := N + 125;
   Name_Link_Name                      : constant Name_Id := N + 126;
   Name_Off                            : constant Name_Id := N + 127;
   Name_On                             : constant Name_Id := N + 128;
   Name_Space                          : constant Name_Id := N + 129;
   Name_Time                           : constant Name_Id := N + 130;

   --  Names of recognized attributes. The entries with the comment "Ada 83"
   --  are attributes that are defined in Ada 83, but not in Ada 9X. These
   --  attributes are implemented in both Ada 83 and Ada 9X modes in GNAT.

   First_Attribute_Name                : constant Name_Id := N + 131;
   Name_Access                         : constant Name_Id := N + 131;
   Name_Address                        : constant Name_Id := N + 132;
   Name_Adjacent                       : constant Name_Id := N + 133;
   Name_Aft                            : constant Name_Id := N + 134;
   Name_Alignment                      : constant Name_Id := N + 135;
   Name_Bit_Order                      : constant Name_Id := N + 136;
   Name_Body_Version                   : constant Name_Id := N + 137;
   Name_Callable                       : constant Name_Id := N + 138;
   Name_Caller                         : constant Name_Id := N + 139;
   Name_Ceiling                        : constant Name_Id := N + 140;
   Name_Component_Size                 : constant Name_Id := N + 141;
   Name_Compose                        : constant Name_Id := N + 142;
   Name_Constrained                    : constant Name_Id := N + 143;
   Name_Copy_Sign                      : constant Name_Id := N + 144;
   Name_Count                          : constant Name_Id := N + 145;
   Name_Delta                          : constant Name_Id := N + 146;
   Name_Denorm                         : constant Name_Id := N + 147;
   Name_Digits                         : constant Name_Id := N + 148;
   Name_Emax                           : constant Name_Id := N + 149; -- Ada 83
   Name_Epsilon                        : constant Name_Id := N + 150; -- Ada 83
   Name_Exponent                       : constant Name_Id := N + 151;
   Name_External_Tag                   : constant Name_Id := N + 152;
   Name_First                          : constant Name_Id := N + 153;
   Name_First_Bit                      : constant Name_Id := N + 154;
   Name_Floor                          : constant Name_Id := N + 155;
   Name_Fore                           : constant Name_Id := N + 156;
   Name_Fraction                       : constant Name_Id := N + 157;
   Name_Identity                       : constant Name_Id := N + 158;
   Name_Image                          : constant Name_Id := N + 159;
   Name_Input                          : constant Name_Id := N + 160;
   Name_Large                          : constant Name_Id := N + 161; -- Ada 83
   Name_Last                           : constant Name_Id := N + 162;
   Name_Last_Bit                       : constant Name_Id := N + 163;
   Name_Leading_Part                   : constant Name_Id := N + 164;
   Name_Length                         : constant Name_Id := N + 165;
   Name_Machine                        : constant Name_Id := N + 166;
   Name_Machine_Emax                   : constant Name_Id := N + 167;
   Name_Machine_Emin                   : constant Name_Id := N + 168;
   Name_Machine_Mantissa               : constant Name_Id := N + 169;
   Name_Machine_Overflows              : constant Name_Id := N + 170;
   Name_Machine_Radix                  : constant Name_Id := N + 171;
   Name_Machine_Rounds                 : constant Name_Id := N + 172;
   Name_Mantissa                       : constant Name_Id := N + 173; -- Ada 83
   Name_Max                            : constant Name_Id := N + 174;
   Name_Max_Size_In_Storage_Elements   : constant Name_Id := N + 175;
   Name_Min                            : constant Name_Id := N + 176;
   Name_Model                          : constant Name_Id := N + 177;
   Name_Model_Emax                     : constant Name_Id := N + 178; -- Ada 83
   Name_Model_Emin                     : constant Name_Id := N + 179;
   Name_Model_Epsilon                  : constant Name_Id := N + 180;
   Name_Model_Large                    : constant Name_Id := N + 181; -- Ada 83
   Name_Model_Mantissa                 : constant Name_Id := N + 182;
   Name_Model_Small                    : constant Name_Id := N + 183;
   Name_Output                         : constant Name_Id := N + 184;
   Name_Pos                            : constant Name_Id := N + 185;
   Name_Position                       : constant Name_Id := N + 186;
   Name_Pred                           : constant Name_Id := N + 187;
   Name_Range                          : constant Name_Id := N + 188;
   Name_Read                           : constant Name_Id := N + 189;
   Name_Remainder                      : constant Name_Id := N + 190;
   Name_Round                          : constant Name_Id := N + 191;
   Name_Rounding                       : constant Name_Id := N + 192;
   Name_Safe_Emax                      : constant Name_Id := N + 193; -- Ada 83
   Name_Safe_First                     : constant Name_Id := N + 194;
   Name_Safe_Large                     : constant Name_Id := N + 195; -- Ada 83
   Name_Safe_Last                      : constant Name_Id := N + 196;
   Name_Safe_Small                     : constant Name_Id := N + 197; -- Ada 83
   Name_Scale                          : constant Name_Id := N + 198;
   Name_Signed_Zeros                   : constant Name_Id := N + 199;
   Name_Size                           : constant Name_Id := N + 200;
   Name_Small                          : constant Name_Id := N + 201;
   Name_Storage_Pool                   : constant Name_Id := N + 202;
   Name_Storage_Size                   : constant Name_Id := N + 203;
   Name_Succ                           : constant Name_Id := N + 204;
   Name_Tag                            : constant Name_Id := N + 205;
   Name_Terminated                     : constant Name_Id := N + 206;
   Name_Truncation                     : constant Name_Id := N + 207;
   Name_Unbiased_Rounding              : constant Name_Id := N + 208;
   Name_Unchecked_Access               : constant Name_Id := N + 209;
   Name_Val                            : constant Name_Id := N + 210;
   Name_Valid                          : constant Name_Id := N + 211;
   Name_Value                          : constant Name_Id := N + 212;
   Name_Version                        : constant Name_Id := N + 213;
   Name_Wide_Image                     : constant Name_Id := N + 214;
   Name_Wide_Value                     : constant Name_Id := N + 215;
   Name_Width                          : constant Name_Id := N + 216;
   Name_Write                          : constant Name_Id := N + 217;

   --  These attributes are the ones that return Types

   First_Type_Attribute_Name           : constant Name_Id := N + 218;
   Name_Base                           : constant Name_Id := N + 218;
   Name_Class                          : constant Name_Id := N + 219;
   Name_Standard_Access                : constant Name_Id := N + 220; -- GNAT
   Last_Type_Attribute_Name            : constant Name_Id := N + 220;
   Last_Attribute_Name                 : constant Name_Id := N + 220;

   --  Names of recognized locking policy identifiers

   First_Locking_Policy_Name           : constant Name_Id := N + 221;
   Name_Ceiling_Locking                : constant Name_Id := N + 221;
   Last_Locking_Policy_Name            : constant Name_Id := N + 221;

   --  Names of recognized locking policy identifiers

   First_Queuing_Policy_Name           : constant Name_Id := N + 222;
   Name_FIFO_Queuing                   : constant Name_Id := N + 222;
   Name_Priority_Queuing               : constant Name_Id := N + 223;
   Last_Queuing_Policy_Name            : constant Name_Id := N + 223;

   --  Names of recognized task dispatching policy identifiers

   First_Task_Dispatching_Policy_Name  : constant Name_Id := N + 224;
   Name_Fifo_Within_Priorities         : constant Name_Id := N + 224;
   Last_Task_Dispatching_Policy_Name   : constant Name_Id := N + 224;

   --  Names of recognized checks for pragma Suppress

   First_Check_Name                    : constant Name_Id := N + 225;
   Name_Access_Check                   : constant Name_Id := N + 225;
   Name_Accessibility_Check            : constant Name_Id := N + 226;
   Name_Discriminant_Check             : constant Name_Id := N + 227;
   Name_Division_Check                 : constant Name_Id := N + 228;
   Name_Elaboration_Check              : constant Name_Id := N + 229;
   Name_Index_Check                    : constant Name_Id := N + 230;
   Name_Length_Check                   : constant Name_Id := N + 231;
   Name_Overflow_Check                 : constant Name_Id := N + 232;
   Name_Range_Check                    : constant Name_Id := N + 233;
   Name_Storage_Check                  : constant Name_Id := N + 234;
   Name_Tag_Check                      : constant Name_Id := N + 235;
   Name_All_Checks                     : constant Name_Id := N + 236;
   Last_Check_Name                     : constant Name_Id := N + 236;

   --  Names corresponding to reserved keywords, excluding those already
   --  declared in the attribute list (Access, Delta, Digits, Range).

   Name_Abort                          : constant Name_Id := N + 237;
   Name_Abs                            : constant Name_Id := N + 238;
   Name_Abstract                       : constant Name_Id := N + 239;
   Name_Accept                         : constant Name_Id := N + 240;
   Name_And                            : constant Name_Id := N + 241;
   Name_All                            : constant Name_Id := N + 242;
   Name_Array                          : constant Name_Id := N + 243;
   Name_At                             : constant Name_Id := N + 244;
   Name_Begin                          : constant Name_Id := N + 245;
   Name_Body                           : constant Name_Id := N + 246;
   Name_Case                           : constant Name_Id := N + 247;
   Name_Constant                       : constant Name_Id := N + 248;
   Name_Declare                        : constant Name_Id := N + 249;
   Name_Delay                          : constant Name_Id := N + 250;
   Name_Do                             : constant Name_Id := N + 251;
   Name_Else                           : constant Name_Id := N + 252;
   Name_Elsif                          : constant Name_Id := N + 253;
   Name_End                            : constant Name_Id := N + 254;
   Name_Entry                          : constant Name_Id := N + 255;
   Name_Exception                      : constant Name_Id := N + 256;
   Name_Exit                           : constant Name_Id := N + 257;
   Name_For                            : constant Name_Id := N + 258;
   Name_Function                       : constant Name_Id := N + 259;
   Name_Generic                        : constant Name_Id := N + 260;
   Name_Goto                           : constant Name_Id := N + 261;
   Name_If                             : constant Name_Id := N + 262;
   Name_In                             : constant Name_Id := N + 263;
   Name_Is                             : constant Name_Id := N + 264;
   Name_Limited                        : constant Name_Id := N + 265;
   Name_Loop                           : constant Name_Id := N + 266;
   Name_Mod                            : constant Name_Id := N + 267;
   Name_New                            : constant Name_Id := N + 268;
   Name_Not                            : constant Name_Id := N + 269;
   Name_Null                           : constant Name_Id := N + 270;
   Name_Of                             : constant Name_Id := N + 271;
   Name_Or                             : constant Name_Id := N + 272;
   Name_Others                         : constant Name_Id := N + 273;
   Name_Out                            : constant Name_Id := N + 274;
   Name_Package                        : constant Name_Id := N + 275;
   Name_Pragma                         : constant Name_Id := N + 276;
   Name_Private                        : constant Name_Id := N + 277;
   Name_Procedure                      : constant Name_Id := N + 278;
   Name_Raise                          : constant Name_Id := N + 279;
   Name_Record                         : constant Name_Id := N + 280;
   Name_Rem                            : constant Name_Id := N + 281;
   Name_Renames                        : constant Name_Id := N + 282;
   Name_Return                         : constant Name_Id := N + 283;
   Name_Reverse                        : constant Name_Id := N + 284;
   Name_Select                         : constant Name_Id := N + 285;
   Name_Separate                       : constant Name_Id := N + 286;
   Name_Subtype                        : constant Name_Id := N + 287;
   Name_Task                           : constant Name_Id := N + 288;
   Name_Terminate                      : constant Name_Id := N + 289;
   Name_Then                           : constant Name_Id := N + 290;
   Name_Type                           : constant Name_Id := N + 291;
   Name_Use                            : constant Name_Id := N + 292;
   Name_When                           : constant Name_Id := N + 293;
   Name_While                          : constant Name_Id := N + 294;
   Name_With                           : constant Name_Id := N + 295;
   Name_Xor                            : constant Name_Id := N + 296;

   --  Reserved words used only in Ada 9X

   First_9X_Reserved_Word              : constant Name_Id := N + 297;
   Name_Aliased                        : constant Name_Id := N + 297;
   Name_Protected                      : constant Name_Id := N + 298;
   Name_Until                          : constant Name_Id := N + 299;
   Name_Requeue                        : constant Name_Id := N + 300;
   Name_Tagged                         : constant Name_Id := N + 301;
   Last_9X_Reserved_Word               : constant Name_Id := N + 301;

   subtype Ada_9X_Reserved_Words is
     Name_Id range First_9X_Reserved_Word .. Last_9X_Reserved_Word;

   --  Mark last defined name for consistency check in Snames body

   Last_Predefined_Name                : constant Name_Id := N + 301;

   ------------------------------
   -- Attribute ID Definitions --
   ------------------------------

   type Attribute_Id is (
      Attribute_Access,
      Attribute_Address,
      Attribute_Adjacent,
      Attribute_Aft,
      Attribute_Alignment,
      Attribute_Bit_Order,
      Attribute_Body_Version,
      Attribute_Callable,
      Attribute_Caller,
      Attribute_Ceiling,
      Attribute_Component_Size,
      Attribute_Compose,
      Attribute_Constrained,
      Attribute_Copy_Sign,
      Attribute_Count,
      Attribute_Delta,
      Attribute_Denorm,
      Attribute_Digits,
      Attribute_Emax,
      Attribute_Epsilon,
      Attribute_Exponent,
      Attribute_External_Tag,
      Attribute_First,
      Attribute_First_Bit,
      Attribute_Floor,
      Attribute_Fore,
      Attribute_Fraction,
      Attribute_Identity,
      Attribute_Image,
      Attribute_Input,
      Attribute_Large,
      Attribute_Last,
      Attribute_Last_Bit,
      Attribute_Leading_Part,
      Attribute_Length,
      Attribute_Machine,
      Attribute_Machine_Emax,
      Attribute_Machine_Emin,
      Attribute_Machine_Mantissa,
      Attribute_Machine_Overflows,
      Attribute_Machine_Radix,
      Attribute_Machine_Rounds,
      Attribute_Mantissa,
      Attribute_Max,
      Attribute_Max_Size_In_Storage_Elements,
      Attribute_Min,
      Attribute_Model,
      Attribute_Model_Emax,
      Attribute_Model_Emin,
      Attribute_Model_Epsilon,
      Attribute_Model_Large,
      Attribute_Model_Mantissa,
      Attribute_Model_Small,
      Attribute_Output,
      Attribute_Pos,
      Attribute_Position,
      Attribute_Pred,
      Attribute_Range,
      Attribute_Read,
      Attribute_Remainder,
      Attribute_Round,
      Attribute_Rounding,
      Attribute_Safe_Emax,
      Attribute_Safe_First,
      Attribute_Safe_Large,
      Attribute_Safe_Last,
      Attribute_Safe_Small,
      Attribute_Scale,
      Attribute_Signed_Zeros,
      Attribute_Size,
      Attribute_Small,
      Attribute_Storage_Pool,
      Attribute_Storage_Size,
      Attribute_Succ,
      Attribute_Tag,
      Attribute_Terminated,
      Attribute_Truncation,
      Attribute_Unbiased_Rounding,
      Attribute_Unchecked_Access,
      Attribute_Val,
      Attribute_Valid,
      Attribute_Value,
      Attribute_Version,
      Attribute_Wide_Image,
      Attribute_Wide_Value,
      Attribute_Width,
      Attribute_Write,

      --  Type attributes

      Attribute_Base,
      Attribute_Class,
      Attribute_Standard_Access);

   subtype Type_Attribute_Id is Attribute_Id range
     Attribute_Base .. Attribute_Standard_Access;

   -------------------------------
   -- Check Name ID Definitions --
   -------------------------------

   type Check_Id is (
      Access_Check,
      Accessibility_Check,
      Discriminant_Check,
      Division_Check,
      Elaboration_Check,
      Index_Check,
      Length_Check,
      Overflow_Check,
      Range_Check,
      Storage_Check,
      Tag_Check,
      All_Checks);

   ------------------------------------
   -- Convention Name ID Definitions --
   ------------------------------------

   type Convention_Id is (
      Convention_Ada,
      Convention_Asm,
      Convention_Assembler,
      Convention_C,
      Convention_Intrinsic);

   -----------------------------------
   -- Locking Policy ID Definitions --
   -----------------------------------

   type Locking_Policy_Id is (
      Locking_Policy_Ceiling_Locking);

   ---------------------------
   -- Pragma ID Definitions --
   ---------------------------

   type Pragma_Id is (
      Pragma_Abort_Defer,
      Pragma_Ada_83,
      Pragma_Ada_9X,
      Pragma_All_Calls_Remote,
      Pragma_Assert,
      Pragma_Asynchronous,
      Pragma_Atomic,
      Pragma_Atomic_Components,
      Pragma_Attach_Handler,
      Pragma_Controlled,
      Pragma_Convention,
      Pragma_Debug,
      Pragma_Elaborate,
      Pragma_Elaborate_All,
      Pragma_Elaborate_Body,
      Pragma_Export,
      Pragma_Import,
      Pragma_Improve,
      Pragma_Inline,
      Pragma_Inspection_Point,
      Pragma_Interface,
      Pragma_Interface_Name,
      Pragma_Interrupt_Handler,
      Pragma_Interrupt_Priority,
      Pragma_List,
      Pragma_Locking_Policy,
      Pragma_Memory_Size,
      Pragma_Normalize_Scalars,
      Pragma_Optimize,
      Pragma_Pack,
      Pragma_Page,
      Pragma_Preelaborate,
      Pragma_Priority,
      Pragma_Pure,
      Pragma_Queuing_Policy,
      Pragma_Remote_Call_Interface,
      Pragma_Remote_Types,
      Pragma_Restrictions,
      Pragma_Reviewable,
      Pragma_Shared,
      Pragma_Shared_Passive,
      Pragma_Storage_Unit,
      Pragma_Suppress,
      Pragma_System_Name,
      Pragma_Task_Dispatching_Policy,
      Pragma_Task_Stack_Size,
      Pragma_Volatile,
      Pragma_Volatile_Components);

   ------------------------------------
   -- Queueing Policy ID definitions --
   ------------------------------------

   type Queuing_Policy_Id is (
      Queuing_Policy_FIFO_Queuing,
      Queuing_Policy_Priority_Queuing);

   --------------------------------------------
   -- Task Dispatching Policy ID definitions --
   --------------------------------------------

   type Task_Dispatching_Policy_Id is (
      Task_Dispatching_FIFO_Within_Priorities);
   --  Id values used to identify task dispatching policies

   -----------------
   -- Subprograms --
   -----------------

   procedure Initialize_Snames;
   --  Called to initialize the preset names in the names table.

   function Is_Attribute_Name (N : Name_Id) return Boolean;
   pragma Inline (Is_Attribute_Name);
   --  Test to see if the name N is the name of a recognized attribute

   function Is_Type_Attribute_Name (N : Name_Id) return Boolean;
   pragma Inline (Is_Type_Attribute_Name);
   --  Test to see if the name N is the name of a recognized type
   --  returning attribute

   function Is_Check_Name (N : Name_Id) return Boolean;
   pragma Inline (Is_Check_Name);
   --  Test to see if the name N is the name of a recognized suppress check
   --  as required by pragma Suppress.

   function Is_Convention_Name (N : Name_Id) return Boolean;
   pragma Inline (Is_Convention_Name);
   --  Test to see if the name N is the name of one of the recognized language
   --  conventions, as required by pragma Convention, Import, Export, Interface

   function Is_Locking_Policy_Name (N : Name_Id) return Boolean;
   pragma Inline (Is_Locking_Policy_Name);
   --  Test to see if the name N is the name of a recognized locking policy

   function Is_Operator_Symbol_Name (N : Name_Id) return Boolean;
   pragma Inline (Is_Operator_Symbol_Name);
   --  Test to see if the name N is the name of an operator symbol

   function Is_Pragma_Name (N : Name_Id) return Boolean;
   pragma Inline (Is_Pragma_Name);
   --  Test to see if the name N is the name of a recognized pragma

   function Is_Queuing_Policy_Name (N : Name_Id) return Boolean;
   pragma Inline (Is_Queuing_Policy_Name);
   --  Test to see if the name N is the name of a recognized queuing policy

   function Is_Task_Dispatching_Policy_Name (N : Name_Id) return Boolean;
   pragma Inline (Is_Task_Dispatching_Policy_Name);
   --  Test to see if the name N is the name of a recognized
   --  task dispatching policy

   function Get_Attribute_Id (N : Name_Id) return Attribute_Id;
   --  Returns Id of attribute corresponding to given name. It is an error to
   --  call this function with a name that is not the name of a attribute.

   function Get_Convention_Id (N : Name_Id) return Convention_Id;
   --  Returns Id of language convention corresponding to given name. It is an
   --  to call this function with a name that is not the name of a check.

   function Get_Check_Id (N : Name_Id) return Check_Id;
   --  Returns Id of suppress check corresponding to given name. It is an error
   --  to call this function with a name that is not the name of a check.

   function Get_Locking_Policy_Id (N : Name_Id) return Locking_Policy_Id;
   --  Returns Id of locking policy corresponding to given name. It is an error
   --  to call this function with a name that is not the name of a check.

   function Get_Pragma_Id (N : Name_Id) return Pragma_Id;
   --  Returns Id of pragma corresponding to given name. It is an error to
   --  call this function with a name that is not the name of a pragma.

   function Get_Queuing_Policy_Id (N : Name_Id) return Queuing_Policy_Id;
   --  Returns Id of queuing policy corresponding to given name. It is an error
   --  to call this function with a name that is not the name of a check.

   function Get_Task_Dispatching_Policy_Id (N : Name_Id)
     return Task_Dispatching_Policy_Id;
   --  Returns Id of task dispatching policy corresponding to given name. It
   --  is an error to call this function with a name that is not the name
   --  of a check.

end Snames;
