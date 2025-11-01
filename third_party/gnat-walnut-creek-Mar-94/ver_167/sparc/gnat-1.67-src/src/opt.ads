------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  O P T                                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.31 $                             --
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

--  This package contains global switches set by the initialization
--  routine from the command line and referenced throughout the compiler
--  or binder. The comments indicate which options are used by the compiler
--  which are used by the binder, and which are common to both programs.

with Types; use Types;
package Opt is

   --------------------------------------------
   -- Switches Common to Compiler and Binder --
   --------------------------------------------

   Brief_Output : Boolean := False;
   --  Force brief error messages to standard error, even if verbose mode is
   --  set (so that main error messages go to standard output).

   Identifier_Character_Set : Char := '1';
   --  This variable indicates the character set to be used for identifiers.
   --  The possible settings are:
   --    '1'  Latin-1
   --    '2'  Latin-2
   --    '3'  Latin-3
   --    '4'  Latin-4
   --    'p'  PC
   --    'f'  Full upper set (all distinct)
   --    'n'  No upper characters (Ada/83 rules)

   --  The setting affects the set of letters allowed in identifiers and the
   --  upper/lower case equivalences. It does not affect the interpretation of
   --  character and string literals, which are always stored using the actual
   --  coding in the source program.

   Maximum_Errors : Int := 9999;
   --  Maximum number of errors before compilation is terminated

   Maximum_File_Name_Length : Int := Int'Last;
   --  Maximum number of characters allowed in a file name, not counting the
   --  extension, as set by the appropriate switch. A value of Int'Last means
   --  that the switch was not present (in which case the maximum file name
   --  length will be taken from Osint.System_Maximum_File_Name_Length).

   Suppress_Options : Suppress_Record;
   --  Flags set True to globally suppress the corresponding checks (i.e. to
   --  cause the compiler to act as though the corresponding Suppress pragma
   --  is always in effect). These are initialized from command line options.

   Verbose_Mode : Boolean := False;
   --  Set to True to get verbose mode (full error message text and location
   --  information sent to standard output, also header, copyright and summary

   type Warning_Mode_Type is (Suppress, Normal, Treat_As_Error);
   Warning_Mode : Warning_Mode_Type := Normal;
   --  Controls treatment of warning messages. If set to Suppress, warning
   --  messages are not generated at all. In Normal mode, they are generated
   --  but do not count as errors. In Treat_As_Error mode, warning messages
   --  are generated and are treated as errors.

   ----------------------------------------
   -- Switches Used Only By The Compiler --
   ----------------------------------------

   Ada_83_Switch : Boolean := False;
   --  This is the value of the command line switch for Ada 83 mode. At the
   --  start of compiling a unit, Ada_9X and Ada_83 are set from this value
   --  but then they can be subsequently modified by pragmas Ada_83, Ada_9X.

   Ada_9X : Boolean := True;
   --  Set True if operating in Ada/9X mode
   --  Set False if operating in Ada/83 mode

   Ada_83 : Boolean := False;
   --  Set True if operating in Ada/83 mode
   --  Set False if operating in Ada/9X mode

   All_Errors_Mode : Boolean := False;
   --  Flag set to force display of multiple errors on a single line

   Assertions_Enabled : Boolean := False;
   --  Enable assertions made using pragma Assert

   Full_List : Boolean := False;
   --  Set True to generate full source listing with embedded errors

   GNAT_Style_Check : Boolean := False;
   --  Set True to perform GNAT style checks. Note that if this is set, then
   --  RM_Column_Check is also set (i.e. GNAT style checking always includes
   --  the RM column checks as well).

   Immediate_Errors : Boolean := False;
   --  If set to True, then error messages are output as soon as they are
   --  detected (useful for navigating around compiler error situations)

   Inline_Active : Boolean := True;
   --  Set False to deactive pragma Inline processing across modules. Default
   --  to inline across module boundaries, as specified in the RM.

   List_Units : Boolean := False;
   --  List units in the active lbrary

   type Mode_Type is (Check_Syntax, Check_Semantics, Generate_Code);
   Operating_Mode : Mode_Type := Generate_Code;
   --  Indicates the operating mode of the compiler. The default is generate
   --  code, which runs the parser, semantics and backend. Switches can be
   --  used to set syntax checking only mode, or syntax and semantics checking
   --  only mode. Operating_Mode can also be modified as a result of detecting
   --  errors during the compilation process. In particular if any error is
   --  detected then Operating_Mode is reset from Generate_Code to
   --  to Check_Semantics after generating an error message.

   Try_Semantics : Boolean := False;
   --  Flag set to force attempt at semantic analysis, even if parser errors
   --  occur. This will probably cause blowups at this stage in the game!

   RM_Column_Check : Boolean := False;
   --  Flag set to cause column alignment to be taken into account in
   --  determining legality of various constructs. Note that this is
   --  always set True if GNAT_Style_Check is set True.

   --------------------------------------
   -- Switches Used Only By The Binder --
   --------------------------------------

   All_Sources : Boolean := False;
   --  Set to True to require all source files to be present

   Bind_Main_Program : Boolean := True;
   --  Set to False if not binding main Ada program

   Check_Only : Boolean := False;
   --  Set to True to do checks only, no output of binder file

   Check_Source_Files : Boolean := False;
   --  Set to True to enable consistency checking for any source files that
   --  are present (i.e. date must match the date in the library info file).

   Elab_Dependency_Output : Boolean := False;
   --  Set to True to output complete list of elaboration constraints

   Elab_Order_Output : Boolean := False;
   --  Set to True to output chosen elaboration order

   Ignore_Time_Stamp_Errors : Boolean := False;
   --  Ignore time stamp mismatch errors (treat as warnings only)

   Output_Filename_Present : Boolean := False;
   --  Set to True when the output C filename is given with option -o

end Opt;
