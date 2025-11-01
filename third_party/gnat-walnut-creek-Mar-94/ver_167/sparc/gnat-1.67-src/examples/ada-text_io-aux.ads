------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      A D A . T E X T _ I O . A U X                       --
--                                                                          --
--                                 S p e c                                  -- 
--                                                                          -- 
--                            $Revision: 1.9 $                              --
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

private package Ada.Text_Io.Aux is

   Current_In  : File_Type;
   Current_Out : File_Type;
   Current_Err : File_Type;
   The_File    : File_Type;

   procedure Create (File : in out File_Type;
                     Mode : in File_Mode := Out_File;
                     Name : in String := "";
                     Form : in String := "");

   procedure Open   (File : in out File_Type;
                     Mode : in File_Mode;
                     Name : in String;
                     Form : in String := "");

   procedure Close  (File : in out File_Type);
   procedure Delete (File : in out File_Type);

   procedure Reset (File : in out File_Type;
                    Mode : in File_Mode);

   function Mode (File : in File_Type) return File_Mode;
   function Name (File : in File_Type) return String;
   function Form (File : in File_Type) return String;

   function Is_Open (File : in File_Type) return Boolean;

   ------------------------------------------------------
   -- Control of default input, output and error files --
   ------------------------------------------------------

   procedure Set_Input  (File : in File_Type);
   procedure Set_Output (File : in File_Type);
   procedure Set_Error (File : in File_Type);

   function Standard_Input return File_Type;
   function Standard_Output return File_Type;
   function Standard_Error return File_Type;

   function Current_Input return File_Type;
   function Current_Output return File_Type;
   function Current_Error return File_Type;

   --------------------------------------------
   -- Specification of line and page lengths --
   --------------------------------------------

   procedure Set_Line_Length (File : in File_Type; To : in Count);
   procedure Set_Page_Length (File : in File_Type; To : in Count);
   function Line_Length (File : in File_Type) return Count;
   function Page_Length (File : in File_Type) return Count;

   ------------------------------------
   -- Column, Line, and Page Control --
   ------------------------------------

   procedure New_Line (File : in File_Type;
                       Spacing : in Positive_Count := 1);

   procedure Skip_Line (File : in File_Type;
                        Spacing : in Positive_Count := 1);

   function End_Of_Line (File : in File_Type) return Boolean;

   procedure New_Page  (File : in File_Type);
   procedure Skip_Page (File : in File_Type);

   function End_Of_Page  (File : in File_Type) return boolean;
   function End_Of_File  (File : in File_Type) return boolean;

   procedure Set_Col  (File : in File_Type; To : in Positive_Count);
   procedure Set_Line (File : in File_Type; To : in Positive_Count);

   function Col   (File : in File_Type) return Positive_Count;
   function Line  (File : in File_Type) return Positive_Count;
   function Page  (File : in File_Type) return Positive_Count;

   -----------------------------
   -- Characters Input-Output --
   -----------------------------

   procedure Get (Item : out Character);
   procedure Put (Item : in Character);

   --------------------------
   -- Strings Input-Output --
   --------------------------

   procedure Get (Item : out String);
   procedure Put (Item : in String);

   procedure Get_Line (File : in File_Type;
                       Item : out String;
                       Last : out Natural);

   procedure Put_Line (File : in File_Type; Item : in String);

   -----------------------------------
   -- Input-Output of Integer Types --
   -----------------------------------

   Default_Width_Int : Field := 11;
   Default_Base_Int  : Number_Base := 10;

   procedure Get_Int (Item  : out Integer;
                      Width : in Field := 0);
   procedure Put_Int (Item  : in Integer;
                      Width : in Field;
                      Base  : in Number_Base);
   procedure Get_Int (From : in String;
                      Item : out Integer;
                      Last : out Positive);
   procedure Put_Int (To   : out String;
                      Item : in Integer;
                      Base : in Number_Base);

   -----------------------------------
   -- Input-Output of Float Types   --
   -----------------------------------

   procedure Get_Float (Item : out Float;
                        Width : in Field);

   procedure Put_Float (Item : in Float;
                        Fore : in Field;
                        Aft  : in Field;
                        Exp  : in Field);

   procedure Get_Float (From : in String;
                        Item : out Float;
                        Last : out Positive);

   procedure Put_Float (To   : out String;
                        Item : in Float;
                        Aft  : in Field;
                        Exp  : in Field);

   procedure Text_IO_Finalization;
   --  Finialization cleanup routine for Text_IO files

   ----------------------------------------------------------------
   --  Variables used to share information with body of Text_IO  --
   ----------------------------------------------------------------

   type Work_String_Type is array (0 .. 1023) of Character;
   Work_String  : Work_String_Type;
   WS_Length    : Natural := 0;
   WS_Index1    : Natural := 0;
   WS_Index2    : Natural := 0;
   WS_Index3    : Natural := 0;
end Ada.Text_Io.Aux;


----------------------
-- REVISION HISTORY --
----------------------

--  ----------------------------
--  revision 1.7
--  date: Mon Dec 13 18:12:23 1993;  author: banner
--  Eliminate Work_String2.
--  (Get_Float, Put_Float): new procedures.
--  (Put_Int): remove default value for Base parameter.
--  ----------------------------
--  revision 1.8
--  date: Mon Dec 20 00:08:33 1993;  author: banner
--  (Current_Error): new Ada 9X function.
--  (Standard_Error): new Ada 9X function.
--  (Set_Error): new Ada 9X function.
--  Add declaration for Current_Err.
--  ----------------------------
--  revision 1.9
--  date: Wed Dec 22 13:23:10 1993;  author: banner
--  Change upper bound of Work_String to 1023 to allow for longer strings.
--  ----------------------------
--  New changes after this line.  Each line starts with: "--  "
