------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                U N A M E                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.25 $                             --
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
with Casing;  use Casing;
with Comperr; use Comperr;
with Lib;     use Lib;
with Limits;  use Limits;
with Namet;   use Namet;
with Output;  use Output;
with Sinfo;   use Sinfo;

package body Uname is

   -----------------------
   -- Name_To_Unit_Name --
   -----------------------

   function Name_To_Unit_Name (N : Name_Id) return Unit_Name_Type is
   begin
      Get_Name_String (N);
      Name_Buffer (Name_Len + 1) := '%';
      Name_Buffer (Name_Len + 2) := 's';
      return Name_Find;
   end Name_To_Unit_Name;

   -------------------
   -- Get_Unit_Name --
   -------------------

   function Get_Unit_Name (N : Node_Id) return Unit_Name_Type is

      Unit_Name_Buffer : Str (1 .. Max_Name_Length);
      --  Buffer used to build name of unit. Note that we cannot use the
      --  Name_Buffer in package Name_Table because we use it to read
      --  component names.

      Unit_Name_Length : Int := 0;
      --  Length of name stored in Unit_Name_Buffer

      Node : Node_Id;
      --  Program unit node

      procedure Add_Char (C : Char) is
      --  Add a single character to stored unit name
      begin
         --  Should really check for max length exceeded here
         Unit_Name_Length := Unit_Name_Length + 1;
         Unit_Name_Buffer (Unit_Name_Length) := C;
      end Add_Char;

      procedure Add_Name (Name : Name_Id) is
      --  Add the characters of a names table entry to stored unit name
      begin
         Get_Name_String (Name);

         for I in 1 .. Name_Len loop
            Add_Char (Name_Buffer (I));
         end loop;
      end Add_Name;

      function Get_Parent (Node : Node_Id) return Node_Id is
      --  Get parent compilation unit of a stub
         N : Node_Id := Node;

      begin
         while Nkind (N) /= N_Compilation_Unit loop
            N := Parent (N);
         end loop;

         return N;
      end Get_Parent;

      procedure Add_Node_Name (Node : Node_Id) is
      --  Recursive procedure adds characters associated with Node
         Kind : Node_Kind := Nkind (Node);

      begin
         --  Just ignore an error node (someone else will give a message)

         if Node = Error then
            return;

         --  Otherwise see what kind of node we have

         elsif Kind = N_Identifier or else Kind = N_Defining_Identifier then
            Add_Name (Chars (Node));

         elsif Kind = N_Defining_Program_Unit_Name then
            Add_Node_Name (Name (Node));
            Add_Char ('.');
            Add_Node_Name (Defining_Identifier (Node));

         elsif Kind = N_Selected_Component then
            Add_Node_Name (Prefix (Node));
            Add_Char ('.');
            Add_Node_Name (Selector_Name (Node));

         elsif Kind in N_Subprogram_Specification
           or else Kind = N_Package_Specification
         then
            Add_Node_Name (Defining_Unit_Name (Node));

         elsif Kind = N_Subprogram_Body
           or else Kind = N_Subprogram_Declaration
           or else Nkind (Node) = N_Package_Declaration
           or else Nkind (Node) in N_Generic_Declaration
         then
            Add_Node_Name (Specification (Node));

         elsif Kind in N_Generic_Instantiation then
            Add_Node_Name (Defining_Unit_Name (Node));

         elsif Kind = N_Package_Body then
            Add_Node_Name (Defining_Unit_Name (Node));

         elsif Kind = N_Task_Body or else Kind = N_Protected_Body then
            Add_Node_Name (Defining_Identifier (Node));

         elsif Kind = N_Package_Renaming_Declaration then
            Add_Node_Name (Defining_Unit_Name (Node));

         elsif Kind = N_Subprogram_Renaming_Declaration then
            Add_Node_Name (Specification (Node));

         elsif Kind = N_Subprogram_Body_Stub then
            Add_Node_Name (Get_Parent (Node));
            Add_Char ('.');
            Add_Node_Name (Specification (Node));

         elsif Kind = N_Compilation_Unit then
            Add_Node_Name (Unit (Node));

         elsif Kind = N_Package_Body_Stub then
            Add_Node_Name (Get_Parent (Node));
            Add_Char ('.');
            Add_Node_Name (Defining_Identifier (Node));

         elsif Kind = N_Task_Body_Stub
           or else Kind = N_Protected_Body_Stub
         then
            Add_Node_Name (Get_Parent (Node));
            Add_Char ('.');
            Add_Node_Name (Defining_Identifier (Node));

         elsif Kind = N_Subunit then
            Add_Node_Name (Name (Node));
            Add_Char ('.');
            Add_Node_Name (Proper_Body (Node));

         elsif Kind = N_With_Clause then
            Add_Node_Name (Name (Node));

         elsif Kind = N_Pragma then
            Add_Node_Name (Expression (First
              (Pragma_Argument_Associations (Node))));

         else
            Compiler_Error;
            Write_Eol;
            Write_String (Node_Kind'Image (Kind));
            Write_Eol;
            Compiler_Abort;
         end if;
      end Add_Node_Name;

   --  Start of processing for Get_Unit_Name

   begin
      Node := N;

      --  If we have Defining_Identifier, find the associated unit node

      if Nkind (Node) = N_Defining_Identifier then
         Node := Parent (Node);

         if Nkind (Node) = N_Defining_Program_Unit_Name then
            Node := Parent (Node);
         end if;
      end if;

      --  Node points to the unit, so get its name and add proper suffix

      Add_Node_Name (Node);
      Add_Char ('%');

      if Nkind (Node) in N_Generic_Declaration
        or else Nkind (Node) = N_Subprogram_Declaration
        or else Nkind (Node) = N_Package_Declaration
        or else Nkind (Node) = N_With_Clause
        or else Nkind (Node) = N_Pragma
        or else Nkind (Node) in N_Generic_Instantiation
        or else Nkind (Node) = N_Package_Renaming_Declaration
        or else Nkind (Node) = N_Subprogram_Renaming_Declaration
      then
         Add_Char ('s');

      elsif Nkind (Node) = N_Subprogram_Body
        or else Nkind (Node) = N_Package_Body
        or else Nkind (Node) = N_Subunit
        or else Nkind (Node) in N_Body_Stub
      then
         Add_Char ('b');

      else
         Compiler_Error;
         Write_Eol;
         Write_String (Node_Kind'Image (Nkind (Node)));
         Write_Eol;
         Compiler_Abort;
      end if;

      Name_Buffer (1 .. Unit_Name_Length)
        := Unit_Name_Buffer (1 .. Unit_Name_Length);
      Name_Len := Unit_Name_Length;
      return Name_Find;

   end Get_Unit_Name;

   -------------------
   -- Get_Body_Name --
   -------------------

   function Get_Body_Name (N : Unit_Name_Type) return Unit_Name_Type is
   begin
      Get_Name_String (N);

      pragma Assert (Name_Len > 2
                       and then Name_Buffer (Name_Len - 1) = '%'
                       and then Name_Buffer (Name_Len) = 's');

      Name_Buffer (Name_Len) := 'b';
      return Name_Find;
   end Get_Body_Name;

   -------------------
   -- Get_Spec_Name --
   -------------------

   function Get_Spec_Name (N : Unit_Name_Type) return Unit_Name_Type is
   begin
      Get_Name_String (N);

      pragma Assert (Name_Len > 2
                       and then Name_Buffer (Name_Len - 1) = '%'
                       and then Name_Buffer (Name_Len) = 'b');

      Name_Buffer (Name_Len) := 's';
      return Name_Find;
   end Get_Spec_Name;

   --------------------------
   -- Get_Parent_Body_Name --
   --------------------------

   function Get_Parent_Body_Name (N : Unit_Name_Type) return Unit_Name_Type is
   begin
      Get_Name_String (N);

      while Name_Buffer (Name_Len) /= '.' loop
         pragma Assert (Name_Len > 1); -- not a child or subunit name
         Name_Len := Name_Len - 1;
      end loop;

      Name_Buffer (Name_Len) := '%';
      Name_Len := Name_Len + 1;
      Name_Buffer (Name_Len) := 'b';
      return Name_Find;

   end Get_Parent_Body_Name;

   --------------------------
   -- Get_Parent_Spec_Name --
   --------------------------

   function Get_Parent_Spec_Name (N : Unit_Name_Type) return Unit_Name_Type is
   begin
      Get_Name_String (N);

      while Name_Buffer (Name_Len) /= '.' loop
         if Name_Len = 1 then
            return No_Name; -- not a child or subunit name
         else
            Name_Len := Name_Len - 1;
         end if;
      end loop;

      Name_Buffer (Name_Len) := '%';
      Name_Len := Name_Len + 1;
      Name_Buffer (Name_Len) := 's';
      return Name_Find;

   end Get_Parent_Spec_Name;

   --------------------------
   -- Get_Unit_Name_String --
   --------------------------

   procedure Get_Unit_Name_String (N : Unit_Name_Type) is
      Unit_Is_Body : Boolean;

   begin
      Get_Name_String (N);
      Unit_Is_Body := Name_Buffer (Name_Len) = 'b';
      Set_Casing (File.Table (Main_Unit).Identifier_Casing, Mixed_Case);

      if Unit_Is_Body then
         Name_Buffer (Name_Len - 1 .. Name_Len + 5) := " (body)";
      else
         Name_Buffer (Name_Len - 1 .. Name_Len + 5) := " (spec)";
      end if;

      for I in 1 .. Name_Len loop
         if Name_Buffer (I) = '-' then
            Name_Buffer (I) := '.';
         end if;
      end loop;

      Name_Len := Name_Len + (7 - 2);
   end Get_Unit_Name_String;

   ---------------------
   -- Write_Unit_Name --
   ---------------------

   procedure Write_Unit_Name (N : Unit_Name_Type) is
   begin
      Get_Unit_Name_String (N);
      Write_Str (Name_Buffer (1 .. Name_Len));
   end Write_Unit_Name;

   ------------------
   -- Is_Spec_Name --
   ------------------

   function Is_Spec_Name (N : Unit_Name_Type) return Boolean is
   begin
      Get_Name_String (N);
      return Name_Len > 2
        and then Name_Buffer (Name_Len - 1) = '%'
        and then Name_Buffer (Name_Len) = 's';
   end Is_Spec_Name;

   ------------------
   -- Is_Body_Name --
   ------------------

   function Is_Body_Name (N : Unit_Name_Type) return Boolean is
   begin
      Get_Name_String (N);
      return Name_Len > 2
        and then Name_Buffer (Name_Len - 1) = '%'
        and then Name_Buffer (Name_Len) = 'b';
   end Is_Body_Name;

   -------------------
   -- Is_Child_Name --
   -------------------

   function Is_Child_Name (N : Unit_Name_Type) return Boolean is
      J : Int;
   begin
      Get_Name_String (N);
      J := Name_Len;

      while Name_Buffer (J) /= '.' loop
         if J = 1 then
            return False; -- not a child or subunit name
         else
            J := J - 1;
         end if;
      end loop;

      return True;
   end Is_Child_Name;

   --------------
   -- Uname_Lt --
   --------------

   function Uname_Lt (Left, Right : Unit_Name_Type) return Boolean is
      Left_Name    : Str (1 .. Max_Name_Length);
      Left_Length  : Int;
      Right_Name   : Str renames Name_Buffer;
      Right_Length : Int renames Name_Len;
      I : Pos;

   begin
      if Left = Right then
         return False;
      end if;

      Get_Name_String (Left);
      Left_Name  (1 .. Name_Len + 1) := Name_Buffer (1 .. Name_Len + 1);
      Left_Length := Name_Len;
      Get_Name_String (Right);
      I := 1;

      loop
         exit when Left_Name (I) = '%';

         if Right_Name (I) = '%' then
            return False; -- left name is longer
         end if;

         pragma Assert (I <= Left_Length and then I <= Right_Length);

         if Left_Name (I) /= Right_Name (I) then
            return Left_Name (I) < Right_Name (I); -- parent names different
         end if;

         I := I + 1;
      end loop;

      --  Come here pointing to % in left name

      if Right_Name (I) /= '%' then
         return True; -- right name is longer
      end if;

      --  Here the parent names are the same and specs sort low. If neither is
      --  a spec, then we are comparing the same name and we want a result of
      --  False in any case.

      return Left_Name (I + 1) = 's';
   end Uname_Lt;

   --------------
   -- Uname_Gt --
   --------------

   function Uname_Gt (Left, Right : Unit_Name_Type) return Boolean is
   begin
      return Left /= Right and then not Uname_Lt (Left, Right);
   end Uname_Gt;

   --------------
   -- Uname_Le --
   --------------

   function Uname_Le (Left, Right : Unit_Name_Type) return Boolean is
   begin
      return Left = Right or else Uname_Lt (Left, Right);
   end Uname_Le;

   --------------
   -- Uname_Ge --
   --------------

   function Uname_Ge (Left, Right : Unit_Name_Type) return Boolean is
   begin
      return Left = Right or else Uname_Gt (Left, Right);
   end Uname_Ge;

end Uname;
