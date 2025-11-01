------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               X R E F _ T A B                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.3 $                              --
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

with Output;  use Output;
with Atree;   use Atree;
with Lib;     use Lib;
with Namet;   use Namet;
with Osint;   use Osint;
with Sinfo;   use Sinfo;
with Sinput;  use Sinput;
with Stand;   use Stand;

package body Xref_Tab is

   -----------------------
   -- Local subprograms --
   -----------------------

   function Scope_Path (The_Entity : Entity_Id) return Str;
   --  Returns the path string of the given entity.
   --  A path string consists of the name of the entity followed
   --  by the hierarchical scopes.
   --  The scope entities are separated by a point. However the first
   --  separator changes to a '#' if the entity is not accessible from
   --  outside the unit.


   ----------------
   -- Add_Entity --
   ----------------

   procedure Add_Entity (To_Etbl     : in     Entity_Table_Acc;
                         Entity_Node : in     Entity_Id;
                         New_Entity  : in out Entity_Acc) is

      The_Kind     : Entity_Kind := Ekind (Entity_Node);
      Parent_Node  : Node_Id     := Parent (Entity_Node);
      Grand_Parent : Node_Id;

   begin

      Get_Name_String (Chars (Entity_Node));

      --  Check if it's an internal entity with two underlines in its
      --  name (e.g. loop__418)

      if not Double_Line_Entity then

         New_Entity := new An_Entity;

         New_Entity.Chars       := new Str'(Name_Buffer (1 .. Name_Len));
         New_Entity.Entity_Node := Entity_Node;
         New_Entity.Entity_Type := The_Kind;

         New_Entity.Line_Number := Int (Get_Line_Number (Sloc (Entity_Node)));
         New_Entity.Is_Internal := Is_Internal (Entity_Node);
         New_Entity.Scope_Path  := new Str'(Scope_Path (Entity_Node));

         if (To_Etbl.Length = 0) then
            To_Etbl.First_Entity := New_Entity;
            To_Etbl.Last_Entity  := New_Entity;
         else
            To_Etbl.Last_Entity.Next_Entity := New_Entity;
            To_Etbl.Last_Entity             := New_Entity;
         end if;

         To_Etbl.Length := To_Etbl.Length + 1;


         --  We give no warnings if certain nodes have no references:
         --
         --  1. an enumeration literal
         --  2. a record component
         --  3. a package name in a package body or body stub
         --  4. a subprogram name or its parameters in a subprogram body
         --     or body stub which does not act as a spec.
         --
         --  We do this because certain hidden references
         --  (e.g. within a range construct or an aggregate)
         --  don't appear in our reference list or because
         --  an identifier always points to the subprogram name
         --  in the spec.

         if The_Kind = E_Void
           or else The_Kind = E_Enumeration_Literal 
           or else Nkind (Parent_Node) = N_Component_Declaration 
           or else Nkind (Parent_Node) = N_Loop_Parameter_Specification
           or else Nkind (Parent_Node) in N_Package_Body .. N_Task_Body
           or else Nkind (Parent_Node) in N_Body_Stub
           or else (The_Kind in Subprogram_Kind
          and then Nkind (Parent (Parent_Node)) = N_Subprogram_Body 
          and then Corresponding_Spec (Parent (Parent_Node)) /= Empty) then

            New_Entity.Give_Warning := False;
         end if;

         if The_Kind in Formal_Kind then

            Grand_Parent := Parent (Parent_Node);
            if ((Nkind (Parent (Grand_Parent)) = N_Subprogram_Body
              or else Nkind (Parent (Grand_Parent)) = N_Subprogram_Body_Stub)
                and then Corresponding_Spec (Parent (Grand_Parent)) /= Empty)
              or else (Is_Overloadable (Defining_Unit_Name (Grand_Parent))
                and then Is_Imported (Defining_Unit_Name (Grand_Parent)))
            then

               New_Entity.Give_Warning := False;
            end if;

            if Is_Internal (Defining_Unit_Name (Grand_Parent)) then
               New_Entity.Is_Internal := True;
            end if;

         end if;
      end if;
   end Add_Entity;


   --------------
   -- Add_Etbl --
   --------------

   procedure Add_Etbl  (First_Etbl  : in out Entity_Table_Acc;
                        Last_Etbl   : in out Entity_Table_Acc;
                        Unit_Number : in     Unit_Number_Type;
                        New_Etbl    : in out Entity_Table_Acc) is

      Unit_Node : Node_Id := Unit (File.Table (Unit_Number).Cunit);
      Etbl_Tmp : Entity_Table_Acc := First_Etbl;
      --  To store the current entity table within the search loop.

      Found     : Boolean := False;

   begin

      --  We look if the entity table is already in the list.

      Get_Name_String (File.Table (Unit_Number).File_Name);

      while not Found loop
         if (Etbl_Tmp = null) then

            --  In this case we add the entity table to our list.

            New_Etbl := new Entity_Table;
            New_Etbl.Next_Etbl := null;
            New_Etbl.File_Name := new Str'(Name_Buffer (1 .. Name_Len));
            Get_Name_String (File.Table (Unit_Number).Unit_Name);
            New_Etbl.Unit_Name := new Str'(Name_Buffer (1 .. Name_Len - 2));
            if Name_Buffer (Name_Len) = 's' then
               New_Etbl.Status := A_Spec;
            elsif Acts_As_Spec (File.Table (Unit_Number).Cunit) then
               New_Etbl.Status := Body_As_Spec;
            else
               New_Etbl.Status := A_Body;
            end if;

            case Nkind (Unit_Node) is
               when N_Subprogram_Declaration |  N_Subprogram_Body |
                    N_Subprogram_Body_Stub =>
                  Unit_Node := Specification (Unit_Node);
                  case Nkind (Unit_Node) is
                     when N_Procedure_Specification =>
                        New_Etbl.Kind := Proc;
                     when N_Function_Specification =>
                        New_Etbl.Kind := Func;
                     when others =>
                        New_Etbl.Kind := Unknown;
                  end case;

               when N_Package_Declaration | N_Package_Instantiation |
                    N_Package_Body        | N_Package_Body_Stub =>
                  New_Etbl.Kind := Pack;

               when N_Generic_Declaration | N_Function_Instantiation |
                    N_Procedure_Instantiation =>
                  New_Etbl.Kind := Genr;

               when N_Task_Body_Stub =>
                  New_Etbl.Kind := Tsk;

               when N_Subunit =>
                  New_Etbl.Kind   := Sub;
                  New_Etbl.Status := Sub_Body;
                  New_Etbl.RU     := True;

               when others =>
                  New_Etbl.Kind := Unknown;

            end case;

            New_Etbl.Top_Node := File.Table (Unit_Number).Cunit;

            if (First_Etbl = null) then
               First_Etbl := New_Etbl;
               Last_Etbl  := New_Etbl;
            else
               Last_Etbl.Next_Etbl := New_Etbl;
               Last_Etbl           := New_Etbl;
            end if;

            Found := True;

         elsif (Etbl_Tmp.File_Name.all = Name_Buffer (1 .. Name_Len)) then

            --  In this case we update only the top Node_Id.

            Etbl_Tmp.Top_Node := File.Table (Unit_Number).Cunit;
            New_Etbl := Etbl_Tmp;

            Found := True;

         else
            Etbl_Tmp := Etbl_Tmp.Next_Etbl;

         end if;
      end loop;

   end Add_Etbl;


   -------------------
   -- Add_Reference --
   -------------------

   procedure Add_Reference (To_Entity :  Entity_Acc;
                            New_Etbl  :  Entity_Table_Acc;
                            New_Ref   :  Node_Id) is

      R_Tmp : Ref_Acc;

      New_Unit : Unit_Number_Type := Get_Sloc_Unit_Number (Sloc (New_Ref));
      New_Sloc : Int := Int (Sloc (New_Ref)) - 
                        Int (File.Table (New_Unit).Source'First);

   begin

      if To_Entity /= null 
        and then (To_Entity.Last_Ref = null 
          or else To_Entity.Last_Ref.Etbl /= New_Etbl
          or else To_Entity.Last_Ref.Sloc <  New_Sloc) then

         R_Tmp := new Ref;
         R_Tmp.Ref_Node    := New_Ref;
         R_Tmp.Sloc        := New_Sloc;
         R_Tmp.Line_Number := Int (Get_Line_Number (Sloc (New_Ref)));
         R_Tmp.Etbl        := New_Etbl;

         if To_Entity.First_Ref = null then
            To_Entity.First_Ref := R_Tmp;
            To_Entity.Last_Ref  := R_Tmp;
         else
            To_Entity.Last_Ref.Next_Ref := R_Tmp;
            To_Entity.Last_Ref          := R_Tmp;
         end if;

         if Nkind (Parent (New_Ref)) = N_Pragma_Argument_Association then
            R_Tmp.Is_Pragma := True;
         else
            To_Entity.Length := To_Entity.Length + 1;
         end if;

      end if;
   end Add_Reference;


   --------------
   -- Add_With --
   --------------

   procedure Add_With (To_Etbl  : Entity_Table_Acc;
                       New_Etbl : Entity_Table_Acc) is

      W_Tmp : With_Acc := To_Etbl.First_With;
      --  To store the current values within the search loop.

      Found  : Boolean  := False; 

   begin

      if W_Tmp = null then

         --  No With_Clause yet !

         To_Etbl.First_With             := new With_Clause;
         To_Etbl.First_With.Withed_Etbl := New_Etbl;

      else

         --  Look for New_Etbl, if not in the list creat a new With_Clause!

         if W_Tmp.Withed_Etbl = New_Etbl then
            Found := True;
         end if;

         while W_Tmp.Next_With /= null and then not Found loop
            W_Tmp := W_Tmp.Next_With;

            if W_Tmp.Withed_Etbl = New_Etbl then
               Found := True;
            end if;
         end loop;

         if not Found then
            W_Tmp.Next_With             := new With_Clause;
            W_Tmp.Next_With.Withed_Etbl := New_Etbl;
         end if;

      end if;
   end Add_With;


   --------------------------   
   -- Clear_And_Mark_Xrefs --
   --------------------------

   procedure Clear_And_Mark_Xrefs (Home_Etbl   : Entity_Table_Acc;
                                   Target_Etbl : Entity_Table_Acc) is

      E_Tmp : Entity_Acc := Home_Etbl.First_Entity;
      R_Tmp : Ref_Acc;
      --  Patrick
      Parent_Node_Of_Ref : Node_Id;
      Is_Used_In_Elaborate : Boolean := False;
      --  End

   begin
      Target_Etbl.Marked := False;

      while (E_Tmp /= null) loop
         E_Tmp.Marks := 0;
         R_Tmp := E_Tmp.First_Ref;

         --  Patrick
         Is_Used_In_Elaborate := False;
         --

         while (R_Tmp /= null) loop
            if R_Tmp.Etbl = Target_Etbl then

               --  Patrick
               --  If the reference is a with clause and the flag elaborate 
               --  present is set then mark the entity
               Parent_Node_Of_Ref := Parent (R_Tmp.Ref_Node);
               if Nkind (Parent_Node_Of_Ref) = N_With_Clause then 
                  if Elaborate_Present (Parent_Node_Of_Ref) then
                     Is_Used_In_Elaborate := True;
                  end if;
               end if;
               --  End

               E_Tmp.Marks  := E_Tmp.Marks + 1;
               R_Tmp.Marked := True;
            else
               R_Tmp.Marked := False;
            end if;
            R_Tmp := R_Tmp.Next_Ref;
         end loop;

         --  We mark the target entity table to signal that there are
         --  some cross references found.
         --  We don't consider the first entity (always referenced in the
         --  with clause) except if the target entity table is a subprogram,
         --  (in this case we're only able to reference the first entity).

         if E_Tmp.Marks /= 0
           and then not Target_Etbl.Marked then

            if E_Tmp /= Home_Etbl.First_Entity
              or else Home_Etbl.Kind in Proc .. Genr
              or else Is_Used_In_Elaborate then  

               Target_Etbl.Marked := True;
            end if;
         end if;

         E_Tmp := E_Tmp.Next_Entity;
      end loop;
   end Clear_And_Mark_Xrefs;


   ------------------
   -- Delete_Table --
   ------------------

   procedure Delete_Table (Old_Etbl : Entity_Table_Acc) is
   begin
      null;
   end Delete_Table;


   ------------------------
   -- Double_Line_Entity --
   ------------------------

   function Double_Line_Entity return Boolean is
   begin
      for I in 1 .. Name_Len loop
         if Name_Buffer (I) = '_' and then Name_Buffer (I + 1) = '_' then
            return True;
         end if;
      end loop;

      return False;
   end Double_Line_Entity;

   -----------------
   -- Entity_Node --
   -----------------

   function Entity_Node (The_Entity : Entity_Acc) return Entity_Id is
   begin
      if (The_Entity = null) then
         return Empty;
      else
         return The_Entity.Entity_Node;
      end if;
   end Entity_Node;


   -----------------
   -- Entity_Type --
   -----------------

   function Entity_Type (The_Entity : Entity_Acc) return Entity_Kind is
   begin
      if (The_Entity = null) then
         return E_Void;
      else
         return The_Entity.Entity_Type;
      end if;
   end Entity_Type;


   -----------
   -- First --
   -----------

   function First (The_Etbl : Entity_Table_Acc) return Entity_Id is
   begin
      return The_Etbl.First_Entity.Entity_Node;
   end First;


   -----------
   -- First --
   -----------

   function First (The_Entity : Entity_Acc) return Ref_Acc is
   begin
      return The_Entity.First_Ref;
   end First;

   ------------------
   -- Give_Warning --
   ------------------

   function Give_Warning (The_Entity : Entity_Acc) return Boolean is
   begin
      return The_Entity.Give_Warning;
   end Give_Warning;

   ---------------
   -- In_E_List --
   ---------------

   function In_E_List (The_Etbl   : Entity_Table_Acc;
                       The_Entity : Entity_Id) return Entity_Acc is

      E_Tmp : Entity_Acc;
      --  To store the current entity within the search loop.

   begin
      E_Tmp := The_Etbl.First_Entity;

      while E_Tmp /= null
        and then E_Tmp.Entity_Node /= The_Entity loop
         E_Tmp := E_Tmp.Next_Entity;
      end loop;

      return E_Tmp;      
   end In_E_List;


   -----------------
   -- In_Ref_List --
   -----------------

   function In_Ref_List (The_Entity : Entity_Acc;
                         The_Ref    : Node_Id)     return Boolean is

      R_Tmp : Ref_Acc;
      --  To store the current reference within the search loop.

   begin

      if The_Entity = null then
         return False;

      else
         R_Tmp := The_Entity.First_Ref;

         while R_Tmp /= null
           and then R_Tmp.Ref_Node /= The_Ref loop
            R_Tmp := R_Tmp.Next_Ref;
         end loop;

         if (R_Tmp = null) then
            return False;
         else
            return True;
         end if;

      end if;      
   end In_Ref_List;


   ------------------
   -- In_With_List --
   ------------------

   function In_With_List (Home_Etbl   : Entity_Table_Acc;
                          Target_Etbl : Entity_Table_Acc) return Boolean is

      W_Tmp : With_Acc;
      --  To store the current entity within the search loop.

   begin
      W_Tmp := Target_Etbl.First_With;

      while W_Tmp /= null
        and then W_Tmp.Withed_Etbl /= Home_Etbl loop
         W_Tmp := W_Tmp.Next_With;
      end loop;

      if W_Tmp = null then
         return False;
      else
         return True;
      end if;

   end In_With_List;


   -------------
   -- Is_Null --
   -------------

   function Is_Null (The_Entity : Entity_Acc) return Boolean is
   begin
      if (The_Entity = null) then
         return True;
      else
         return False;
      end if;
   end Is_Null;


   -------------
   -- Is_Null --
   -------------

   function Is_Null (The_Ref : Ref_Acc) return Boolean is
   begin
      if The_Ref = null then
         return True;
      else
         return False;
      end if;
   end Is_Null;


   ---------------
   -- Is_Pragma --
   ---------------

   function Is_Pragma (The_Ref : Ref_Acc) return Boolean is
   begin
      return The_Ref.Is_Pragma;
   end Is_Pragma;


   -----------------
   -- Mark_Entity --
   -----------------

   procedure Mark_Entity (Old_Entity : Entity_Acc) is
   begin
      if Old_Entity /= null then
         Old_Entity.Marks := Old_Entity.Marks + 1;
      end if;
   end Mark_Entity;


   --------------------------
   -- Mark_Withed_Entities --
   --------------------------

   procedure Mark_Withed_Entities (The_Etbl : Entity_Table_Acc) is

      R : Ref_Acc := The_Etbl.First_Entity.First_Ref;

      Current_Etbl  : Entity_Table_Acc;
      Previous_Etbl : Entity_Table_Acc;
      --  To supress multiple calls of Mark_Xrefs for the same client.

      First : Boolean := True;

   begin

      --  We loop through all the references of the unit name entity.
      --  Each client must have at least one such reference in
      --  this list (the one of the with clause).

      while (R /= null) loop

         Current_Etbl := R.Etbl;

         if Current_Etbl /= Previous_Etbl
           and then Current_Etbl /= The_Etbl
           and then Current_Etbl.RU then

            --  If we find a cross reference of a new entity table then 
            --  we mark the referenced entities.

            if First then
               Clear_And_Mark_Xrefs (The_Etbl, Current_Etbl);
               First := False;
            else
               Mark_Xrefs (The_Etbl, Current_Etbl);
            end if;

            Previous_Etbl := Current_Etbl;
         end if;

         R := R.Next_Ref;
      end loop;

   end Mark_Withed_Entities;


   ----------------   
   -- Mark_Xrefs --
   ----------------

   procedure Mark_Xrefs (Home_Etbl   : Entity_Table_Acc;
                         Target_Etbl : Entity_Table_Acc) is

      E_Tmp    : Entity_Acc := Home_Etbl.First_Entity;
      Old_Marks : Natural;
      R_Tmp    : Ref_Acc;

      --  Patrick
      Parent_Node_Of_Ref : Node_Id;
      Is_Used_In_Elaborate : Boolean := False;
      --  End

   begin
      Target_Etbl.Marked := False;

      while (E_Tmp /= null) loop

         Old_Marks := E_Tmp.Marks;
         R_Tmp := E_Tmp.First_Ref;

         --  Patrick
         Is_Used_In_Elaborate := False;
         --

         while R_Tmp /= null loop
            if R_Tmp.Etbl = Target_Etbl 
              and then not R_Tmp.Marked then

               --  Patrick
               --  If the reference is a with clause and the flag elaborate 
               --  present is set then mark the entity
               Parent_Node_Of_Ref := Parent (R_Tmp.Ref_Node);
               if Nkind (Parent_Node_Of_Ref) = N_With_Clause then
                  if Elaborate_Present (Parent_Node_Of_Ref) then
                     Is_Used_In_Elaborate := True;
                  end if;
               end if;
               --  End

               E_Tmp.Marks  := E_Tmp.Marks + 1;
               R_Tmp.Marked := True;
            end if;

            R_Tmp := R_Tmp.Next_Ref;
         end loop;

         if E_Tmp.Marks /= Old_Marks
           and then not Target_Etbl.Marked then

            if E_Tmp /= Home_Etbl.First_Entity
              or else Home_Etbl.Kind in Proc .. Genr 
              or else Is_Used_In_Elaborate then
               Target_Etbl.Marked := True;
            end if;

         end if;

         E_Tmp := E_Tmp.Next_Entity;
      end loop;
   end Mark_Xrefs;


   ----------
   -- Next --
   ----------

   function Next (The_Entity : Entity_Acc) return Entity_Acc is
   begin
      if The_Entity = null then
         return null;
      else
         return The_Entity.Next_Entity;
      end if;
   end Next;


   ----------
   -- Next --
   ----------

   function Next (The_Ref : Ref_Acc) return Ref_Acc is
   begin
      if The_Ref = null then
         return null;
      else
         return The_Ref.Next_Ref;
      end if;
   end Next;


   ---------------------
   -- Number_Of_Marks --
   ---------------------

   function Number_Of_Marks (The_Entity : Entity_Acc) return Natural is
   begin
      if (The_Entity = null) then
         return 0;
      else
         return The_Entity.Marks;
      end if;      
   end Number_Of_Marks;


   --------------------
   -- Number_Of_Refs --
   --------------------

   function Number_Of_Refs (The_Entity : Entity_Acc) return Natural is
   begin
      if (The_Entity = null) then
         return 0;         
      else
         return The_Entity.Length;
      end if;      
   end Number_Of_Refs;


   ----------------   
   -- Scope_Path --
   ----------------

   function Scope_Path (The_Entity : Entity_Id) return Str is

      Scope_Node : Node_Id;

      Max_Buffer_Length : constant Nat := 100;
      --  The length of Buffer is limited to 100 characters.

      Buffer : Str (1 .. Max_Buffer_Length);
      --  The buffer variable to enable formatted output.
      --  The string in Buffer is *not* NUL terminated!
      --
      --  Note: We fill the Buffer from the right to the left.
      --        So we can do with iteration instead of recursion.

      Buffer_Entry : Pos := Max_Buffer_Length;
      --  The current entry into Buffer (points to the last empty field).

      Loop_String  : constant Str (1 .. 4) := "loop";
      Block_String : constant Str (1 .. 5) := "block";


      procedure Insert_Char_In_Buffer (The_Char : Char) is
      begin
         if Buffer_Entry = 0 then
            null;
         else

            Buffer (Buffer_Entry) := The_Char;
            Buffer_Entry := Buffer_Entry - 1;

         end if;
      end Insert_Char_In_Buffer;


      procedure Insert_Str_In_Buffer (Insert : Str) is
      begin
         if Insert'Length > Buffer_Entry then
            null;
         else

            Buffer (Buffer_Entry - Insert'Length + 1 .. Buffer_Entry)
              := Insert (1 .. Insert'Length);
            Buffer_Entry := Buffer_Entry - Insert'Length;

         end if;
      end Insert_Str_In_Buffer;


   begin

      Insert_Char_In_Buffer ('/');

      --  If the entity is visible from outside we add a # to its scope.
      --  Thus we can easily distinguish between entities declared within
      --  the body and those declared within the spec.

      Scope_Node := The_Entity;
      Get_Name_String (Chars (Scope_Node));
      Insert_Str_In_Buffer (Name_Buffer (1 .. Name_Len));
      Scope_Node := Scope (Scope_Node);

      while Scope_Node > Last_Standard_Node_Id loop
      --  We stop adding scopes if we find a scope which is declared within
      --  the Standard package.

         if Scope (Scope_Node) < Last_Standard_Node_Id then
         --  Do this for the last scope part to mark the entity as
         --  visible or not.

            if Is_Public (The_Entity) then
               Insert_Char_In_Buffer ('.');
               --  If the entity is public add a # after its scope.

            else            
               Insert_Char_In_Buffer ('#');
               --  If the entity is not public add a # after its scope.

            end if;
         else
            Insert_Char_In_Buffer ('.'); 

         end if;

         Get_Name_String (Chars (Scope_Node));

         if Ekind (Scope_Node) = E_Loop and then Double_Line_Entity then
            Insert_Str_In_Buffer (Loop_String);
            --  Given 'loop__456' we supress the '__456'.

         elsif Ekind (Scope_Node) = E_Block and then Double_Line_Entity then
            Insert_Str_In_Buffer (Block_String);
            --  Given 'block__1002' we supress the '__1002'.

         else
            Insert_Str_In_Buffer (Name_Buffer (1 .. Name_Len));
         end if;

         Scope_Node := Scope (Scope_Node);
      end loop;

      Insert_Char_In_Buffer ('/');
      return Buffer (Buffer_Entry + 1 .. Max_Buffer_Length);

   end Scope_Path;


   --------------
   -- The_Node --
   --------------

   function The_Node (The_Ref : Ref_Acc) return Node_Id is
   begin
      return The_Ref.Ref_Node;
   end The_Node;


   --------------------
   -- Unmark_Entity  --
   --------------------

   procedure Unmark_Entity (The_Table  : Entity_Table_Acc;
                            Old_Entity : Entity_Acc) is
   begin
      if The_Table /= null
        and then Old_Entity /= null then
         Old_Entity.Marks := 0;         
      end if;
   end Unmark_Entity;


   ----------------------
   -- Unmark_Reference --
   ----------------------

   procedure Unmark_Reference (The_Entity : Entity_Acc;
                               Old_Ref    : Node_Id) is

      R_Tmp : Ref_Acc;
      --  To store the current references within the search loop.

   begin
      if The_Entity /= null then

         --  First we search the fitting reference,

         R_Tmp := The_Entity.First_Ref;

         while R_Tmp /= null
           and then R_Tmp.Ref_Node /= Old_Ref loop
            R_Tmp := R_Tmp.Next_Ref;
         end loop;

         --  then we unmark it.

         if R_Tmp /= null
           and then R_Tmp.Marked = True then
            R_Tmp.Marked := False;
            The_Entity.Marks  := The_Entity.Marks - 1;
         end if;

      end if;
   end Unmark_Reference;


   -------------------
   -- Update_Entity --
   -------------------

   procedure Update_Entity (To_Etbl     : in     Entity_Table_Acc;
                            Entity_Node : in     Entity_Id;
                            New_Entity  : in out Entity_Acc) is

      New_Line : Int;
      Found    : Boolean := False;
      Path_Ptr : Str_Ptr;

   begin
      New_Entity := To_Etbl.First_Entity;
      New_Line   := Int (Get_Line_Number (Sloc (Entity_Node)));

      if To_Etbl.Kind = Genr then

         Path_Ptr := new Str'(Scope_Path (Entity_Node));
         --  In the case of generics we have to compare the whole
         --  path string since we have lots of entities with same
         --  line numbers and chars.

      else
         Get_Name_String (Chars (Entity_Node));
         --  Otherwise it's enough to compare the line numbers and chars.

      end if;

      --  First we look if the entity is already in the list.

      while not Found loop
         if New_Entity = null then

            Add_Entity (To_Etbl, Entity_Node, New_Entity);
            Found := True;

         elsif New_Entity.Line_Number = New_Line then

            if To_Etbl.Kind = Genr then

               if New_Entity.Scope_Path.all = Path_Ptr.all then
                  New_Entity.Entity_Node := Entity_Node;
                  Found := True;
               else
                  New_Entity := New_Entity.Next_Entity;
               end if;

            else
               if New_Entity.Chars.all = Name_Buffer (1 .. Name_Len) then
                  New_Entity.Entity_Node := Entity_Node;
                  Found := True;
               else
                  New_Entity := New_Entity.Next_Entity;
               end if;

            end if;

         else
            New_Entity := New_Entity.Next_Entity;

         end if;
      end loop;
   end Update_Entity;


   ----------------------
   -- Update_Reference --
   ----------------------

   procedure Update_Reference (To_Entity :  Entity_Acc;
                               New_Etbl  :  Entity_Table_Acc;
                               New_Ref   :  Node_Id) is

      R_Tmp : Ref_Acc;
      --  To store the current values within the search loop.

      New_Unit : Unit_Number_Type := Get_Sloc_Unit_Number (Sloc (New_Ref));
      New_Sloc : Int              := Int (Sloc (New_Ref)) - 
                                     Int (File.Table (New_Unit).Source'First);
      Found    : Boolean          := False;

   begin

      if To_Entity /= null then

         --  We look if the reference is already in the list.

         R_Tmp := To_Entity.First_Ref;

         while not Found loop
            if R_Tmp = null then

               Add_Reference (To_Entity, New_Etbl, New_Ref);
               Found  := True;

            elsif R_Tmp.Etbl = New_Etbl
              and then R_Tmp.Sloc = New_Sloc then

               --  In this case we update only the Node_Id.

               R_Tmp.Ref_Node    := New_Ref;
               Found := True;

            else
               R_Tmp := R_Tmp.Next_Ref;

            end if;
         end loop;

      end if;
   end Update_Reference;


   ----------
   -- Writ --
   ----------

   procedure Writ (The_Etbl       : Entity_Table_Acc;
                   Level          : Output_Level) is

      Header_Full : constant Str (1 ..  4) := "%%%%";
      Header_Stub : constant Str (1 ..  4) := "----";

      Header_Proc : constant Str (1 .. 12) := " procedure /";
      Header_Func : constant Str (1 .. 11) := " function /";
      Header_Genr : constant Str (1 .. 10) := " generic /";
      Header_Pack : constant Str (1 .. 10) := " package /";
      Header_Task : constant Str (1 ..  7) := " task /";
      Header_Sub  : constant Str (1 .. 10) := " subunit /";
      Header_Unkn : constant Str (1 .. 10) := " unknown /";

      Header_Spec : constant Str (1 ..  8) := "/ SPEC: ";
      Header_Body : constant Str (1 ..  8) := "/ BODY: ";
      Header_BoSp : constant Str (1 .. 15) := "/ SPEC & BODY: ";


      Warning_String_1 : constant Str (1 ..  4) := ">>> ";
      Warning_String_2 : constant Str (1 ..  6) := "unused";
      Warning_String_3 : constant Str (1 .. 17) := "withed but unused";
      Warning_String_4 : constant Str (1 ..  4) := " <<<";
      Warning_String_5 : constant Str (1 ..  4) := " in ";
      Warning_String_6 : constant Str (1 ..  9) := "Warning: ";
      Warning_String_7 : constant Str (1 .. 38) :=
                                    "         +-> with clause should be in ";
      Warning_String_8 : constant Str (1 .. 36) :=
                                    "         --> with clause already in ";
      Warning_String_9 : constant Str (1 .. 20) := "withed unnecessarily";

      Line_Length      : constant Nat := 73;
      Entity_Indent    : constant Int :=  0;
      Reference_Indent : constant Int :=  5;
      Too_Long_Indent  : constant Int :=  1;
      --  These four constants should be transferred into types.ads !!
      --  Used for the formatted output of Write_Buffer.

      Line_Too_Long : Boolean := False;
      --  To signal a previous truncated line.
      --  The indent then changes to be Too_Long_Indent spaces larger.      

      E_Tmp : Entity_Acc;
      --  To store the current entity within the search loop.

      Buffer : Str (1 .. Line_Length + 1);
      --  The buffer variable to enable formatted output.
      --  The string in Buffer is *not* NUL terminated!

      Buffer_Length : Nat := 0;
      --  The current length of Buffer.
      --  Points to the last element in Buffer.

      Indent : Int;
      --  Current indention.


      procedure Add_Char_To_Buffer (The_Char : Char);
      --  Appends the given Char to the end of Buffer.
      --  Used for calling Unix_Write which accepts text only in form
      --  of a Str.

      procedure Add_Int_To_Buffer (Number : Int);
      --  Recursive procedure for writing integers.

      procedure Add_Str_To_Buffer (Append : Str);
      --  Appends the given string to the end of Buffer.

      procedure Check_Withing_Units;
      --  Looks for unnecessary with clauses in the clients of the Withed_Spec.
      --  Gives warnings both on the screen and in the Xref file if such
      --  an unnecessary with clause is found.

      procedure Write_Marked_References (First : Ref_Acc);
      --  Same as Write_References with the difference that here only
      --  references with the field Marked = True are considered.

      procedure Write_Path (The_Entity : Entity_Acc);
      --  Places the path string of the given entity in Buffer.
      --  A path string consists of the name of the entity followed
      --  by the hierarchical scopes.

      procedure Write_Place_Of_Declaration (The_Entity : Entity_Acc);
      --  Places the declaration string of the given entity in Buffer.
      --  A declaration string consists of the file name in
      --  which the entity is declared followed by the line number.

      procedure Write_References (First : Ref_Acc);
      --  Places the reference string of a given entity in Buffer.
      --  The reference string consists of source name files followed by the
      --  line numbers of the references within these files.

      procedure Write_Type (Text : String);
      --  Places the Entity_Kind string of a given Entity_Kind'Image in Buffer.
      --  The first two characters get cut ("E_");

      procedure Write_Warning (The_Entity : Entity_Acc);
      --  Places a warning message in Buffer and writes a warning message on
      --  the screen if an entity is not used within its program unit.

      type Withed_Warning_Type is (Norm, Should, Already);
      procedure Write_Withed_Warning (Withing_Etbl : Entity_Table_Acc;
                                      Warning_Kind : Withed_Warning_Type;
                                      Extra_Etbl   : Entity_Table_Acc);
      --  Places a warning message in Buffer and writes a warning message on
      --  the screen if
      --
      --   1.) a withed unit is not used (Norm).
      --         Here the field Extra_Etbl is redundant.
      --   2.) the same with clause appears within a predecessor (Already).
      --        ' ->  already in Extra_Etbl'
      --   3.) a with clause should be moved into a successor (Should).
      --        ' ->  should be in Extra_Etbl'.



      ------------------------
      -- Add_Char_To_Buffer --
      ------------------------

      procedure Add_Char_To_Buffer (The_Char : Char) is

      begin
         if Buffer_Length = 0 then

            --  Do the correct indention.

            for I in 1 .. Indent loop
               Buffer (I) := ' ';
            end loop;
            Buffer_Length := Indent;

            --  Ignore a leading space.

            if The_Char /= ' ' then
               Buffer (Buffer_Length + 1) := The_Char;
               Buffer_Length := Buffer_Length + 1;
            end if;

         elsif Buffer_Length + 1 > Line_Length then
            if not Line_Too_Long then
               Line_Too_Long := True;
               Indent := Indent + Too_Long_Indent;
            end if;
            Write_Xref_Info (Buffer, Eol => False);
            Add_Char_To_Buffer (The_Char);

         else      
            Buffer (Buffer_Length + 1) := The_Char;
            Buffer_Length := Buffer_Length + 1;

         end if;
      end Add_Char_To_Buffer;



      -----------------------
      -- Add_Int_To_Buffer --
      -----------------------

      procedure Add_Int_To_Buffer (Number : Int) is

         Minibuffer : Str (1 .. 11);
         Free_Space : Int := 10;
         N          : Int := Number;

      begin

         while N > 0 loop
            Minibuffer (Free_Space) := Char'VAL ((N mod 10) + 48);
            Free_Space := Free_Space - 1;
            N := N / 10;

         end loop;
         Add_Str_To_Buffer (Minibuffer (Free_Space + 1 .. 10));

      end Add_Int_To_Buffer;


      -----------------------
      -- Add_Str_To_Buffer --
      -----------------------

      procedure Add_Str_To_Buffer (Append : Str) is
      begin
         if Buffer_Length = 0 then

            --  Do the correct indention.

            for I in 1 .. Indent loop
               Buffer (I) := ' ';
            end loop;
            Buffer_Length := Indent;
         end if;

         if Buffer_Length + Append'Length <= Line_Length then

            --  All OK: no new line!

            Buffer (Buffer_Length + 1 .. Buffer_Length + Append'Length) :=
               Append (Append'First .. Append'Last);
            Buffer_Length := Buffer_Length + Append'Length;

         elsif Append'Length > Line_Length - Indent - Too_Long_Indent then

            --  New line and truncation of the string.

            if Buffer_Length > Indent then      
               Write_Xref_Info (Buffer, Eol => False);
            end if;
            Add_Str_To_Buffer (Append 
              (Append'First .. Append'First + Line_Length - Indent - 1));
            Write_Xref_Info (Buffer, Eol => False);
            if not Line_Too_Long then
               Line_Too_Long := True;
               Indent := Indent + Too_Long_Indent;
               Add_Str_To_Buffer (Append
                 (Append'First + Line_Length - Indent + 1 .. Append'Last));
            else
               Add_Str_To_Buffer (Append
                 (Append'First + Line_Length - Indent .. Append'Last));
            end if;
         else

            --  Only new line!

            if not Line_Too_Long then
               Line_Too_Long := True;
               Indent := Indent + Too_Long_Indent;
            end if;
            Write_Xref_Info (Buffer, Eol => False);
            Add_Str_To_Buffer (Append);

         end if;
      end Add_Str_To_Buffer;



      -------------------------
      -- Check_Withing_Units --
      -------------------------


      procedure Check_Withing_Units is

         To_Clear     : Boolean;
         --  To distinguish between Clear_And_Mark_Xrefs and Mark_Xrefs.

         Etbl_Tmp     : Entity_Table_Acc;
         Withing_Etbl : Entity_Table_Acc;
         Etbl_Succ    : Entity_Table_Acc;

         Succ_Used    : Boolean;

      begin
         To_Clear := True;

         Etbl_Tmp := First_Etbl;
         while Etbl_Tmp /= null loop

            if Etbl_Tmp.Predecessor = null then
            --  We start at a spec of an Ada object.

               --  Find the with clause with the highest priority.
               --
               --  A special case appears in one file mode where we mark the
               --  used entities of the spec but don't give any warnings.

               if Etbl_Tmp = The_Etbl then
                  Withing_Etbl := Etbl_Tmp.Successor;
               else
                  Withing_Etbl := Etbl_Tmp;
                  while Withing_Etbl /= null
                    and then not In_With_List (The_Etbl, Withing_Etbl) loop
                     Withing_Etbl := Withing_Etbl.Successor;
                  end loop;
               end if;

               --  Mark the used entities:
               --  1. To check if there are unnecessary with clauses.
               --  2. To write only the referenced entities of a non RU.
               --
               --  Examine the withing compilation unit and all its successors
               --  which get the visibility automatically.

               Etbl_Succ := Withing_Etbl;
               while Etbl_Succ /= null loop
                  if Etbl_Succ.RU then
                     if To_Clear then
                        Clear_And_Mark_Xrefs (The_Etbl, Etbl_Succ);
                        To_Clear := False;
                     else
                        Mark_Xrefs (The_Etbl, Etbl_Succ);
                     end if;
                  else
                     Etbl_Succ.Marked := False;
                  end if;
                  Etbl_Succ := Etbl_Succ.Successor;
               end loop;

               --  Give the correct warnings.

               if Withing_Etbl /= null
                 and then Etbl_Tmp /= The_Etbl then

                  if not Withing_Etbl.Marked
                    and then Withing_Etbl.RU then

                     Succ_Used := False;

                     case Withing_Etbl.Status is

                     when A_Spec | Withed_Spec =>

                        --  If the withed unit is not used by the spec but
                        --  is used by a successor then give a hint to move the
                        --  with clause into the body.

                        Etbl_Succ := Withing_Etbl.Successor;
                        while Etbl_Succ /= null loop
                           if Etbl_Succ.Marked then
                              Succ_Used := True;
                           end if;
                           Etbl_Succ := Etbl_Succ.Successor;
                        end loop;
                        if Succ_Used and then not
                          In_With_List (The_Etbl, Withing_Etbl.Successor) then
                           Write_Withed_Warning (Withing_Etbl, Should,
                             Withing_Etbl.Successor);
                        else
                           Write_Withed_Warning (Withing_Etbl, Norm, null);
                        end if;

                        --  Warnings for subunits.

                        if Withing_Etbl.Successor /= null then
                           Etbl_Succ := Withing_Etbl.Successor.Successor;
                           while Etbl_Succ /= null loop
                              if In_With_List (The_Etbl, Etbl_Succ) then
                                 if Succ_Used then
                                    Write_Withed_Warning (Etbl_Succ, Already,
                                      Withing_Etbl);
                                 else
                                    Write_Withed_Warning (Etbl_Succ, 
                                      Norm, null);
                                 end if;
                              end if;
                              Etbl_Succ := Etbl_Succ.Successor;
                           end loop;
                        end if;

                     when A_Body | Body_As_Spec =>

                        --  If the withed unit is not used by the body but
                        --  is used by a subunit then give no warning.

                        Etbl_Succ := Withing_Etbl.Successor;
                        while Etbl_Succ /= null loop
                           if Etbl_Succ.Marked then
                              Succ_Used := True;
                           end if;
                           Etbl_Succ := Etbl_Succ.Successor;
                        end loop;
                        if not Succ_Used then
                           Write_Withed_Warning (Withing_Etbl, Norm, null);
                        end if;

                        --  Warnings for subunits.

                        Etbl_Succ := Withing_Etbl.Successor;
                        while Etbl_Succ /= null loop
                           if In_With_List (The_Etbl, Etbl_Succ) then
                              if Succ_Used then
                                 Write_Withed_Warning (Etbl_Succ, Already,
                                   Withing_Etbl);
                              else
                                 Write_Withed_Warning (Etbl_Succ, Norm, null);
                              end if;
                           end if;
                           Etbl_Succ := Etbl_Succ.Successor;
                        end loop;

                     when Sub_Body =>

                        Write_Withed_Warning (Withing_Etbl, Norm, null);

                        --  Warnings for further subunits.

                        Etbl_Succ := Withing_Etbl.Successor;
                        while Etbl_Succ /= null loop
                           if In_With_List (The_Etbl, Etbl_Succ) 
                             and then not Etbl_Succ.Marked then
                              Write_Withed_Warning (Etbl_Succ, Norm, null);
                           end if;
                           Etbl_Succ := Etbl_Succ.Successor;
                        end loop;

                     end case;

                  elsif Withing_Etbl.Status /= Sub_Body then               

                     --  If the with clause is USED then all the further with
                     --  clauses in any successor are redundant.

                     Etbl_Succ := Withing_Etbl.Successor;
                     while Etbl_Succ /= null loop
                        if In_With_List (The_Etbl, Etbl_Succ) then
                           Write_Withed_Warning (Etbl_Succ, Already,
                             Withing_Etbl);
                        end if;
                        Etbl_Succ := Etbl_Succ.Successor;
                     end loop;

                  end if;
               end if;               
            end if;

            Etbl_Tmp := Etbl_Tmp.Next_Etbl;
         end loop;

      end Check_Withing_Units;


      -----------------------------
      -- Write_Marked_References --
      -----------------------------

      procedure Write_Marked_References (First : Ref_Acc) is

         --  These Previous_xxx  variables are used to supress the repetition
         --  of same units and same line numbers.

         Current_Ref   : Ref_Acc := First;

         Current_Etbl  : Entity_Table_Acc;
         Previous_Etbl : Entity_Table_Acc;

         Current_Line  : Int;
         Previous_Line : Int;

      begin
         while Current_Ref /= null loop
         --  Loop through all the references of the list.

            if Current_Ref.Marked then

               Current_Etbl := Current_Ref.Etbl;

               if Current_Etbl /= Previous_Etbl then

                  --  If we find a reference in a new file we add the new
                  --  file name and the line number.

                  if (Previous_Etbl /= null) then
                     Add_Char_To_Buffer ('}');
                     Write_Xref_Info (Buffer);
                  end if;
                  --  Supress the } for the first file.

                  Previous_Etbl := Current_Etbl;
                  Previous_Line := -1;           
                  Add_Str_To_Buffer (Current_Etbl.File_Name.all);
                  Add_Char_To_Buffer (' ');
                  Add_Char_To_Buffer ('{');
               end if;

               --  Normally we only add the line number.

               Current_Line := Current_Ref.Line_Number;

               if Current_Line /= Previous_Line then
                  if (Previous_Line /= -1) then
                     Add_Char_To_Buffer (' ');
                  end if;
                  --  Supress the space for the first reference.

                  Previous_Line := Current_Line;
                  Add_Int_To_Buffer (Current_Line);
               end if;
            end if;

            Current_Ref := Current_Ref.Next_Ref;
         end loop;

         if Previous_Etbl /= null then         
            Add_Char_To_Buffer ('}');
            Write_Xref_Info (Buffer);
         end if;

      end Write_Marked_References;



      ----------------
      -- Write_Path --
      ----------------

      procedure Write_Path (The_Entity : Entity_Acc) is
      begin
         Add_Str_To_Buffer (The_Entity.Scope_Path.all);
      end Write_Path;



      --------------------------------
      -- Write_Place_Of_Declaration --
      --------------------------------

      procedure Write_Place_Of_Declaration (The_Entity : Entity_Acc) is
      begin

         Add_Char_To_Buffer (' ');
         Add_Int_To_Buffer (The_Entity.Line_Number);
         Add_Char_To_Buffer (' ');

      end Write_Place_Of_Declaration;



      ----------------------
      -- Write_References --
      ----------------------

      procedure Write_References (First : Ref_Acc) is

         --  These Previous_xxx  variables are used to supress the repetition
         --  of same units and same line numbers.

         Current_Ref   : Ref_Acc := First;

         Current_Etbl  : Entity_Table_Acc;
         Previous_Etbl : Entity_Table_Acc;

         Current_Line  : Int;
         Previous_Line : Int;

      begin
         while Current_Ref /= null loop
         --  Loop through all the references of the list.

            Current_Etbl := Current_Ref.Etbl;

            if Current_Etbl /= Previous_Etbl then

               --  If we find a reference in a new file we add the new
               --  file name and the line number.

               if Previous_Etbl /= null then
                  Add_Char_To_Buffer ('}');
                  Write_Xref_Info (Buffer);
               end if;
               --  Supress the } for the first file.

               Previous_Etbl := Current_Etbl;
               Previous_Line := -1;           
               Add_Str_To_Buffer (Current_Etbl.File_Name.all);
               Add_Char_To_Buffer (' ');
               Add_Char_To_Buffer ('{');
            end if;

            --  Normally we only add the line number.

            Current_Line := Current_Ref.Line_Number;

            if Current_Line /= Previous_Line then
               if Previous_Line /= -1 then
                  Add_Char_To_Buffer (' ');
               end if;
               --  Supress the space for the first reference.

               Previous_Line := Current_Line;
               Add_Int_To_Buffer (Current_Line);
            end if;

            Current_Ref := Current_Ref.Next_Ref;
         end loop;

         if Previous_Etbl /= null then         
            Add_Char_To_Buffer ('}');
            Write_Xref_Info (Buffer);
         end if;

      end Write_References;



      ----------------
      -- Write_Type --
      ----------------

      procedure Write_Type (Text : String) is
      begin
         Add_Char_To_Buffer (' ');

         for I in Text'First + 2 .. Text'Last loop
            if Text (I) in 'A' .. 'Z' then
               Add_Char_To_Buffer (Char'Val (Character'Pos (Text (I)) + 32));
            else
               Add_Char_To_Buffer (Char'Val (Character'Pos (Text (I))));
            end if;
         end loop;

      end Write_Type;



      -------------------
      -- Write_Warning --
      -------------------

      procedure Write_Warning (The_Entity : Entity_Acc) is
      begin

         --  File

         if Xref_Flag then
            Add_Str_To_Buffer (Warning_String_1);
            Add_Str_To_Buffer (Warning_String_6);
            Add_Str_To_Buffer (Warning_String_2);
            Add_Str_To_Buffer (Warning_String_4);
         end if;

         --  Screen

         Write_Char ('"');
         Write_Str (The_Etbl.File_Name.all);
         Write_String (""", line ");
         Write_Int (The_Entity.Line_Number);
         Write_Char (':');
         Write_Char (' ');
         Write_Str (Warning_String_6);
         Write_Str (The_Entity.Scope_Path.all);
         Write_Char (' ');
         Write_Str (Warning_String_2);
         Write_Eol;

      end Write_Warning;



      --------------------------
      -- Write_Withed_Warning --
      --------------------------

      procedure Write_Withed_Warning (Withing_Etbl : Entity_Table_Acc;
                                      Warning_Kind : Withed_Warning_Type;
                                      Extra_Etbl   : Entity_Table_Acc) is
      begin
         if With_Warnings then

            case Warning_Kind is

               when Norm =>

                  if Xref_Flag then               
                     Add_Str_To_Buffer (Warning_String_1);
                     Add_Str_To_Buffer (Warning_String_6);
                     Add_Str_To_Buffer (Warning_String_3);
                     Add_Str_To_Buffer (Warning_String_5);
                     Add_Str_To_Buffer (Withing_Etbl.File_Name.all);
                     Add_Str_To_Buffer (Warning_String_4);
                     Write_Xref_Info (Buffer);
                  end if;

                  Write_Str (Warning_String_6);
                  Write_Str (The_Etbl.First_Entity.Scope_Path.all);
                  Write_Char (' ');
                  Write_Str (Warning_String_3);
                  Write_Str (Warning_String_5);
                  Write_Str (Withing_Etbl.File_Name.all);
                  Write_Eol;

               when Should =>

                  if Xref_Flag then
                     Add_Str_To_Buffer (Warning_String_1);
                     Add_Str_To_Buffer (Warning_String_6);
                     Add_Str_To_Buffer (Warning_String_3);
                     Add_Str_To_Buffer (Warning_String_5);
                     Add_Str_To_Buffer (Withing_Etbl.File_Name.all);
                     Add_Str_To_Buffer (Warning_String_4);
                     Write_Xref_Info (Buffer);
                     Add_Str_To_Buffer (Warning_String_7);
                     Add_Str_To_Buffer (Extra_Etbl.File_Name.all);
                     Write_Xref_Info (Buffer);
                  end if;

                  Write_Str (Warning_String_6);
                  Write_Str (The_Etbl.First_Entity.Scope_Path.all);
                  Write_Char (' ');
                  Write_Str (Warning_String_3);
                  Write_Str (Warning_String_5);
                  Write_Str (Withing_Etbl.File_Name.all);
                  Write_Eol;
                  Write_Str (Warning_String_7);
                  Write_Str (Extra_Etbl.File_Name.all);
                  Write_Eol;

               when Already =>

                  if Xref_Flag then
                     Add_Str_To_Buffer (Warning_String_1);
                     Add_Str_To_Buffer (Warning_String_6);
                     Add_Str_To_Buffer (Warning_String_9);
                     Add_Str_To_Buffer (Warning_String_5);
                     Add_Str_To_Buffer (Withing_Etbl.File_Name.all);
                     Add_Str_To_Buffer (Warning_String_4);
                     Write_Xref_Info (Buffer);
                     Add_Str_To_Buffer (Warning_String_8);
                     Add_Str_To_Buffer (Extra_Etbl.File_Name.all);
                     Write_Xref_Info (Buffer);
                  end if;

                  Write_Str (Warning_String_6);
                  Write_Str (The_Etbl.First_Entity.Scope_Path.all);
                  Write_Char (' ');
                  Write_Str (Warning_String_9);
                  Write_Str (Warning_String_5);
                  Write_Str (Withing_Etbl.File_Name.all);
                  Write_Eol;
                  Write_Str (Warning_String_8);
                  Write_Str (Extra_Etbl.File_Name.all);
                  Write_Eol;

            end case;

         end if;
      end Write_Withed_Warning;



   --------------------------
   -- begin of Write_Table --
   --------------------------

   begin

      case Level is

      when Full_Xref =>

         --  Write a pretty headline.

         Indent := Entity_Indent;

         Write_Xref_Info (Buffer);
         Add_Str_To_Buffer (Header_Full);

         case The_Etbl.Kind is
            when Proc =>
               Add_Str_To_Buffer (Header_Proc);
            when Func =>
               Add_Str_To_Buffer (Header_Func);
            when Genr =>
               Add_Str_To_Buffer (Header_Genr);
            when Pack =>
               Add_Str_To_Buffer (Header_Pack);
            when Tsk =>
               Add_Str_To_Buffer (Header_Task);
            when Sub =>
               Add_Str_To_Buffer (Header_Sub);
            when Unknown =>
               Add_Str_To_Buffer (Header_Unkn);
         end case;      

         Add_Str_To_Buffer (The_Etbl.Unit_Name.all);

         case The_Etbl.Status is
            when A_Body  | Sub_Body =>
               Add_Str_To_Buffer (Header_Body);
            when Body_As_Spec =>
               Add_Str_To_Buffer (Header_BoSp);
            when A_Spec | Withed_Spec =>
               Add_Str_To_Buffer (Header_Spec);
         end case;

         Add_Str_To_Buffer (The_Etbl.File_Name.all);
         Add_Char_To_Buffer (' ');
         Add_Str_To_Buffer (Header_Full);
         Write_Xref_Info (Buffer);

         if The_Etbl.Status = Withed_Spec then
            Check_Withing_Units;           
         end if;

         --  Loop through all the entities in Entity_Table.

         E_Tmp := The_Etbl.First_Entity;
         while E_Tmp /= null loop

            if not E_Tmp.Is_Internal then
            --  Don't write anonymous entities.

               --  First we write the entity,

               Indent := Entity_Indent;
               Write_Path (E_Tmp);

               --  then its type.

               if Entity_Info_In_Xref then
                  Write_Type (Entity_Kind'Image (E_Tmp.Entity_Type));
               end if;

               --  then its place of declaration.

               Write_Place_Of_Declaration (E_Tmp);

               --  Give warnings if the entity is not used.

               if Entity_Warnings
                 and then E_Tmp.Length = 0
                 and then E_Tmp.Give_Warning then
                  Write_Warning (E_Tmp);
               end if;

               if E_Tmp.First_Ref /= null then

                  --  If some references exist, then write them.

                  Write_Xref_Info (Buffer);
                  Indent := Reference_Indent;
                  Write_References (E_Tmp.First_Ref);

               else
                  Write_Xref_Info (Buffer);

               end if;
            end if;

            E_Tmp := E_Tmp.Next_Entity;
         end loop;

      when Smart_Xref =>

         --  Write a pretty headline.

         Indent := Entity_Indent;

         Write_Xref_Info (Buffer);
         Add_Str_To_Buffer (Header_Stub);

         case The_Etbl.Kind is
            when Proc =>
               Add_Str_To_Buffer (Header_Proc);
            when Func =>
               Add_Str_To_Buffer (Header_Func);
            when Genr =>
               Add_Str_To_Buffer (Header_Genr);
            when Pack =>
               Add_Str_To_Buffer (Header_Pack);
            when Tsk =>
               Add_Str_To_Buffer (Header_Task);
            when Sub =>
               Add_Str_To_Buffer (Header_Sub);
            when Unknown =>
               Add_Str_To_Buffer (Header_Unkn);
         end case;      

         Add_Str_To_Buffer (The_Etbl.Unit_Name.all);
         Add_Str_To_Buffer (Header_Spec);
         Add_Str_To_Buffer (The_Etbl.File_Name.all);
         Add_Char_To_Buffer (' ');
         Add_Str_To_Buffer (Header_Stub);

         Write_Xref_Info (Buffer);

         --  Loop through all the entities in Entity_Table.
         --  In this case we write something only if the entity is used within
         --  the target compilation unit.

         Check_Withing_Units;

         E_Tmp := The_Etbl.First_Entity;
         while E_Tmp /= null loop

            if E_Tmp.Marks > 0 then
            --  Don't write unmarked entities.

               --  First we write the entity,

               Indent := Entity_Indent;
               Write_Path (E_Tmp);

               --  then its type.

               if  Entity_Info_In_Xref then
                  Write_Type (Entity_Kind'Image (E_Tmp.Entity_Type));
               end if;

               --  then its place of declaration,

               Write_Place_Of_Declaration (E_Tmp);

               --  and finally its references.

               Write_Xref_Info (Buffer);
               Indent := Reference_Indent;
               Write_Marked_References (E_Tmp.First_Ref);

            end if;

            E_Tmp := E_Tmp.Next_Entity;
         end loop;

      when Full_Only_Screen =>

         if The_Etbl.Status = Withed_Spec then
            Check_Withing_Units;
         end if;   

         --  Loop through all the entities in Entity_Table.

         E_Tmp := The_Etbl.First_Entity;
         while E_Tmp /= null loop

            --  Give warnings if the entity is not used.

            if Entity_Warnings
              and then E_Tmp.Length = 0
              and then not E_Tmp.Is_Internal
              and then E_Tmp.Give_Warning then
               Write_Warning (E_Tmp);
            end if;

            E_Tmp := E_Tmp.Next_Entity;
         end loop;

      when Smart_Only_Screen =>

         Check_Withing_Units;

      end case;
   end Writ;

end Xref_Tab;
