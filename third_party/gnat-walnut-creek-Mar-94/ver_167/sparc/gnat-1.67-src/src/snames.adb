------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               S N A M E S                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.35 $                             --
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

with Comperr; use Comperr;
with Namet;   use Namet;
with Output;  use Output;

package body Snames is

   --  Table of names to be set by Initialize_Snames. Each name is terminated
   --  by a single #, and the end of the list is marked by a null entry,
   --  i.e. by two # marks in succession. Note that the table does not include
   --  the entries for a-z, since these are initialized by Namet itself.

   Preset_Names : constant Str :=
     "_parent#" &
     "_size#" &
     "_chain#" &
     "_equality#" &
     "_expunge#" &
     "_idepth#" &
     "_init#" &
     "_master#" &
     "_priority#" &
     "_task#" &
     "_tb_snam#" &
     "_trace_sp#" &
     "_tag#" &
     "_tags#" &
     "_task_id#" &
     "version_b#" &
     "version_s#" &
     "<error>#" &
     "go#" &
     "to#" &
     "system#" &
     "unchecked_conversion#" &
     """abs""#" &
     """and""#" &
     """mod""#" &
     """not""#" &
     """or""#" &
     """rem""#" &
     """xor""#" &
     """=""#" &
     """/=""#" &
     """<""#" &
     """<=""#" &
     """>""#" &
     """>=""#" &
     """+""#" &
     """-""#" &
     """&""#" &
     """*""#" &
     """/""#" &
     """**""#" &
     """in""#" &
     """not in""#" &
     """and then""#" &
     """or else""#" &
     "abort_defer#" &
     "ada_83#" &
     "ada_9x#" &
     "all_calls_remote#" &
     "assert#" &
     "asynchronous#" &
     "atomic#" &
     "atomic_components#" &
     "attach_handler#" &
     "controlled#" &
     "convention#" &
     "debug#" &
     "elaborate#" &
     "elaborate_all#" &
     "elaborate_body#" &
     "export#" &
     "import#" &
     "improve#" &
     "inline#" &
     "inspection_point#" &
     "interface#" &
     "interface_name#" &
     "interrupt_handler#" &
     "interrupt_priority#" &
     "list#" &
     "locking_policy#" &
     "memory_size#" &
     "normalize_scalars#" &
     "optimize#" &
     "pack#" &
     "page#" &
     "preelaborate#" &
     "priority#" &
     "pure#" &
     "queuing_policy#" &
     "remote_call_interface#" &
     "remote_types#" &
     "restrictions#" &
     "reviewable#" &
     "shared#" &
     "shared_passive#" &
     "storage_unit#" &
     "suppress#" &
     "system_name#" &
     "task_dispatching_policy#" &
     "task_stack_size#" &
     "volatile#" &
     "volatile_components#" &
     "ada#" &
     "asm#" &
     "assembler#" &
     "intrinsic#" &
     "entity#" &
     "gcc#" &
     "gnat#" &
     "link_name#" &
     "off#" &
     "on#" &
     "space#" &
     "time#" &
     "access#" &
     "address#" &
     "adjacent#" &
     "aft#" &
     "alignment#" &
     "bit_order#" &
     "body_version#" &
     "callable#" &
     "caller#" &
     "ceiling#" &
     "component_size#" &
     "compose#" &
     "constrained#" &
     "copy_sign#" &
     "count#" &
     "delta#" &
     "denorm#" &
     "digits#" &
     "emax#" &
     "epsilon#" &
     "exponent#" &
     "external_tag#" &
     "first#" &
     "first_bit#" &
     "floor#" &
     "fore#" &
     "fraction#" &
     "identity#" &
     "image#" &
     "input#" &
     "large#" &
     "last#" &
     "last_bit#" &
     "leading_part#" &
     "length#" &
     "machine#" &
     "machine_emax#" &
     "machine_emin#" &
     "machine_mantissa#" &
     "machine_overflows#" &
     "machine_radix#" &
     "machine_rounds#" &
     "mantissa#" &
     "max#" &
     "max_size_in_storage_elements#" &
     "min#" &
     "model#" &
     "model_emax#" &
     "model_emin#" &
     "model_epsilon#" &
     "model_large#" &
     "model_mantissa#" &
     "model_small#" &
     "output#" &
     "pos#" &
     "position#" &
     "pred#" &
     "range#" &
     "read#" &
     "remainder#" &
     "round#" &
     "rounding#" &
     "safe_emax#" &
     "safe_first#" &
     "safe_large#" &
     "safe_last#" &
     "safe_small#" &
     "scale#" &
     "signed_zeros#" &
     "size#" &
     "small#" &
     "storage_pool#" &
     "storage_size#" &
     "succ#" &
     "tag#" &
     "terminated#" &
     "truncation#" &
     "unbiased_rounding#" &
     "unchecked_access#" &
     "val#" &
     "valid#" &
     "value#" &
     "version#" &
     "wide_image#" &
     "wide_value#" &
     "width#" &
     "write#" &
     "base#" &
     "class#" &
     "standard_access#" &
     "ceiling_locking#" &
     "fifo_queuing#" &
     "priority_queuing#" &
     "fifo_within_priorities#" &
     "access_check#" &
     "accessibility_check#" &
     "discriminant_check#" &
     "division_check#" &
     "elaboration_check#" &
     "index_check#" &
     "length_check#" &
     "overflow_check#" &
     "range_check#" &
     "storage_check#" &
     "tag_check#" &
     "all_checks#" &
     "abort#" &
     "abs#" &
     "abstract#" &
     "accept#" &
     "and#" &
     "all#" &
     "array#" &
     "at#" &
     "begin#" &
     "body#" &
     "case#" &
     "constant#" &
     "declare#" &
     "delay#" &
     "do#" &
     "else#" &
     "elsif#" &
     "end#" &
     "entry#" &
     "exception#" &
     "exit#" &
     "for#" &
     "function#" &
     "generic#" &
     "goto#" &
     "if#" &
     "in#" &
     "is#" &
     "limited#" &
     "loop#" &
     "mod#" &
     "new#" &
     "not#" &
     "null#" &
     "of#" &
     "or#" &
     "others#" &
     "out#" &
     "package#" &
     "pragma#" &
     "private#" &
     "procedure#" &
     "raise#" &
     "record#" &
     "rem#" &
     "renames#" &
     "return#" &
     "reverse#" &
     "select#" &
     "separate#" &
     "subtype#" &
     "task#" &
     "terminate#" &
     "then#" &
     "type#" &
     "use#" &
     "when#" &
     "while#" &
     "with#" &
     "xor#" &
     "aliased#" &
     "protected#" &
     "until#" &
     "requeue#" &
     "tagged#" &
      "#";

   -----------------------
   -- Initialize_Snames --
   -----------------------

   procedure Initialize_Snames is
      P_Index      : Int := 0;
      Discard_Name : Name_Id;

   begin
      loop
         Name_Len := 0;

         while Preset_Names (P_Index) /= '#' loop
            Name_Len := Name_Len + 1;
            Name_Buffer (Name_Len) := Preset_Names (P_Index);
            P_Index := P_Index + 1;
         end loop;

         --  We do the Name_Find call to enter the name into the table, but
         --  we don't need to do anything with the result, since we already
         --  initialized all the preset names to have the right value (we
         --  are depending on the order of the names and Preset_Names).

         Discard_Name := Name_Find;
         P_Index := P_Index + 1;
         exit when Preset_Names (P_Index) = '#';
      end loop;

      --  Make sure that number of names in standard table is correct. If
      --  this check fails, run utility program XSNAMES to construct a new
      --  properly matching version of the body.

      if Discard_Name /= Last_Predefined_Name then
         Compiler_Error;
         Write_Eol;
         Write_String ("Discard name = ");
         Write_Int (Int (Discard_Name));
         Write_Eol;
         Write_String ("Last predefined name = ");
         Write_Int (Int (Last_Predefined_Name ));
         Write_Eol;
         Compiler_Abort;
      end if;

   end Initialize_Snames;

   -----------------------
   -- Is_Attribute_Name --
   -----------------------

   function Is_Attribute_Name (N : Name_Id) return Boolean is
   begin
      return First_Attribute_Name <= N and then N <= Last_Attribute_Name;
   end Is_Attribute_Name;

   ----------------------------
   -- Is_Type_Attribute_Name --
   ----------------------------

   function Is_Type_Attribute_Name (N : Name_Id) return Boolean is
   begin
      return First_Type_Attribute_Name <= N
        and then N <= Last_Type_Attribute_Name;
   end Is_Type_Attribute_Name;

   -------------------
   -- Is_Check_Name --
   -------------------

   function Is_Check_Name (N : Name_Id) return Boolean is
   begin
      return First_Check_Name <= N and then N <= Last_Check_Name;
   end Is_Check_Name;

   ------------------------
   -- Is_Convention_Name --
   ------------------------

   function Is_Convention_Name (N : Name_Id) return Boolean is
   begin
      return (First_Convention_Name <= N and then N <= Last_Convention_Name)
        or else N = Name_C;
   end Is_Convention_Name;

   ----------------------------
   -- Is_Locking_Policy_Name --
   ----------------------------

   function Is_Locking_Policy_Name (N : Name_Id) return Boolean is
   begin
      return First_Locking_Policy_Name <= N
        and then N <= Last_Locking_Policy_Name;
   end Is_Locking_Policy_Name;

   -----------------------------
   -- Is_Operator_Symbol_Name --
   -----------------------------

   function Is_Operator_Symbol_Name (N : Name_Id) return Boolean is
   begin
      return First_Operator_Name <= N and then N <= Last_Operator_Name;
   end Is_Operator_Symbol_Name;

   --------------------
   -- Is_Pragma_Name --
   --------------------

   function Is_Pragma_Name (N : Name_Id) return Boolean is
   begin
      return First_Pragma_Name <= N and then N <= Last_Pragma_Name;
   end Is_Pragma_Name;

   ----------------------------
   -- Is_Queuing_Policy_Name --
   ----------------------------

   function Is_Queuing_Policy_Name (N : Name_Id) return Boolean is
   begin
      return First_Queuing_Policy_Name <= N
        and then N <= Last_Queuing_Policy_Name;
   end Is_Queuing_Policy_Name;

   -------------------------------------
   -- Is_Task_Dispatching_Policy_Name --
   -------------------------------------

   function Is_Task_Dispatching_Policy_Name (N : Name_Id) return Boolean is
   begin
      return First_Task_Dispatching_Policy_Name <= N
        and then N <= Last_Task_Dispatching_Policy_Name;
   end Is_Task_Dispatching_Policy_Name;

   ----------------------
   -- Get_Attribute_Id --
   ----------------------

   function Get_Attribute_Id (N : Name_Id) return Attribute_Id is
   begin
      return Attribute_Id'Val (N - First_Attribute_Name);
   end Get_Attribute_Id;

   ------------------
   -- Get_Check_Id --
   ------------------

   function Get_Check_Id (N : Name_Id) return Check_Id is
   begin
      return Check_Id'Val (N - First_Check_Name);
   end Get_Check_Id;

   -----------------------
   -- Get_Convention_Id --
   -----------------------

   function Get_Convention_Id (N : Name_Id) return Convention_Id is
   begin
      if N = Name_C then
         return Convention_C;
      else
         return Convention_Id'Val (N - First_Convention_Name);
      end if;
   end Get_Convention_Id;

   ---------------------------
   -- Get_Locking_Policy_Id --
   ---------------------------

   function Get_Locking_Policy_Id (N : Name_Id) return Locking_Policy_Id is
   begin
      return Locking_Policy_Id'Val (N - First_Locking_Policy_Name);
   end Get_Locking_Policy_Id;

   -------------------
   -- Get_Pragma_Id --
   -------------------

   function Get_Pragma_Id (N : Name_Id) return Pragma_Id is
   begin
      return Pragma_Id'Val (N - First_Pragma_Name);
   end Get_Pragma_Id;

   ---------------------------
   -- Get_Queuing_Policy_Id --
   ---------------------------

   function Get_Queuing_Policy_Id (N : Name_Id) return Queuing_Policy_Id is
   begin
      return Queuing_Policy_Id'Val (N - First_Queuing_Policy_Name);
   end Get_Queuing_Policy_Id;

   ------------------------------------
   -- Get_Task_Dispatching_Policy_Id --
   ------------------------------------

   function Get_Task_Dispatching_Policy_Id (N : Name_Id)
     return Task_Dispatching_Policy_Id is
   begin
      return Task_Dispatching_Policy_Id'Val
        (N - First_Task_Dispatching_Policy_Name);
   end Get_Task_Dispatching_Policy_Id;

end Snames;
