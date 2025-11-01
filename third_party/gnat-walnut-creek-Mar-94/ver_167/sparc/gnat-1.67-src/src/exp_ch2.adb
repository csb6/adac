------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 2                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.4 $                              --
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

with Atree;  use Atree;
with Einfo;  use Einfo;
with Sinfo;  use Sinfo;

package body Exp_Ch2 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Expand_Name (N : Node_Id);
   --  An occurence of a discriminant within a discriminated type is replaced
   --  with the corresponding discriminal, that is to say the formal parameter
   --  of the initialization procedure for the type that is associated with
   --  that particular discriminant. This is replacement is not performed for
   --  discriminants of records that appear in constraints of component of the
   --  record,  because Gigi uses the discriminant name to retrieve its value.
   --  See Einfo.ads for additional details, and Exp_ch3, Exp_ch9 for examples
   --  of use.

   -----------------
   -- Expand_Name --
   -----------------

   procedure Expand_Name (N : Node_Id) is
   begin
      if Ekind (Entity (N)) = E_Discriminant  then
         if Ekind (Scope (Entity (N))) = E_Record_Type
           and then (Nkind (Parent (N)) = N_Range
             or else Nkind (Parent (N)) = N_Index_Or_Discriminant_Constraint)
         then
            null;
         else
            Set_Entity (N, Discriminal (Entity (N)));
         end if;
      end if;
   end Expand_Name;

   ----------------------------
   -- Expand_N_Expanded_Name --
   ----------------------------

   --  Performs discriminal replacement as described for Expand_Name above

   --  Should really be a renaming of Expand_Name, but this doesn't work yet???

   procedure Expand_N_Expanded_Name (N : Node_Id) is
   begin
      if Ekind (Entity (N)) = E_Discriminant then
         Expand_Name (N);
      end if;
   end Expand_N_Expanded_Name;

   -------------------------
   -- Expand_N_Identifier --
   -------------------------

   --  Performs discriminal replacement as described for Expand_Name above

   --  Should really be a renaming of Expand_Name, but this doesn't work yet???

   procedure Expand_N_Identifier (N : Node_Id) is
   begin
      if Ekind (Entity (N)) = E_Discriminant then
         Expand_Name (N);
      end if;
   end Expand_N_Identifier;

end Exp_Ch2;
