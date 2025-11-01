------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P A R . C H 8                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.8 $                              --
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

separate (Par)
package body Ch8 is

   ---------------------
   -- 8.4  Use Clause --
   ---------------------

   --  USE_CLAUSE ::= USE_PACKAGE_CLAUSE | USE_TYPE_CLAUSE

   --  USE_PACKAGE_CLAUSE ::= use package_NAME {, package_NAME};

   --  USE_TYPE_CLAUSE ::= use type SUBTYPE_MARK {, SUBTYPE_MARK};

   --  The caller has checked that the initial token is USE

   --  Error recovery: cannot raise Error_Resync

   function P_Use_Clause return Node_Id is
      Use_Node : Node_Id;

   begin
      Scan; -- past USE

      if Token = Tok_Type then
         Use_Node := New_Node (N_Use_Type_Clause, Prev_Token_Ptr);
         Set_Subtype_Marks (Use_Node, New_List);

         if Ada_83 then
            Error_Msg_SC ("type based use clause not allowed in Ada 83!");
         end if;

         Scan; -- past TYPE

         loop
            Append (P_Subtype_Mark, Subtype_Marks (Use_Node));
            No_Constraint;
            exit when Token /= Tok_Comma;
            Scan; -- past comma
         end loop;

      else
         Use_Node := New_Node (N_Use_Package_Clause, Prev_Token_Ptr);
         Set_Names (Use_Node, New_List);

         loop
            Append (P_Qualified_Simple_Name, Names (Use_Node));
            exit when Token /= Tok_Comma;
            Scan; -- past comma
         end loop;
      end if;

      TF_Semicolon;
      return Use_Node;
   end P_Use_Clause;

   -------------------------------
   -- 8.5  Renaming Declaration --
   -------------------------------

   --  For renaming declaration, package, function and procedure cases are
   --  parsed in the procedures with those P_names, and the object and
   --  exception name cases are parsed in P_Identifier_Declaration.

end Ch8;
