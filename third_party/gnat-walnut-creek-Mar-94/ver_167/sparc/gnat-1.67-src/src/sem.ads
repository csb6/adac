------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  S E M                                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.45 $                             --
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

with Opt;     use Opt;
with Snames;  use Snames;
with Table;
with Types;   use Types;

package Sem is

   Subunit_Found : exception;
   --  This exception is used when the main unit is a subunit, and the current
   --  unit is one of its parents. In this case the parent is being analyzed
   --  only to provide the necessary context for the subunit, so as soon as
   --  the subunit has been analyzed, there is no need to continue with the
   --  analysis of the parent, so this exception is raised to get out.

   Implicit_Type_List : List_Id;
   --  A list of N_Anonynmous_Type nodes which represent implicitly declared
   --  entities resulting from a given declaration. These nodes are placed on
   --  this list by New_Implicit_Type and are inserted in the declarations
   --  list by Analyze_Declaration after the analysis of each declaration.

   Scope_Suppress : Suppress_Record := Suppress_Options;
   --  This record contains the current scope based settings of the suppress
   --  switches. It is initialized from the options as shown, and then modified
   --  by pragma Suppress. On entry to each scope, the current setting is saved
   --  the scope stack, and then restored on exit from the scope.

   -----------------
   -- Scope Stack --
   -----------------

   --  The scope stack holds all entries of the scope table. As in the parser,
   --  we use Last as the stack pointer, so that we can always find the scope
   --  that is currently open in Scope_Stack.Table (Scope_Stack.Last). The
   --  oldest entry, at Scope_Stack (0) is Standard. The entries in the table
   --  include the entity for the referenced scope, together with information
   --  used to restore the proper setting of check suppressions on scope exit.

   --  There are two kinds of suppress checks, scope based suppress checks
   --  (from initial command line arguments, or from Suppress pragmas not
   --  including an entity name). The scope based suppress checks are recorded
   --  in the Sem.Supress variable, and all that is necessary is to save the
   --  state of this variable on scope entry, and restore it on scope exit.

   --  The other kind of suppress check is entity based suppress checks, from
   --  Suppress pragmas giving an Entity_Id. These checks are reflected by the
   --  appropriate bit being set in the corresponding entity, and restoring the
   --  setting of these bits is a little trickier. In particular a given pragma
   --  Suppress may or may not affect the current state. If it sets a check for
   --  an entity that is already checked, then it is important that this check
   --  not be restored on scope exit. The situation is made more complicated
   --  by the fact that a given suppress pragma can specify multiple entities
   --  (in the overloaded case), and multiple checks (by using All_Checks), so
   --  that it may be partially effective. On exit only checks that were in
   --  fact effective must be removed. Logically we could do this by saving
   --  the entire state of the entity flags on scope entry and restoring them
   --  on scope exit, but that would be ludicrous, so what we do instead is to
   --  maintain the following differential structure that shows what checks
   --  were installed for the current scope.

   --  Note: Suppress pragmas that specify entities defined in a package
   --  spec do not make entries in this table, since such checks suppress
   --  requests are valid for the entire life of the entity.

   type Entity_Check_Suppress_Record is record
      Entity : Entity_Id;
      --  Entity to which the check applies

      Check : Check_Id;
      --  Check which is set (note this cannot be All_Checks, if the All_Checks
      --  case, a sequence of eentries appears for the individual checks.
   end record;

   --  Entity_Suppress is a stack, to which new entries are added as they
   --  are processed (see pragma Suppress circuit in Sem_Prag). The scope
   --  stack entry simply saves the stack pointer on entry, and restores
   --  it on exit by reversing the checks one by one.

   package Entity_Suppress is new Table (
      Component_Type => Entity_Check_Suppress_Record,
      Index_Type     => Int,
      Low_Bound      => 0,
      Initial        => 1000,
      Increment      => 100,
      Table_Name     => "Sem.Entity_Suppress");

   --  Here is the scope stack itself

   type Scope_Stack_Entry is record
      Entity               : Entity_Id;
      --  Entity representing the scope

      Save_Scope_Suppress  : Suppress_Record;
      --  Save contents of Scope_Suppress on entry

      Save_Entity_Suppress : Int;
      --  Save contents of Entity_Suppress.Last on entry

      Transient : Boolean;
      --  A flag set true for transient scopes. See section "Handling of
      --  Transient Data" in the body of package Sem_Ch8 for details.

   end record;

   package Scope_Stack is new Table (
      Component_Type => Scope_Stack_Entry,
      Index_Type     => Int,
      Low_Bound      => 0,
      Initial        => 50,
      Increment      => 100,
      Table_Name     => "Sem.Scope_Stack");

   Transient_Scopes_Present : Boolean := False;
   --  A global flag that is set True if the scope stack for the current
   --  unit contains a transient scope (at most one such scope can be present)

   -----------------
   -- Subprograms --
   -----------------

   procedure Semantics (Cunit : Node_Id);
   --  This procedure is called to perform semantic analysis on the specified
   --  node which is the N_Compilation_Unit node for the unit.

   procedure Analyze (N : Node_Id);
   --  This is the recursive procedure which is applied to individual nodes
   --  of the tree, starting at the top level node (compilation unit node)
   --  and then moving down the tree in a top down traversal. It calls
   --  individual routines with names Analyze_xxx to analyze node xxx. Each
   --  of these routines is responsible for calling Analyze on the components
   --  of the subtree.

   --  Note: Analyze routines must be written in an idempotent manner, so that
   --  they can be repeatedly called on the same node without any deleterious
   --  effects. However, there is an Analyzed flag in each node that is used
   --  to avoid such duplicate Analyze calls where posssible to speed up
   --  processing, but it is not considered correct to rely on this.

   --  Note: In the case of expression components (nodes whose Nkind is in
   --  N_Subexpr), the call to Analyze does not complete the semantic analysis
   --  of the node, since the type resolution cannot be completed until the
   --  complete context is analyzed. The completion of the type analysis occurs
   --  in the corresponding Resolve routine, which gets called from the top of
   --  the complete context during the downward pass.

end Sem;
