------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ R E S                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.2 $                              --
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
package Sem_Res is

   --  As described in Sem_Ch4, the type resolution proceeds in two phases.
   --  The first phase is a bottom up pass that is achieved during the
   --  recursive traversal performed by the Analyze procedures. This phase
   --  determines unambiguous types, and collects sets of possible types
   --  where the interpretation is potentially ambiguous.

   --  On completing this bottom up pass, which corresponds to a call to
   --  Analyze on a complete context, the Resolve_Complete_Context routine
   --  is called which performs a top down resolution by making recursive
   --  calls the Resolve_Subexpr routine.

   --  Since in practice a lot of semantic analysis has to be postponed until
   --  types are known (e.g. static folding, setting of suppress flags), the
   --  Resolve routines also complete the semantic analyze, and also call the
   --  expander for possibly expansion of the completely type resolved node.

   procedure Resolve_Complete_Context (N : Node_Id; Typ : Entity_Id);
   --  Top level type-checking procedure, called in a complete context. The
   --  construct N, which is a subexpression, has already been analyzed, and
   --  is required to be of type Typ given the analysis of the context (which
   --  uses the information gathered on the bottom up phase in Analyze).
   --  The call to Resolve_Complete_Context also performs required static
   --  evaluation of the entire expression.

   procedure Resolve_Subexpr (N : Node_Id; Typ : Entity_Id);
   --  Recursive type and overload resolution procedure. Performs top-down
   --  type propagation over expression tree. This routine differs from the
   --  Resolve_Complete_Context call in that it does not reinitialize the
   --  overload resolution data structures, and does not perform static
   --  evaluation, since this is performed only at the top level. As implied,
   --  by the name of the procedure, Nkind (N) is always in N_Subexpr.

end Sem_Res;
