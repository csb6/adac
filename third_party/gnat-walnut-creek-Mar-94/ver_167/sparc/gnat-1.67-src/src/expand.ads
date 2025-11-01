------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               E X P A N D                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.5 $                              --
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

--  This procedure performs any required expansion for the specified node.
--  The argument is the node that is a candidate for possible expansion.
--  If no expansion is required, then Expand returns without doing anything.

--  If the node does need expansion, then the subtree is replaced by the
--  tree corresponding to the required rewriting. This tree is a syntactic
--  tree, except that all Entity fields must be correctly set on all
--  direct names, since the expander presumably knows what it wants, and in
--  any case it doesn't work to have the semantic analyzer perform visibility
--  analysis on these trees (they may have references to non-visible runtime
--  routines etc.) There are a few exceptions to this rule in special cases,
--  but they must be documented clearly.

--  Expand is called in two different situations:

--    Nodes that are not subexpressions (Nkind not in N_Subexpr)

--      In this case, Expand is called from the body of Sem, immediately
--      after completing semantic analysis by calling the corresponding
--      Analyze_N_xxx procedure. If expansion occurs, the given node must
--      be replaced with another node that is also not a subexpression.
--      This seems naturally to be the case, since it is hard to imagine any
--      situation in which it would make sense to replace a non-expression
--      subtree with an expression. Once the substitution is completed, the
--      Expand routine must call Analyze on the resulting node to do any
--      required semantic analysis. Note that references to children copied
--      from the old tree won't be reanalyzed, since their Analyze flag is set.

--    Nodes that are subexpressions (Nkind in N_Subexpr)

--      In this case, Expand is called from Sem_Res.Resolve_Subexpr after
--      completing the resolution of the subexpression (this means that the
--      expander sees the fully typed subtree). If expansion occurs, the
--      given node must be replaced by a node that is also a subexpression.
--      Again it is hard to see how this restriction could possibly be
--      violated (in the case where statements are required, they appear in
--      an N_Expression_Actions node, which is a subexpression). Once the
--      substitution is completed, the Expand routine must first call Analyze
--      on the resulting node to do any required semantic analysis, and then
--      call Resolve_Subexpr on the node to set the type (typically the type
--      will be the same as the original type of the input node).

--  Note that in both cases, the expander must use Rewrite_Substitute_Tree
--  or Copy_Node to achieve the actual replacement of the node, since it is
--  only passed the Id of the node to be expanded, and the resulting expanded
--  node Id must be the same (the parameter to Expand is mode in, not mode
--  in-out). It is not necessary to preserve the original tree in the Expand
--  routines, unlike the case for tree manipulation routines in the Analyzer.
--  This is because anyone interested in working with the original tree (like
--  ASIS) is required to compile in semantic checks only mode.

--  Note: the front end avoids calls to any of the expand routines if code 
--  is not being generated. This is done for three reasons:

--    1.  Make sure tree does not get mucked up by the expander if no
--        code is being generated, and is thus usable by ASIS etc.

--    2.  Save time, since expansion is not needed if a compilation is
--        being done only to check the semantics, or if code generation
--        has been canceled due to previously detected errors.

--    3.  Allow the expand routines to assume that the tree is error free.
--        This results from the fact that code generation mode is always
--        cancelled when any error occurs.

--  If we ever decide to implement a feature allowing object modules to be
--  generated even if errors have been detected, then point 3 will no longer
--  hold, and the expand routines will have to be modified to operate properly
--  in the presence of errors (for many reasons this is not currently true).

--  Note: a consequence of this approach is that error messages must never
--  be generated in the expander, since this would mean that such error
--  messages are not generated when the expander is not being called.

with Types; use Types;

procedure Expand (N : Node_Id);
