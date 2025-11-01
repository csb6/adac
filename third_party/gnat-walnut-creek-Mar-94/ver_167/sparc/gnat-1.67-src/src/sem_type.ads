------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ T Y P E                              --
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

--  This unit contains the routines used to handle type determination,
--  including the routine used to support overload resolution.

with Table;
with Types; use Types;

package Sem_Type is

   ---------------------------------------------
   -- Data Structures for Overload Resolution --
   ---------------------------------------------

   --  To determine the unique meaning of an identifier, overload resolution
   --  may have to be performed if the visibility rules alone identify more
   --  than one possible entity as the denotation of a given identifier. When
   --  the visibility rules find such a potential ambiguity, the set of
   --  possible interpretations must be attached to the identifier, and
   --  overload resolution must be performed over the innermost enclosing
   --  complete context. At the end of the resolution,  either a single
   --  interpretation is found for all identifiers in the context, or else a
   --  type error (invalid type or ambiguous reference) must be signalled.

   --  The set of interpretations of a given name is stored in a data structure
   --  that is separate from the syntax tree, because it corresponds to
   --  transient information.  The interpretations themselves are stored in
   --  table All_Interp. A mapping from tree nodes to sets of interpretations
   --  called Interp_Map, is maintained by the overload resolution routines.
   --  Both these structures are initialized at the beginning of every complete
   --  context.

   --  Corresponding to the set of interpretation for a given overloadable
   --  identifier, there is a set of possible types corresponding to the types
   --  that the overloaded call may return. We keep a 1-to-1 correspondence
   --  between interpretations and types: for user-defined subprograms the
   --  type is the declared return type. For operators, the type is determined
   --  by the type of the arguments. If the arguments themselves are
   --  overloaded, we enter the operator name in the names table for each
   --  possible result type. In most cases, arguments are not overloaded and
   --  only one interpretation is present anyway.

   --  Note: the All_Interp table is reset initially by calling Init_Interp.
   --  Subsequently, it is reset by Resolve_Complete_Context, since a call to
   --  Resolve_Complete_Context means that a complete context has been resolved
   --  and thus the interpretations stored in All_Interp are no longer needed.

   type Interp is record
      Nam : Entity_Id;
      Typ : Entity_Id;
   end record;

   No_Interp : constant Interp := (Empty, Empty);

   package All_Interp is new Table (
     Component_Type => Interp,
     Index_Type     => Int,
     Low_Bound      => 0,
     Initial        => 100,
     Increment      => 10,
     Table_Name     => "All_Interp");

   --  The following data structures establish a mapping between nodes and
   --  their interpretations. Eventually the Interp_Index corresponding to
   --  the first interpretation of a node may be stored directly in the
   --  corresponding node.

   subtype Interp_Index is Int;

   type Interp_Ref is record
      Node  : Node_Id;
      Index : Interp_Index;
   end record;

   package Interp_Map is new Table (
     Component_Type => Interp_Ref,
     Index_Type     => Int,
     Low_Bound      => 0,
     Initial        => 1000,
     Increment      => 10,
     Table_Name     => "Interp_Map");

   --  For now Interp_Map is searched sequentially

   -----------------
   -- Subprograms --
   -----------------

   procedure Init_Interp;
   --  Overload resolution is performed over each complete context. The data
   --  structures that store interpretations of overloaded nodes are reset
   --  by this call. It is not required that Init_Interp be called other
   --  than at the start of a compilation to initialize the data structures.
   --  However, efficiency in both time and space is improved by resetting
   --  the data structures more frequently between complete contexts.

   procedure Collect_Interps (N : Node_Id);
   --  Invoked when the name N has more than one visible interpretation.
   --  This is the high level routine which accumulates the possible
   --  interpretations of the node. The first meaning and type of N have
   --  already been stored in N. If the name is an expanded name, the homonyms
   --  are only those that belong to the same scope.

   procedure New_Interps (N : Node_Id);
   --  Initialize collection of interpretations for the given node, which is
   --  either an overloaded entity, or an operation whose arguments have
   --  multiple intepretations. Interpretations can be added to only one
   --  node at a time.

   procedure Add_One_Interp (N : Node_Id; E : Entity_Id; T : Entity_Id);
   --  Add (E, T) to the list of interpretations of the node being resolved.
   --  For calls and operators, i.e. for nodes that have a name field,
   --  E is an overloadable entity, and T is its type. For constructs such
   --  as indexed expressions, the caller sets E equal to T, because the
   --  overloading comes from other fields, and the node itself has no name
   --  to resolve. Add_One_Interp includes the semantic processing to deal
   --  with adding entries that hide one another etc.

   procedure End_Interp_List;
   --  End the list of interpretations of current node.

   procedure Get_First_Interp (N  : Node_Id;
                               I  : out Interp_Index;
                               It : out Interp);
   --  Initialize iteration over set of interpretations for Node N. The first
   --  interpretation is placed in It, and I is initialized for subsequent
   --  calls to Get_Next_Interp.

   procedure Get_Next_Interp (I : in out Interp_Index; It : out Interp);
   --  Iteration step over set of interpretations. Using the value in I, which
   --  was set by a previous call to Get_First_Interp or Get_Next_Interp, the
   --  next interpretation is placed in It, and I is updated for the next call.
   --  The end of the list of interpretations is signalled by It.Nam = Empty.

   procedure Remove_Interp (I : in out Interp_Index);
   --  Remove an interpretation that his hidden by another, or that does not
   --  match the context. The value of I on input was set by a call to either
   --  Get_First_Interp or Get_Next_Interp and references the interpretation
   --  to be removed. The only allowed use of the exit value of I is as input
   --  to a subsequent call to Get_Next_Interp, which yields the interpretation
   --  following the removed one.

   function Covers (T1, T2 : Entity_Id) return Boolean;
   --  This is the basic type compatibility routine. T1 is the expexted
   --  type, imposed by context, and T2 is the actual type. The processing
   --  reflects both the definition of type coverage and the rules
   --  for operand matching.

   function Disambiguate (Nam1, Nam2 : Entity_Id; Typ : Entity_Id)
                                                      return Entity_Id;
   --  If two interpretations of a name in a call are visible,  apply
   --  preference rules (universal types first) and operator visibility
   --  in order to remove ambiguity.

   function Find_Unique_Type (L : Node_Id; R : Node_Id) return Entity_Id;
   --  Used in second pass of resolution,  for equality and comparison nodes.
   --  L is the left operand, whose type is known to be correct, and R is
   --  the right operand,  which has one interpretation compatible with that
   --  of L. Return the type intersection of the two.

   function Has_Compatible_Type (N : Node_Id;
                                  Typ : Entity_Id) return Boolean;
   --  Verify that some interpretation of the node N has a type compatible
   --  with Typ. If N is not overloaded, then its unique type must be
   --  compatible with Typ. Otherwise iterate through the interpretations
   --  of N looking for a compatible one.

   function Hides_Op (F : Entity_Id; Op : Entity_Id) return boolean;
   --  A user-defined function hides a predefined operator if it is
   --  matches the signature of the operator, and is declared in an
   --  open scope, or in the scope of the result type.

   function Is_Subtype_Of (T1 : Entity_Id; T2 : Entity_Id) return Boolean;
   --  Checks whether T1 is any subtype of T2 directly or indirectly.

   function Intersect_Types (L, R : Node_Id) return Entity_Id;
   --  Simplest type checking for operators, no overloading: bottom-up check
   --  that arguments are consistent. If one of the arguments is universal,
   --  the result has the more specific type of the two.

   function Operator_Matches_Spec (Op,  New_S : Entity_Id) return Boolean;
   --  Used to resolve subprograms renaming operators, and calls to user
   --  defined operators. Determines whether a given operator Op, matches
   --  a specification, New_S.

   function Valid_Comparison_Arg (T : Entity_Id) return Boolean;
   --  A valid argument to an ordering operator must be a discrete type, a
   --  real type, or a one dimensional array with a discrete component type.

   function Valid_Boolean_Arg (T : Entity_Id) return Boolean;
   --  A valid argument of a boolean operator is either some boolean type,
   --  or a one-dimensional array of boolean type.

   procedure Write_Overloads (N : Node_Id);
   --  Debugging procedure to output info on possibly overloaded entities
   --  for specified node.

end Sem_Type;
