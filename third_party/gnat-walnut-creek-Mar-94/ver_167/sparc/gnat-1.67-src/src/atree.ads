------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                A T R E E                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.43 $                             --
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

with Alloc; use Alloc;
with Sinfo; use Sinfo;
with Einfo; use Einfo;
with Types; use Types;
with Table;

package Atree is

--  This package defines the format of the tree used to represent the Ada
--  program internally. Syntactic and semantic information is combined in
--  this tree. There is no separate symbol table structure.

--  WARNING: There is a C version of this package. Any changes to this
--  source file must be properly reflected in the C header file tree.h

--  Package Atree defines the basic structure of the tree and its nodes and
--  provides the basic abstract interface for manipulating the tree. Two
--  other packages use this interface to define the representation of Ada
--  programs using this tree format. The package Sinfo defines the basic
--  representation of the syntactic structure of the program, as output
--  by the parser. The package Entity_Info defines the semantic information
--  which is added to the tree nodes that represent declared entities (i.e.
--  the information which might typically be described in a separate symbol
--  table structure.

--  The front end of the compiler first parses the program and generates a
--  tree that is simply a syntactic representation of the program in abstract
--  syntax tree format. Subsequent processing in the front end traverses the
--  tree, transforming it in various ways and adding semantic information.

   ----------------------------------------
   -- Definitions of Fields in Tree Node --
   ----------------------------------------

   --  The representation of the tree is completely hidden, using a functional
   --  interface for accessing and modifying the contents of nodes. Logically
   --  a node contains a number of fields, much as though the nodes were
   --  defined as a record type. The fields in a node are as follows:

   --   Nkind            Indicates the kind of the node. This field is present
   --                    in all nodes. The type is Node_Kind, which is declared
   --                    in the package Sinfo.

   --   Sloc             Location (Source_Ptr) of the corresponding token
   --                    in the Source buffer. The individual node definitions
   --                    show which token is referenced by this pointer.

   --   Field1           An Int value stored in the tree

   --   Node1            Synonym for Field1 typed as Node_Id
   --   List1            Synonym for Field1 typed as List_Id
   --   Elist1           Synonym for Field1 typed as Elist_Id
   --   Name1            Synonym for Field1 typed as Name_Id
   --   Char_Code1       Synonym for Field1 typed as Char_Code
   --   Uint1            Synonym for Field1 typed as Uint

   --   Note: the actual usage of Field1 (i.e. whether it contains a Node_Id,
   --   List_Id, Elist_Id, Name_Id, Char_Code or Uint), depends on the value
   --   in Nkind (and, for entities, the value in Ekind). Generally the access
   --   to this field is always via the functional interface, so the field
   --   names Field1, Node1, List1, Elist1, Name1, Char_Code1 and Uint1 are
   --   used only in the bodies of the access functions (i.e. in the bodies
   --   of Sinfo and Einfo). These access functions contain debugging code
   --   that checks that the usage is consistent with Nkind and Ekind.

   --   However, in specialized circumstances (examples are the circuit in
   --   generic instantiation to copy trees, and in the tree dump routine),
   --   it is useful to be able to do untyped traversals, and an internal
   --   package in Atree allows for direct untyped accesses in such cases.

   --   Similar definitions for Field2, Field3, Field4, Field5 (and also
   --   Node2-5, List2-5, Elist2-5, Name2-5, Char_Code2-5, Uint2-5)

   --   Flag1            Twenty Boolean flags (use depends on Nkind and
   --   Flag2            Ekind, as described for Fieldn). Again the access
   --   Flag3            is usually via subprograms in Sinfo and Einfo which
   --   Flag4            provide high-level synonyms for these flags, and
   --   Flag5            contain debugging code that checks that the values
   --   Flag6            in Nkind and Ekind are appropriate for the access.
   --   Flag7
   --   Flag8
   --   Flag9
   --   Flag10
   --   Flag11
   --   Flag12
   --   Flag13
   --   Flag14
   --   Flag15
   --   Flag16
   --   Flag17
   --   Flag18
   --   Flag19
   --   Flag20

   --   Parent           Pointer to parent node, i.e. node which points to
   --                    the node which references this field. The parent
   --                    field can only be set in a node which is not in
   --                    a list, or in a list header. The parent field of
   --                    nodes that are in a list is automatically set to
   --                    the parent of the list header, and cannot be
   --                    explicitly set. This field is considered private
   --                    to Atree.

   --  The following additional fields are present in extended nodes
   --  (i.e. nodes extended by a call to the Extend_Node function)

   --   Ekind            Entity type. This field is present only in entity
   --                    nodes (i.e. Nkind in N_Entity). It is of type
   --                    Entity_Kind, which is defined in package Einfo.
   --                    All entity nodes are extended nodes.

   --   Flag21           Twenty-three additional flags
   --   Flag22
   --   Flag23
   --   Flag24
   --   Flag25
   --   Flag26
   --   Flag27
   --   Flag28
   --   Flag29
   --   Flag30
   --   Flag31
   --   Flag32
   --   Flag33
   --   Flag34
   --   Flag35
   --   Flag36
   --   Flag37
   --   Flag38
   --   Flag39
   --   Flag40
   --   Flag41
   --   Flag42
   --   Flag43

   --   Field6           Additional Int value stored in tree

   --   Node6            Synonym for Field6 typed as Node_Id
   --   List6            Synonym for Field6 typed as List_Id
   --   Elist6           Synonym for Field6 typed as Elist_Id
   --   Uint6            Synonym for Field6 typed as Uint

   --   Similar definitions for Field7 to Field12 (and Node7-Node12,
   --   List7-List12, Elist7-Elist12, Uint7-Uint12).

   function Last_Node_Id return Node_Id;
   pragma Inline (Last_Node_Id);
   --  Returns Id of last allocated node Id

   function Last_List_Id return List_Id;
   pragma Inline (Last_List_Id);
   --  Returns Id of last allocated list Id

   -----------------------
   -- Use of Empty Node --
   -----------------------

   --  The special Node_Id Empty is used to mark missing fields. Whenever the
   --  syntax has an optional component, then the corresponding field will be
   --  set to Empty if the component is missing.

   --  Note: Empty is not used to describe an empty list. Instead in this
   --  case the node field contains a list which is empty, and these cases
   --  should be distinguished (essentially from a type point of view, Empty
   --  is a Node, and is thus not a list).

   --  Note: Empty does in fact correspond to an allocated node. Only the
   --  Nkind field of this node may be referenced. It contains N_Empty, which
   --  uniquely identifies the empty case. This allows the Nkind field to be
   --  dereferenced before the check for Empty which is sometimes useful.

   -------------------------------
   -- Default Setting of Fields --
   -------------------------------

   --  Nkind is always set, there is no default value

   --  Ekind is always set, there is no default value

   --  Sloc is always set, there is no default value

   --  Field1-5 fields are set to Empty

   --  Field6-12 fields in extended nodes are set to Empty

   --  Parent is set to Empty

   --  All Boolean flag fields are set to False

   --  Note: the value Empty is used in Field1-Field12 to indicate a null node.
   --  The usage varies. The common uses are to indicate absence of an
   --  optional clause or a completely unused Field1-12 field.

   -------------------------------------
   -- Use of Synonyms for Node Fields --
   -------------------------------------

   --  A subpackage Atree.Unchecked_Access provides routines for reading and
   --  writing the fields defined above (Field1-12, Node1-12, Flag1-29 etc).
   --  These unchecked access routines can be used for untyped traversals. In
   --  In addition they are used in the implementations of the Sinfo and
   --  Einfo packages. These packages both provide logical synonyms for
   --  the generic fields, together with an appropriate set of access routines.
   --  Normally access to information within tree nodes uses these synonyms,
   --  providing a high level typed interface to the tree information.

   ----------------
   -- Node Lists --
   ----------------

   --  A node list is a list that is threaded through nodes (using the Link
   --  field). This means that it takes minimum space, but a node can be on
   --  at most one such node list. For each node list, a list header is
   --  allocated in the lists table, and a List_Id value references this
   --  header which may be used to access the nodes in the list using the
   --  following set of routines that define the interface:

   function New_List return List_Id;
   --  Creates a new empty node list. Typically this is used to initialize
   --  a field in some other node which points to a node list where the list
   --  is then subsequently filled in using Append calls.

   function Empty_List return List_Id renames New_List;
   --  Used in contexts where an empty list (as opposed to an initially empty
   --  list to be filled in) is required.

   function New_List_1 (A                         : Node_Id) return List_Id;
   function New_List_2 (A, B                      : Node_Id) return List_Id;
   function New_List_3 (A, B, C                   : Node_Id) return List_Id;
   function New_List_4 (A, B, C, D                : Node_Id) return List_Id;
   function New_List_5 (A, B, C, D, E             : Node_Id) return List_Id;
   function New_List_6 (A, B, C, D, E, F          : Node_Id) return List_Id;
   function New_List_7 (A, B, C, D, E, F, G       : Node_Id) return List_Id;
   function New_List_8 (A, B, C, D, E, F, G, H    : Node_Id) return List_Id;
   function New_List_9 (A, B, C, D, E, F, G, H, I : Node_Id) return List_Id;
   --  Build lists with indicated number of initial node values. Exactly
   --  Equivalent to calling New_List and then calling Append to insert
   --  the nodes A-I in sequence, in particular, if any of the nodes contain
   --  Error, then they are not inserted.

   function New_List_Copy (List : List_Id) return List_Id;
   --  Creates a new list containing copies (made with New_Copy) of every
   --  node in the original list. If the argument is No_List, then the
   --  returned result is No_List. If the argument is an empty list, then
   --  the returned result is a new empty list.

   function First (List : List_Id) return Node_Id;
   pragma Inline (First);
   --  Obtains the first element of the given node list or, if the node list
   --  has no items, then Empty is returned. It is an error to call First with
   --  a pointer to a node or Empty (Empty is not considered to be the
   --  same as an empty node list).

   function Last (List : List_Id) return Node_Id;
   pragma Inline (Last);
   --  Obtains the last element of the given node list or, if the node list
   --  has no items, then Empty is returned. It is an error to call Last with
   --  a pointer to a node or Empty (Empty is not considered to be the
   --  same as an empty node list).

   function Nth (List : List_Id; Index : Pos) return Node_Id;
   --  Returns the Index'th item in List (first node = 1). If Index is
   --  greater than the number of items in the list, Empty is returned.

   function Next (Node : Node_Id) return Node_Id;
   pragma Inline (Next);
   --  This function returns the next node on a node list, or Empty if Node is
   --  the last element of the node list. The argument must be a member of a

   function Prev (Node : Node_Id) return Node_Id;
   --  This function returns the previous node on a node list list, or Empty if
   --  Node is the first element of the node list. The argument must be a
   --  member of a node list.

   function Is_Empty_List (L : List_Id) return Boolean;
   pragma Inline (Is_Empty_List);
   --  This function determines if a given list id references a node list that
   --  contains no items. No_List is a not a legitimate argument.

   function Is_Non_Empty_List (L : List_Id) return Boolean;
   --  This function determines if a given list id references a node list that
   --  contains at least one item. No_List is not a legitimate argument.

   function Is_List_Member (Node : Node_Id) return Boolean;
   pragma Inline (Is_List_Member);
   --  This function determines if a given node is a member of a node list.
   --  It is an error for Node to be Empty, or to be a node list.

   function List_Containing (Node : Node_Id) return List_Id;
   --  This function provides a pointer to the node list containing Node.
   --  Node must be a member of a node list.

   procedure Append (Node : Node_Id; To : List_Id);
   --  Appends Node at the end of node list To. Node must be a non-empty node
   --  that is not already a member of a node list, and To must be a
   --  node list. An attempt to append an error node is ignored without
   --  complaint and the list is unchanged.

   procedure Append_To (To : List_Id; Node : Node_Id);
   pragma Inline (Append_To);
   --  Like Append, but arguments are the other way round

   procedure Append_List (List : List_Id; To : List_Id);
   --  Appends node list List to the end of node list To. On return,
   --  List is reset to be empty.

   procedure Append_List_To (To : List_Id; List : List_Id);
   pragma Inline (Append_List_To);
   --  Like Append_List, but arguments are the other way round

   procedure Insert_After (After : Node_Id; Node : Node_Id);
   --  Insert Node, which must be a non-empty node that is not already a
   --  member of a node list, immediately past node After, which must be a
   --  node that is currently a member of a node list. An attempt to insert
   --  an error node is ignored without complaint (and the list is unchanged).

   procedure Insert_List_After (After : Node_Id; List : List_Id);
   --  Inserts the entire contents of node list List immediately after node
   --  After, which must be a member of a node list. On return, the node list
   --  List is reset to be the empty node list.

   procedure Insert_Before (Before : Node_Id; Node : Node_Id);
   --  Insert Node, which must be a non-empty node that is not already a
   --  member of a node list, immediately before Before, which must be a node
   --  that is currently a member of a node list. An attempt to insert an
   --  error node is ignored without complaint (and the list is unchanged).

   procedure Insert_List_Before (Before : Node_Id; List : List_Id);
   --  Inserts the entire contents of node list List immediately before node
   --  Before, which must be a member of a node list. On return, the node list
   --  List is reset to be the empty node list.

   procedure Prepend (Node : Node_Id; To : List_Id);
   --  Prepends Node at the start of node list To. Node must be a non-empty
   --  node that is not already a member of a node list, and To must be a
   --  node list. An attempt to prepend an error node is ignored without
   --  complaint and the list is unchanged.

   procedure Prepend_To (To : List_Id; Node : Node_Id);
   pragma Inline (Prepend_To);
   --  Like Prepend, but arguments are the other way round

   procedure Remove (Node : Node_Id);
   --  Removes Node, which must be a node that is a member of a node list,
   --  from this node list. The contents of Node are not otherwise affected.

   function Remove_Head (List : List_Id) return Node_Id;
   --  Removes the head element of a node list, and returns the node (whose
   --  contents are not otherwise affected) as the result. If the node list is
   --  empty, then Empty is returned.

   function Remove_Next (Node : Node_Id) return Node_Id;
   --  Removes the item immediately following the given node, and returns it
   --  as the result. If Node is the last element of the list, then Empty is
   --  returned.

   -------------------
   -- Element Lists --
   -------------------

   --  As described above, node lists are threaded through nodes. This is
   --  space efficient, since the only space overhead is for the list header,
   --  but limited in two. First a node can be only on one such list.
   --  Second, since there is only room for one pointer in a list, the list
   --  is singly linked, and consequently the Prev operation is slow.

   --  Element lists are more general and resolve all these limitations. They
   --  use separate list elements to represent the list. This takes extra
   --  space, but it allows a node to be on multiple lists. In addition, there
   --  is room in a list element for both a forward and backward pointer, so
   --  operations like Prev are efficient.

   --  Note that element lists do NOT have a parent pointer. This is because
   --  element lists are never used for structural representation of the
   --  tree (e.g. the parser never generates element lists).

   --  The following set of routines define the interface for element lists

   function Id_Of (Elmt : Elmt_Id) return Node_Id;
   pragma Inline (Id_Of);
   --  Returns the value of a given list element

   function New_Elmt_List return Elist_Id;
   --  Creates a new empty element list. Typically this is used to initialize
   --  a field in some other node which points to an element list where the
   --  list is then subsequently filled in using Append calls.

   function Empty_Elmt_List return Elist_Id renames New_Elmt_List;
   --  Used in contexts where an empty element list (as opposed to an
   --  initially empty element list to be filled in) is required.

   function New_Elmt_List_Copy (List : Elist_Id) return Elist_Id;
   --  Creates a new list containing copies (made with New_Copy) of every
   --  node in the original list. If the argument is No_Elist, then the
   --  result is No_Elist. If the argument is an empty element list, then
   --  the result is a new empty element list.

   function First_Elmt (List : Elist_Id) return Elmt_Id;
   pragma Inline (First_Elmt);
   --  Obtains the first element of the given element list or, if the element
   --  list has no items, then No_Elmt is returned. It is an error to call
   --  First_Elmt with anything other than an element list.

   function Last_Elmt (List : Elist_Id) return Elmt_Id;
   pragma Inline (Last_Elmt);
   --  Obtains the last element of the given element list or, if the element
   --  list has no items, then No_Elmt is returned. It is an error to call
   --  Last_Elmt with anything other than an element list.

   function Nth_Elmt (List : Elist_Id; Index : Pos) return Elmt_Id;
   --  Returns the Index'th element in the element list List (first node = 1).
   --  If Index exceeds the number of items in the list, No_Elmt is returned.

   function Next_Elmt (Elmt : Elmt_Id) return Elmt_Id;
   pragma Inline (Next_Elmt);
   --  This function returns the next element on an element list. The argument
   --  must be a list element other than No_Elmt. Returns No_Elmt if the given
   --  element is the last element of the list.

   function Prev_Elmt (Elmt : Elmt_Id) return Elmt_Id;
   pragma Inline (Prev_Elmt);
   --  This function returns the previous element on an element list. The
   --  argument must be a list element other than No_Elmt. Returns No_Elmt
   --  if the given element is the last element of the list.

   function Is_Empty_Elmt_List (L : Elist_Id) return Boolean;
   --  This function determines if a given tree id references an element list
   --  that contains no items.

   function Is_Non_Empty_Elmt_List (L : Elist_Id) return Boolean;
   --  This function determines if a given tree id references an element
   --  list that contains at least one item.

   procedure Append_Elmt (Node : Node_Id; To : Elist_Id);
   --  Appends Node at the end of To, allocating a new element.

   procedure Append_Elmt_List (List : Elist_Id; To : Elist_Id);
   --  Appends element list List to the end of element list To. On return,
   --  List is reset to be empty.

   procedure Insert_Elmt_After (After : Elmt_Id; Node : Node_Id);
   --  Insert Node immediately after After, allocating a new element

   procedure Insert_Elmt_List_After (After : Elmt_Id; List : Elist_Id);
   --  Inserts the entire contents of element list List immediately after
   --  element After. On return, List is reset to be the empty element list.

   procedure Insert_Elmt_Before (Before : Elmt_Id; Node : Node_Id);
   --  Insert Node immediately before Before, allocating a new element

   procedure Insert_Elmt_List_Before (Before : Elmt_Id; List : Elist_Id);
   --  Inserts the entire contents of element list List immediately before
   --  element Before. On return, List is reset to be the empty element list.

   procedure Remove_Elmt (Elmt : Elmt_Id);
   --  Removes the given element from the list. The contents of the item
   --  referenced by the list element are not affected. The space used by
   --  the list element itself is freed for reuse.

   function Remove_Head_Elmt (List : Elist_Id) return Node_Id;
   --  Removes the head element of an element list, and returns the item
   --  referenced by this head element. The space used by the list element
   --  itself is freed for reuse. If the element list is empty, then Empty
   --  is returned.

   function Remove_Next_Elmt (Elmt : Elmt_Id) return Node_Id;
   --  Removes the element immediately following the given element and returns
   --  the node referenced by this element. If the element is the last element
   --  of the list, then Empty is returned. The space used by the removed list
   --  element itself is freed for reuse.

   --------------------------------------------------
   -- Node Allocation and Modification Subprograms --
   --------------------------------------------------

   --  Generally the parser builds the tree amd then it is further decorated
   --  (e.g. by setting the entity fields), but not fundamentally modified.
   --  However, there are cases in which the tree must be restructured by
   --  adding and rearranging nodes, as a result of disambiguating cases
   --  which the parser could not parse correctly, and adding additional
   --  semantic information (e.g. making constraint checks explicit). The
   --  following subprograms are used for constructing the tree in the first
   --  place, and then for subsequent modifications as required

   procedure Initialize_Atree;
   --  Called at the start of compilation to initialize the allocation of
   --  the node and list tables and make the standard entries for Empty,
   --  Error and Error_List.

   function New_Node (New_Node_Kind : Node_Kind; New_Sloc : Source_Ptr)
     return Node_Id;
   --  Allocates a completely new node with the given node type and source
   --  location values. All other fields are set to their standard defaults:

   --    Empty for all Fieldn fields
   --    False for all Flagn fields

   --  The usual approach is to build a new node using this function and
   --  then, using the value returned, use the Set_xxx functions to set
   --  fields of the node as required. Note that nodes allocated using this
   --  function never have an extension. If an extension is required, then
   --  a subsequent call to Extend_Node must be made.

   function Extend_Node (Node : Node_Id) return Node_Id;
   --  This function returns a copy of its input node with an extension added.
   --  The fields of the extension are set to Empty. Due to the way extensions
   --  are handled (as two consecutive array elements), it may be necessary to
   --  reallocate the node, so that the returned value is not the same as the
   --  input value, but where possible the returned value will be the same as
   --  the input value (i.e. the extension will occur in place). It is the
   --  caller's responsibility to ensure that any pointers to the original
   --  node are appropriately updated.

   function Has_Extension (N : Node_Id) return Boolean;
   pragma Inline (Has_Extension);
   --  Returns True if the given node has an extension (i.e. was the result of
   --  a Extend_Node call). Returns False for a non-extended node.

   procedure Change_Node (N : Node_Id; New_Node_Kind : Node_Kind);
   --  This procedure replaces the given node by setting its Nkind field to
   --  the indicated value and resetting all other fields to their default
   --  values except for Sloc, which is unchanged, and the Parent pointer
   --  and list links, which are also unchanged. All other information in
   --  the original node is lost. The new node has an extension if the
   --  original node had an extension.

   procedure Copy_Node (Source : Node_Id; Destination : Node_Id);
   --  Copy the entire contents of the source node to the destination node.
   --  The contents of the source node is not affected. If the source node
   --  has an extension, then the destination must have an extension also.
   --  The parent pointer of the destination and its list link, if any, are
   --  not affected by the copy.

   function New_Copy (Source : Node_Id) return Node_Id;
   --  This function allocates a completely new node, and then initializes
   --  it by copying the contents of the source node into it. The contents
   --  of the source node is not affected. The target node is always marked
   --  as not being in a list (even if the source is a list member). The
   --  new node will have an extension if the source has an extension.
   --  New_Copy (Empty) returns Empty and New_Copy (Error) returns Error.

   procedure Exchange_Nodes (Node1 : Node_Id; Node2 : Node_Id);
   --  Exchange the contents of the two nodes, except that the Parent pointers,
   --  and list pointers are not modified. If either node has an extension then
   --  the other node must also have an extension.

   procedure Delete_Node (Node : Node_Id);
   --  The node is first removed from its containing list if it is a list
   --  member. Then it is deleted from the tree and its type is set to N_Void.
   --  It is an error (not necessarily detected) to reference this node after
   --  it has been deleted. The implementation of the body of Atree is free to
   --  reuse the node to satisfy future node allocation requests.

   ---------------------------
   -- Node Access Functions --
   ---------------------------

   --  The following functions return the contents of the indicated field of
   --  the node referenced by the argument, which is a Node_Id.

   function Nkind (N : Node_Id) return Node_Kind;
   pragma Inline (Nkind);

   function Ekind (N : Node_Id) return Entity_Kind;
   pragma Inline (Ekind);

   function Sloc  (N : Node_Id) return Source_Ptr;
   pragma Inline (Sloc);

   function Parent (N : Node_Id)  return Node_Id;
   pragma Inline (Parent);

   function List_Parent (L : List_Id) return Node_Id;
   pragma Inline (List_Parent);

   function Elist_Parent (E : Elist_Id) return Node_Id;
   pragma Inline (Elist_Parent);

   function No (N : Node_Id) return Boolean;
   pragma Inline (No);
   --  Tests given Id for equality with the Empty node. This allows notations
   --  like "if No (Variant_Part) then" as opposed to "if Variant_Part = Empty
   --  then". Note that this function may not be used with Elist values (use
   --  explicit compare with No_Elist for this case).

   function Present (N : Node_Id) return Boolean;
   pragma Inline (Present);
   --  Tests given Id for inequality with the Empty node. This allows notations
   --  like "if Present (Variant_Part) then" as opposed to "if Variant_Part /=
   --  Empty then". Note that this function may not be used with Elist values
   --  (used explicit compare with No_Elist for this case).

   function List_Present (L : List_Id) return Boolean;
   pragma Inline (List_Present);

   ----------------------------
   -- Node Update Procedures --
   ----------------------------

   --  The following functions set a specified field in the node whose Id is
   --  passed as the first argument. The second parameter is the new value
   --  to be set in the specified field. Note that Set_Nkind is in the next
   --  section, since its use is restricted.

   procedure Set_Ekind (N : Node_Id; Val : Entity_Kind);
   pragma Inline (Set_Ekind);

   procedure Set_Sloc (N : Node_Id; Val : Source_Ptr);
   pragma Inline (Set_Sloc);

   procedure Set_Parent (N : Node_Id;  Val : Node_Id);
   procedure Set_Parent (L : List_Id;  Val : Node_Id);
   procedure Set_Parent (E : Elist_Id; Val : Node_Id);
   pragma Inline (Set_Parent);

   ---------------------------
   -- Tree Rewrite Routines --
   ---------------------------

   --  During the compilation process it is necessary in a number of situations
   --  to rewrite the tree. In some cases, such rewrites do not affect the
   --  structure of the tree, for example, when an indexed component node is
   --  replaced by the corresponding call node (the parser cannot distinguish
   --  between these two cases).

   --  In other situations, the rewrite does affect the structure of the
   --  tree. Examples are the replacement of a generic instantiation by the
   --  instantiated spec and body, and the static evaluation of expressions.

   --  If such structural modifications are done by the expander, there are
   --  no difficulties, since the form of the tree after the expander has no
   --  special significance, except as input to the backend of the compiler.
   --  However, if these modifications are done by the semantic phase, then
   --  it is important that they be done in a manner which allows the original
   --  tree to be preserved. This is because tools like pretty printers need
   --  to have this original tree structure available.

   --  The subprograms in this section allow rewriting of the tree by either
   --  insertion of new nodes in an existing list, or complete replacement of
   --  a subtree. The resulting tree for most purposes looks as though it has
   --  been really changed, and there is no trace of the original. However,
   --  special subprograms, also defined in this section, allow the original
   --  tree to be reconstructed if necessary.

   --  For tree modifications done in the expander, it is permissible to
   --  destroy the original tree, although it is also allowable to use the
   --  tree rewrite routines where it is convenient to do so.

   procedure Mark_Rewrite_Insertion (New_Node : Node_Id);
   pragma Inline (Mark_Rewrite_Insertion);
   --  This procedure marks the given node as an insertion made during a tree
   --  rewriting operation. Only the root needs to be marked. The call does
   --  not do the actual insertion, which must be done using one of the normal
   --  list insertion routines. The node is treated normally in all respects
   --  except for its response to Is_Rewrite_Insertion.

   function Is_Rewrite_Insertion (Node : Node_Id) return Boolean;
   pragma Inline (Is_Rewrite_Insertion);
   --  Tests whether the given node was marked using Set_Rewrite_Insert. This
   --  is used in reconstructing the original tree (where such nodes are to
   --  be eliminated from the reconstructed tree).

   procedure Rewrite_Substitute_Tree (Old_Node, New_Node : Node_Id);
   --  This is used when a complete subtree is to be replaced. Old_Node is the
   --  root of the old subtree to be replaced, and New_Node is the root of the
   --  newly constructed replacement subtree. The actual mechanism is to swap
   --  the contents of these two nodes fixing up parent pointers appropriately.
   --  At the current time, there is a limitation that requires that neither
   --  Old_Node, nor New_Node can be extended nodes. New_Node should *not*
   --  be marked using Mark_Rewrite_Insertion.

   function Is_Rewrite_Substitution (Node : Node_Id) return Boolean;
   pragma Inline (Is_Rewrite_Substitution);
   --  Return True iff Node has been rewritten (i.e. if Node is the root
   --  or a subtree which was installed using Rewrite_Substitute_Tree)

   function Original_Node (Node : Node_Id) return Node_Id;
   --  If Node has not been rewritten, then returns its input argument
   --  unchanged, else returns the Node for the original subtree. If
   --  applied to a node marked by Set_Rewrite_Insert, returns Empty.

   --  Note: there is no direct mechanism for deleting an original subtree.
   --  Instead it must be rewritten using Rewrite_Substitute_Tree (one
   --  possibility is to substitute a null statement).

   -----------------------------------
   -- Generic Field Access Routines --
   -----------------------------------

   --  This subpackage provides the functions for accessing and procedures
   --  for setting fields that are normally referenced by their logical
   --  synonyms defined in packages Sinfo and Einfo. As previously
   --  described the implementations of these packages use the package
   --  Atree.Unchecked_Access.

   package Unchecked_Access is

      --  Functions to fetch contents of indicated field. It is an error
      --  to attempt to read the value of a field which is not present.

      function Field1 (N : Node_Id) return Int;
      pragma Inline (Field1);

      function Field2 (N : Node_Id) return Int;
      pragma Inline (Field2);

      function Field3 (N : Node_Id) return Int;
      pragma Inline (Field3);

      function Field4 (N : Node_Id) return Int;
      pragma Inline (Field4);

      function Field5 (N : Node_Id) return Int;
      pragma Inline (Field5);

      function Field6 (N : Node_Id) return Int;
      pragma Inline (Field6);

      function Field7 (N : Node_Id) return Int;
      pragma Inline (Field7);

      function Field8 (N : Node_Id) return Int;
      pragma Inline (Field8);

      function Field9 (N : Node_Id) return Int;
      pragma Inline (Field9);

      function Field10 (N : Node_Id) return Int;
      pragma Inline (Field10);

      function Field11 (N : Node_Id) return Int;
      pragma Inline (Field11);

      function Field12 (N : Node_Id) return Int;
      pragma Inline (Field12);

      function Node1 (N : Node_Id) return Node_Id;
      pragma Inline (Node1);

      function Node2 (N : Node_Id) return Node_Id;
      pragma Inline (Node2);

      function Node3 (N : Node_Id) return Node_Id;
      pragma Inline (Node3);

      function Node4 (N : Node_Id) return Node_Id;
      pragma Inline (Node4);

      function Node5 (N : Node_Id) return Node_Id;
      pragma Inline (Node5);

      function Node6 (N : Node_Id) return Node_Id;
      pragma Inline (Node6);

      function Node7 (N : Node_Id) return Node_Id;
      pragma Inline (Node7);

      function Node8 (N : Node_Id) return Node_Id;
      pragma Inline (Node8);

      function Node9 (N : Node_Id) return Node_Id;
      pragma Inline (Node9);

      function Node10 (N : Node_Id) return Node_Id;
      pragma Inline (Node10);

      function Node11 (N : Node_Id) return Node_Id;
      pragma Inline (Node11);

      function Node12 (N : Node_Id) return Node_Id;
      pragma Inline (Node12);

      function List1 (N : Node_Id) return List_Id;
      pragma Inline (List1);

      function List2 (N : Node_Id) return List_Id;
      pragma Inline (List2);

      function List3 (N : Node_Id) return List_Id;
      pragma Inline (List3);

      function List4 (N : Node_Id) return List_Id;
      pragma Inline (List4);

      function List5 (N : Node_Id) return List_Id;
      pragma Inline (List5);

      function List6 (N : Node_Id) return List_Id;
      pragma Inline (List6);

      function List7 (N : Node_Id) return List_Id;
      pragma Inline (List7);

      function List8 (N : Node_Id) return List_Id;
      pragma Inline (List8);

      function List9 (N : Node_Id) return List_Id;
      pragma Inline (List9);

      function List10 (N : Node_Id) return List_Id;
      pragma Inline (List10);

      function List11 (N : Node_Id) return List_Id;
      pragma Inline (List11);

      function List12 (N : Node_Id) return List_Id;
      pragma Inline (List12);

      function Elist1 (N : Node_Id) return Elist_Id;
      pragma Inline (Elist1);

      function Elist2 (N : Node_Id) return Elist_Id;
      pragma Inline (Elist2);

      function Elist3 (N : Node_Id) return Elist_Id;
      pragma Inline (Elist3);

      function Elist4 (N : Node_Id) return Elist_Id;
      pragma Inline (Elist4);

      function Elist5 (N : Node_Id) return Elist_Id;
      pragma Inline (Elist5);

      function Elist6 (N : Node_Id) return Elist_Id;
      pragma Inline (Elist6);

      function Elist7 (N : Node_Id) return Elist_Id;
      pragma Inline (Elist7);

      function Elist8 (N : Node_Id) return Elist_Id;
      pragma Inline (Elist8);

      function Elist9 (N : Node_Id) return Elist_Id;
      pragma Inline (Elist9);

      function Elist10 (N : Node_Id) return Elist_Id;
      pragma Inline (Elist10);

      function Elist11 (N : Node_Id) return Elist_Id;
      pragma Inline (Elist11);

      function Elist12 (N : Node_Id) return Elist_Id;
      pragma Inline (Elist12);

      function Name1 (N : Node_Id) return Name_Id;
      pragma Inline (Name1);

      function Name2 (N : Node_Id) return Name_Id;
      pragma Inline (Name2);

      function Name3 (N : Node_Id) return Name_Id;
      pragma Inline (Name3);

      function Name4 (N : Node_Id) return Name_Id;
      pragma Inline (Name4);

      function Name5 (N : Node_Id) return Name_Id;
      pragma Inline (Name5);

      function Char_Code1 (N : Node_Id) return Char_Code;
      pragma Inline (Char_Code1);

      function Char_Code2 (N : Node_Id) return Char_Code;
      pragma Inline (Char_Code2);

      function Char_Code3 (N : Node_Id) return Char_Code;
      pragma Inline (Char_Code3);

      function Char_Code4 (N : Node_Id) return Char_Code;
      pragma Inline (Char_Code4);

      function Char_Code5 (N : Node_Id) return Char_Code;
      pragma Inline (Char_Code5);

      function Str1 (N : Node_Id) return String_Id;
      pragma Inline (Str1);

      function Str2 (N : Node_Id) return String_Id;
      pragma Inline (Str2);

      function Str3 (N : Node_Id) return String_Id;
      pragma Inline (Str3);

      function Str4 (N : Node_Id) return String_Id;
      pragma Inline (Str4);

      function Str5 (N : Node_Id) return String_Id;
      pragma Inline (Str5);

      function Uint1 (N : Node_Id) return Uint;
      pragma Inline (Uint1);

      function Uint2 (N : Node_Id) return Uint;
      pragma Inline (Uint2);

      function Uint3 (N : Node_Id) return Uint;
      pragma Inline (Uint3);

      function Uint4 (N : Node_Id) return Uint;
      pragma Inline (Uint4);

      function Uint5 (N : Node_Id) return Uint;
      pragma Inline (Uint5);

      function Uint6 (N : Node_Id) return Uint;
      pragma Inline (Uint6);

      function Uint7 (N : Node_Id) return Uint;
      pragma Inline (Uint7);

      function Uint8 (N : Node_Id) return Uint;
      pragma Inline (Uint8);

      function Uint9 (N : Node_Id) return Uint;
      pragma Inline (Uint9);

      function Uint10 (N : Node_Id) return Uint;
      pragma Inline (Uint10);

      function Uint11 (N : Node_Id) return Uint;
      pragma Inline (Uint11);

      function Uint12 (N : Node_Id) return Uint;
      pragma Inline (Uint12);

      function Flag1 (N : Node_Id) return Boolean;
      pragma Inline (Flag1);

      function Flag2 (N : Node_Id) return Boolean;
      pragma Inline (Flag2);

      function Flag3 (N : Node_Id) return Boolean;
      pragma Inline (Flag3);

      function Flag4 (N : Node_Id) return Boolean;
      pragma Inline (Flag4);

      function Flag5 (N : Node_Id) return Boolean;
      pragma Inline (Flag5);

      function Flag6 (N : Node_Id) return Boolean;
      pragma Inline (Flag6);

      function Flag7 (N : Node_Id) return Boolean;
      pragma Inline (Flag7);

      function Flag8 (N : Node_Id) return Boolean;
      pragma Inline (Flag8);

      function Flag9 (N : Node_Id) return Boolean;
      pragma Inline (Flag9);

      function Flag10 (N : Node_Id) return Boolean;
      pragma Inline (Flag10);

      function Flag11 (N : Node_Id) return Boolean;
      pragma Inline (Flag11);

      function Flag12 (N : Node_Id) return Boolean;
      pragma Inline (Flag12);

      function Flag13 (N : Node_Id) return Boolean;
      pragma Inline (Flag13);

      function Flag14 (N : Node_Id) return Boolean;
      pragma Inline (Flag14);

      function Flag15 (N : Node_Id) return Boolean;
      pragma Inline (Flag15);

      function Flag16 (N : Node_Id) return Boolean;
      pragma Inline (Flag16);

      function Flag17 (N : Node_Id) return Boolean;
      pragma Inline (Flag17);

      function Flag18 (N : Node_Id) return Boolean;
      pragma Inline (Flag18);

      function Flag19 (N : Node_Id) return Boolean;
      pragma Inline (Flag19);

      function Flag20 (N : Node_Id) return Boolean;
      pragma Inline (Flag20);

      function Flag21 (N : Node_Id) return Boolean;
      pragma Inline (Flag21);

      function Flag22 (N : Node_Id) return Boolean;
      pragma Inline (Flag22);

      function Flag23 (N : Node_Id) return Boolean;
      pragma Inline (Flag23);

      function Flag24 (N : Node_Id) return Boolean;
      pragma Inline (Flag24);

      function Flag25 (N : Node_Id) return Boolean;
      pragma Inline (Flag25);

      function Flag26 (N : Node_Id) return Boolean;
      pragma Inline (Flag26);

      function Flag27 (N : Node_Id) return Boolean;
      pragma Inline (Flag27);

      function Flag28 (N : Node_Id) return Boolean;
      pragma Inline (Flag28);

      function Flag29 (N : Node_Id) return Boolean;
      pragma Inline (Flag29);

      function Flag30 (N : Node_Id) return Boolean;
      pragma Inline (Flag30);

      function Flag31 (N : Node_Id) return Boolean;
      pragma Inline (Flag31);

      function Flag32 (N : Node_Id) return Boolean;
      pragma Inline (Flag32);

      function Flag33 (N : Node_Id) return Boolean;
      pragma Inline (Flag33);

      function Flag34 (N : Node_Id) return Boolean;
      pragma Inline (Flag34);

      function Flag35 (N : Node_Id) return Boolean;
      pragma Inline (Flag35);

      function Flag36 (N : Node_Id) return Boolean;
      pragma Inline (Flag36);

      function Flag37 (N : Node_Id) return Boolean;
      pragma Inline (Flag37);

      function Flag38 (N : Node_Id) return Boolean;
      pragma Inline (Flag38);

      function Flag39 (N : Node_Id) return Boolean;
      pragma Inline (Flag39);

      function Flag40 (N : Node_Id) return Boolean;
      pragma Inline (Flag40);

      function Flag41 (N : Node_Id) return Boolean;
      pragma Inline (Flag41);

      function Flag42 (N : Node_Id) return Boolean;
      pragma Inline (Flag42);

      function Flag43 (N : Node_Id) return Boolean;
      pragma Inline (Flag43);

      --  Procedures to set value of indicated field

      procedure Set_Nkind (N : Node_Id; Val : Node_Kind);
      pragma Inline (Set_Nkind);

      procedure Set_Field1 (N : Node_Id; Val : Int);
      pragma Inline (Set_Field1);

      procedure Set_Field2 (N : Node_Id; Val : Int);
      pragma Inline (Set_Field2);

      procedure Set_Field3 (N : Node_Id; Val : Int);
      pragma Inline (Set_Field3);

      procedure Set_Field4 (N : Node_Id; Val : Int);
      pragma Inline (Set_Field4);

      procedure Set_Field5 (N : Node_Id; Val : Int);
      pragma Inline (Set_Field5);

      procedure Set_Field6 (N : Node_Id; Val : Int);
      pragma Inline (Set_Field6);

      procedure Set_Field7 (N : Node_Id; Val : Int);
      pragma Inline (Set_Field7);

      procedure Set_Field8 (N : Node_Id; Val : Int);
      pragma Inline (Set_Field8);

      procedure Set_Field9 (N : Node_Id; Val : Int);
      pragma Inline (Set_Field9);

      procedure Set_Field10 (N : Node_Id; Val : Int);
      pragma Inline (Set_Field10);

      procedure Set_Field11 (N : Node_Id; Val : Int);
      pragma Inline (Set_Field11);

      procedure Set_Field12 (N : Node_Id; Val : Int);
      pragma Inline (Set_Field12);

      procedure Set_Node1 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node1);

      procedure Set_Node2 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node2);

      procedure Set_Node3 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node3);

      procedure Set_Node4 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node4);

      procedure Set_Node5 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node5);

      procedure Set_Node6 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node6);

      procedure Set_Node7 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node7);

      procedure Set_Node8 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node8);

      procedure Set_Node9 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node9);

      procedure Set_Node10 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node10);

      procedure Set_Node11 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node11);

      procedure Set_Node12 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node12);

      procedure Set_List1 (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List1);

      procedure Set_List2 (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List2);

      procedure Set_List3 (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List3);

      procedure Set_List4 (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List4);

      procedure Set_List5 (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List5);

      procedure Set_List6 (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List6);

      procedure Set_List7 (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List7);

      procedure Set_List8 (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List8);

      procedure Set_List9 (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List9);

      procedure Set_List10 (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List10);

      procedure Set_List11 (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List11);

      procedure Set_List12 (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List12);

      procedure Set_Elist1 (N : Node_Id; Val : Elist_Id);
      pragma Inline (Set_Elist1);

      procedure Set_Elist2 (N : Node_Id; Val : Elist_Id);
      pragma Inline (Set_Elist2);

      procedure Set_Elist3 (N : Node_Id; Val : Elist_Id);
      pragma Inline (Set_Elist3);

      procedure Set_Elist4 (N : Node_Id; Val : Elist_Id);
      pragma Inline (Set_Elist4);

      procedure Set_Elist5 (N : Node_Id; Val : Elist_Id);
      pragma Inline (Set_Elist5);

      procedure Set_Elist6 (N : Node_Id; Val : Elist_Id);
      pragma Inline (Set_Elist6);

      procedure Set_Elist7 (N : Node_Id; Val : Elist_Id);
      pragma Inline (Set_Elist7);

      procedure Set_Elist8 (N : Node_Id; Val : Elist_Id);
      pragma Inline (Set_Elist8);

      procedure Set_Elist9 (N : Node_Id; Val : Elist_Id);
      pragma Inline (Set_Elist9);

      procedure Set_Elist10 (N : Node_Id; Val : Elist_Id);
      pragma Inline (Set_Elist10);

      procedure Set_Elist11 (N : Node_Id; Val : Elist_Id);
      pragma Inline (Set_Elist11);

      procedure Set_Elist12 (N : Node_Id; Val : Elist_Id);
      pragma Inline (Set_Elist12);

      procedure Set_Name1 (N : Node_Id; Val : Name_Id);
      pragma Inline (Set_Name1);

      procedure Set_Name2 (N : Node_Id; Val : Name_Id);
      pragma Inline (Set_Name2);

      procedure Set_Name3 (N : Node_Id; Val : Name_Id);
      pragma Inline (Set_Name3);

      procedure Set_Name4 (N : Node_Id; Val : Name_Id);
      pragma Inline (Set_Name4);

      procedure Set_Name5 (N : Node_Id; Val : Name_Id);
      pragma Inline (Set_Name5);

      procedure Set_Char_Code1 (N : Node_Id; Val : Char_Code);
      pragma Inline (Set_Char_Code1);

      procedure Set_Char_Code2 (N : Node_Id; Val : Char_Code);
      pragma Inline (Set_Char_Code2);

      procedure Set_Char_Code3 (N : Node_Id; Val : Char_Code);
      pragma Inline (Set_Char_Code3);

      procedure Set_Char_Code4 (N : Node_Id; Val : Char_Code);
      pragma Inline (Set_Char_Code4);

      procedure Set_Char_Code5 (N : Node_Id; Val : Char_Code);
      pragma Inline (Set_Char_Code5);

      procedure Set_Str1 (N : Node_Id; Val : String_Id);
      pragma Inline (Set_Str1);

      procedure Set_Str2 (N : Node_Id; Val : String_Id);
      pragma Inline (Set_Str2);

      procedure Set_Str3 (N : Node_Id; Val : String_Id);
      pragma Inline (Set_Str3);

      procedure Set_Str4 (N : Node_Id; Val : String_Id);
      pragma Inline (Set_Str4);

      procedure Set_Str5 (N : Node_Id; Val : String_Id);
      pragma Inline (Set_Str5);

      procedure Set_Uint1 (N : Node_Id; Val : Uint);
      pragma Inline (Set_Uint1);

      procedure Set_Uint2 (N : Node_Id; Val : Uint);
      pragma Inline (Set_Uint2);

      procedure Set_Uint3 (N : Node_Id; Val : Uint);
      pragma Inline (Set_Uint3);

      procedure Set_Uint4 (N : Node_Id; Val : Uint);
      pragma Inline (Set_Uint4);

      procedure Set_Uint5 (N : Node_Id; Val : Uint);
      pragma Inline (Set_Uint5);

      procedure Set_Uint6 (N : Node_Id; Val : Uint);
      pragma Inline (Set_Uint5);

      procedure Set_Uint7 (N : Node_Id; Val : Uint);
      pragma Inline (Set_Uint7);

      procedure Set_Uint8 (N : Node_Id; Val : Uint);
      pragma Inline (Set_Uint8);

      procedure Set_Uint9 (N : Node_Id; Val : Uint);
      pragma Inline (Set_Uint9);

      procedure Set_Uint10 (N : Node_Id; Val : Uint);
      pragma Inline (Set_Uint10);

      procedure Set_Uint11 (N : Node_Id; Val : Uint);
      pragma Inline (Set_Uint11);

      procedure Set_Uint12 (N : Node_Id; Val : Uint);
      pragma Inline (Set_Uint12);

      procedure Set_Flag1 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag1);

      procedure Set_Flag2 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag2);

      procedure Set_Flag3 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag3);

      procedure Set_Flag4 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag4);

      procedure Set_Flag5 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag5);

      procedure Set_Flag6 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag6);

      procedure Set_Flag7 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag7);

      procedure Set_Flag8 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag8);

      procedure Set_Flag9 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag9);

      procedure Set_Flag10 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag10);

      procedure Set_Flag11 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag11);

      procedure Set_Flag12 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag12);

      procedure Set_Flag13 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag13);

      procedure Set_Flag14 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag14);

      procedure Set_Flag15 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag15);

      procedure Set_Flag16 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag16);

      procedure Set_Flag17 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag17);

      procedure Set_Flag18 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag18);

      procedure Set_Flag19 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag19);

      procedure Set_Flag20 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag20);

      procedure Set_Flag21 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag21);

      procedure Set_Flag22 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag22);

      procedure Set_Flag23 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag23);

      procedure Set_Flag24 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag24);

      procedure Set_Flag25 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag25);

      procedure Set_Flag26 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag26);

      procedure Set_Flag27 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag27);

      procedure Set_Flag28 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag28);

      procedure Set_Flag29 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag29);

      procedure Set_Flag30 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag30);

      procedure Set_Flag31 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag31);

      procedure Set_Flag32 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag32);

      procedure Set_Flag33 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag33);

      procedure Set_Flag34 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag34);

      procedure Set_Flag35 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag35);

      procedure Set_Flag36 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag36);

      procedure Set_Flag37 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag37);

      procedure Set_Flag38 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag38);

      procedure Set_Flag39 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag39);

      procedure Set_Flag40 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag40);

      procedure Set_Flag41 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag41);

      procedure Set_Flag42 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag42);

      procedure Set_Flag43 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag43);

      --  The following versions of Set_Noden also set the parent
      --  pointer of the referenced node if it is non_Empty

      procedure Set_Node1_With_Parent (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node1);

      procedure Set_Node2_With_Parent (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node2);

      procedure Set_Node3_With_Parent (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node3);

      procedure Set_Node4_With_Parent (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node4);

      procedure Set_Node5_With_Parent (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node5);

      procedure Set_Node6_With_Parent (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node6);

      procedure Set_Node7_With_Parent (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node7);

      procedure Set_Node8_With_Parent (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node8);

      procedure Set_Node9_With_Parent (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node9);

      procedure Set_Node10_With_Parent (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node10);

      procedure Set_Node11_With_Parent (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node11);

      procedure Set_Node12_With_Parent (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node12);

      --  The following versions of Set_Listn also set the parent pointer of
      --  the referenced node if it is non_Empty. The procedures for List6
      --  to List12 can only be applied to nodes which have an extension.

      procedure Set_List1_With_Parent (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List1_With_Parent);

      procedure Set_List2_With_Parent (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List2_With_Parent);

      procedure Set_List3_With_Parent (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List3_With_Parent);

      procedure Set_List4_With_Parent (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List4_With_Parent);

      procedure Set_List5_With_Parent (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List5_With_Parent);

      procedure Set_List6_With_Parent (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List6_With_Parent);

      procedure Set_List7_With_Parent (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List7_With_Parent);

      procedure Set_List8_With_Parent (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List8_With_Parent);

      procedure Set_List9_With_Parent (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List9_With_Parent);

      procedure Set_List10_With_Parent (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List10_With_Parent);

      procedure Set_List11_With_Parent (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List11_With_Parent);

      procedure Set_List12_With_Parent (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List12_With_Parent);

   end Unchecked_Access;

   -----------------------------
   -- Private Part Subpackage --
   -----------------------------

   --  The following package contains the definition of the data structure
   --  used by the implementation of the Atree package. Logically it really
   --  corresponds to the private part, hence the name. The reason that it
   --  is defined as a sub-package is to allow special access from clients
   --  that need to see the internals of the data structures.

   package Atree_Private_Part is

      -------------------------
      -- Tree Representation --
      -------------------------

      --  The nodes of the tree are stored in a table (i.e. an array). In the
      --  case of extended nodes, two consecutive components in the array are
      --  used. There are thus two formats for array components. One is used
      --  for non-extended nodes, and for the first component of extended
      --  nodes. The other is used for the extension (second component) of
      --  an extended node. A variant record structure is used to distinguish
      --  the two formats.

      type Node_Record (Is_Extension : Boolean := False) is record

         --  Logically, the only field in the common part is the above
         --  Is_Extension discriminant (a single bit). However, Gigi cannot
         --  yet handle such a structure, so we fill out the common part of
         --  the record with fields that are used in different ways for
         --  normal nodes and node extensions.

         In_List : Boolean;
         --  Flag used to indicate if node is a member of a list.
         --  This field is considered private to the Atree package.

         Rewrite_Sub : Boolean;
         --  Flag set if this node was result of Rewrite_Substitute_Tree.
         --  This field is considered private to the Atree package.

         Rewrite_Ins : Boolean;
         --  Flag set by Mark_Rewrite_Insertion procedure.
         --  This field is considered private to the Atree package.

         Flag1  : Boolean;
         Flag2  : Boolean;
         Flag3  : Boolean;
         Flag4  : Boolean;
         Flag5  : Boolean;
         Flag6  : Boolean;
         Flag7  : Boolean;
         Flag8  : Boolean;
         Flag9  : Boolean;
         Flag10 : Boolean;
         Flag11 : Boolean;
         Flag12 : Boolean;
         Flag13 : Boolean;
         Flag14 : Boolean;
         Flag15 : Boolean;
         Flag16 : Boolean;
         Flag17 : Boolean;
         Flag18 : Boolean;
         Flag19 : Boolean;
         Flag20 : Boolean;
         --  The twenty flags for a normal node

         --  The above fields are used as follows in a node extension

         --    In_List         used as     Flag21 in extension
         --    Rewrite_Sub     used as     Flag22 in extension
         --    Rewrite_Ins     used as     Flag23 in extension
         --    Flag1           used as     Flag24 in extension
         --    Flag2           used as     Flag25 in extension
         --    Flag3           used as     Flag26 in extension
         --    Flag4           used as     Flag27 in extension
         --    Flag5           used as     Flag28 in extension
         --    Flag6           used as     Flag29 in extension
         --    Flag7           used as     Flag30 in extension
         --    Flag8           used as     Flag31 in extension
         --    Flag9           used as     Flag32 in extension
         --    Flag10          used as     Flag33 in extension
         --    Flag11          used as     Flag34 in extension
         --    Flag12          used as     Flag35 in extension
         --    Flag13          used as     Flag36 in extension
         --    Flag14          used as     Flag37 in extension
         --    Flag15          used as     Flag38 in extension
         --    Flag16          used as     Flag39 in extension
         --    Flag17          used as     Flag40 in extension
         --    Flag18          used as     Flag41 in extension
         --    Flag19          used as     Flag42 in extension
         --    Flag20          used as     Flag43 in extension

         Nkind : Node_Kind;
         --  For a non-extended node, or the initial section of an extended
         --  node, this field holds the Node_Kind value. For an extended node,
         --  this field is used (by means of unchecked conversion) to hold
         --  the Ekind field of the entity.

         --  Now finally (on an 32-bit boundary!) comes the variant part

         case Is_Extension is

            --  Non-extended node, or first component of extended node

            when False =>

               Sloc : Source_Ptr;
               --  Source location for this node

               Link : Int;
               --  This field is used either as the Parent pointer (if In_List
               --  is False), or as the list link pointer (if In_List is True)
               --  This field is considered private to the Atree package.

               Field1 : Int;
               Field2 : Int;
               Field3 : Int;
               Field4 : Int;
               Field5 : Int;
               --  Five general use fields, which can contain Node_Id, List_Id,
               --  Elist_Id, String_Id, Name_Id, or Char_Code values depending
               --  on the values in Nkind and (for extended nodes), in Ekind.
               --  See packages Sinfo and Einfo for details of their use.

            --  Extension (second component) of extended node

            when True =>

               Field6  : Int;
               Field7  : Int;
               Field8  : Int;
               Field9  : Int;
               Field10 : Int;
               Field11 : Int;
               Field12 : Int;
               --  Seven additional general fields available only for entities
               --  See package Einfo for details of their use (which depends
               --  on the value in the Ekind field).

         end case;
      end record;

      pragma Pack (Node_Record);

      --  The following defines the extendible array used for the nodes table
      --  Nodes with extensions use two consecutive entries in the array

      package Nodes is new Table (
         Component_Type => Node_Record,
         Index_Type     => Node_Id,
         Low_Bound      => First_Node_Id,
         Initial        => Alloc_Nodes_Initial,
         Increment      => Alloc_Nodes_Increment,
         Table_Name     => "Nodes");

      --  The following is the structure used for the lists table. Nodes in
      --  this table are used in three different ways:

      --  For node list headers
      --    Lfield1 points to the first node in the list (Empty if none)
      --    Lfield2 points to the last node in the list (Empty if none)
      --    Lnode points to the parent (Empty if none)

      --  For element list headers
      --    Lfield1 points to the first element in the list (No_Elmt if none)
      --    Lfield2 points to the last element in the list (No_Elmt if none)
      --    Lnode points to the parent (Empty if none)

      --  For elements in an element list
      --    Lfield1 points to the prev element or to header if at list start
      --    Lfield2 points to the next element or to header if at list end
      --    Lnode contains pointer to referenced Val of element

      type List_Header is record
         Lfield1 : Int;
         Lfield2 : Int;
         Lnode   : Node_Id;
      end record;

      for List_Header'Size use 8 * 12;

      List_Header_Bytes : constant := 12;
      --  Size of list header in bytes

      package Lists is new Table (
         Component_Type => List_Header,
         Index_Type     => List_Id,
         Low_Bound      => First_List_Id,
         Initial        => Alloc_Lists_Initial,
         Increment      => Alloc_Lists_Increment,
         Table_Name     => "Lists");

   end Atree_Private_Part;

end Atree;
