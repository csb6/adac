------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  L I B                                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.29 $                             --
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

with Casing; use Casing;
with Table;
with Types;  use Types;

package Lib is

--  This package contains routines for accessing and outputting the library
--  information. It contains the routine to load subsidiary units.

   --------------------------------------------
   -- General Approach to Library Management --
   --------------------------------------------

   --  As described in GNote #1, when a unit is compiled, all its subsidiary
   --  units are recompiled, including the following:

   --    (a) Corresponding spec for a body
   --    (b) Parent spec of a child library spec
   --    (d) With'ed specs
   --    (d) Parent body of a subunit
   --    (e) Subunits corresponding to any specified stubs
   --    (f) Bodies of inlined subprograms that are called
   --    (g) Bodies of generic subprograms or packages that are instantiated
   --    (h) Bodies of packages containing either of the above two items
   --    (i) Specs and bodies of runtime units
   --    (j) Parent specs for with'ed child library units

   --  If a unit is being compiled only for syntax checking, then no subsidiary
   --  units are loaded, the the syntax check applies only to the main unit,
   --  i.e. the one contained in the source submitted to the library.

   --  If a unit is being compiled for syntax and semantic checking, then only
   --  cases (a)-(d) loads are performed, since the full semantic checking can
   --  be carried out without needing (e)-(i) loads. In this case no object
   --  file, or library information file, is generated, so the missing units
   --  do not affect the results.

   --  Specifications of library subprograms, subunits, and generic specs
   --  and bodies, can only be compiled in syntax/semantic checking mode,
   --  since no code is ever generated directly for these units. In the case
   --  of subunits, only the compilation of the ultimate parent unit generates
   --  actual code. If a subunit is submitted to the compiler in syntax/
   --  semantic checking mode, the parent (or parents in the nested case) are
   --  semantically checked only up to the point of the corresponding stub.

   --  If code is being generated, then all the above units are required,
   --  although the need for bodies of inlined procedures can be suppressed
   --  by the use of a switch that sets the mode to ignore pragma Inline
   --  statements.

   --  The two main sections of the font end, Par and Sem, are recursive.
   --  Compilation proceeds unit by unit making recursive calls as necessary.
   --  The process is controlled from the GNAT main program, which makes calls
   --  to Par and Sem sequence for the main unit.

   --  Par parses the given unit, and then, after the parse is complete, uses
   --  the Par.Load subprogram to load all its subsidiary units in categories
   --  (a)-(d) above, installing pointers to the loaded units in the parse
   --  tree, as described in a later section of this spec. If any of these
   --  required units is missing, a fatal error is signalled, so that no
   --  attempt is made to run Sem in such cases, since it is assumed that
   --  too many cascaded errors would result, and the confusion would not
   --  be helpful.

   --  Following the call to Par on the main unit, the entire tree of required
   --  units is thus loaded, and Sem is called on the main unit. The parameter
   --  passed to Sem is the unit to be analyzed. The visibility table, which
   --  is a single global structure, starts out containing only the entries
   --  for the visible entities in Standard. Every call to Sem establishes a
   --  new scope stack table, pushing an entry for Standard on entry to provide
   --  the proper initial scope environment.

   --  Sem first proceeds to perform semantic analysis on the currently loaded
   --  units as follows:

   --    In the case of a body (case (a) above), Sem analyzes the corresponding
   --    spec, using a recursive call to Sem. As is always expected to be the
   --    case with calls to Sem, any entities installed in the visibility table
   --    are removed on exit from Sem, so that these entities have to be
   --    reinstalled on return to continue the analysis of the body which of
   --    course needs visibility of these entities.
   --
   --    In the case of the parent of a child spec (case (b) above), a similar
   --    call is made to Sem to analyze the parent. Again, on return, the
   --    entities from the analyzed parent spec have to be installed in the
   --    visibility table of the caller (the child unit), which must have
   --    visibility to the entities in its parent spec.

   --    For with'ed specs (case (c) above), a recursive call to Sem is made
   --    to analyze each spec in turn. After all the spec's have been analyzed,
   --    but not till that point, the entities from all the with'ed units are
   --    reinstalled in the visibility table so that the caller can proceed
   --    with the analysis of the unit doing the with's with the necessary
   --    entities made either use visible or visible by selection as needed.

   --    Case (d) arises when Sem is passed a subunit to analyze. This means
   --    that the main unit is a subunit, and the unit passed to Sem is either
   --    the main unit, or one of its ancestors that is still a subunit. Since
   --    analysis must start at the top of the tree, Sem essentially cancels
   --    the current call by immediately making a call to analyze the parent
   --    (when this call is finished it immediately returns, so logically this
   --    call is like a goto). The subunit will then be analyzed at the proper
   --    time as described for the stub case. Note that we also turn off the
   --    indication that code should be generated in this case, since the only
   --    time we generate code for subunits is when compiling the main parent.

   --    Case (e), subunits corresponding to stubs, are handled as the stubs
   --    are encountered. There are three sub-cases:

   --      If the subunit has already been loaded, then this means that the
   --      main unit was a subunit, and we are back on our way down to it
   --      after following the initial processing described for case (d).
   --      In this case we analyze this particular subunit, as described
   --      for the case where we are generating code, but when we get back
   --      we are all done, since the rest of the parent is irrelevant. To
   --      get out of the parent, we raise the exception Subunit_Found, which
   --      is handled at the outer level of Sem.

   --      The cases where the subunit has not already been loaded correspond
   --      to cases where the main unit was a parent. In this case the action
   --      depends on whether or not we are generating code. If we are not
   --      generating code, then this is the case where we can simply ignore
   --      the subunit, since in checking mode we don't even want to insist
   --      that the subunit exist, much less waste time checking it.

   --      If we are generating code, then we need to load and analyze
   --      all subunits. This is achieved with a call to Lib.Load to load
   --      and parse the unit, followed by processing that installs the
   --      context clause of the subunit, analyzes the subunit, and then
   --      removes the context clause (from the visibility chains of the
   --      parent). Note that we do *not* do a recursive call to Sem in
   --      this case, precisely because we need to do the analysis of the
   --      subunit with the current visibility table and scope stack.

   --    Case (f) applies only to subprograms for which a pragma Inline is
   --    given, providing that the compiler is operating in the mode where
   --    pragma Inline's are activated. When the expander encounters a call
   --    to such a subprogram, it loads the body of the subprogram if it has
   --    not already been loaded, and calls Sem to process it.

   --    Case (g) is similar to case (f), except that the body of a generic
   --    is unconditionally required, regardless of compiler mode settings.
   --    As in the subprogram case, when the expander encounters a generic
   --    instantiation, it loads the generic body of the subprogram if it
   --    has not already been loaded, and calls Sem to process it.

   --    Case (h) arises when a package contains either an inlined subprogram
   --    which is called, or a generic which is instantiated. In this case the
   --    body of the package must be loaded and analyzed with a call to Sem.

   --    Case (i) is handled by adding implicit with clauses to the context
   --    clauses of all units that potentially reference the relevant runtime
   --    entities. Note that since we have the full set of units available,
   --    the parser can always determine the set of runtime units that is
   --    needed. These with clauses do not have associated use clauses, so
   --    all references to the entities must be by selection. Once the with
   --    clauses have been added, subsequent processing is as for normal
   --    with clauses.

   --    Case (j) is also handled by adding appropriate implicit with clauses
   --    to any unit that withs a child unit. Again there is no use clause,
   --    and subsequent processing proceeds as for an explicit with clause.

   --  Sem thus completes the loading of all required units, except those
   --  required for inline subprogram bodies or inlined generics. If any
   --  of these load attempts fails, then the expander will not be called,
   --  even if code was to be generated. If the load attempts all succeed
   --  then the expander is called, though the attempt to generate code may
   --  still fail if an error occurs during a load attempt for an inlined
   --  body or a generic body.

   ----------------
   -- File Table --
   ----------------

   --  The file table has an entry for each source file accessed for the
   --  current compilation. The table is indexed by the unit number value,
   --  since there is a one-to-one correspondence between files and units.
   --  The first entry in the table, subscript 0, is for the main file.

   type File_Record is record
      Unit_Name : Unit_Name_Type;
      --  Unit name for this entry

      File_Name : File_Name_Type;
      --  Corresponding file name (simple name with no directory info)

      Full_File_Name : Name_Id;
      --  Full file name (full name with directory info), used for 
      --  generation of error messages (and not for any other purpose)

      Loading : Boolean;
      --  A flag that is used to catch circular WITH dependencies. It is set
      --  True when an entry is initially created in the file table, and set
      --  False when the load is completed, or ends with an error.

      Source : Source_Buffer_Ptr;
      --  Text of source file. Set to null if a file is not found.

      Version : Str (1 .. 6);
      --  Six character version identifier, set by the scanner if a revision
      --  number is present in the file header, and the -dv debug switch is
      --  set, otherwise it is set to all blanks.

      Time_Stamp : Time_Stamp_Type;
      --  Time stamp of the source file (not initialized if file not found)

      Lines_Table : Lines_Table_Ptr;
      --  Pointer to lines table for this source

      Last_Line : Line_Number_Type;
      --  Subscript of last entry in Lines_Table (may be different from 'Last
      --  value because of the use of expandable tables in package Lines). On
      --  completion of compilation of a unit (status = loaded), this is the
      --  number of source lines in the file.

      Cunit : Node_Id;
      --  Pointer to N_Compilation_Unit node if Status = Compiled

      Entity : Node_Id;
      --  Pointer to defining entity node for compilation unit if compiled

      Not_Found_Msg : Boolean;
      --  A flag used to suppress multiple not-found messages for the same
      --  file. Set False when an entry is created, and then set True when
      --  a file not found message is generated (in response to a Load
      --  call with Required set to True and file not found).

      Body_Spec : Boolean;
      --  This flag is set by the caller if the loaded unit is a procedure
      --  body that is used as a spec (because the spec is not present). It
      --  is initialized to False by Lib, and read by the Writ routine to
      --  mark this special dependency in the ALI file.

      Fatal_Error : Boolean;
      --  A flag that is initialized to False, and gets set to True if a fatal
      --  error occurs during the processing of a unit. A fatal error is one
      --  defined as serious enough to stop the next phase of the compiler
      --  from running (i.e. fatal error during parsing stops semantics,
      --  fatal error during semantics stops code generation). Note that
      --  currently, errors of any kind cause Fatal_Error to be set, but
      --  eventually perhaps only errors labeled as Fatal_Errors should be
      --  this severe if we decide to try Sem on sources with minor errors.

      Keyword_Casing : Casing_Type;
      --  Casing style used in file for keyword casing. This is initialized
      --  to Unknown, and then set from the first occurrence of a keyword. This
      --  value is used only for formatting of error messages.

      Identifier_Casing : Casing_Type;
      --  Casing style used in file for identifier casing. This is initialized
      --  to Unknown, and then set from an identifier in the program as soon as
      --  one is found whose casing is sufficiently clear to make a decision.
      --  This value is used for formatting of error messages, and also is used
      --  in the detection of keywords misused as identifiers.

      Generate_Code : Boolean;
      --  This flag is set True for all units in the current file for which
      --  code is to be generated. This includes the units explicitly compiled,
      --  together with their corresponding specifications if they exist).
   end record;

   --  The following is the file table itself

   package File is new Table (
      Component_Type => File_Record,
      Index_Type     => Unit_Number_Type,
      Low_Bound      => 0,
      Initial        => 50,
      Increment      => 100,
      Table_Name     => "Lib.File");

   function Get_Sloc_Unit_Number (S : Source_Ptr) return Unit_Number_Type;
   --  Return unit number of file identified by given source pointer value.
   --  This call must always succeed, since any valid source pointer value
   --  belongs to some previously loaded module.

   function Get_Cunit_Unit_Number (N : Node_Id) return Unit_Number_Type;
   --  Return unit number of file containing the unit whose N_Compilation_Unit
   --  node is the one passed as an argument. This must always succeed since
   --  the node could not have been built without making an entry in the
   --  file table.

   ---------------------------------
   -- Initialization/Finalization --
   ---------------------------------

   procedure Initialize_Lib;
   --  Called at the start of compiling a new main source unit to initialize
   --  the library processing for the new main source. Establishes and
   --  initializes the File.Table entry for the new main unit (leaving
   --  File.Table (Main_Unit).File_Name set to No_File if there are no more
   --  files. Otherwise the main source file has been opened and read and
   --  then closed on return.

   procedure Finalize_Lib;
   --  Called at the end of compiling a main source unit to terminate the
   --  library processing and free up its dynamically allocated storage.

   ---------------------------------------
   -- Loading Separately Compiled Units --
   ---------------------------------------

   function Load (Uname : Unit_Name_Type; Required : Boolean; Enode : Node_Id)
     return Unit_Number_Type;
   --  This function loads and parses the unit specified by Unit_Name_Type
   --  (or returns the unit number for the previously constructed File.Table
   --  entry if this is not the first call for this unit). The value returned
   --  is the unit number that indexes the corresponding entry in File.Table.
   --  The Fatal_Error flag of this entry is set if a serious enough parser
   --  error occurs to prevent subsequent semantic analysis. If the file that
   --  corresponds to Uname is not found, then No_Unit is returned as a result.
   --  The Required parameter is accessed only in this file-not-found case to
   --  indicate whether or not an error message should be generated. At most
   --  one such error message is generated for any given file, and only if
   --  Required is set to True. The Enode parameter is used in this case and
   --  in the circular dependency case. It indicates the source location to
   --  which these error messages are to be attached but *only* if this
   --  location is in the main source (at lower levels, the source message is
   --  always attached to the Enode value from the main source call. Enode is
   --  also used to allow Load to set the Fatal_Error flag of the calling unit
   --  if the loaded unit has a fatal error. A special case allows Enode to
   --  be set to Empty. The result is that Load will simply signal unit not
   --  found without issuing an error message on any error. In this case the
   --  caller is expected to treat the unit not found error as fatal (this
   --  happens in the case of calls from Rtsload).
   --  Note: in the current version of GNAT, any parse error, or parse warning
   --  with treat-warnings-as-errors mode set causes Fatal_Error to be set,
   --  but this may change in some future version if we get confident enough
   --  to analyze bad units!

   function Is_Loaded (Uname : Unit_Name_Type) return Boolean;
   --  Determines if unit with given name is already loaded, i.e. there is
   --  already an entry in the file table with this unit name for which the
   --  corresponding file was found and parsed. Note that the Fatal_Error flag
   --  of this entry must be checked before proceeding with further processing.

   procedure List;
   --  Lists units in active library (i.e. generates output consisting of a
   --  sorted listing of the units represented in File table, with the
   --  exception of the main unit).

   ---------------------------------
   -- Writing Library Information --
   ---------------------------------

   procedure Writ;
   --  This procedure writes the library information for the current main unit

end Lib;
