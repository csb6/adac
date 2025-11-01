------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                O S I N T                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.21 $                             --
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

with Types;  use Types;
package Osint is

--  This package contains the low level, operating system routines used in
--  the GNAT compiler and binder for command line processing and file
--  input output. The specification is suitable for use with MS/DOS or
--  Unix of similar systems. Separate bodies are provided for different
--  operating systems environments.

   type Program_Type is (Compiler, Binder, Make);
   Program : Program_Type;
   --  Program currently running (set by Initialize_OS_Interface)

   procedure Initialize_OS_Interface (P : Program_Type);
   --  This routine scans parameters and initializes for the first call to
   --  Open_Next_File. It also resets any of the variables in Options in
   --  response to command switch settings. Init may terminate execution
   --  if the parameters are invalid or some other fatal error is encountered.
   --  The interface is set up to accomodate scanning a series of files (e.g.
   --  as the result of wild card references in DOS, or an expanded list of
   --  source files in Unix). Of course it is perfectly possible to ignore
   --  this in the implementation and provide for opening only one file.
   --  The parameter is the program that is running.

   procedure Write_Program_Name;
   --  Writes name of program as invoked to standard output file

   function System_Maximum_File_Name_Length return Pos;
   --  Returns the maximum number of characters allowed in a file name, not
   --  counting the extension. If there is no limit, then Int'Last is returned.

   -----------------------
   -- Source File Input --
   -----------------------

   --  Source file input routines are used by the compiler to read the main
   --  source files and the subsidiary source files (e.g. with'ed units), and
   --  also by the binder and make to check presence/time stamps of sources.

   function More_Source_Files return Boolean;
   --  Indicates whether more source file remain to be processed. Returns
   --  False right away if no source files, or if all source files have
   --  been processed.

   function Next_Main_Source return File_Name_Type;
   --  This function returns the name of the next main source file specified
   --  on the command line. It is an error to call Next_Main_Source if no more
   --  source files exist (i.e. Next_Main_Source may be called only if a
   --  previous call to More_Source_Files returned True). This name is the
   --  simple file name (without any directory information).

   function Read_Source_File (Name : File_Name_Type; Fatal_Err : Boolean)
     return Source_Buffer_Ptr;
   --  Allocates a Text_Buffer of appropriate length and then reads the entire
   --  contents of the source file Name into the buffer. The text is terminated
   --  by an EOF character which is not necessarily the last text character.
   --  The lower bound of the text buffer for the first file opened after a
   --  call to Next_Main_Source is zero. The lower bound for buffers returned
   --  by subsequent calls are one greater than the previous high bound. If
   --  the given file cannot be opened, then the action depends on Fatal_Err.
   --  If Fatal_Err is True, an error message is given, otherwise if Fatal_Err
   --  is False, then null is returned. Note that the Name passed to this
   --  function is the simple file name, without any directory information.
   --  The implementation of Read_Source_File is responsible for searching for
   --  the file in appropriate directories.

   function Current_Source_File_Stamp return Time_Stamp_Type;
   --  Returns the time stamp of the source file most recently read by a
   --  call to Read_Source_File.

   function Full_Source_Name return Name_Id;
   --  This function returns the full name of the source file most recently
   --  read using Read_Source_File. This name includes the appropriate
   --  directory information (typically a relative directory name in a
   --  Unix like system). This name is only used for the purpose of error
   --  message construction and need not be accessible by Name_Find (i.e.
   --  it may be created by Name_Enter).

   function Source_File_Stamp (Name : File_Name_Type) return Time_Stamp_Type;
   --  Returns the time stamp of the specified source file. If the source
   --  file cannot be opened, an all blank time stamp is returned (this is
   --  not an error situation).

   -------------------------------------------
   -- Representation of Library Information --
   -------------------------------------------

   --  Associated with each compiled source file is library information,
   --  a string of bytes whose exact format is described in packag Libfmt.
   --  Compiling a source file generates this library information for the
   --  compiled unit, and access the library information for units that
   --  were compiled previously on which the unit being compiled depends.

   --  How this information is stored is up to the implementation of this
   --  package. At the interface level, this information is simply associated
   --  with its corresponding source.

   --  Several different implementations are possible:

   --    1. The information could be directly associated with the source file,
   --       e.g. placed in a resource fork of this file on the Mac, or on
   --       MS-DOS, written to the source file after the end of file mark.

   --    2. The information could be written into the generated object module
   --       if the system supports the inclusion of arbitrary informational
   --       byte streams into object files. In this case there must be a naming
   --       convention that allows object files to be located given the name of
   --       the corresponding source file.

   --    3. The information could be written to a separate file, whose name is
   --       related to the name of the source file by a fixed convention.

   --  Which of these three methods is chosen depends on the contraints of the
   --  host operating system. The interface described here is independent of
   --  which of these approaches is used.

   -------------------------------
   -- Library Information Input --
   -------------------------------

   --  These subprograms are used by the binder to read library information
   --  files, see section above for representation of these files.

   function More_Lib_Files return Boolean;
   --  Indicates whether more library information files remain to be processed.
   --  Returns False right away if no source files, or if all source files
   --  have been processed.

   function Next_Main_Lib_File return File_Name_Type;
   --  This function returns the name of the next library info file specified
   --  on the command line. It is an error to call Next_Main_Lib_File if no
   --  more library information files exist (i.e. Next_Main_Lib_File may be
   --  called only if a previous call to More_Lib_Files returned True). This
   --  name is the simple name, excluding any directory information.

   function Read_Library_Info (Lib_File : File_Name_Type; Fatal_Err : Boolean)
     return Text_Buffer_Ptr;
   --  Allocates a Text_Buffer of appropriate length and reads in the entire
   --  source of the library information from the library information file
   --  whose name (obtained by a previous call to Lib_File_Name) is given by
   --  the parameter Name. An EOF character terminates the text (but is not
   --  necessarily the last character of the text). The lower bound of the
   --  Text_Buffer is always zero. If the library information for the given
   --  file cannot be located, then null is returned. If the given file cannot
   --  be opened, then the action depends on Fatal_Err. If Fatal_Err is True,
   --  an error message is given, otherwise if Fatal_Err is False, then null
   --  is returned. Note that the Lib_File is a simple name which does not
   --  include any directory information. The implementation is responsible
   --  for searching for the file in appropriate directories.

   function Full_Library_Info_Name return Name_Id;
   --  This function returns the full name of the library information file
   --  most recently read using Read_Library_Info, including appropriate
   --  directory information (typically a relative directory name in a        
   --  Unix like system). This name is only used for the purpose of error     
   --  message construction and need not be accessible by Name_Find (i.e.      
   --  it may be created by Name_Enter).                                       

   --------------------------------
   -- Library Information Output --
   --------------------------------

   --  These routines are used by the compiler to generate the library
   --  information file for the main source file being compiled. See section
   --  above for a discussion of how library information files are stored.
   --  The Lib_File_Name routine is also used by Make.

   procedure Create_Output_Library_Info;
   --  Creates the output library information file for the source file which
   --  is currently being compiled (i.e. the file which was most recently
   --  returned by Next_Main_Source).

   procedure Write_Library_Info (Info : Str);
   --  Writes the contents of the referenced string to the library information
   --  file for the main source file currently being compiled (i.e. the file
   --  which was most recently opened with a call to Read_Next_File). Info
   --  represents a single line in the file, but does not contain any line
   --  termination characters. The implementation of Write_Library_Info is
   --  responsible for adding necessary end of line and end of file control
   --  characters to the generated file.

   procedure Close_Output_Library_Info;
   --  Closes the file created by Create_Output_Library_Info, flushing any
   --  buffers etc from writes by Write_Library_Info.

   function Lib_File_Name (Source_File : File_Name_Type) return File_Name_Type;
   --  Given the name of a source file, returns the name of the corresponding
   --  library information file. This may be the name of the object file, or
   --  of a separate file used to store the library information. In either case
   --  the returned result is suitable for use in a call to Read_Library_Info.
   --  Note: this subprogram is in this section because it is used by the
   --  compiler to determine the proper library information names to be placed
   --  in the generated library information file.

   -------------------
   -- Binder Output --
   -------------------

   --  These routines are used by the binder to generate the C source file
   --  containing the binder output. The format of this file is described
   --  in the package Bindfmt.

   procedure Create_Binder_Output;
   --  Creates the binder output file corresponding to the library information
   --  file for the main program (i.e. for the file returned by the previous
   --  call to Next_Main_Lib_File).

   procedure Write_Binder_Info (Info : Str);
   --  Writes the contents of the referenced string to the binder output file
   --  created by a previous call to Create_Binder_Output. Info represents a
   --  single line in the file, but does not contain any line termination
   --  characters. The implementation of Write_Library_Info is responsible
   --  for adding necessary end of line and end of file control characters as
   --  required by the operating system.

   procedure Close_Binder_Output;
   --  Closes the file created by Create_Binder_Output, flushing any
   --  buffers etc from writes by Write_Binder_Info.

   -----------------
   -- Xref Output --
   -----------------

   --  These routines are used by the xref tool to generate the .ref files
   --  containing the xref output.

   function Number_Of_Files return Int;
   --  gives the total number of filenames found on command line.

   procedure Create_Req_Output;
   --  Create the output file for the required interface of the unit I. The
   --  file name must be in Buffer_name befor calling this procedure.

   procedure Create_Xref_Output;
   --  Creates the xref output file. If there is only one file to be analysed
   --  it will create 'file.ref' otherwise 'X.ref'

   procedure Write_Xref_Info (Info : Str; Eol : Boolean := True);
   --  Writes the contents of the referenced string to the Xref output file
   --  created by a previous call to Create_Xref_Output. Info represents a
   --  single line in the file, but does not contain any line termination
   --  characters. The implementation of Write_Xref_Info is responsible
   --  for adding necessary end of line and end of file control characters as
   --  required by the operating system.

   procedure Close_Xref_Output;
   --  Closes the file created by Create_Xref_Output, flushing any
   --  buffers etc from writes by Write_Xref_Info.

   -----------------
   -- Termination --
   -----------------

   type Exit_Code_Type is (E_Success, E_Warnings, E_Errors, E_Fatal, E_Abort);
   --  This type defines the possible exit statuses for the Exit_Program
   --  call. Success is used for normal termination. Warnings indicates that
   --  warning messages were posted, but no errors. Errors indicates that
   --  at least one error message was generated. Fatal indicates that a fatal
   --  error condition was detected such as source file not found. The status
   --  code E_Abort is only used for an internally detected compiler error.
   --  If possible, a traceback or other appropriate debugging information
   --  should be generated in this case.

   procedure Exit_Program (Exit_Code : Exit_Code_Type);
   --  A call to Exit_Program terminates execution with the given status.
   --  A status of zero indicates normal completion, a non-zero status
   --  indicates abnormal termination.

   -----------------------
   -- Program Execution --
   -----------------------

   --  These routines are used by Make to call the compiler and binder. The
   --  options are supplied by Osint from the original command line, using
   --  an appropriate convention to separate Make options, Compile options
   --  and Make options.

   function Do_Compile (Source_File : File_Name_Type) return Exit_Code_Type;
   --  Perform a compilation of the given source file. The returned value
   --  indicates whether or not the compilation succeeded. If the return
   --  type is E_Success, then a matching ALI file has been created.

   function Do_Bind (Lib_File : File_Name_Type) return Exit_Code_Type;
   --  Perform a bind of the given library file. The exit code will be
   --  returned as the exit code of Make itself. Note that Do_Bind is
   --  called only if all Do_Compile steps succeeded (returned E_Success).
   --  Otherwise the exit code from Make is the highest exit code returned
   --  by any Do_Compile step.

end Osint;
