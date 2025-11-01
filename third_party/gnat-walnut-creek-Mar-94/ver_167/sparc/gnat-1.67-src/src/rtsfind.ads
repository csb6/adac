------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              R T S F I N D                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.16 $                             --
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
package Rtsfind is

--  This package contains the routine that is used to obtain runtime library
--  entities, loading in the required runtime library packages on demand. It
--  is also used for such purposes as finding System.Address when System has
--  not been explicitly With'ed.

   ------------------------
   -- Runtime Unit Table --
   ------------------------

   --  The following type includes an enumeration entry for each runtime
   --  unit. The enumeration literal represents the fully qualified
   --  name of the unit, as follows:

   --    Names of the form RTU_xxx are top level library packages whose name
   --    is xxx. For example the name RTU_System corresponds to package System.

   --    Names of the form RTUA_xxx are first level children of Ada, whose
   --    name is Ada.xxx. For example, the name RTUA_Tags corresponds to
   --    package Ada.Tags.

   --    Names of the form RTUS_xxx are first level children of System, whose
   --    name is System.xxx. For example, the name RTUS_Task_Ids corresponds
   --    to package System.Task_Ids.

   --  This list can contain both subprogram and package unit names. For
   --  packages, the accessible entities in the package are separately
   --  listed in the package entity table.

   type RTU_Id is (
      RTU_System,                        -- package System

      --  Runtime packages, for list of accessible entities in each package,
      --  see declarations in the runtime entity table below.

      RTUA_Tags,                         -- package Ada.Tags
      RTUS_Abort_Control,                -- package System.Abort_Control
      RTUS_Assertions,                   -- package System.Assertions
      RTUS_Image,                        -- package System.Image
      RTUS_Machine_Specifics,            -- package System.Machine_Specifics
      RTUS_Rendezvous,                   -- package System.Rendezvous
      RTUS_Task_IDs,                     -- package System.Task_IDs
      RTUS_Task_Stages,                  -- package System.Task_Stages
      RTUS_Traceback,                    -- package System.Traceback

      --  Functions for Image attribute

      RTUS_Img_B,                        -- Boolean'Image
      RTUS_Img_C,                        -- Character'Image
      RTUS_Img_F,                        -- Float'Image
      RTUS_Img_I,                        -- Integer'Image
      RTUS_Img_LF,                       -- Long_Float'Image
      RTUS_Img_LLF,                      -- Long_Long_Float'Image
      RTUS_Img_LLI,                      -- Long_Long_Integer'Image
      RTUS_Img_SF,                       -- Short_Float'Image

      --  Functions for exponentiation

      RTUS_Xp_F,                         -- exponentiate Float
      RTUS_Xp_I,                         -- exponentiate Integer
      RTUS_Xp_LF,                        -- exponentiate Long_Float
      RTUS_Xp_LLF,                       -- exponentiate Long_Long_Float
      RTUS_Xp_LI,                        -- exponentiate Long_Integer
      RTUS_Xp_LLI,                       -- exponentiate Long_Long_Integer
      RTUS_Xp_SF,                        -- exponentiate Short_Float
      RTUS_Xp_SI,                        -- exponentiate Short_Integer
      RTUS_Xp_SSI);                      -- exponentiate Short_Short_Integer

   --------------------------
   -- Runtime Entity Table --
   --------------------------

   --  This is the enumeration type used to define the argument passed to
   --  the RTE function. The name must exactly match the name of the entity
   --  involved, and in the case of a package entity, this name must uniquely
   --  imply the package containing the entity (i.e. it is not permissible for
   --  two runtime packages to share an entity name if the compiler must be
   --  able to generate references to both entities).

   --  Note that not all entities in the units contained in the runtime unit
   --  table are included in the following table, only those that actually
   --  have to be referenced from generated code.

   type RE_Id is (

      --  Functions for exponentiation

      RE_Xp_F,
      RE_Xp_I,
      RE_Xp_LF,
      RE_Xp_LLF,
      RE_Xp_LI,
      RE_Xp_LLI,
      RE_Xp_SF,
      RE_Xp_SI,
      RE_Xp_SSI,

      --  Functions for Image attribute

      RE_Img_B,
      RE_Img_C,
      RE_Img_F,
      RE_Img_I,
      RE_Img_LF,
      RE_Img_LLF,
      RE_Img_LLI,                     
      RE_Img_SF,

      --  Entities defined in runtime package System

      RE_Address,
      RE_Any_Priority,
      RE_Default_Priority,
      RE_Interrupt_Priority,
      RE_Null_Address,
      RE_Priority,

      --  Entities defined in runtime package Ada.Tags

      RE_Tag,
      RE_Expanded_Name,
      RE_External_Tag,
      RE_Internal_Tag,
      RE_Tag_Error,

      --  Entities defined in runtime package System.Abort_Control

      RE_Abort,
      RE_Abort_Defer,
      RE_Abort_Undefer,

      --  Entities defined in runtime package System.Assertions

      RE_Assert_Failure,

      --  Entities defined in runtime package System.Machine_Specifics

      RE_Init_State,
      RE_Pre_Call_State,
      RE_Task_Storage_Size,
      RE_Machine_Exceptions,
      RE_Interrupt_ID,
      RE_Interrupt_Info,
      RE_Error_Information,

      --  Entities defined in runtime package System.Rendezvous

      RE_Accept_Alternative,
      RE_Accept_Call,
      RE_Accept_List,
      RE_Accept_Trivial,
      RE_Callable,
      RE_Call_Simple,
      RE_Cancel_Task_Entry_Call,
      RE_Complete_Rendezvous,
      RE_Complete_Task_Entry_Call,
      RE_Count,
      RE_Exceptional_Cancel_Task_Entry_Call,
      RE_Exceptional_Complete_Rendezvous,
      RE_Max_Select,
      RE_Max_Task_Entry,
      RE_No_Rendezvous,
      RE_Null_Task_Entry,
      RE_Positive_Select_Index,
      RE_Requeue_Protected_Entry,
      RE_Requeue_Task_Entry,
      RE_Select_Index,
      RE_Select_Modes,
      RE_Selective_Wait,
      RE_Task_Entry_Call,
      RE_Task_Entry_Index,

      --  Entities defined in runtime package System.Task_IDs

      RE_Self,
      RE_Task_ID,
      RE_Task_Image,
      RE_Task_String,

      --  Entities defined in runtime package System.Task_Stages

      RE_Activate_Tasks,
      RE_Activation_Chain,
      RE_Complete_Activation,
      RE_Current_Master,
      RE_Complete_Master,
      RE_Complete_Task,
      RE_Create_Task,
      RE_Enter_Master,
      RE_Expunge_Unactivated_Tasks,
      RE_Master_Id,
      RE_Size_Type,
      RE_Terminated,
      RE_Unspecified_Priority,
      RE_Unspecified_Size,

      --  Entities defined in runtime package System.Traceback

      RE_TB_Line_Num,
      RE_Store_TB,
      RE_Output_Traceback);

   --  The following declarations build a table that is indexed by the
   --  RTE function to determine the unit containing the given entity.

   RE_Unit_Table : array (RE_Id) of RTU_Id := (
      RE_Img_B                               => RTUS_Img_B,
      RE_Img_C                               => RTUS_Img_C,
      RE_Img_F                               => RTUS_Img_F,
      RE_Img_I                               => RTUS_Img_I,
      RE_Img_LF                              => RTUS_Img_LF,
      RE_Img_LLF                             => RTUS_Img_LLF,
      RE_Img_LLI                             => RTUS_Img_LLI,
      RE_Img_SF                              => RTUS_Img_SF,

      RE_Xp_F                                => RTUS_Xp_F,
      RE_Xp_I                                => RTUS_Xp_I,
      RE_Xp_LF                               => RTUS_Xp_LF,
      RE_Xp_LLF                              => RTUS_Xp_LLF,
      RE_Xp_LI                               => RTUS_Xp_LI,
      RE_Xp_LLI                              => RTUS_Xp_LLI,
      RE_Xp_SF                               => RTUS_Xp_SF,
      RE_Xp_SI                               => RTUS_Xp_SI,
      RE_Xp_SSI                              => RTUS_Xp_SSI,

      RE_Address                             => RTU_System,
      RE_Any_Priority                        => RTU_System,
      RE_Default_Priority                    => RTU_System,
      RE_Interrupt_Priority                  => RTU_System,
      RE_Null_Address                        => RTU_System,
      RE_Priority                            => RTU_System,

      RE_Tag                                 => RTUA_Tags,
      RE_Expanded_Name                       => RTUA_Tags,
      RE_External_Tag                        => RTUA_Tags,
      RE_Internal_Tag                        => RTUA_Tags,
      RE_Tag_Error                           => RTUA_Tags,

      RE_Abort                               => RTUS_Abort_Control,
      RE_Abort_Defer                         => RTUS_Abort_Control,
      RE_Abort_Undefer                       => RTUS_Abort_Control,

      RE_Assert_Failure                      => RTUS_Assertions,

      RE_Init_State                          => RTUS_Machine_Specifics,
      RE_Pre_Call_State                      => RTUS_Machine_Specifics,
      RE_Task_Storage_Size                   => RTUS_Machine_Specifics,
      RE_Machine_Exceptions                  => RTUS_Machine_Specifics,
      RE_Interrupt_ID                        => RTUS_Machine_Specifics,
      RE_Interrupt_Info                      => RTUS_Machine_Specifics,
      RE_Error_Information                   => RTUS_Machine_Specifics,

      RE_Accept_Alternative                  => RTUS_Rendezvous,
      RE_Accept_Call                         => RTUS_Rendezvous,
      RE_Accept_List                         => RTUS_Rendezvous,
      RE_Accept_Trivial                      => RTUS_Rendezvous,
      RE_Callable                            => RTUS_Rendezvous,
      RE_Call_Simple                         => RTUS_Rendezvous,
      RE_Cancel_Task_Entry_Call              => RTUS_Rendezvous,
      RE_Complete_Rendezvous                 => RTUS_Rendezvous,
      RE_Complete_Task_Entry_Call            => RTUS_Rendezvous,
      RE_Count                               => RTUS_Rendezvous,
      RE_Exceptional_Cancel_Task_Entry_Call  => RTUS_Rendezvous,
      RE_Exceptional_Complete_Rendezvous     => RTUS_Rendezvous,
      RE_Max_Select                          => RTUS_Rendezvous,
      RE_Max_Task_Entry                      => RTUS_Rendezvous,
      RE_No_Rendezvous                       => RTUS_Rendezvous,
      RE_Null_Task_Entry                     => RTUS_Rendezvous,
      RE_Positive_Select_Index               => RTUS_Rendezvous,
      RE_Requeue_Protected_Entry             => RTUS_Rendezvous,
      RE_Requeue_Task_Entry                  => RTUS_Rendezvous,
      RE_Select_Index                        => RTUS_Rendezvous,
      RE_Select_Modes                        => RTUS_Rendezvous,
      RE_Selective_Wait                      => RTUS_Rendezvous,
      RE_Task_Entry_Call                     => RTUS_Rendezvous,
      RE_Task_Entry_Index                    => RTUS_Rendezvous,

      RE_Self                                => RTUS_Task_IDs,
      RE_Task_ID                             => RTUS_Task_IDs,
      RE_Task_Image                          => RTUS_Task_IDs,
      RE_Task_String                         => RTUS_Task_IDs,

      RE_Activate_Tasks                      => RTUS_Task_Stages,
      RE_Activation_Chain                    => RTUS_Task_Stages,
      RE_Complete_Activation                 => RTUS_Task_Stages,
      RE_Create_Task                         => RTUS_Task_Stages,
      RE_Current_Master                      => RTUS_Task_Stages,
      RE_Complete_Master                     => RTUS_Task_Stages,
      RE_Complete_Task                       => RTUS_Task_Stages,
      RE_Enter_Master                        => RTUS_Task_Stages,
      RE_Expunge_Unactivated_Tasks           => RTUS_Task_Stages,
      RE_Master_Id                           => RTUS_Task_Stages,
      RE_Size_Type                           => RTUS_Task_Stages,
      RE_Terminated                          => RTUS_Task_Stages,
      RE_Unspecified_Priority                => RTUS_Task_Stages,
      RE_Unspecified_Size                    => RTUS_Task_Stages,

      RE_TB_Line_Num                         => RTUS_Traceback,
      RE_Store_TB                            => RTUS_Traceback,
      RE_Output_Traceback                    => RTUS_Traceback);

   -----------------
   -- Subprograms --
   -----------------

   procedure Initialize_Rtsfind;
   --  Procedure to initialize data structures used by RTE. Called at the
   --  start of processing a new main source file. Must be called after
   --  Initialize_Snames (since names it enters into name table must come
   --  after names entered by Snames).

   function RTE (E : RE_Id) return Entity_Id;
   --  Given the entity defined in the above tables, as identified by the
   --  corresponding value in the RE_Id enumeration type, returns the Id
   --  of the corresponding entity, first loading in (parsing, analyzing and
   --  expanding) its spec if the unit has not already been loaded. If the
   --  unit cannot be found, or if it does not contain the specified entity,
   --  then an appropriate error message is output ("Runtime Configuration
   --  Error") and an Unrecoverable_Error exception is raised.

end Rtsfind;
