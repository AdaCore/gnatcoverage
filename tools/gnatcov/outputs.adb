------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2024, AdaCore                     --
--                                                                          --
-- GNATcoverage is free software; you can redistribute it and/or modify it  --
-- under terms of the GNU General Public License as published by the  Free  --
-- Software  Foundation;  either version 3,  or (at your option) any later  --
-- version. This software is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;

with GNAT.Exception_Actions;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with Command_Line;
with Support_Files;
with Switches;
with Version;

package body Outputs is

   Report_Output_Dir : String_Access := null;
   --  Name of the output directory. The reports will be generated
   --  in this directory.

   --------------------------------------------------------
   -- Saved information to contexualize internal crashes --
   --------------------------------------------------------

   --  The following data structures keep track of the "context" information to
   --  provide to users in case of internal error. The goal is to maintain
   --  relevant information that the Print_Internal_Error procedure knows what
   --  gnatcov was doing when a crash occurs.
   --
   --  For convenience, we maintain a stack of context information: each time
   --  gnatcov starts something new, we push info, and each time it ends, we
   --  pop that information. This nicely maps to gnatcov's recursive
   --  processings.
   --
   --  To avoid constantly deallocating/reallocating data structures, we have
   --  two stacks: one that contains the currently active stack of context
   --  information, and another one of free'd entries, available for the next
   --  context entry to push.

   type Context_Entry;
   type Context_Entry_Access is access Context_Entry;
   type Context_Entry is record
      Info : Unbounded_String;
      Next : Context_Entry_Access;
   end record;

   Current_Context : Context_Entry_Access;
   --  Access to the current context information (that Print_Internal_Error
   --  must process), or null if there is no context for now.
   --  Current_Context.Next refers to the context that was active when
   --  Current_Context was pushed.

   Freed_Context : Context_Entry_Access;
   --  Linked list (through Context_Entry.Next) of already allocated and
   --  available Context_Entry records.

   -------------------------
   --  Create_Output_File --
   -------------------------

   procedure Create_Output_File (File : out File_Type; File_Name : String) is
      Full_Path_Name : constant String :=
        Get_Output_Dir & Directory_Separator & File_Name;
   begin
      Create (File, Out_File, Full_Path_Name);
   exception
      when E : Name_Error =>
         Error ("failed to create output file " & Full_Path_Name & ":");
         Error (Exception_Information (E));
         raise;
   end Create_Output_File;

   ---------------
   -- Clean_Dir --
   ---------------

   procedure Clean_Dir
     (Dir           : String;
      Pattern       : String;
      Ignored_Files : String_Sets.Set := String_Sets.Empty_Set)
   is
      use Ada.Directories;

      --  Removing items in a directory and iterate on these items at the same
      --  time is not supported: first collect all files to remove (iteration)
      --  and then remove them.

      To_Delete : String_Vectors.Vector;
      Search    : Search_Type;
      Dir_Entry : Directory_Entry_Type;
   begin
      --  Nothing to do if Dir does not exist or is not a directory

      if not Ada.Directories.Exists (Dir) or else Kind (Dir) /= Directory then
         return;
      end if;

      --  Collect the files to delete

      Start_Search
        (Search,
         Directory => Dir,
         Pattern   => Pattern,
         Filter    => (Ordinary_File => True, others => False));
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Dir_Entry);
         declare
            Name      : constant String := Simple_Name (Dir_Entry);
            Full_Name : constant Unbounded_String := +(Dir / Name);
         begin
            if not Ignored_Files.Contains (Full_Name) then
               To_Delete.Append (Full_Name);
            end if;
         end;
      end loop;
      End_Search (Search);

      --  Do the deletion

      for Name of To_Delete loop
         Delete_File (+Name);
      end loop;
   end Clean_Dir;

   -----------
   -- Error --
   -----------

   procedure Error (Msg : String) is
   begin
      Warning_Or_Error (Support_Files.Gnatcov_Command_Name & ": " & Msg);
      Set_Exit_Status (Failure);
   end Error;

   ------------------
   --  Fatal_Error --
   ------------------

   procedure Fatal_Error (Msg : String) is
   begin
      Error (Msg);
      raise Xcov_Exit_Exc;
   end Fatal_Error;

   ----------------------------
   -- Fatal_Error_With_Usage --
   ----------------------------

   procedure Fatal_Error_With_Usage (Msg : String) is
   begin
      Error (Msg);
      Command_Line.Parser.Print_Usage
        (Switches.Arg_Parser, False, True, Switches.Args.Command);
      raise Xcov_Exit_Exc;
   end Fatal_Error_With_Usage;

   ---------------------
   --  Get_Output_Dir --
   ---------------------

   function Get_Output_Dir return String is
   begin
      if Report_Output_Dir = null then
         Set_Output_Dir ("./");
      end if;

      return Report_Output_Dir.all;
   end Get_Output_Dir;

   -----------------
   -- Normal_Exit --
   -----------------

   procedure Normal_Exit is
   begin
      Set_Exit_Status (Success);
      raise Xcov_Exit_Exc;
   end Normal_Exit;

   ----------------------
   -- Register_Warning --
   ----------------------

   procedure Register_Warning is
   begin
      Warnings_Registered := True;
      if Switches.Warnings_As_Errors then
         Set_Exit_Status (Failure);
      end if;
   end Register_Warning;

   --------------------
   -- Create_Context --
   --------------------

   function Create_Context (Message : String) return Context_Handle is
      Ctx : Context_Entry_Access;
   begin
      --  Allocate a Context_Entry record, or use an already allocated one that
      --  is free.

      if Freed_Context = null then
         Ctx := new Context_Entry;
      else
         Ctx := Freed_Context;
         Freed_Context := Ctx.Next;
      end if;

      --  Properly fill it and put it on the current contexts stack

      Ctx.Info := +Message;
      Ctx.Next := Current_Context;
      Current_Context := Ctx;

      return (Ada.Finalization.Limited_Controlled with null record);
   end Create_Context;

   --------------------------
   -- Print_Internal_Error --
   --------------------------

   procedure Print_Internal_Error (Exc : Ada.Exceptions.Exception_Occurrence)
   is
   begin
      New_Line;
      Put_Line ("== gnatcov bug detected ==");
      New_Line;
      Put_Line ("gnatcov just encountered an internal error:");
      Put_Line (Exception_Information (Exc));
      New_Line;
      if Current_Context = null then
         Put_Line ("No coverage processing context information available.");
      else
         Put_Line (+Current_Context.Info);
      end if;
      New_Line;
      Put_Line ("This is gnatcov version " & Version.Xcov_Version);
   end Print_Internal_Error;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (Dummy : in out Context_Handle) is

      --  Pop the Current_Context stack and push the pop'ed item to
      --  Freed_Context.

      Ctx : constant Context_Entry_Access := Current_Context;
   begin
      Current_Context := Ctx.Next;
      Ctx.Next := Freed_Context;
      Freed_Context := Ctx;
   end Finalize;

   ------------------------------
   -- String_To_Internal_Error --
   ------------------------------

   function String_To_Internal_Error
     (Name : String) return Any_Internal_Error_Trigger is
   begin
      --  Look for an enumeration value whose lower-case-with-dashes name
      --  matches Name.

      for Trigger in Any_Internal_Error_Trigger'Range loop
         declare
            Trigger_Name : String :=
              Ada.Characters.Handling.To_Lower (Trigger'Image);
         begin
            for C of Trigger_Name loop
               if C = '_' then
                  C := '-';
               end if;
            end loop;

            if Name = Trigger_Name then
               return Trigger;
            end if;
         end;
      end loop;

      raise Constraint_Error with "no such internal error trigger: " & Name;
   end String_To_Internal_Error;

   -----------------------------------
   -- Raise_Stub_Internal_Error_For --
   -----------------------------------

   procedure Raise_Stub_Internal_Error_For
     (Trigger : Any_Internal_Error_Trigger) is
   begin
      if Internal_Error_Trigger = Trigger then
         raise Constraint_Error with Trigger'Image;
      end if;
   end Raise_Stub_Internal_Error_For;

   ------------------------
   -- Output_Dir_Defined --
   ------------------------

   function Output_Dir_Defined return Boolean
   is (Report_Output_Dir /= null);

   ---------------------
   --  Set_Output_Dir --
   ---------------------

   procedure Set_Output_Dir (Output_Dir : String; Subdir : Boolean := False) is
   begin
      Free (Report_Output_Dir);

      if not Is_Directory (Output_Dir) then
         if not Switches.Quiet and then not Subdir then
            Put_Line
              (Standard_Error, "info: creating output path " & Output_Dir);
         end if;
         begin
            Ada.Directories.Create_Path (Output_Dir);
         exception
            when Exc : Name_Error | Use_Error =>
               Fatal_Error
                 ("cannot create output path "
                  & Output_Dir
                  & ": "
                  & Exception_Message (Exc));
         end;
      end if;

      Report_Output_Dir := new String'(Output_Dir);
   end Set_Output_Dir;

   ----------
   -- Warn --
   ----------

   procedure Warn (Msg : String) is
   begin
      Warning_Or_Error ("warning: " & Msg);
   end Warn;

   ----------------------
   -- Warning_Or_Error --
   ----------------------

   procedure Warning_Or_Error (Msg : String) is
   begin
      --  Maybe Msg is an error, maybe it is a warning. In both cases, it is
      --  fine to plan for a failure exit code in warnings-as-errors mode.

      Register_Warning;
      Put_Line (Standard_Error, Msg);
   end Warning_Or_Error;

begin
   Internal_Error_Trigger :=
     String_To_Internal_Error
       (Value ("GNATCOV_INTERNAL_ERROR_TRIGGER", "none"));
   GNAT.Exception_Actions.Register_Global_Unhandled_Action
     (Print_Internal_Error'Access);
end Outputs;
