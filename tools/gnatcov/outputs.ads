------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2022, AdaCore                     --
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

--  This packages provides a set of routines to manage xcov's outputs
--  (error messages and annotated reports).

private with Ada.Finalization;
with Ada.Text_IO; use Ada.Text_IO;

package Outputs is

   ------------
   -- Errors --
   ------------

   Xcov_Exit_Exc : exception;
   --  Cause Xcov to terminate. Do not use in an explicit raise; call
   --  Fatal_Error or Normal_Exit instead.

   procedure Warn (Msg : String);
   --  Display Msg on standard error prefixed with "warning: "

   procedure Error (Msg : String);
   --  Display Msg on standard error and set exit status to failure

   procedure Warning_Or_Error (Msg : String);
   --  Display Msg on standard error

   procedure Fatal_Error (Msg : String);
   pragma No_Return (Fatal_Error);
   --  Same as Error, and cause Xcov to terminate by raising Fatal_Error_Exc

   procedure Fatal_Error_With_Usage (Msg : String);
   --  Same as Fatal_Error, but print the command-line usage before terminating

   procedure Normal_Exit;
   --  Cause Xcov to terminate. exit status OK

   ---------------------
   -- Internal errors --
   ---------------------

   --  What we call here internal errors are the unexpected exceptions raised
   --  during gnatcov's execution. Such exceptions are, by nature, unhandled,
   --  and thus lead to a crash. The goal of the following API is to provide
   --  users as much precise context information as possible for them to
   --  have an idea about what caused the crash, either for debugging purposes,
   --  or to ease the search for a workaround.
   --
   --  The way this works is simple: gnatcov code calls the Create_* functions
   --  below to create Context_Handle objects. These objects store context
   --  information during their lifetime (info is discarded when the object is
   --  finalized). This package registers a callback for unhandled exceptions
   --  through the GNAT.Exception_Actions runtime unit so that we print the
   --  last context info created for still living Context_Handle objects (i.e.
   --  the most recent/relevant info).

   type Context_Handle is limited private;
   --  Object to host context information to contextualize internal errors.
   --  That information is discarded when such objects are finalized.

   function Create_Context (Message : String) return Context_Handle;
   --  Create context information for internal errors. Intended use:
   --
   --     declare
   --        Dummy_Ctx : constant Context_Handle :=
   --          Create_Context ("Processing foobar");
   --     begin
   --        ... process foobar...
   --     end;

   type Any_Internal_Error_Trigger is (
      None,
      --  Do not create an artificial internal error

      Arguments_Loading,
      --  Raise an error after Argparse's work and before loading arguments

      Ada_Instrument_Start_File,
      --  Raise an error when starting the instrumentation of a source file

      Ada_Instrument_Null_Proc,
      --  Raise an error when instrumenting a null procedure

      Ada_Instrument_Insert_Stmt_Witness
      --  Raise an error when inserting a witness call for a statement
   );

   Internal_Error_Trigger : Any_Internal_Error_Trigger := None;
   --  Active trigger for the artificial internal error. Set at elaboration
   --  from the GNATCOV_INTERNAL_ERROR_TRIGGER environment variable.

   function String_To_Internal_Error
     (Name : String) return Any_Internal_Error_Trigger;
   --  Return the internal error trigger corresponding to Name. Raise a
   --  Constraint_Error if Name does not denote a trigger.
   --
   --  Trigger names are the same as enumeration values, but lower case and
   --  with underscores replaced with dashes.

   procedure Raise_Stub_Internal_Error_For
     (Trigger : Any_Internal_Error_Trigger);
   --  If Internal_Error_Trigger = Trigger, raise a Constraint_Error

   -----------------------
   -- Output management --
   -----------------------

   function Output_Dir_Defined return Boolean;
   --  True if the output directory has been set

   procedure Set_Output_Dir (Output_Dir : String);
   --  Set the path to the directory where report files will be stored.
   --  This procedure shall be called before any use of Create_Output_File
   --  (presumably when parsing xcov's options). The default output dir is
   --  the root project's object directory if using a project, or the current
   --  directory if not.

   function Get_Output_Dir return String;
   --  Return the output dir. If Report_Output_Dir is null, initialize it
   --  to current dir.

   procedure Create_Output_File
     (File      : out File_Type;
      File_Name : String);
   --  Create a file whose name is File_Name and located in output dir, as
   --  specified by Set_Output_Dir; then open it for write access and
   --  return its file descriptor.
   --  If output dir has not been initialized by Set_Output_Dir, it is
   --  set to the current directory.

private

   type Context_Handle is
      new Ada.Finalization.Limited_Controlled with null record;

   overriding procedure Finalize (Dummy : in out Context_Handle);

end Outputs;
