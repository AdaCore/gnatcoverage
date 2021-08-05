------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2021, AdaCore                     --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;

with Inputs;   use Inputs;
with Outputs;  use Outputs;
with Project;  use Project;
with Switches; use Switches;

package body Switches is

   use Command_Line.Parser;

   function Command_Line_Args return String_List_Access;
   --  Return a dynamically alocated list of arguments to hold arguments from
   --  Ada.Command_Line.

   function Parse
     (Argv         : GNAT.Strings.String_List_Access;
      With_Command : Command_Type := No_Command;
      Callback     : access procedure (Result : in out Parsed_Arguments;
                                       Ref    : Option_Reference) := null)
      return Parsed_Arguments;
   --  Parse Args using Arg_Parser. Deallocate Args before returning. If there
   --  is an error, call Fatal_Error with the error message.

   procedure Load_Target_Option (Default_Target : Boolean);
   --  Split the --target option into its family name (Target_Family) and the
   --  board name (Target_Board), if any.
   --
   --  If Default_Target is True and the target option is not present or empty,
   --  use the native target. The target has the following format:
   --  FAMILY[,BOARD]. In this case, the returned Target_Family is never null.
   --  Otherwise, leave it unmodified.
   --
   --  In any case, the returned Target_Board may be null.

   procedure Load_Project_Arguments (From_Driver : Boolean);
   --  Load the project, if any, specified in Args, get the command-line
   --  arguments it may specify in its Coverage package corresponding to
   --  Args.Command. Then decode them and merge them with Args into Args
   --  itself.
   --
   --  Note that this also writes Args.String_Args (Opt_Target) (same for
   --  Opt_Runtime) if they are not present to mirror the target/RTS used to
   --  load the project file.
   --
   --  From_Driver is passed as Project.Load_Root_Project's From_Driver
   --  argument.

   --------------
   -- Copy_Arg --
   --------------

   procedure Copy_Arg
     (Option   : String_Options;
      Variable : out String_Access)
   is
      Opt : String_Option renames Args.String_Args (Option);
   begin
      if Opt.Present then
         Variable := new String'(+Opt.Value);
      end if;
   end Copy_Arg;

   -------------------
   -- Copy_Arg_List --
   -------------------

   procedure Copy_Arg_List
     (Option : String_List_Options;
      List   : in out Inputs.Inputs_Type) is
   begin
      for Arg of Args.String_List_Args (Option) loop
         Inputs.Add_Input (List, +Arg);
      end loop;
   end Copy_Arg_List;

   -----------------------
   -- Command_Line_Args --
   -----------------------

   function Command_Line_Args return String_List_Access is
      Result : constant String_List_Access :=
        new String_List (1 .. Ada.Command_Line.Argument_Count);
   begin
      for I in Result'Range loop
         Result (I) := new String'(Ada.Command_Line.Argument (I));
      end loop;
      return Result;
   end Command_Line_Args;

   ------------------------
   -- Load_Target_Option --
   ------------------------

   procedure Load_Target_Option (Default_Target : Boolean) is
      Target_Arg : String_Option renames Args.String_Args (Opt_Target);
   begin
      if not Default_Target and then not Target_Arg.Present then

         --  We have no target information and we are asked not to use a
         --  default one: do nothing.

         return;
      end if;

      declare
         Real_Target : constant String :=
           (if Target_Arg.Present
            then +Target_Arg.Value
            else Standard'Target_Name);
      begin
         --  If we find a comma, then we have both a target family and a board
         --  name.

         for I in Real_Target'Range loop
            if Real_Target (I) = ',' then
               Target_Family := new String'
                 (Real_Target (Real_Target'First .. I - 1));
               Target_Board  := new String'
                 (Real_Target (I + 1 .. Real_Target'Last));
               return;
            end if;
         end loop;

         --  Otherwise, it's just a family

         Target_Family := new String'(Real_Target);
         Target_Board := null;
      end;
   end Load_Target_Option;

   ----------------------------
   -- Load_Project_Arguments --
   ----------------------------

   procedure Load_Project_Arguments (From_Driver : Boolean) is

      procedure Check_Allowed_Option (Result : in out Parsed_Arguments;
                                      Ref    : Option_Reference);
      --  Put an error message in Result if Ref is an option that is forbidden
      --  in project files.

      --------------------------
      -- Check_Allowed_Option --
      --------------------------

      procedure Check_Allowed_Option (Result : in out Parsed_Arguments;
                                      Ref    : Option_Reference)
      is
         Complain : Boolean := False;
      begin
         case Ref.Kind is
            when String_Opt =>
               Complain := Ref.String_Option in
                 Opt_Project | Opt_Target | Opt_Runtime | Opt_Subdirs;
            when others =>
               null;
         end case;
         if Complain then
            Result.Error := +(Option_Name (Arg_Parser, Ref)
                              & " may not be specified in a project.");
         end if;
      end Check_Allowed_Option;

      Project_Args : Parsed_Arguments;

   --  Start of processing for Load_Project_Arguments

   begin
      if not Args.String_Args (Opt_Project).Present then
         return;
      end if;

      --  In order to load the project file we need to set:
      --    * scenario variables;
      --    * the object subdir;
      --    * the target architecture;
      --    * the runtime system (RTS);
      --    * the requested list of projects of interest (if any);
      --    * the requested list of units of interest (if any);
      --    * whether to process recursively the project tree.

      Root_Project := new String'(+Args.String_Args (Opt_Project).Value);

      for S_Var of Args.String_List_Args (Opt_Scenario_Var) loop
         --  Get name and value from "-X<name>=<value>"

         declare
            Str                    : constant String := +S_Var;
            Name_Last, Value_First : Natural;
         begin
            Name_Last := Str'First - 1;
            while Name_Last < Str'Last
              and then Str (Name_Last + 1) /= '='
            loop
               Name_Last := Name_Last + 1;
            end loop;

            Value_First := Name_Last + 2;

            S_Variables.Include
              (Str (Str'First .. Name_Last),
               Str (Value_First .. Str'Last));
         end;
      end loop;

      if Args.String_Args (Opt_Subdirs).Present then
         Set_Subdirs (+Args.String_Args (Opt_Subdirs).Value);
      end if;

      if Args.Bool_Args (Opt_Externally_Built_Projects) then
         Enable_Externally_Built_Projects_Processing;
      end if;

      --  If the project file does not define a target, loading it needs the
      --  target information: load it here. Likewise for the runtime system.

      Load_Target_Option (Default_Target => False);
      Copy_Arg (Opt_Runtime, Runtime);

      if Args.String_Args (Opt_Config).Present
           and then
         (Args.String_Args (Opt_Target).Present
          or else Args.String_Args (Opt_Runtime).Present)
      then
         Fatal_Error ("--config cannot be used with --target and --RTS");
      end if;
      Copy_Arg (Opt_Config, CGPR_File);

      --  Communicate to our project handling code the list of project files to
      --  consider.

      for Arg of Args.String_List_Args (Opt_Projects) loop
         Project.Add_Project (+Arg);
      end loop;

      Switches.Recursive_Projects := not Args.Bool_Args (Opt_No_Subprojects);
      Copy_Arg_List (Opt_Units, Units_Inputs);

      if Is_Present
        (Args, Option_Reference'(String_List_Opt, Opt_Enabled_Languages))
      then
         declare
            Temp_List : Inputs.Inputs_Type;

            procedure Add_Language (L : String);
            --  Add a lower-cased language to Enabled_Languages

            ------------------
            -- Add_Language --
            ------------------

            procedure Add_Language (L : String) is
            begin
               String_Sets.Insert (Enabled_Languages, +(To_Lower (L)));
            end Add_Language;
         begin
            Copy_Arg_List (Opt_Enabled_Languages, Temp_List);
            Iterate (Temp_List, Add_Language'Access);
         end;
      else
         String_Sets.Insert (Enabled_Languages, +"ada");
      end if;

      --  All -X command line switches have now been processed: initialize the
      --  project subsystem and load the root project.

      Load_Root_Project
        (Root_Project.all, Target_Family, Runtime, CGPR_File, Units_Inputs,
         From_Driver);

      --  Get common and command-specific switches, decode them (if any) and
      --  store the result in Project_Args, then merge it into Args.

      declare
         Command_Name     : constant String :=
           Parser.Command_Name (Arg_Parser, Args.Command);
         Common_Switches  : constant String_List_Access :=
           Project.Switches ("*");
         Command_Switches : constant String_List_Access :=
           Project.Switches (Command_Name);
      begin
         if Common_Switches /= null then
            Project_Args := Parse
              (Common_Switches,
               With_Command => Args.Command,
               Callback     => Check_Allowed_Option'Access);
         end if;

         if Command_Switches /= null then
            Merge
              (Project_Args,
               Parse
                 (Command_Switches,
                  With_Command => Args.Command,
                  Callback     => Check_Allowed_Option'Access));
         end if;

         --  Project_Args have precedence over Args, so merge in Project_Args
         --  first.

         Merge (Project_Args, Args);
         Args := Project_Args;
      end;

      --  Set default output directory, target and runtime from the project

      if not Args.String_Args (Opt_Output_Directory).Present then
         Args.String_Args (Opt_Output_Directory) :=
           (Present => True, Value => +Project.Output_Dir);
      end if;

      if not Args.String_Args (Opt_Target).Present
        and then Project.Target /= ""
      then
         Args.String_Args (Opt_Target) :=
           (Present => True, Value => +Project.Target);
      end if;

      if not Args.String_Args (Opt_Runtime).Present
         and then Project.Runtime /= ""
      then
         Args.String_Args (Opt_Runtime) :=
           (Present => True, Value => +Project.Runtime);
      end if;
   end Load_Project_Arguments;

   -----------
   -- Parse --
   -----------

   function Parse
     (Argv         : GNAT.Strings.String_List_Access;
      With_Command : Command_Type := No_Command;
      Callback     : access procedure (Result : in out Parsed_Arguments;
                                       Ref    : Option_Reference) := null)
      return Parsed_Arguments
   is
      Args_Var : GNAT.Strings.String_List_Access := Argv;
      Result   : constant Parsed_Arguments :=
        Parse (Arg_Parser, Argv, With_Command, Callback);
      Error    : constant String := +Result.Error;
   begin
      Free (Args_Var);
      if Error'Length /= 0 then
         Args.Command := Result.Command;
         Fatal_Error_With_Usage (Error);
      end if;
      return Result;
   end Parse;

   ---------------------
   -- Parse_Arguments --
   ---------------------

   procedure Parse_Arguments (From_Driver : Boolean) is
   begin
      --  Require at least one argument

      if Ada.Command_Line.Argument_Count = 0 then
         Print_Usage (Arg_Parser, False, False);
         Normal_Exit;
      end if;

      --  Parse actual command-line arguments, then load the project, which may
      --  contain additional arguments.

      Args := Parse (Command_Line_Args);
      Load_Project_Arguments (From_Driver);

      --  Loading the project may have set a new target/RTS: update our
      --  internal state accordingly.

      Load_Target_Option (Default_Target => True);
      Copy_Arg (Opt_Runtime, Runtime);
      Copy_Arg (Opt_Config, CGPR_File);
   end Parse_Arguments;

end Switches;
