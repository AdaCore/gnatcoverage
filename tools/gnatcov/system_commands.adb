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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with GNAT.Expect;
with GNAT.OS_Lib;
with GNAT.Strings; use GNAT.Strings;

with Switches; use Switches;

package body System_Commands is

   --------------------------------
   -- Command formatting helpers --
   --------------------------------

   ----------------
   -- Append_Arg --
   ----------------

   procedure Append_Arg (Cmd : Command_Access; Arg : String) is
   begin
      Cmd.Arguments.Append (+Arg);
   end Append_Arg;

   ----------------
   -- Append_Arg --
   ----------------

   procedure Append_Arg (Cmd : Command_Access; Opt, Arg : String) is
   begin
      Append_Arg (Cmd, Opt);
      Append_Arg (Cmd, Arg);
   end Append_Arg;

   ----------------
   -- Append_Arg --
   ----------------

   procedure Append_Args
     (Cmd : Command_Access;
      Args : String_Vectors.Vector)
   is
   begin
      Cmd.Arguments.Append_Vector (Args);
   end Append_Args;

   -----------
   -- Error --
   -----------

   procedure Error (Msg : String) is
   begin
      Put_Line (Standard_Error, Msg);
      Set_Exit_Status (Failure);
   end Error;

   -----------------
   -- Run_Command --
   -----------------

   procedure Run_Command
     (Command             : Command_Type;
      Origin_Command_Name : String;
      Output_File         : String := "";
      Err_To_Out          : Boolean := True)
   is
      use String_Maps;
      Success : Boolean;
      Prg     : String_Access;
      Args    : String_List (1 .. Natural (Command.Arguments.Length));
      Input   : constant String := +Command.Input;

      Cmd : constant String := +Command.Command;
   begin

      --  Honor a possible empty command text, meaning no actual
      --  command to run.

      if Cmd'Length = 0 then
         return;
      end if;

      --  Find executable

      Prg := GNAT.OS_Lib.Locate_Exec_On_Path (Cmd);
      if Prg = null then
         Error (Origin_Command_Name & ": cannot find "
                & Cmd & " on your path");
         raise Exec_Error;
      end if;

      --  Instantiate the argument list

      declare
         I : Positive := 1;
      begin
         for S of Command.Arguments loop
            Args (I) := new String'(+S);
            I := I + 1;
         end loop;
      end;

      --  Run

      for Env_Var in Command.Environment.Iterate loop
         if Verbose then
            Put_Line ("env: " & (+Key (Env_Var))
                      & "=" & (+Element (Env_Var)));
         end if;
         GNAT.OS_Lib.Setenv (+Key (Env_Var), +Element (Env_Var));
      end loop;

      if Verbose then
         Put ("exec: ");
         Put (Prg.all);
         for S of Command.Arguments loop
            Put (' ');
            Put (+S);
         end loop;
         New_Line;
      end if;

      if Input'Length = 0 then
         if Output_File = "" then
            GNAT.OS_Lib.Spawn (Prg.all, Args, Success);
         else
            declare
               Return_Code : Integer;
            begin
               GNAT.OS_Lib.Spawn (Program_Name => Prg.all,
                                  Args         => Args,
                                  Output_File  => Output_File,
                                  Success      => Success,
                                  Return_Code  => Return_Code,
                                  Err_To_Out   => Err_To_Out);
            end;
         end if;
      else
         declare
            File     : File_Type;
            Status   : aliased Integer;
            Status_A : constant access Integer := Status'Access;
            Res      : constant String :=
              GNAT.Expect.Get_Command_Output (Prg.all, Args, Input, Status_A,
                                             Err_To_Out => True);
         begin
            if Output_File /= "" then
               Create (File => File,
                       Mode => Out_File,
                       Name => Output_File);
               Put (File, Res);
               Close (File);
            end if;
            Success := Status = 0;
         end;
      end if;

      if not Success then
         if Verbose then
            Error (Origin_Command_Name & " failed");
         end if;
         raise Exec_Error;
      end if;

      if Verbose then
         Put_Line (Cmd & " finished");
      end if;
   end Run_Command;

end System_Commands;
