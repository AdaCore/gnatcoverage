------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2012-2014, AdaCore                     --
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

with Text_IO;                   use Text_IO;
with Ada.Command_Line;          use Ada.Command_Line;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

procedure crun is


   Executable_Path : String_Access;

   Success : Boolean;

   CWD : String := Get_Current_Dir;

   Python_P    : String_Access;
   Python_Args : Argument_List (1 .. 2);

   Gnatcov_P   : String_Access;
   Gnatcov_Args : Argument_List (1 .. 11);
   Next_Arg_Idx : Natural;

   Crun_Dir : Dir_Type;
   Isys_Pfile : String (1 .. 15);
   Lasti : Natural;
   Crun_Dir_Name : String := Normalize_Pathname ("..", Command_Name);

   Wspace_Dir_Name : String := Normalize_Pathname ("isyswspace");

   Itmp_Dir_Name : String := Normalize_Pathname ("isystemps", Crun_Dir_Name);
   Temp_Dir : Dir_Type;
   Temp_Name : String (1 .. 80);

   procedure Empty_Temps_Dir;

   procedure Empty_Temps_Dir is
   begin
      Open (Temp_Dir, Itmp_Dir_Name);
      loop
         Read (Temp_Dir, Temp_Name, Lasti);
         exit when Lasti = 0;
         Delete_File
          (Normalize_Pathname (Temp_Name (1 .. Lasti), Itmp_Dir_Name), Success);
      end loop;
      Close (Temp_Dir);
   end Empty_Temps_Dir;

   type Poss_Arg is record
     Arg_Name : String_Access;
     Required : Boolean;
     Seen     : Boolean;
   end record;
   type Poss_Arg_Array is array (Natural range <>) of Poss_Arg;
   Poss_Args : Poss_Arg_Array := (
     (new String'("--target"), True, False),
     (new String'("--output"), False, False),
     (new String'("--tag"), False, False),
     (new String'("--level"),  False, False),
     (new String'("--scos"), False, False));
   Target_Exec_Seen : Boolean := False;

   Argslog_File : File_Type;
   Argslog_Name : String := "runargs.log";
begin
   Create (File => Argslog_File, Name => Argslog_Name);
      Put_Line (Argslog_File, "INCOMING......");
      for J in 1 .. Argument_Count loop
         Put_Line (Argslog_File, Argument (J));
      end loop;
   Close (Argslog_File);

   Executable_Path := new String'(Normalize_Pathname (Argument (2)));

   declare
   begin
      Remove_Dir (Wspace_Dir_Name, Recursive => True);
   exception
      when Directory_Error =>
         null;
   end;
   declare
   begin
      Make_Dir (Wspace_Dir_Name);
   exception
      when Directory_Error =>
         Put_Line (Standard_Error, "Error creating workspace.");
         OS_Exit (1);
   end;
   declare
   begin
      Make_Dir (Itmp_Dir_Name);
   exception
      when Directory_Error =>
         null; -- Okay if temp dir already exists
   end;

   Open (Crun_Dir, Crun_Dir_Name);
   loop
      Read (Crun_Dir, Isys_Pfile, Lasti);
      exit when Lasti = 0;
      if Lasti >= 8 and then Isys_Pfile (1 .. 8) = "justrun." then

         Copy_File (
          Normalize_Pathname (Isys_Pfile (1 .. Lasti), Crun_Dir_Name),
          Normalize_Pathname (Isys_Pfile (1 .. Lasti), Wspace_Dir_Name),
          Success);
      end if;
   end loop;
   CLose (Crun_Dir);

   Python_P := Locate_Exec_On_Path ("python");
   if Python_P = null then
      Put_Line (Standard_Error, "python not found.");
      OS_Exit (1);
   end if;
   Python_Args (1) :=
     new String'(Normalize_Pathname ("get_trace.py", Crun_Dir_Name));

   Python_Args (2) := new String'(Executable_Path.all);

   Open (Argslog_File, Append_File, Argslog_Name);
   New_Line (Argslog_File);
   Put_Line (Argslog_File, "PYTHON ARGS....");
   Put_Line (Argslog_File, Python_Args (1).all);
   Put_Line (Argslog_File, Python_Args (2).all);
   Close (Argslog_File);


   Setenv ("ISYSTEM_TEMP", Itmp_Dir_Name);
   Spawn (Python_P.all, Python_Args, Success);
   if not Success then
      Put_Line (Standard_Error, "winIDEA python script failed.");
      Empty_Temps_Dir;
      OS_Exit (1);
   end if;

   Gnatcov_P := Locate_Exec_On_Path ("gnatcov");
   if Gnatcov_P = null then
      Put_Line (Standard_Error, "gnatcov not found.");
      OS_Exit (1);
   end if;
   Gnatcov_Args (1) := new String'("convert");
   Next_Arg_Idx := 2;
   for J in 1 .. Argument_Count loop
      if Argument (J) (1) /= '-' then
         if Target_Exec_Seen then
            Put_Line (Standard_Error, "Multiple exec args seen.");
            OS_Exit (1);
         else
            Gnatcov_Args (Next_Arg_Idx) :=
              new String'("--exec=" & Argument (J));
            Next_Arg_Idx := Next_Arg_Idx + 1;
            Target_Exec_Seen := True;
         end if;
      else
         for K in Poss_Args'Range loop
            if Argument (J)'Length > Poss_Args (K).Arg_Name'Length + 1
              and then Argument (J) (1 .. Poss_Args (K).Arg_Name'Length)
                = Poss_Args (K).Arg_Name.all
            then
               if Poss_Args (K).Seen then
                  Put_Line (Standard_Error,
                    Poss_Args (K).Arg_Name.all & " seen twice");
                  OS_Exit (1);
               end if;
               if Poss_Args (K).Arg_Name'Length = 8 and then
                 Poss_Args (K).Arg_Name.all = "--target"
               then
                  Gnatcov_Args (Next_Arg_Idx) := new String'(
                    "--trace-source" & Argument (J) (9 .. Argument (J)'Last));
               else
                  Gnatcov_Args (Next_Arg_Idx) := new String'(Argument (J));
               end if;
               Next_Arg_Idx := Next_Arg_Idx + 1;
               Poss_Args (K).Seen := True;
               exit;
            elsif K = Poss_Args'Last then
               Put_Line (Standard_Error,
                 "Argument """ & Argument (J) & """ not recognized.");
               OS_Exit (1);
            end if;
         end loop;
      end if;
   end loop;
   for K in Poss_Args'Range loop
      if Poss_Args (k).Required and then not Poss_Args (K).Seen then
         Put_Line (Standard_Error, Poss_Args (K).Arg_Name.all & " missing.");
         OS_Exit (1);
      end if;
   end loop;
   Gnatcov_Args (Next_Arg_Idx) :=
     new String'("--hw-trigger-traces=IAC1,main,IAC2");
   Next_Arg_Idx := Next_Arg_Idx + 1;
   Gnatcov_Args (Next_Arg_Idx) :=
     new String'("--input=" & Wspace_Dir_Name & "\nexus_trace.bin");

   Open (Argslog_File, Append_File, Argslog_Name);
   New_Line (Argslog_File);
   Put_Line (Argslog_File, "GNATCOV ARGS....");
   for J in 1 .. Next_Arg_Idx loop
      Put_Line (Argslog_File, Gnatcov_Args (J).all);
   end loop;
   Close (Argslog_File);

   Spawn (Gnatcov_P.all, Gnatcov_Args (1 .. Next_Arg_Idx), Success);
   if not Success then
      Put_Line (Standard_Error, "gnatcov convert failed.");
      OS_Exit (1);
   end if;

   Empty_Temps_Dir;

end crun;
