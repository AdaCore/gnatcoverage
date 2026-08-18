------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2026, AdaCore                       --
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

with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with Outputs;      use Outputs;
with Paths;        use Paths;
with PECoff_Files; use PECoff_Files;
with Subprocesses; use Subprocesses;

package body Shared_Lib_Deps is

   ----------------------
   -- Dump_Shared_Libs --
   ----------------------

   procedure Dump_Shared_Libs
     (Exe_Inputs : String_Vectors.Vector; Output : String_Access)
   is
      Result : File_Sets.Set;
   begin
      for F of Exe_Inputs loop
         Result.Union (Imported_Shared_Libs (+F));
      end loop;

      if Output = null then
         for Filename of Result loop
            Put_Line (+Filename.Full_Name);
         end loop;
      else
         declare
            F : File_Type;
         begin
            Create (F, Out_File, Output.all);
            for Filename of Result loop
               Put_Line (F, +Filename.Full_Name);
            end loop;
            Close (F);
         end;
      end if;
   end Dump_Shared_Libs;

   --------------------------
   -- Imported_Shared_Libs --
   --------------------------

   function Imported_Shared_Libs (Filename : String) return File_Sets.Set is
   begin
      --  Use our own PE file parser to extract DLL dependencies from Windows
      --  executables.

      declare
         Fd    : File_Descriptor;
         Is_PE : Boolean;
      begin
         Fd := Open_Read (Filename, Binary);
         Is_PE := Fd /= Invalid_FD and then Is_PE_File (Fd);
         Close (Fd);
         if Is_PE then
            return Imported_DLLs (Filename);
         end if;
      end;

      --  For all other executables, delegate this to the system ldd.

      declare
         Temp_Fd       : File_Descriptor;
         Temp_File     : File_Type;
         Temp_Filename : String_Access;

         Ldd_Args : String_Vectors.Vector;
         Success  : Boolean;

         Result : File_Sets.Set;
      begin
         Create_Temp_File (Temp_Fd, Temp_Filename);
         if Temp_Fd = Invalid_FD then
            Fatal_Error ("Could not create a temporary file");
         end if;
         Close (Temp_Fd);

         Ldd_Args.Append (+Filename);
         Success :=
           Run_Command
             (Command             => +"ldd",
              Arguments           => Ldd_Args,
              Origin_Command_Name => "gnatcov dmp-shared-lib-deps",
              Output_File         => Temp_Filename.all,
              Ignore_Error        => True);

         Open (Temp_File, In_File, Temp_Filename.all);
         Free (Temp_Filename);
         if not Success then
            if Ada.Strings.Fixed.Trim (Get_Line (Temp_File), Ada.Strings.Both)
              /= "not a dynamic executable"
            then
               --  The executable does not depend on any dynamic library,
               --  nothing to do here.

               Close (Temp_File);
               return Result;
            end if;

            --  Otherwise, something went wrong in the ldd call, abort.

            Close (Temp_File);
            Fatal_Error
              ("Error while running LDD on the instrumented executable");
         end if;

         --  Inspect the output of lld to find the dependencies

         while not End_Of_File (Temp_File) loop
            declare
               use Ada.Strings.Fixed;
               Line         : constant String := Get_Line (Temp_File);
               Arrow_Index  : constant Natural := Index (Line, "=>");
               Filename_End : constant Natural :=
                 Index (Line, " ", Line'Last, Going => Ada.Strings.Backward)
                 - 1;

               Lib_File : Virtual_File := No_File;
            begin
               --  The format of the output of ldd is:
               --
               --  <lib_relative_name>
               --     (=> <lib_fullname> (<load address>))?
               --
               --  We use the fullname, when available, otherwise we use the
               --  relative name. If the library could not be find through its
               --  relative name, then we skip it.

               if Arrow_Index /= 0 then
                  declare
                     Lib_Filename : constant String :=
                       Line (Arrow_Index + 3 .. Filename_End);
                  begin
                     --  If the library is not on the PATH/LD_LIBRARY_PATH, it
                     --  will be displayed as:
                     --
                     --  <lib_basename> => not found

                     if Line (Arrow_Index + 3 .. Line'Last) = "not found" then
                        Warn_Not_Found
                          (Line (Line'First + 1 .. Arrow_Index - 2));
                     else
                        Lib_File := GNATCOLL.VFS.Create (+Lib_Filename);
                     end if;
                  end;
               else
                  declare
                     Lib_Filename : constant String :=
                       Line (Strings.Index_Non_Blank (Line) .. Filename_End);
                  begin
                     Lib_File := GNATCOLL.VFS.Create (+Lib_Filename);
                  end;
               end if;

               --  Check that the library relative filename / fullname exists.
               --  It sometimes does not, e.g. when loading a kernel system,
               --  specified by its basename only. For instance:
               --
               --  linux-vdso.so.1 (0x00007ffe40383000)
               --
               --  or when the dynamic library is specified as such:
               --
               --  <lib_basename> => (<load_address>)

               if GNATCOLL.VFS.Is_Regular_File (Lib_File) then
                  Result.Include (Lib_File);
               end if;
            end;
         end loop;
         Close (Temp_File);
         return Result;
      end;
   end Imported_Shared_Libs;

   --------------------
   -- Warn_Not_Found --
   --------------------

   procedure Warn_Not_Found (Filename : String) is
      Path : constant String :=
        (if On_Windows then "PATH" else "LD_LIBRARY_PATH");
   begin
      Warn
        ("Could not find library "
         & Filename
         & ". Add its directory to the "
         & Path
         & " if this is an instrumented library.");
   end Warn_Not_Found;

end Shared_Lib_Deps;
