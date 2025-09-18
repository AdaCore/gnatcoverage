------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2019-2024, AdaCore                     --
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

with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

with GNAT.OS_Lib;

with Outputs;

package body Text_Files is

   -------------
   -- Is_Open --
   -------------

   function Is_Open (Self : File_Type) return Boolean is
   begin
      return Is_Open (Self.File);
   end Is_Open;

   ----------
   -- Mode --
   ----------

   function Mode (Self : File_Type) return File_Mode is
   begin
      return Mode (Self.File);
   end Mode;

   ----------
   -- Open --
   ----------

   function Open
     (Self : in out File_Type;
      Name : String;
      Mode : File_Mode := Ada.Text_IO.In_File) return Boolean is
   begin
      Open (Self.File, Mode, Name);
      Self.Filename := +Name;
      return True;
   exception
      when Use_Error | Name_Error =>
         return False;
   end Open;

   ------------
   -- Create --
   ------------

   function Create
     (Self : in out File_Type;
      Name : String;
      Mode : File_Mode := Ada.Text_IO.Out_File) return Boolean is
   begin
      Create (Self.File, Mode, Name);
      Self.Filename := +Name;
      return True;
   exception
      when Use_Error | Name_Error =>
         return False;
   end Create;

   ----------
   -- Open --
   ----------

   procedure Open
     (Self : in out File_Type;
      Name : String;
      Mode : File_Mode := Ada.Text_IO.In_File) is
   begin
      if not Self.Open (Name, Mode) then
         Outputs.Fatal_Error ("cannot open " & Name);
      end if;
   end Open;

   ------------
   -- Create --
   ------------

   procedure Create
     (Self : in out File_Type;
      Name : String;
      Mode : File_Mode := Ada.Text_IO.Out_File) is
   begin
      if not Self.Create (Name, Mode) then
         Outputs.Fatal_Error ("cannot open " & Name);
      end if;
   end Create;

   ---------
   -- Put --
   ---------

   procedure Put (Self : in out File_Type; Item : String) is
   begin
      Put (Self.File, Item);
   end Put;

   procedure Put (Self : in out File_Type; Item : Unbounded_String) is
   begin
      Put (Self.File, Item);
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Self : in out File_Type; Item : String) is
   begin
      Put_Line (Self.File, Item);
   end Put_Line;

   procedure Put_Line (Self : in out File_Type; Item : Unbounded_String) is
   begin
      Put_Line (Self.File, Item);
   end Put_Line;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (Self : in out File_Type) is
   begin
      New_Line (Self.File);
   end New_Line;

   -----------
   -- Close --
   -----------

   procedure Close (Self : in out File_Type) is
   begin
      Close (Self.File);
   end Close;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (Self : in out File_Type) is
   begin
      if Self.Is_Open then
         Close (Self.File);
      end if;
   end Finalize;

   ----------------
   -- Run_GNATpp --
   ----------------

   procedure Run_GNATpp (Filename : String) is
      use GNAT.OS_Lib;

      Args    : constant Argument_List := (1 => Filename'Unrestricted_Access);
      GNATpp  : String_Access := Locate_Exec_On_Path ("gnatpp");
      Success : Boolean;
   begin
      if GNATpp = null then
         Put_Line ("gnatpp not available");
         return;
      end if;
      Spawn (GNATpp.all, Args, Success);
      Free (GNATpp);
      if not Success then
         Outputs.Warn ("pretty-printing " & Filename & " failed!");
      end if;
   end Run_GNATpp;

   ----------------------
   -- Run_Clang_Format --
   ----------------------

   procedure Run_Clang_Format (Filename : String) is
      use GNAT.OS_Lib;

      Inplace      : String := "-i";
      Args         : constant Argument_List :=
        (1 => Inplace'Unrestricted_Access, 2 => Filename'Unrestricted_Access);
      Clang_Format : String_Access := Locate_Exec_On_Path ("clang-format");
      Success      : Boolean;
   begin
      if Clang_Format = null then
         Put_Line ("clang-format not available");
         return;
      end if;
      Spawn (Clang_Format.all, Args, Success);
      Free (Clang_Format);
      if not Success then
         Put_Line ("pretty-printing " & Filename & " failed!");
      end if;
   end Run_Clang_Format;

end Text_Files;
