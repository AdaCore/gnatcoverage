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

--  Source instrumentation

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Directories;
with Ada.Strings;
with Ada.Strings.Hash;

with GNAT.OS_Lib;

with Interfaces; use Interfaces;

with GNATCOLL.VFS; use GNATCOLL.VFS;

--  ??? Remove pragma Warnings once eng/toolchain/gnat#1283 is fixed

pragma Warnings (Off, "not referenced");
with GPR2.Project.View.Set;
pragma Warnings (On, "not referenced");

with Command_Line;   use Command_Line;
with Files_Handling; use Files_Handling;
with Hex_Images;     use Hex_Images;
with Outputs;
with Paths;

package body Instrument is

   function Hash_32_Image (S : String) return String
   is (Hex_Image (Unsigned_32 (Ada.Strings.Hash (S))));
   --  Return the 8-bytes image of a hash for S

   -------------------
   -- Language_Kind --
   -------------------

   function Language_Kind
     (Language : Some_Language) return Supported_Language_Kind is
   begin
      return
        (case Language is
           when Ada_Language              => Unit_Based_Language,
           when C_Language | CPP_Language => File_Based_Language);
   end Language_Kind;

   ------------
   -- To_Ada --
   ------------

   function To_Ada (Name : Ada_Qualified_Name) return String is
      Result : Unbounded_String;
   begin
      for Id of Name loop
         if Result /= "" then
            Append (Result, ".");
         end if;
         Append (Result, To_String (Id));
      end loop;

      return +Result;
   end To_Ada;

   ----------
   -- Read --
   ----------

   procedure Read
     (CLS   : in out Checkpoints.Checkpoint_Load_State;
      Value : out Ada_Identifier) is
   begin
      Value := Ada_Identifier (CLS.Read_Unbounded_String);
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (CSS : in out Checkpoints.Checkpoint_Save_State; Value : Ada_Identifier)
   is
   begin
      CSS.Write (Unbounded_String (Value));
   end Write;

   -------------------------
   -- Qualified_Name_Slug --
   -------------------------

   function Qualified_Name_Slug
     (Name     : Ada_Qualified_Name;
      Use_Hash : Boolean := not Switches.Use_Full_Slugs) return String
   is
      First  : Boolean := True;
      Result : Ada_Identifier;
   begin
      --  Create a unique slug from the qualified name: replace occurrences of
      --  'z' with 'zz' and insert '_z_' between identifiers.

      for Id of Name loop
         if First then
            First := False;
         else
            Append (Result, "_z_");
         end if;
         for I in 1 .. Length (Id) loop
            declare
               Char : constant Character := Element (Id, I);
            begin
               if Char in 'Z' | 'z' then
                  Append (Result, "zz");
               else
                  Append (Result, Char);
               end if;
            end;
         end loop;
      end loop;

      if Use_Hash then

         --  Prefix the hash with "z" to ensure the unit name slug doesn't
         --  start with a digit.

         return "z" & Hash_32_Image (To_String (Result));
      else
         return To_String (Result);
      end if;
   end Qualified_Name_Slug;

   -------------------
   -- Filename_Slug --
   -------------------

   function Filename_Slug (Fullname : String) return String is
      use Ada.Directories;
      Result : Ada_Identifier;

      Full_Name_Hash : constant String := Hash_32_Image (Fullname);
   begin
      if not Switches.Use_Full_Slugs then

         --  Prefix the hash with "z_" to ensure the filename slug doesn't
         --  start with a digit.

         return "z" & Full_Name_Hash;
      end if;

      --  We use the basename slug, followed by a hash of the fullname, which
      --  makes us confident that homonym files will be correctly handled.

      --  File names can contain characters that cannot appear in identifiers.
      --  Furthermore, unlike for the identifier to return, file names may
      --  be case sensitive. In order to produce valid identifiers, encode
      --  everything that isn't a lower case letter or a digit.

      --  To avoid a leading underscore, add a prefix

      Append (Result, "z");

      for C of Base_Name (Fullname) loop
         if C in 'a' .. 'z' | '0' .. '9' then
            Append (Result, C);
         else
            Append
              (Result,
               "_" & Hex_Image (Interfaces.Unsigned_8'(Character'Pos (C))));
         end if;
      end loop;

      --  Then, suffix with the hash

      Append (Result, Full_Name_Hash);
      return To_String (Result);
   end Filename_Slug;

   -----------------------
   -- To_Qualified_Name --
   -----------------------

   function To_Qualified_Name (Name : String) return Ada_Qualified_Name is
      First : Positive := Name'First;
      Unit  : Ada_Qualified_Name;
   begin
      --  Split Ada qualified name into its components

      for J in Name'First .. Name'Last + 1 loop
         if J = Name'Last + 1 or else Name (J) = '.' then
            Unit.Append (To_Unbounded_String (Name (First .. J - 1)));
            First := J + 1;
         end if;
      end loop;
      return Unit;
   end To_Qualified_Name;

   ------------------
   -- Canonicalize --
   ------------------

   function Canonicalize (Name : Ada_Qualified_Name) return Ada_Qualified_Name
   is
   begin
      return Result : Ada_Qualified_Name := Name do
         for N of Result loop
            N := To_Unbounded_String (To_Lower (To_String (N)));
         end loop;
      end return;
   end Canonicalize;

   --------------------
   -- To_Symbol_Name --
   --------------------

   function To_Symbol_Name (Name : Ada_Qualified_Name) return String is
      Result : Unbounded_String;
   begin
      for Id of Name loop
         if Result /= "" then
            Append (Result, "_");
         end if;
         Append (Result, To_Lower (To_String (Id)));
      end loop;

      return +Result;
   end To_Symbol_Name;

   ----------------------------
   -- Find_Instrumented_Unit --
   ----------------------------

   function Find_Instrumented_Unit (Filename : Unbounded_String) return CU_Id
   is
      use Instrumented_Unit_To_CU_Maps;

      Position : constant Cursor := Instrumented_Unit_CUs.Find (Filename);
   begin
      if Has_Element (Position) then
         return Element (Position);
      else
         return No_CU_Id;
      end if;
   end Find_Instrumented_Unit;

   ------------------------
   -- Casing_From_String --
   ------------------------

   function Casing_From_String (Value, Context : String) return Casing_Type is
   begin
      if Value = "lowercase" then
         return Lowercase;
      elsif Value = "uppercase" then
         return Uppercase;
      elsif Value = "mixedcase" then
         return Mixedcase;
      else
         Outputs.Fatal_Error ("Invalid casing from " & Context & ": " & Value);
      end if;
   end Casing_From_String;

   ----------------------------
   -- Load_From_Command_Line --
   ----------------------------

   function Load_From_Command_Line return Prj_Desc is
      use Command_Line.Parser;
      Language : constant Some_Language :=
        To_Language (+Args.String_Args (Opt_Lang).Value);
      Prj_Name : Unbounded_String;
      Result   : Prj_Desc;

      procedure Fill_If_Present
        (Opt : String_Options; Field : out Unbounded_String);
      --  If option Opt is present on the command line, copy its valaue to
      --  Field.

      ---------------------
      -- Fill_If_Present --
      ---------------------

      procedure Fill_If_Present
        (Opt : String_Options; Field : out Unbounded_String)
      is
         Opt_Ref : constant Option_Reference := (String_Opt, Opt);
      begin
         if Is_Present (Args, Opt_Ref) then
            Field := +Value (Args, Opt);
         end if;
      end Fill_If_Present;

      --  Start of processing for Load_From_Command_Line

   begin
      if Language in C_Family_Language then
         Fill_If_Present
           (Opt_Compiler_Driver, Result.Compiler_Driver (Language));
      end if;
      Fill_If_Present (Opt_Output_Directory, Result.Output_Dir);
      Fill_If_Present (Opt_Project_Name, Prj_Name);
      Result.Prj_Name := To_Qualified_Name (+Prj_Name);

      declare
         NS     : Naming_Scheme_Desc renames Result.Naming_Scheme;
         Casing : String_Option renames Args.String_Args (Opt_Casing);
      begin
         Fill_If_Present (Opt_Spec_Suffix, NS.Spec_Suffix (Language));
         Fill_If_Present (Opt_Body_Suffix, NS.Body_Suffix (Language));
         Fill_If_Present (Opt_Dot_Replacement, NS.Dot_Replacement);
         if Casing.Present then
            NS.Casing :=
              Casing_From_String (+Casing.Value, "argument --casing");
         else
            NS.Casing := Lowercase;
         end if;
      end;

      for Item of Args.String_List_Args (Opt_Special_Output_Dirs) loop
         declare
            Sep_Index : constant Natural :=
              US.Index (Item, (1 => Paths.Path_Separator));
         begin
            Result.Special_Output_Dirs.Insert
              (US.Unbounded_Slice (Item, 1, Sep_Index - 1),
               US.Unbounded_Slice (Item, Sep_Index + 1, US.Length (Item)));
         end;
      end loop;

      --  Compiler options are loaded through the --c/c++-opts switch

      return Result;
   end Load_From_Command_Line;

   -------------
   -- Unparse --
   -------------

   function Unparse
     (Desc      : Prj_Desc;
      Unit_Name : Unbounded_String;
      Lang      : Src_Supported_Language) return String_Vectors.Vector
   is
      Result        : String_Vectors.Vector;
      Compiler_Opts : String_Vectors.Vector;
   begin
      --  Pass naming scheme settings

      declare
         NS : Naming_Scheme_Desc renames Desc.Naming_Scheme;
      begin
         if NS.Body_Suffix (Lang) /= "" then
            Result.Append (+"--body-suffix");
            Result.Append (NS.Body_Suffix (Lang));
         end if;

         if NS.Spec_Suffix (Lang) /= "" then
            Result.Append (+"--spec-suffix");
            Result.Append (NS.Spec_Suffix (Lang));
         end if;

         if NS.Dot_Replacement /= "" then
            Result.Append (+"--dot-replacement");
            Result.Append (NS.Dot_Replacement);
         end if;

         Result.Append (+"--casing");
         Result.Append (+To_Lower (NS.Casing'Image));
      end;

      if Lang in C_Family_Language then
         Compiler_Opts.Append (Desc.Search_Paths);
         declare
            File : constant Virtual_File := Create_Normalized (+Unit_Name);
         begin
            if Desc.Compiler_Options_Unit.Contains (File) then
               Compiler_Opts.Append
                 (Desc.Compiler_Options_Unit.Element (File));
            else
               Compiler_Opts.Append (Desc.Compiler_Options (Lang));
            end if;
         end;
         if not Compiler_Opts.Is_Empty then
            case C_Family_Language (Lang) is
               when CPP_Language =>
                  Result.Append (+"--c++-opts");

               when C_Language   =>
                  Result.Append (+"--c-opts");
            end case;
         end if;
         declare
            First             : Boolean := True;
            Compiler_Opts_Str : Unbounded_String;
         begin
            for Compiler_Opt of Compiler_Opts loop
               if First then
                  First := False;
               else
                  Append (Compiler_Opts_Str, ",");
               end if;
               Append (Compiler_Opts_Str, Compiler_Opt);
            end loop;
            Result.Append (Compiler_Opts_Str);
         end;

         if Desc.Compiler_Driver (Lang) /= "" then
            Result.Append (+"--compiler-driver");
            Result.Append (Desc.Compiler_Driver (Lang));
         end if;
      end if;

      Result.Append (+"--output-dir");
      Result.Append (Desc.Output_Dir);

      for Cur in Desc.Special_Output_Dirs.Iterate loop
         declare
            Source_File : constant Unbounded_String := String_Maps.Key (Cur);
            Output_Dir  : constant Unbounded_String :=
              String_Maps.Element (Cur);
         begin
            Result.Append
              (+("--special-output-dir="
                 & (+Source_File)
                 & Paths.Path_Separator
                 & (+Output_Dir)));
         end;
      end loop;

      return Result;
   end Unparse;

   -------------------------
   -- Instrumentation_Tag --
   -------------------------

   function Instrumentation_Tag return String is
      Time : constant Unsigned_64 :=
        Unsigned_64 (GNAT.OS_Lib.To_C (GNAT.OS_Lib.Current_Time));
      Tag  : constant String :=
        Hex_Images.Strip_Zero_Padding (Hex_Images.Hex_Image (Time));
      --  Tag for the current instrumentation run. Passed on to instrument-main
      --  invocations, to have the same tag for mains instrumented at the
      --  same time.
   begin
      return Tag;
   end Instrumentation_Tag;

end Instrument;
