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

with Command_Line;   use Command_Line;
with Files_Handling; use Files_Handling;
with Hex_Images;     use Hex_Images;

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
      return (case Language is
              when Ada_Language => Unit_Based_Language,
              when C_Language
                 | CPP_Language => File_Based_Language);
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

   procedure Read
     (CLS   : in out Checkpoints.Checkpoint_Load_State;
      Value : out Compilation_Unit_Part)
   is
      CUP : Compilation_Unit_Part (CLS.Read_Language_Kind);
   begin
      case CUP.Language_Kind is
         when Unit_Based_Language =>
            Read (CLS, CUP.Unit);
            CUP.Part := Unit_Parts'Val (CLS.Read_U8);

         when File_Based_Language =>
            CUP.Filename := CLS.Read_Unbounded_String;
      end case;

      Value := CUP;
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

   procedure Write
     (CSS   : in out Checkpoints.Checkpoint_Save_State;
      Value : Compilation_Unit_Part) is
   begin
      CSS.Write (Value.Language_Kind);
      case Value.Language_Kind is
         when Unit_Based_Language =>
            Write (CSS, Value.Unit);
            CSS.Write_U8 (Unit_Parts'Pos (Value.Part));

         when File_Based_Language =>
            CSS.Write (Value.Filename);
      end case;
   end Write;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Compilation_Unit_Part) return Boolean is
   begin
      if Left.Language_Kind = Right.Language_Kind then
         case Left.Language_Kind is
            when Unit_Based_Language =>
               if Left.Part = Right.Part then
                  if Left.Unit.Length = Right.Unit.Length then
                     for I in 1 .. Integer (Left.Unit.Length) loop
                        declare
                           Left_Id  : constant Unbounded_String :=
                             Unbounded_String (Left.Unit.Element (I));
                           Right_Id : constant Unbounded_String :=
                             Unbounded_String (Right.Unit.Element (I));
                        begin
                           if not Strings.Equal_Case_Insensitive
                                    (Left_Id, Right_Id)
                           then
                              return Strings.Less_Case_Insensitive
                                       (Left_Id, Right_Id);
                           end if;
                        end;
                     end loop;

                     --  If we get there, they are equal

                     return False;
                  else
                     return Left.Unit.Length < Right.Unit.Length;
                  end if;
               else
                  return Left.Part < Right.Part;
               end if;
            when File_Based_Language =>
               return Left.Filename < Right.Filename;
         end case;
      else
         return Left.Language_Kind < Right.Language_Kind;
      end if;
   end "<";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Compilation_Unit_Part) return Boolean
   is
      use Ada_Identifier_Vectors;
   begin
      if Left.Language_Kind = Right.Language_Kind then
         case Left.Language_Kind is
            when Unit_Based_Language =>
               if Left.Part = Right.Part
                  and then Length (Left.Unit) = Length (Right.Unit)
               then
                  for I in 1 .. Integer (Length (Left.Unit)) loop
                     if not Strings.Equal_Case_Insensitive
                       (Unbounded_String (Left.Unit.Element (I)),
                        Unbounded_String (Right.Unit.Element (I)))
                     then
                        return False;
                     end if;
                  end loop;

                  --  If we get there, they are equal

                  return True;
               end if;
               return False;
            when File_Based_Language =>
               return Left.Filename = Right.Filename;
         end case;
      else
         return False;
      end if;
   end "=";
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

   ----------------------------
   -- Instrumented_Unit_Slug --
   ----------------------------

   function Instrumented_Unit_Slug
     (Instrumented_Unit : Compilation_Unit_Part) return String
   is
   begin
      case Instrumented_Unit.Language_Kind is
         when Unit_Based_Language =>
            declare
               Result : Ada_Identifier;
            begin
               --  Add a single letter so that the spec and body of the same
               --  unit don't conflict.

               Append (Result, Part_Tags (Instrumented_Unit.Part) & '_');

               --  Append a unique suffix corresponding to the qualified name
               --  of the unit to instrument.

               Append (Result, Qualified_Name_Slug (Instrumented_Unit.Unit));
               return To_String (Result);
            end;

         when File_Based_Language =>
            return Filename_Slug (+Instrumented_Unit.Filename);
      end case;
   end Instrumented_Unit_Slug;

   -------------------
   -- Filename_Slug --
   -------------------

   function Filename_Slug
     (Fullname : String;
      Use_Hash : Boolean := not Switches.Use_Full_Slugs) return String
   is
      use Ada.Directories;
      Result : Ada_Identifier;

      Full_Name_Hash : constant String := Hash_32_Image (Fullname);
   begin
      if Use_Hash then

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
               "_" & Hex_Image
                 (Interfaces.Unsigned_8'(Character'Pos (C))));
         end if;
      end loop;

      --  Then, suffix with the hash

      Append (Result, Full_Name_Hash);
      return To_String (Result);
   end Filename_Slug;

   -----------
   -- Image --
   -----------

   function Image (CU_Name : Compilation_Unit_Part) return String is
   begin
      case CU_Name.Language_Kind is
         when Unit_Based_Language =>
            return To_Ada (CU_Name.Unit)
              & " "
              & (case CU_Name.Part is
                    when GNATCOLL.Projects.Unit_Spec     => "spec",
                    when GNATCOLL.Projects.Unit_Body     => "body",
                    when GNATCOLL.Projects.Unit_Separate => "subunit");
         when File_Based_Language =>
            return +CU_Name.Filename;
      end case;
   end Image;

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

   ----------------------
   -- CU_Name_For_Unit --
   ----------------------

   function CU_Name_For_Unit
     (Unit : Ada_Qualified_Name;
      Part : Unit_Parts) return Compilation_Unit_Part
   is
   begin
      return (Unit_Based_Language, Unit, Part);
   end CU_Name_For_Unit;

   -----------------------------
   -- CU_Name_For_File --
   -----------------------------

   function CU_Name_For_File
     (Filename : Unbounded_String) return Compilation_Unit_Part is
   begin
      return (File_Based_Language, Filename);
   end CU_Name_For_File;

   ------------------------------
   -- To_Compilation_Unit_Name --
   ------------------------------

   function To_Compilation_Unit_Name
     (Source_File : GNATCOLL.Projects.File_Info) return Compilation_Unit_Part
   is
   begin
      case Language_Kind (To_Language (Source_File.Language)) is
         when Unit_Based_Language =>
            return CU_Name_For_Unit
              (Unit => To_Qualified_Name (Source_File.Unit_Name),
               Part => Source_File.Unit_Part);
         when File_Based_Language =>
            return CU_Name_For_File
              (Filename => +(+Source_File.File.Full_Name));
      end case;
   end To_Compilation_Unit_Name;

   ----------------------------
   -- Find_Instrumented_Unit --
   ----------------------------

   function Find_Instrumented_Unit
     (CU_Name : Compilation_Unit_Part) return CU_Id
   is
      use Instrumented_Unit_To_CU_Maps;

      Position : constant Cursor := Instrumented_Unit_CUs.Find (CU_Name);
   begin
      if Has_Element (Position) then
         return Element (Position);
      else
         return No_CU_Id;
      end if;
   end Find_Instrumented_Unit;

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
      Fill_If_Present (Opt_Spec_Suffix, Result.Spec_Suffix (Language));
      Fill_If_Present (Opt_Body_Suffix, Result.Body_Suffix (Language));
      Fill_If_Present (Opt_Project_Name, Prj_Name);
      Fill_If_Present (Opt_Dot_Replacement, Result.Dot_Replacement);

      Result.Prj_Name := To_Qualified_Name (+Prj_Name);

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
      --  Pass the right body / spec suffixes

      if Desc.Body_Suffix (Lang) /= "" then
         Result.Append (+"--body-suffix");
         Result.Append (Desc.Body_Suffix (Lang));
      end if;

      if Desc.Spec_Suffix (Lang) /= "" then
         Result.Append (+"--spec-suffix");
         Result.Append (Desc.Spec_Suffix (Lang));
      end if;

      if Desc.Dot_Replacement /= "" then
         Result.Append (+"--dot-replacement");
         Result.Append (Desc.Dot_Replacement);
      end if;

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
               when C_Language =>
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

      return Result;
   end Unparse;

   -------------------------
   -- Instrumentation_Tag --
   -------------------------

   function Instrumentation_Tag return String
   is
      Time : constant Unsigned_64 :=
        Unsigned_64
          (GNAT.OS_Lib.To_C (GNAT.OS_Lib.Current_Time));
      Tag  : constant String :=
        Hex_Images.Strip_Zero_Padding
          (Hex_Images.Hex_Image (Time));
      --  Tag for the current instrumentation run. Passed on to instrument-main
      --  invocations, to have the same tag for mains instrumented at the
      --  same time.
   begin
      return Tag;
   end Instrumentation_Tag;

end Instrument;
