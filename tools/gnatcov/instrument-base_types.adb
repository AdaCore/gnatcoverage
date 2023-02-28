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

with Ada.Containers;          use Ada.Containers;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded.Equal_Case_Insensitive;
with Ada.Strings.Unbounded.Less_Case_Insensitive;

with Interfaces; use Interfaces;

with GNATCOLL.VFS;

with Hex_Images; use Hex_Images;

package body Instrument.Base_Types is

   -------------------
   -- Language_Kind --
   -------------------

   function Language_Kind (Language : Some_Language) return Any_Language_Kind
   is
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
         if Length (Result) > 0 then
            Append (Result, ".");
         end if;
         Append (Result, To_String (Id));
      end loop;

      return +Result;
   end To_Ada;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Compilation_Unit_Name) return Boolean is
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
                           if not Equal_Case_Insensitive (Left_Id, Right_Id)
                           then
                              return Less_Case_Insensitive (Left_Id, Right_Id);
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
               if Equal_Case_Insensitive
                 (Left.Project_Name, Right.Project_Name)
               then
                  return Left.Filename < Right.Filename;
               else
                  return Less_Case_Insensitive
                    (Left.Project_Name, Right.Project_Name);
               end if;
         end case;
      else
         return Left.Language_Kind < Right.Language_Kind;
      end if;
   end "<";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Compilation_Unit_Name) return Boolean
   is
      use Ada_Identifier_Vectors;
   begin
      if Left.Language_Kind = Right.Language_Kind then
         case Left.Language_Kind is
            when Unit_Based_Language =>
               if Left.Part = Right.Part
                  and then Left.Unit.Length = Right.Unit.Length
               then
                  for I in 1 .. Integer (Left.Unit.Length) loop
                     if not Equal_Case_Insensitive
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
               return Left.Filename = Right.Filename
                 and then Equal_Case_Insensitive
                   (Left.Project_Name, Right.Project_Name);
         end case;
      else
         return False;
      end if;
   end "=";

   -------------------------
   -- Qualified_Name_Slug --
   -------------------------

   function Qualified_Name_Slug (Name : Ada_Qualified_Name) return String
   is
      First  : Boolean := True;
      Result : Ada_Identifier;
   begin
      --  Create a unique slug from the qualified name: replace occurences of
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
      return To_String (Result);
   end Qualified_Name_Slug;

   ----------------------------
   -- Instrumented_Unit_Slug --
   ----------------------------

   function Instrumented_Unit_Slug
     (Instrumented_Unit : Compilation_Unit_Name) return String
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
            declare
               Result : Ada_Identifier;
            begin
               --  For a compilation unit in a file-based language, relying on
               --  the filename only is not enough, as there can be multiple
               --  sources with the same name belonging to different projects
               --  in a project tree. To avoid name clashes, prepend the name
               --  of the owning project to the computed slug.

               Append
                 (Result,
                  Qualified_Name_Slug
                    (To_Qualified_Name (+Instrumented_Unit.Project_Name)));

               --  Add an unambiguous separator between the project name and
               --  the rest of the slug.

               Append (Result, "_z_z");

               --  File names can contain characters that cannot appear in
               --  identifiers. Furthermore, unlike for the identifier to
               --  return, file names may be case sensitive. In order to
               --  produce valid identifiers, encode everything that isn't a
               --  lower case letter or a digit.

               for C of "+" (Instrumented_Unit.Filename) loop
                  if C in 'a' .. 'z' | '0' .. '9' then
                     Append (Result, C);
                  else
                     Append
                       (Result,
                        "_" & Hex_Image (Unsigned_8'(Character'Pos (C))));
                  end if;
               end loop;

               return To_String (Result);
            end;
      end case;
   end Instrumented_Unit_Slug;

   -----------
   -- Image --
   -----------

   function Image (CU_Name : Compilation_Unit_Name) return String is
   begin
      case CU_Name.Language_Kind is
         when Unit_Based_Language =>
            return To_Ada (CU_Name.Unit)
              & " "
              & (case CU_Name.Part is
                    when Unit_Spec     => "spec",
                    when Unit_Body     => "body",
                    when Unit_Separate => "subunit");
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
         if Length (Result) > 0 then
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
      Part : Unit_Parts) return Compilation_Unit_Name
   is
   begin
      return (Unit_Based_Language, Unit, Part);
   end CU_Name_For_Unit;

   -----------------------------
   -- CU_Name_For_File --
   -----------------------------

   function CU_Name_For_File
     (Filename     : Unbounded_String;
      Project_Name : Unbounded_String) return Compilation_Unit_Name
   is
   begin
      return (File_Based_Language, Filename, Project_Name);
   end CU_Name_For_File;

   ------------------------------
   -- To_Compilation_Unit_Name --
   ------------------------------

   function To_Compilation_Unit_Name
     (Source_File : GNATCOLL.Projects.File_Info) return Compilation_Unit_Name
   is
      use GNATCOLL.VFS;
   begin
      case Language_Kind (To_Language (Source_File.Language)) is
         when Unit_Based_Language =>
            return CU_Name_For_Unit
              (Unit => To_Qualified_Name (Source_File.Unit_Name),
               Part => Source_File.Unit_Part);
         when File_Based_Language =>
            return CU_Name_For_File
              (Filename     => +GNATCOLL.VFS."+" (Source_File.File.Base_Name),
               Project_Name => +Source_File.Project.Name);
      end case;
   end To_Compilation_Unit_Name;

   -----------------
   -- To_Filename --
   -----------------

   function To_Filename
     (Project  : Project_Type;
      CU_Name  : Compilation_Unit_Name;
      Language : Any_Language) return String
   is
      use GNATCOLL.VFS;

   begin
      case CU_Name.Language_Kind is
         when Unit_Based_Language =>
            return +Project.File_From_Unit
              (Unit_Name       => To_Ada (CU_Name.Unit),
               Part            => CU_Name.Part,
               Language        => Image (Language),
               File_Must_Exist => False);
         when File_Based_Language =>
            return +CU_Name.Filename;
      end case;
   end To_Filename;

   ----------------------------
   -- Find_Instrumented_Unit --
   ----------------------------

   function Find_Instrumented_Unit
     (CU_Name : Compilation_Unit_Name) return CU_Id
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

end Instrument.Base_Types;
