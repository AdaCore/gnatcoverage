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

--  Source instrumentation

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers;          use Ada.Containers;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Strings.Unbounded.Equal_Case_Insensitive;
with Ada.Strings.Unbounded.Less_Case_Insensitive;
with Ada.Unchecked_Deallocation;

with Interfaces; use Interfaces;
with GNAT.OS_Lib;

with GNATCOLL.JSON; use GNATCOLL.JSON;
with GNATCOLL.VFS;  use GNATCOLL.VFS;

with Checkpoints;
with Coverage;
with Files_Table;
with Hex_Images;        use Hex_Images;
with Instrument.Ada_Unit;
with Instrument.Clean_Objdirs;
with Instrument.C;
with Instrument.Common; use Instrument.Common;
with JSON;              use JSON;
with Outputs;
with Paths;             use Paths;
with Project;

package body Instrument is

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
      Language : Any_Language) return String is
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

   package GPR renames GNATCOLL.Projects;

   type CU_Name_With_Ignore is record
      Name    : Compilation_Unit_Name;
      Ignored : Boolean;
   end record;

   function "<" (Left, Right : CU_Name_With_Ignore) return Boolean
   is (if Left.Name = Right.Name
       then Left.Ignored < Right.Ignored
       else Left.Name < Right.Name);

   package CU_Name_Vectors is new Ada.Containers.Vectors
     (Positive, CU_Name_With_Ignore);

   type Library_Unit_Info is record
      CU_Names : CU_Name_Vectors.Vector;
      --  List of compilation units implementing this library unit

      Body_Project, Spec_Project : GPR.Project_Type;
      --  Projects that own the body/spec for this library unit

      Language_Kind : Any_Language_Kind;
      --  Higher level representation of a language (unit-based or file-based)

      Language : Any_Language;
      --  Actual language representation
   end record;
   type Library_Unit_Info_Access is access Library_Unit_Info;

   type Ignored_Unit_Info is record
      Filename : Unbounded_String;
      --  Name of the source file for this unit

      Prj_Info : Project_Info_Access;
      --  Reference to the Project_Info record corresponding to the project
      --  that owns the source file for this unit.
   end record;

   type Ignored_Unit_Info_Access is access all Ignored_Unit_Info;

   package Ignored_Units_Maps is new Ada.Containers.Ordered_Maps
     (Compilation_Unit_Name, Ignored_Unit_Info_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Ignored_Unit_Info, Ignored_Unit_Info_Access);

   package Library_Unit_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Library_Unit_Info_Access);
   --  Map to associate a list of compilation units to instrument to a library
   --  unit (indexed by the library unit name, i.e. the unit name or the
   --  full name depending on the language kind for the library unit).
   --
   --  For file-based languages, the library unit only has one compilation
   --  unit associated to it (that is the library unit itself, for which the
   --  name is the actual base filename).

   procedure Get_Or_Create
     (Map          : in out Library_Unit_Maps.Map;
      Library_Unit : String;
      Info         : out Library_Unit_Info_Access);
   --  Look for the info corresponding to Library_Unit in Map. Create it if
   --  it does not exist yet and put it in Info.

   function SID_Filename
     (Cur            : Library_Unit_Maps.Cursor;
      In_Library_Dir : Boolean) return String;
   --  Return the filename of the SID file to create for the given library
   --  unit. If In_Library_Dir is true, then return a filename located in the
   --  project library directory. Otherwise, the filename is located in the
   --  object directory.

   procedure Prepare_Output_Dirs (IC : Inst_Context);
   --  Make sure we have the expected tree of directories for the
   --  instrumentation output.

   -------------------
   -- Get_Or_Create --
   -------------------

   procedure Get_Or_Create
     (Map          : in out Library_Unit_Maps.Map;
      Library_Unit : String;
      Info         : out Library_Unit_Info_Access)
   is
      use Library_Unit_Maps;
      Cur : constant Cursor := Map.Find (Library_Unit);
   begin
      if Has_Element (Cur) then
         Info := Element (Cur);
      else
         Info := new Library_Unit_Info;
         Info.Body_Project := GPR.No_Project;
         Info.Spec_Project := GPR.No_Project;
         Map.Insert (Library_Unit, Info);
      end if;
   end Get_Or_Create;

   ------------------
   -- SID_Filename --
   ------------------

   function SID_Filename
     (Cur            : Library_Unit_Maps.Cursor;
      In_Library_Dir : Boolean) return String
   is
      use Library_Unit_Maps;

      LU_Name : constant String := Key (Cur);
      Info    : Library_Unit_Info renames Element (Cur).all;

      --  Determine in which project we will put this SID file, and the
      --  basename for the SID file to create. Mimic how GNAT creates ALI
      --  files: use the project of the main source of the library unit, start
      --  from the basename of that source file, replace the last extension
      --  with ".sid". Also make sure to use the most extending project in the
      --  hierarchy, which is where GPRbuild puts ALI/object files.

      SID_Basename : US.Unbounded_String;

      Use_Spec : constant Boolean :=
        Info.Body_Project = GPR.No_Project;
      Project  : constant GPR.Project_Type :=
        GPR.Extending_Project
          (Project => (if Use_Spec
                       then Info.Spec_Project
                       else Info.Body_Project),
           Recurse => True);
      pragma Assert (Project /= GPR.No_Project);

      Output_Directory : constant Virtual_File :=
        (if In_Library_Dir
         then Project.Library_Ali_Directory
         else Project.Object_Dir);
   begin
      case Info.Language_Kind is
         when Unit_Based_Language =>
            declare
               Src_Basename  : constant String := +Project.File_From_Unit
                 (Unit_Name       => LU_Name,
                  Part            => (if Use_Spec
                                      then Unit_Spec
                                      else Unit_Body),
                  Language        =>  Image (Info.Language),
                  File_Must_Exist => False);
               Src_Ext_Index : constant Positive :=
                 Ada.Strings.Fixed.Index
                   (Src_Basename, ".", Ada.Strings.Backward);
            begin
               SID_Basename :=
                 +(Src_Basename (Src_Basename'First ..  Src_Ext_Index)
                   & "sid");
            end;
         when File_Based_Language =>
            SID_Basename := +(Ada.Directories.Simple_Name (LU_Name & ".sid"));
      end case;

      return String'(+Output_Directory.Full_Name) / (+SID_Basename);
   end SID_Filename;

   -------------------------
   -- Prepare_Output_Dirs --
   -------------------------

   procedure Prepare_Output_Dirs (IC : Inst_Context) is
      use Project_Info_Maps;
   begin
      for Cur in IC.Project_Info_Map.Iterate loop
         declare
            Prj_Info   : Project_Info renames Element (Cur).all;
            Output_Dir : constant String :=
               +(Element (Cur).Output_Dir);
         begin
            --  Do not create output directories for externally built projects:
            --  we don't instrument them and we may not have filesystem
            --  permissions to create directories there.

            if not Prj_Info.Externally_Built
               and then not Ada.Directories.Exists (Output_Dir)
            then
               Ada.Directories.Create_Path (Output_Dir);
            end if;
         end;
      end loop;
   end Prepare_Output_Dirs;

   ----------------------------------
   -- Instrument_Units_Of_Interest --
   ----------------------------------

   procedure Instrument_Units_Of_Interest
     (Dump_Config          : Any_Dump_Config;
      Language_Version     : Any_Language_Version;
      Ignored_Source_Files : access GNAT.Regexp.Regexp;
      Mains                : String_Vectors.Vector)
   is
      use String_Vectors;

      --  Initialize all the instrumenters

      Ada_Instrumenter : aliased Instrument.Ada_Unit.Ada_Instrumenter_Type :=
        Instrument.Ada_Unit.Create_Ada_Instrumenter (Language_Version);
      C_Instrumenter   : aliased Instrument.C.C_Instrumenter_Type :=
        (null record);
      CPP_Instrumenter : aliased Instrument.C.CPP_Instrumenter_Type :=
        (null record);

      Instrumenters    : constant array (Src_Supported_Language)
        of access Language_Instrumenter'Class :=
          (Ada_Language => Ada_Instrumenter'Access,
           C_Language   => C_Instrumenter'Access,
           CPP_Language => CPP_Instrumenter'Access);

      --  Create a map from library units to lists of compilation units to
      --  instrument for them.

      LU_Map          : Library_Unit_Maps.Map;
      Current_LU_Info : Library_Unit_Info_Access;
      Ignored_Units   : Ignored_Units_Maps.Map;

      Mains_To_Instrument : array (Src_Supported_Language)
                            of Main_To_Instrument_Vectors.Vector;
      --  For each supported language, list of mains to instrument. Always
      --  empty when Dump_Config.Trigger is Manual.

      --  Create the instrumenter context

      IC : Inst_Context := Create_Context (Ignored_Source_Files);

      Root_Project_Info : constant Project_Info_Access :=
         Get_Or_Create_Project_Info (IC, Project.Project.Root_Project);

      procedure Add_Instrumented_Unit_Wrapper
        (Project : GPR.Project_Type; Source_File : GPR.File_Info);
      --  Add this source file to the list of units to instrument. For unit-
      --  based languages, also add subunits that depend on this source file.

      -----------------------------------
      -- Add_Instrumented_Unit_Wrapper --
      -----------------------------------

      procedure Add_Instrumented_Unit_Wrapper
        (Project : GPR.Project_Type; Source_File : GPR.File_Info)
      is
         procedure Add_Instrumented_Unit
           (CU_Name : Compilation_Unit_Name; Info : GPR.File_Info);
         --  Wrapper for Instrument.Common.Add_Instrumented_Unit

         ---------------------------
         -- Add_Instrumented_Unit --
         ---------------------------

         procedure Add_Instrumented_Unit
           (CU_Name : Compilation_Unit_Name; Info : GPR.File_Info)
         is
            Should_Ignore : constant Boolean :=
              Is_Ignored_Source_File (IC, +Info.File.Base_Name);
         begin
            Current_LU_Info.CU_Names.Append
              (CU_Name_With_Ignore'(Name    => CU_Name,
                                    Ignored => Should_Ignore));
            if Should_Ignore then
               Ignored_Units.Insert
                 (CU_Name,
                  new Ignored_Unit_Info'
                    (Filename => To_Unbounded_String (+Info.File.Base_Name),
                     Prj_Info => Get_Or_Create_Project_Info
                       (IC, Info.Project)));
            else
               Add_Instrumented_Unit (IC, Info.Project, Info);
            end if;
         end Add_Instrumented_Unit;

         Language : constant Src_Supported_Language :=
           To_Language (Source_File.Language);

      --  Start of processing for Add_Instrumented_Unit_Wrapper

      begin
         --  Skip this source file if the instrumenter requires it

         if Instrumenters (Language).Skip_Source_File (Source_File) then
            return;
         end if;

         declare
            CU_Name : constant Compilation_Unit_Name :=
               To_Compilation_Unit_Name (Source_File);

            Unit_Name : constant String :=
              (case CU_Name.Language_Kind is
                  when Unit_Based_Language => To_Ada (CU_Name.Unit),

                  --  For file-based languages, we need to use the full
                  --  name to account for homonyms.

                  when File_Based_Language => +Source_File.File.Full_Name);

         begin
            --  Get the vector in which we will record the compilation units
            --  that the following call to Find_Units will list.

            Get_Or_Create (LU_Map, Unit_Name, Current_LU_Info);

            --  Keep track of projects that own this library unit's source
            --  files for its spec/body.

            case Source_File.Unit_Part is
               when GPR.Unit_Body => Current_LU_Info.Body_Project := Project;
               when GPR.Unit_Spec => Current_LU_Info.Spec_Project := Project;

               --  Subunits cannot be units of interest, so Enumerate_Sources
               --  should not be able to call Find_Units_Wrapper with a
               --  subunit.  Hence, the following should be unreachable.

               when GPR.Unit_Separate =>
                  raise Program_Error with "unreachable code";
            end case;

            Current_LU_Info.Language_Kind := CU_Name.Language_Kind;
            Current_LU_Info.Language := To_Language (Source_File.Language);

            case CU_Name.Language_Kind is
               when Unit_Based_Language =>
                  Instrument.Ada_Unit.Find_Ada_Units
                    (Ada_Instrumenter,
                     CU_Name,
                     Source_File,
                     Add_Instrumented_Unit'Access);
               when File_Based_Language =>
                  Add_Instrumented_Unit (CU_Name, Source_File);
            end case;
         end;
      end Add_Instrumented_Unit_Wrapper;

   --  Start of processing for Instrument_Units_Of_Interest

   begin
      --  Delete output directories from previous instrumentations

      Clean_Objdirs;

      --  First get the list of all units of interest

      for Lang in Src_Supported_Language loop
         if Src_Enabled_Languages (Lang) then
            Project.Enumerate_Sources
              (Add_Instrumented_Unit_Wrapper'Access, Lang);
         end if;
      end loop;

      --  If we need to instrument all the mains, also go through them now, so
      --  that we can prepare output directories for their projects later on.
      --  Note that for user convenience, we want to do this for all the
      --  languages that gnatcov supports, even those that are not considered
      --  for coverage analysis.

      if Dump_Config.Trigger /= Manual then

         --  If no source file was specified on the command line to be a main,
         --  use the list of mains specified in project files.

         if Mains.Is_Empty then
            for Lang in Src_Supported_Language loop
               for Main of Project.Enumerate_Mains (Lang) loop
                  Register_Main_To_Instrument
                    (IC, Mains_To_Instrument (Lang), Main.File, Main.Project);
               end loop;
            end loop;

         --  Otherwise, make sure we can find the source file of each main in
         --  the project tree and that we can instrument them (supported
         --  language).

         else
            for Filename of Mains loop
               declare
                  F       : constant String := +Filename;
                  Info    : constant File_Info :=
                    Project.Project.Root_Project.Create_From_Project (+F);
                  File    : constant Virtual_File := Info.File;
                  Project : constant Project_Type := Info.Project;
                  Lang    : Any_Language;
               begin
                  if File = No_File or else Project = No_Project then
                     Outputs.Fatal_Error ("No such source file: " & F);
                  end if;

                  Lang := To_Language_Or_All (Info.Language);
                  if Lang not in Src_Supported_Language then
                     Outputs.Fatal_Error
                       ("Cannot instrument main source file (unsupported"
                        & " language): " & F);
                  end if;

                  Register_Main_To_Instrument
                    (IC, Mains_To_Instrument (Lang), File, Project);
               end;
            end loop;
         end if;
      end if;

      --  Know that we know all the sources we need to instrument, prepare
      --  output directories.

      Prepare_Output_Dirs (IC);

      --  For each library unit...

      for Cur in LU_Map.Iterate loop

         --  Instrument compilation units (only the ones this library unit
         --  owns). Sort them first, so that the instrumentation order is
         --  deterministic.

         declare
            package Sorting is new CU_Name_Vectors.Generic_Sorting;

            LU_Info              : constant Library_Unit_Info_Access :=
               Library_Unit_Maps.Element (Cur);
            All_Externally_Built : Boolean := True;
         begin
            Sorting.Sort (LU_Info.CU_Names);
            for CU of LU_Info.CU_Names loop
               if CU.Ignored then
                  declare
                     Unit_Info : Ignored_Unit_Info renames
                       Ignored_Units.Element (CU.Name).all;
                     Filename  : constant String := To_String
                       (Unit_Info.Filename);
                  begin
                     --  Simply add the unit to the file table if it is not
                     --  externally built.

                     if not Unit_Info.Prj_Info.Externally_Built then
                        All_Externally_Built := False;
                        Files_Table.Consolidate_Ignore_Status
                          (Index  => Files_Table.Get_Index_From_Generic_Name
                             (Name => Filename,
                              Kind => Files_Table.Source_File),
                           Status => Files_Table.Always);
                     end if;
                  end;
               else
                  declare
                     Unit_Info : Instrumented_Unit_Info renames
                       IC.Instrumented_Units.Element (CU.Name).all;
                     Filename  : constant String :=
                       To_String (Unit_Info.Filename);
                     Basename  : constant String :=
                       Ada.Directories.Simple_Name (Filename);
                  begin
                     --  Do not process units from externally built projects

                     if not Unit_Info.Prj_Info.Externally_Built then

                        --  Keep a note that we are processing at least one
                        --  source file from a non-externally built project.

                        All_Externally_Built := False;

                        --  Run the instrumentation for this file

                        if Verbose then
                           Put_Line ("Instrumenting " & Basename);
                        end if;
                        Instrumenters (Unit_Info.Language).Instrument_Unit
                          (CU.Name, Unit_Info);

                        --  Update the Ignore_Status of the CU we instrumented

                        Files_Table.Consolidate_Ignore_Status
                          (Index  => Files_Table.Get_Index_From_Generic_Name
                             (Name                => Filename,
                              Kind                => Files_Table.Source_File,
                              Indexed_Simple_Name => True),
                           Status => Files_Table.Never);
                     end if;
                  end;
               end if;
            end loop;

            --  Except for units entirely externally built (the spec, the body,
            --  and potential subunits all belong to externally built
            --  projects), emit a SID file to contain mappings between bits in
            --  coverage buffers and SCOs.

            if not All_Externally_Built then
               declare
                  Context : aliased Coverage.Context := Coverage.Get_Context;
                  Obj_SID : constant String :=
                     SID_Filename (Cur, In_Library_Dir => False);
                  Lib_SID : constant String :=
                     SID_Filename (Cur, In_Library_Dir => True);
                  Success : Boolean;
               begin
                  Checkpoints.Checkpoint_Save
                    (Obj_SID,
                     Context'Access,
                     Purpose => Checkpoints.Instrumentation);

                  --  If the object directory is different from the library
                  --  directory, copy the SID file to the library directory.
                  --  This allows "gnatcov coverage" to automatically pick it
                  --  up if the project is later made externally built.

                  if Obj_SID /= Lib_SID then

                     --  Unlike the object directory, which GNATCOLL.Project
                     --  creates automatically, the library directory may not
                     --  exist: create it if needed.

                     begin
                        Create (Create (+Lib_SID).Dir_Name).Make_Dir;
                     exception
                        when Exc : VFS_Directory_Error =>
                           Outputs.Fatal_Error
                             (Ada.Exceptions.Exception_Message (Exc));
                     end;

                     GNAT.OS_Lib.Copy_File
                       (Name     => Obj_SID,
                        Pathname => Lib_SID,
                        Success  => Success,
                        Mode     => GNAT.OS_Lib.Overwrite);
                     if not Success then
                        Outputs.Fatal_Error
                          ("Error while copying " & Obj_SID
                           & " to the library directory: " & Lib_SID);
                     end if;
                  end if;
               end;

               if Switches.Verbose then
                  SC_Obligations.Dump_All_SCOs;
               end if;
            end if;
         end;

         Checkpoints.Checkpoint_Clear;
      end loop;

      if IC.Instrumented_Units.Length = 0 then
         Outputs.Fatal_Error ("No unit to instrument.");
      end if;

      declare
         Instrumented_Units : Instrument.Common.CU_Name_Vectors.Vector;
         --  List of instrumented units

      begin
         for Cur in IC.Instrumented_Units.Iterate loop
            Instrumented_Units.Append (Instrumented_Unit_Maps.Key (Cur));
         end loop;

         --  Emit the unit to contain the list of coverage buffers, exported to
         --  a C symbol, in one of the language supported by the project.
         --
         --  Note that this has an implicit hack to it: if Ada is a language of
         --  the project, it will pick it over the others (as it is the first
         --  enumeration member of the Src_Supported_Language type). This
         --  matters as we make the assumption in the Emit_Dump_Helper_Unit
         --  implementation in instrument-ada_unit.adb (when instrumenting for
         --  an Ada main) that the Ada package for buffers list units always
         --  exists: we need to include it in the main closure, as it puts
         --  buffer units in scope by importing them (otherwise they aren't
         --  as they are used through C symbol importations).

         for Language in Src_Supported_Language loop
            if Project.Project.Root_Project.Has_Language (Image (Language))
            then
               Instrumenters (Language).Emit_Buffers_List_Unit
                 (Root_Project_Info.all, Instrumented_Units);
               exit;
            end if;
         end loop;

         --  Instrument all the mains to add the dump of coverage buffers.
         --  Make sure to pass the instrumented version if the main is a unit
         --  of interest.

         for Language in Src_Supported_Language loop

            for Main of Mains_To_Instrument (Language) loop
               declare
                  Filename : constant String :=
                    (if IC.Instrumented_Units.Contains (Main.CU_Name)
                     then (+Main.Prj_Info.Output_Dir) / (+Main.File.Base_Name)
                     else +Main.File.Full_Name);
               begin
                  Instrumenters (Language).Auto_Dump_Buffers_In_Main
                    (Filename,
                     Instrumented_Units,
                     Dump_Config,
                     Main.Prj_Info.all);
               end;
            end loop;
         end loop;
      end;

      --  Deallocate Ignored_Unit_Infos

      for IU of Ignored_Units loop
         Free (IU);
      end loop;
      Ignored_Units := Ignored_Units_Maps.Empty_Map;

      --  Save the dump trigger+channel information in the root project's
      --  object directory. This allows user scripts to automatically know
      --  where to expect source trace files (dump channel) without inspecting
      --  all inputs (command-line arguments, project file, instrumentation
      --  runtime, etc.) and whether that info is reliable (it is not if the
      --  dump trigger is manual).

      declare
         J        : constant JSON_Value := Create_Object;
         Filename : constant String :=
           Project.Output_Dir & "/gnatcov-instr.json";
      begin
         J.Set_Field ("dump-trigger", Image (Dump_Config.Trigger));
         J.Set_Field ("dump-channel", Image (Dump_Config.Channel));
         Write (Filename, J, Compact => False);
      end;
   end Instrument_Units_Of_Interest;

end Instrument;
