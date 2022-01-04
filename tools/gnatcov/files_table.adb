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

with Ada.Containers.Hashed_Maps;
with Ada.Characters.Handling;
with Ada.Containers.Ordered_Sets;
with Ada.Directories;
with Ada.Streams;
with Ada.Unchecked_Deallocation;

with System;
with System.Address_To_Access_Conversions;

with GNAT.OS_Lib;
with GNAT.Regpat;
with Osint;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with Checkpoints;   use Checkpoints;
with Coverage;      use Coverage;
with Outputs;
with Paths;         use Paths;
with Perf_Counters; use Perf_Counters;
with Project;
with Switches;

package body Files_Table is

   procedure Free is new Ada.Unchecked_Deallocation
     (Object_Coverage_Info_Array, Object_Coverage_Info_Array_Acc);
   procedure Free is new Ada.Unchecked_Deallocation
     (SCO_Id_Array, SCO_Id_Array_Acc);
   procedure Free is new Ada.Unchecked_Deallocation
     (Sloc_To_SCO_Map_Array, Sloc_To_SCO_Map_Array_Acc);
   procedure Free is new Ada.Unchecked_Deallocation
     (Message_Array, Message_Array_Acc);
   procedure Free is new Ada.Unchecked_Deallocation
     (File_Info, File_Info_Access);

   package File_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Valid_Source_File_Index,
      Element_Type => File_Info_Access);

   Files_Table : File_Vectors.Vector;

   Files_Table_Frozen : Boolean := False;
   --  Whether Files_Table is frozen. When it's frozen, we can compute
   --  Unique_Name fields for its elements and we can build Sorted_Files_Table.
   --  It is invalid to add files to the table after this is set to True.

   Empty_Sloc_To_SCO_Map : aliased constant Sloc_To_SCO_Maps.Map :=
      Sloc_To_SCO_Maps.Empty_Map;

   type Sorted_File_Index is new Valid_Source_File_Index;

   package File_Index_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Sorted_File_Index,
      Element_Type => Valid_Source_File_Index);

   Sorted_Files_Table : File_Index_Vectors.Vector;
   --  Sorted list of indexes corresponding to Files_Table entries. Files are
   --  sorted by simple name first, then by full name. Computed when freezing
   --  Files_Table.

   procedure Freeze_Files_Table;
   --  Freeze the files table and compute Unique_Name fields for file entries

   function Create_File_Info
     (Kind                   : File_Kind;
      Full_Name, Simple_Name : String_Access;
      Indexed_Simple_Name    : Boolean)
      return File_Info_Access;
   --  Allocate a new File_Info record of type Kind and for the given file
   --  names. This does not modify Files_Table nor full/simple name maps.

   function Kind_Name (Kind : File_Kind) return String is
     (case Kind is
      when Stub_File    => "stub file",
      when Source_File  => "source file",
      when Library_File => "library file");

   procedure Expand_Line_Table (FI : File_Info_Access; Line : Positive);

   package Filename_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Virtual_File,
      Element_Type    => Source_File_Index,
      Hash            => Full_Name_Hash,
      Equivalent_Keys => "=",
      "="             => "=");

   package Simple_Name_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Virtual_File,
      Element_Type    => Source_File_Index,
      Hash            => Full_Name_Hash,
      Equivalent_Keys => "=",
      "="             => "=");

   package Filename_Rebase_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Virtual_File,
      Element_Type    => Virtual_File,
      Hash            => Full_Name_Hash,
      Equivalent_Keys => "=",
      "="             => "=");

   Simple_Name_Map : Simple_Name_Maps.Map;
   Full_Name_Map   : Filename_Maps.Map;

   Renaming_Map : Filename_Rebase_Maps.Map;
   --  Cache for source rebasing/source search. When a file is successfully
   --  rebased/seached, an entry will be added to this map with the original
   --  full path as the key and the rebase full path as the element. This
   --  limits the number of call to Locate_Source when querying mutliple times
   --  the file index for one given file.

   Current_File_Line_Cache : File_Info_Access := null;
   --  Current file whose lines are cached in the file table. There is
   --  only one entry in the cache for now, which is reasonable for the
   --  use that we make: the line content is mostly needed by package
   --  Annotations, which only manipulates one source file at a time.

   --  Source rebase/search types

   type Source_Search_Entry;
   type Source_Search_Entry_Acc is access Source_Search_Entry;
   type Source_Search_Entry is record
      Prefix : String_Access;
      Next : Source_Search_Entry_Acc;
   end record;

   type Pattern_Matcher_Acc is access all GNAT.Regpat.Pattern_Matcher;

   type Source_Rebase_Entry;
   type Source_Rebase_Entry_Acc is access Source_Rebase_Entry;
   type Source_Rebase_Entry is record
      Old_Prefix : Pattern_Matcher_Acc;
      New_Prefix : String_Access;
      Next : Source_Rebase_Entry_Acc;
   end record;

   First_Source_Rebase_Entry : Source_Rebase_Entry_Acc := null;
   Last_Source_Rebase_Entry  : Source_Rebase_Entry_Acc := null;

   First_Source_Search_Entry : Source_Search_Entry_Acc := null;
   Last_Source_Search_Entry  : Source_Search_Entry_Acc := null;

   function Use_Renaming_Map return Boolean
   is (First_Source_Rebase_Entry /= null
       or else First_Source_Search_Entry /= null);
   --  Whether we should even try to rebase files using Renaming_Map or not

   function Locate_Source (File : Virtual_File) return Virtual_File;
   --  Try to find the actual location on disk of File. This returns File if it
   --  exists in the filesystem. Otherwise, it tries to locate it transforming
   --  File according to the source rebase/search parameters
   --  (see --source-rebase and --source-search command line arguments).
   --  Return No_File if we could not locate the file.

   --  Executable search prefix

   Exec_Search_Prefix : Virtual_File;

   ----------------------------
   --  Line info allocations --
   ----------------------------
   --
   --  We generally need to allocate lots of Line_Info records. For performance
   --  and to avoid memory fragmentation, we allocate them in chunks. We then
   --  need to keep track of all allocated chunks so that we can free them in
   --  Checkpoint_Clear: each chunk remembers what was the previous allocated
   --  one.

   Line_Info_Chunk_Size : constant := 512;
   --  Number of Line_Info records allocated at once

   type Line_Info_Chunk;
   type Line_Info_Chunk_Access is access all Line_Info_Chunk;
   type Line_Info_Array is array (Positive range <>) of aliased Line_Info;
   type Line_Info_Chunk is record
      Lines : Line_Info_Array (1 .. Line_Info_Chunk_Size);
      --  Individual Line_Info records

      Previous_Chunk : Line_Info_Chunk_Access;
      --  Line_Info_Chunk that was allocated right before this one. This makes
      --  a linked list, easy to use for deallocion.
   end record;
   procedure Free is new Ada.Unchecked_Deallocation
     (Line_Info_Chunk, Line_Info_Chunk_Access);

   Current_LI_Chunk_Next_Line : Positive := 1;
   --  Index of the next available cell in Current_LI_Chunk.Lines

   Current_LI_Chunk : Line_Info_Chunk_Access := null;
   --  Last allocated chunk, if we allocated at least one chunk, null
   --  otherwise.

   function Create_Line_Info return Line_Info_Access;
   --  Return a new Line_Info record

   procedure Consolidate_Source_File_Unit
     (Index    : Valid_Source_File_Index;
      New_Unit : Ada.Strings.Unbounded.Unbounded_String) with
     Pre => Get_File (Index).Kind = Source_File;
   --  Update the unit name info for the source file represented by Index.
   --  Does nothing if the new unit name is the empty string.
   --
   --  Calling this procedure is valid iff the source file at Index has no
   --  owning unit already, or that it is the same as New_Unit.

   ---------------------
   -- Append_To_Array --
   ---------------------

   procedure Append_To_Array
      (A : in out Resizeable_Array_Access;
       E : Element_Type)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Resizeable_Array, Resizeable_Array_Access);

      New_A : Resizeable_Array_Access;

   begin
      if A = null then
         A := new Resizeable_Array'(Index_Type'First => E);
      else
         New_A := new Resizeable_Array'(A.all & E);
         Free (A);
         A := New_A;
      end if;
   end Append_To_Array;

   ----------------------------------
   -- Add_Line_For_Object_Coverage --
   ----------------------------------

   procedure Add_Line_For_Object_Coverage
     (File  : Source_File_Index;
      State : Line_State;
      Line  : Positive;
      Addrs : Address_Info_Acc;
      Base  : Traces_Base_Acc;
      Exec  : Exe_File_Acc)
   is
      procedure Append_Obj is new Append_To_Array
        (Natural, Object_Coverage_Info,
         Object_Coverage_Info_Array,
         Object_Coverage_Info_Array_Acc);

      FI   : constant File_Info_Access := Files_Table.Element (File);
      LI   : Line_Info_Access;

   begin
      FI.Has_Object_Coverage_Info := True;
      Expand_Line_Table (File, Line);
      LI := FI.Lines.Element (Line);
      Append_Obj
        (LI.Obj_Infos,
         (State           => State,
          Instruction_Set => Addrs,
          Base            => Base,
          Exec            => Exec));
   end Add_Line_For_Object_Coverage;

   ----------------------------------
   -- Add_Line_For_Source_Coverage --
   ----------------------------------

   procedure Add_Line_For_Source_Coverage
     (File : Source_File_Index;
      Line : Positive;
      SCO  : SCO_Id)
   is
      procedure Append_SCO is new Append_To_Array
        (Natural, SCO_Id,
         SCO_Id_Array,
         SCO_Id_Array_Acc);

      FI : constant File_Info_Access := Files_Table.Element (File);
   begin
      FI.Has_Source_Coverage_Info := True;
      Expand_Line_Table (File, Line);
      Append_SCO (FI.Lines.Element (Line).all.SCOs, SCO);
   end Add_Line_For_Source_Coverage;

   -----------------------
   -- Add_Source_Rebase --
   -----------------------

   procedure Add_Source_Rebase (Old_Prefix : String; New_Prefix : String) is
      use GNAT.Regpat;
      E : Source_Rebase_Entry_Acc;

      Regexp : constant String := "^" & Paths.Glob_To_Regexp (Old_Prefix);
      --  Add a begining of line anchor to the regular expression to make sure
      --  we are matching a prefix.

      Flags : constant Regexp_Flags :=
        (if GNAT.OS_Lib.Directory_Separator = '\'
         then Case_Insensitive
         else No_Flags);
      --  Windows paths are canonicalized so use case insensitive regexp to
      --  make sure the prefix has a chance to match.

   begin
      --  Canonicalize the new prefix so all absolute paths in the file table
      --  are canonicalized.

      E := new Source_Rebase_Entry'
        (Old_Prefix => new Pattern_Matcher'(Compile (Regexp, Flags)),
         New_Prefix => Canonicalize_Filename (New_Prefix),
         Next       => null);
      if First_Source_Rebase_Entry = null then
         First_Source_Rebase_Entry := E;
      else
         Last_Source_Rebase_Entry.Next := E;
      end if;

      Last_Source_Rebase_Entry := E;
   end Add_Source_Rebase;

   -----------------------
   -- Add_Source_Search --
   -----------------------

   procedure Add_Source_Search (Prefix : String) is
      E : Source_Search_Entry_Acc;
   begin
      --  Canonicalize the source search prefix so that all absolute filenames
      --  in the filetable are canonicalized.

      E := new Source_Search_Entry'(Prefix => Canonicalize_Filename (Prefix),
                                    Next => null);

      if First_Source_Search_Entry = null then
         First_Source_Search_Entry := E;
      else
         Last_Source_Search_Entry.Next := E;
      end if;

      Last_Source_Search_Entry := E;
   end Add_Source_Search;

   ---------------------
   -- Set_Exec_Prefix --
   ---------------------

   procedure Set_Exec_Prefix (Prefix : String) is
   begin
      Exec_Search_Prefix := Create (+Prefix);
   end Set_Exec_Prefix;

   -----------------
   -- Lookup_Exec --
   -----------------

   function Lookup_Exec (Name : String) return String
   is
      function Lookup_Path (Path : String) return String;
      --  If Path, or a name derived from Path (i.e. with the ".exe" suffix on
      --  Windows) exists, return it. Return an empty string otherwise.

      -----------------
      -- Lookup_Path --
      -----------------

      function Lookup_Path (Path : String) return String is
      begin
         if GNAT.OS_Lib.Is_Readable_File (Path) then
            return Path;

         else
            --  On Windows, users can omit ".exe" suffixes for executables,
            --  so add it if it's missing on this platform. Be careful not
            --  to add it everytime since we sometimes have to deal with
            --  other kind of files (for instance object files).

            declare
               Exe_Path : constant String := Osint.Executable_Name (Path);
            begin
               if GNAT.OS_Lib.Is_Readable_File (Exe_Path) then
                  return Exe_Path;
               end if;
            end;
         end if;

         return "";
      end Lookup_Path;

      Path : constant String := Lookup_Path (Name);

   --  Start of processing for Lookup_Exec

   begin
      --  If we could not find Name, look in Exec_Search_Prefix

      if Path'Length = 0 and then Exec_Search_Prefix /= No_File then
         declare
            Base_Name : constant String := Ada.Directories.Simple_Name (Name);
            VPath     : constant Virtual_File :=
              Create_From_Dir (Exec_Search_Prefix, +Base_Name);
            Path      : constant String := Lookup_Path (+Full_Name (VPath));
         begin
            return Path;
         end;
      else
         return Path;
      end if;

   end Lookup_Exec;

   ---------------------
   -- End_Lex_Element --
   ---------------------

   function End_Lex_Element (Sloc : Source_Location) return Source_Location
   is
      use Ada.Characters.Handling;

      type Lex_Type is (String_Literal, Numeric_Literal, Other);

      Lex    : Lex_Type;
      Line   : constant String := Get_Line (Sloc);
      Column : Natural := Sloc.L.Column;
      Result : Source_Location := Sloc;

   begin
      --  If the column is out of range, the source files and the sloc
      --  information are probably not synchronized. So just return Sloc
      --  unchanged.

      if Column > Line'Last then
         return Sloc;
      end if;

      if Column + 1 <= Line'Last
        and then Line (Column) = '-'
        and then Line (Column + 1) = '-'
      then
         --  Comment; return the whole line
         Result.L.Column := Line'Last;
         return Result;

      elsif Column + 2 <= Line'Last
        and then Line (Column) = '''
        and then Line (Column + 2) = '''
      then
         --  Character literal; skip two character and return
         Result.L.Column := Column + 2;
         return Result;

      elsif Line (Column) = '"' then
         --  String literal
         Lex := String_Literal;

      elsif Is_Decimal_Digit (Line (Column))
        or else Line (Column) = '-'
      then
         --  Numeric literal
         Lex := Numeric_Literal;

      else
         --  Other: identifier, pragma, reserved word
         Lex := Other;
      end if;

      for J in Sloc.L.Column + 1 .. Line'Last loop
         case Lex is
            when String_Literal =>
               exit when Line (J) = '"';

            when Numeric_Literal =>
               exit when not Is_Decimal_Digit (Line (J))
                 and then Line (J) /= '#'
                 and then Line (J) /= 'E'
                 and then Line (J) /= '.'
                 and then Line (J) /= '_';

            when Other =>
               exit when not Is_Alphanumeric (Line (J))
                 and then Line (J) /= '_';

         end case;
         Column := J;
      end loop;

      Result.L.Column := Natural'Min (Column, Line'Last);
      return Result;
   end End_Lex_Element;

   ----------------------
   -- Create_Line_Info --
   ----------------------

   function Create_Line_Info return Line_Info_Access is
   begin
      --  Allocate a new chunk if there was no chunk or if there is no room
      --  left in the current one.

      if Current_LI_Chunk = null
         or else Current_LI_Chunk_Next_Line > Current_LI_Chunk.Lines'Last
      then
         --  Allocate the record first and then initialize the Lines array.
         --  Don't do it in one single statement as this would create a big
         --  aggregate on the stack, triggering a stack overflow when the
         --  number of line is too high.

         Current_LI_Chunk := new Line_Info_Chunk'
           (Lines => <>, Previous_Chunk => Current_LI_Chunk);
         for Line of Current_LI_Chunk.Lines loop
            Line := (State => (others => No_Code), others => <>);
         end loop;
         Current_LI_Chunk_Next_Line := Current_LI_Chunk.Lines'First;

         Bump (Line_Table_Alloc);
         Bump (Line_Table_Alloc_Size,
               How_Many => Current_LI_Chunk.Lines'Length);
      end if;

      return Result : constant Line_Info_Access :=
         Current_LI_Chunk.Lines (Current_LI_Chunk_Next_Line)'Access
      do
         Current_LI_Chunk_Next_Line := Current_LI_Chunk_Next_Line + 1;
      end return;
   end Create_Line_Info;

   -----------------------
   -- Expand_Line_Table --
   -----------------------

   procedure Expand_Line_Table (FI : File_Info_Access; Line : Positive) is
   begin
      if Line <= FI.Lines.Last_Index then
         return;
      end if;

      FI.Lines.Reserve_Capacity (Ada.Containers.Count_Type (Line));
      for J in FI.Lines.Last_Index + 1 .. Line loop
         FI.Lines.Append (Create_Line_Info);
      end loop;
   end Expand_Line_Table;

   procedure Expand_Line_Table (File : Source_File_Index; Line : Positive) is
      FI : File_Info_Access renames Files_Table.Element (File);
   begin
      Expand_Line_Table (FI, Line);
   end Expand_Line_Table;

   --------------------------
   --  Files_Table_Iterate --
   --------------------------

   procedure Files_Table_Iterate
     (Process : not null access procedure (Index : Source_File_Index)) is
   begin
      Freeze_Files_Table;
      for Index of Sorted_Files_Table loop
         Process (Index);
      end loop;
   end Files_Table_Iterate;

   ----------------
   -- First_File --
   ----------------

   function First_File return Source_File_Index is
   begin
      return Files_Table.First_Index;
   end First_File;

   ---------------
   -- Last_File --
   ---------------

   function Last_File return Source_File_Index is
   begin
      return Files_Table.Last_Index;
   end Last_File;

   -------------------------------
   -- Consolidate_Ignore_Status --
   -------------------------------

   procedure Consolidate_Ignore_Status
     (Index  : Valid_Source_File_Index;
      Status : Any_Ignore_Status)
   is
      FI : File_Info renames Files_Table.Element (Index).all;
   begin
      if FI.Kind /= Source_File then
         return;
      end if;
      if Status = Sometimes then
         FI.Ignore_Status := Sometimes;
      elsif FI.Ignore_Status = Unknown then
         FI.Ignore_Status := Status;
      elsif Status /= Unknown and then FI.Ignore_Status /= Status then
         FI.Ignore_Status := Sometimes;
      end if;

   end Consolidate_Ignore_Status;

   ----------------------------------
   -- Consolidate_Source_File_Unit --
   ----------------------------------

   procedure Consolidate_Source_File_Unit
     (Index    : Valid_Source_File_Index;
      New_Unit : Ada.Strings.Unbounded.Unbounded_String)
   is
      use Ada.Strings.Unbounded;
      FI : File_Info renames Files_Table.Element (Index).all;
   begin
      if FI.Kind /= Source_File or else New_Unit = Null_Unbounded_String then
         return;
      end if;
      if not FI.Unit.Known then
         FI.Unit := (Known => True,
                     Name  => New_Unit);
      elsif FI.Unit.Known then
         pragma Assert (FI.Unit.Name = New_Unit);
      end if;
   end Consolidate_Source_File_Unit;

   procedure Consolidate_Source_File_Unit
     (Index    : Valid_Source_File_Index;
      New_Unit : String)
   is
   begin
      Consolidate_Source_File_Unit
        (Index, Ada.Strings.Unbounded.To_Unbounded_String (New_Unit));
   end Consolidate_Source_File_Unit;

   ---------------------
   -- Fill_Line_Cache --
   ---------------------

   procedure Fill_Line_Cache (FI : File_Info_Access) is
      F          : Ada.Text_IO.File_Type;
      Has_Source : Boolean;
      Line       : Natural := 1;
      LI         : Line_Info_Access;

      --  Start of processing for Cache_Lines

   begin
      if FI = Current_File_Line_Cache then
         return;
      end if;

      Open (F, FI, Has_Source);

      if Has_Source then
         if Current_File_Line_Cache /= null then
            Invalidate_Line_Cache (Current_File_Line_Cache);
         end if;

         while not End_Of_File (F) loop
            Expand_Line_Table (FI, Line);
            LI := FI.Lines.Element (Line);

            if LI.Line_Cache /= null then
               Free (LI.Line_Cache);
            end if;

            LI.Line_Cache := new String'(Get_Line (F));
            Line := Line + 1;
         end loop;

         Current_File_Line_Cache := FI;
         Close (F);
      end if;
   end Fill_Line_Cache;

   --------------
   -- Get_File --
   --------------

   function Get_File (Index : Source_File_Index) return File_Info_Access is
   begin
      return Files_Table.Element (Index);
   end Get_File;

   -------------------
   -- Get_Full_Name --
   -------------------

   function Get_Full_Name
     (Index : Source_File_Index; Or_Simple : Boolean := False) return String
   is
      File : File_Info renames Files_Table.Element (Index).all;
   begin
      if File.Full_Name /= null then
         return File.Full_Name.all;
      elsif Or_Simple then
         return File.Simple_Name.all;
      else
         Outputs.Fatal_Error ("No full path name for " & File.Simple_Name.all);
      end if;
   end Get_Full_Name;

   ------------------------------
   -- Get_Index_From_Full_Name --
   ------------------------------

   function Get_Index_From_Full_Name
     (Full_Name           : String;
      Kind                : File_Kind;
      Insert              : Boolean := True;
      Indexed_Simple_Name : Boolean := False) return Source_File_Index
   is
      use Filename_Maps;
      use Filename_Rebase_Maps;
      use Simple_Name_Maps;

      Original_Full_Path : constant Virtual_File :=
        Create (+Canonicalize_Filename (Full_Name));
      Full_Path          : Virtual_File := Original_Full_Path;
      --  Full_Path can be modified to hold the result of the source rebase

      Cur         : Filename_Maps.Cursor;
      Rebase_Cur  : Filename_Rebase_Maps.Cursor;
      Res         : Source_File_Index;
      Info_Simple : File_Info_Access;

   begin
      if Switches.Debug_File_Table then
         Put_Line ("GIFN: <<" & Full_Name & ">> ISN=" & Indexed_Simple_Name'Img
                   & " Insert=" & Insert'Img);
      end if;

      --  First lookup the full path in the Renaming_Map as this file can
      --  already have been renamed. Otherwise, if we can modify the file
      --  table, find its location on disk and store the entry in the cache.

      if Use_Renaming_Map then
         Rebase_Cur := Renaming_Map.Find (Full_Path);
         if Rebase_Cur /= Filename_Rebase_Maps.No_Element then
            Full_Path := Element (Rebase_Cur);
         elsif Insert then
            Full_Path := Locate_Source (Original_Full_Path);
            if Full_Path = No_File then

               --  If the file was not found, reset it to his original full
               --  path. Still make the association in Renaming_Map to avoid
               --  subsequent calls to Locate_Source. We don't need to update
               --  the Has_Source field of the corresponding File_Info, this
               --  will be done when trying to open the source file.

               Full_Path := Original_Full_Path;
            end if;
            Renaming_Map.Insert (Original_Full_Path, Full_Path);
         end if;
      end if;

      --  Lookup the full name map

      Cur := Full_Name_Map.Find (Full_Path);

      if Cur /= Filename_Maps.No_Element then
         Res := Element (Cur);
         goto Do_Return;
      end if;

      --  Here if full name not found

      declare
         Simple_Path : constant Virtual_File := Create (Base_Name (Full_Path));
         Simple_Cur  : Simple_Name_Maps.Cursor :=
           Simple_Name_Map.Find (Simple_Path);

      begin
         --  Look for registered occurrence of simple name

         if Simple_Cur /= Simple_Name_Maps.No_Element then
            Res := Element (Simple_Cur);
            pragma Assert (Res /= No_Source_File);
            Consolidate_File_Kind (Res, Kind);

            --  If we are not allowed to insert something, do not modify
            --  existing entries.

            if not Insert then
               goto Do_Return;
            end if;

            Info_Simple := Files_Table.Element (Res);

            if Info_Simple.Full_Name = null then
               Info_Simple.Full_Name :=
                 new String'(+GNATCOLL.VFS.Full_Name (Full_Path));
               Full_Name_Map.Insert (Full_Path, Res);
               goto Do_Return;

            else
               if Create (+Info_Simple.Full_Name.all) /= Full_Path then
                  Put_Line ("Warning: same base name for files:");
                  Put_Line ("  " & (+GNATCOLL.VFS.Full_Name (Full_Path)));
                  Put_Line ("  " & Info_Simple.Full_Name.all);
               end if;
            end if;

         --  Here if not found by simple name either

         elsif not Insert then
            Res := No_Source_File;
            goto Do_Return;
         end if;

         --  If we reach this point, we are inserting a new file into the table

         pragma Assert (not Files_Table_Frozen);

         Files_Table.Append (Create_File_Info
           (Kind,
            new String'(+GNATCOLL.VFS.Full_Name (Full_Path)),
            new String'(+GNATCOLL.VFS.Full_Name (Simple_Path)),
            Indexed_Simple_Name));
         Res := Files_Table.Last_Index;

         --  If needed, add an entry into the simple name map. It will help
         --  aliasing computation.

         if Indexed_Simple_Name
           and then Simple_Cur = Simple_Name_Maps.No_Element
         then
            declare
               Inserted : Boolean;

            begin
               Simple_Name_Map.Insert (Simple_Path, Res, Simple_Cur, Inserted);
               pragma Assert (Inserted);
            end;
         end if;

         Full_Name_Map.Insert (Full_Path, Res);
      end;

   <<Do_Return>>
      if Switches.Debug_File_Table then
         Put_Line (" ->" & Res'Img);
      end if;
      if Res in Valid_Source_File_Index then
         Consolidate_File_Kind (Res, Kind);
      end if;
      return Res;
   end Get_Index_From_Full_Name;

   --------------------------------
   -- Get_Index_From_Simple_Name --
   --------------------------------

   function Get_Index_From_Simple_Name
     (Simple_Name : String;
      Kind        : File_Kind;
      Insert      : Boolean := True) return Source_File_Index
   is
      use Simple_Name_Maps;

      Simple_Path : constant Virtual_File := Create (+Simple_Name);

      Cur : constant Simple_Name_Maps.Cursor :=
        Simple_Name_Map.Find (Simple_Path);
      Res : Source_File_Index;
   begin
      if Switches.Debug_File_Table then
         Put ("GISN: <<" & Simple_Name & ">> Insert=" & Insert'Img);
      end if;

      if Cur /= Simple_Name_Maps.No_Element then
         Res := Element (Cur);

      elsif not Insert then
         Res := No_Source_File;

      else
         pragma Assert (not Files_Table_Frozen);

         Files_Table.Append (Create_File_Info
           (Kind                => Kind,
            Full_Name           => null,
            Simple_Name         => new String'(+Full_Name (Simple_Path)),
            Indexed_Simple_Name => True));
         Res := Files_Table.Last_Index;
         Simple_Name_Map.Insert (Simple_Path, Res);
      end if;

      if Switches.Debug_File_Table then
         Put_Line (" ->" & Res'Img);
      end if;

      if Res in Valid_Source_File_Index then
         Consolidate_File_Kind (Res, Kind);
      end if;
      return Res;
   end Get_Index_From_Simple_Name;

   ---------------------------------
   -- Get_Index_From_Generic_Name --
   ---------------------------------

   function Get_Index_From_Generic_Name
     (Name                : String;
      Kind                : File_Kind;
      Insert              : Boolean := True;
      Indexed_Simple_Name : Boolean := False) return Source_File_Index
   is
      Result : Source_File_Index;
   begin
      if Switches.Debug_File_Table then
         Put ("GIGN: <<" & Name
              & ">> ISN=" & Indexed_Simple_Name'Img & " -> ");
      end if;

      if Is_Absolute_Path (Name) then
         Result := Get_Index_From_Full_Name
           (Name, Kind, Insert, Indexed_Simple_Name);
      else
         Result := Get_Index_From_Simple_Name (Name, Kind, Insert);
      end if;
      return Result;
   end Get_Index_From_Generic_Name;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (Sloc : Source_Location) return Line_Info_Access is
   begin
      if Sloc = Slocs.No_Location then
         return null;
      end if;

      return Get_Line (Get_File (Sloc.Source_File), Sloc.L.Line);
   end Get_Line;

   function Get_Line
     (File  : File_Info_Access;
      Index : Positive) return Line_Info_Access
   is
   begin
      if File.Kind = Source_File
         and then Index in File.Lines.First_Index .. File.Lines.Last_Index
      then
         return File.Lines.Element (Index);
      else
         --  Get_Line may be called on a stub file or with no source
         --  information loaded, for example when emitting a diagnostic with
         --  sloc information based on SCOs only. In that case return a null
         --  pointer, since we do not have any available Line_Info structure.

         return null;
      end if;
   end Get_Line;

   function Get_Line (Sloc : Source_Location) return String is
   begin
      if Sloc = Slocs.No_Location then
         return "";
      end if;

      return Get_Line (Get_File (Sloc.Source_File), Sloc.L.Line);
   end Get_Line;

   function Get_Line
     (File  : File_Info_Access;
      Index : Positive) return String
   is
      Line : String_Access;
   begin
      if not File.Has_Source then
         return "";
      end if;

      Fill_Line_Cache (File);

      if Index in File.Lines.First_Index .. File.Lines.Last_Index then
         Line := File.Lines.Element (Index).Line_Cache;

         if Line /= null then
            return Line.all;
         else
            return "";
         end if;

      else
         return "";
      end if;
   end Get_Line;

   ---------------------
   -- Get_Simple_Name --
   ---------------------

   function Get_Simple_Name (Index : Source_File_Index) return String is
   begin
      return Files_Table.Element (Index).Simple_Name.all;
   end Get_Simple_Name;

   ------------------------
   -- Freeze_Files_Table --
   ------------------------

   procedure Freeze_Files_Table is

      package Conversions is new System.Address_To_Access_Conversions
        (Object => File_Info);
      function "+" (A : File_Info_Access) return System.Address is
        (Conversions.To_Address (Conversions.Object_Pointer (A)));
      function "<" (Left, Right : File_Info_Access) return Boolean is
        (System."<" (+Left, +Right));
      --  Utilities to deal with File_Info_Access in ordered sets

      package File_Sets is new Ada.Containers.Ordered_Sets
        (Element_Type => File_Info_Access);
      type File_Set_Access is access File_Sets.Set;
      --  Set of files that have the same name

      procedure Free is new Ada.Unchecked_Deallocation
        (Object => File_Sets.Set, Name => File_Set_Access);

      package Alias_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type        => Virtual_File,
         Element_Type    => File_Set_Access,
         Hash            => Full_Name_Hash,
         Equivalent_Keys => "=");
      --  Mapping from file name to a set of files that have the same name

      use type Ada.Containers.Count_Type;
      use Alias_Maps;

      procedure Add_File
        (Key  : String_Access;
         File : File_Info_Access;
         Map  : in out Alias_Maps.Map);
      --  Add a file to the corresponding file set in Map, according to Key.
      --  Create the file set if it does not exist.

      procedure Clear (Map : in out Alias_Maps.Map);
      --  Free all file sets in Map

      procedure Grow_Unique_Name (File : File_Info_Access);
      --  If File.Unique_Name is null, set it to the last subdirectory plus the
      --  base name. Otherwise, extend it to include the previous containing
      --  subdirectory. If it cannot be extended, raise a Program_Error: this
      --  is really not supposed to happen.
      --
      --  For instance, considering a file /foo/bar/baz, successive calls to
      --  Grow_Unique_Name will turn Unique_Name from null to "bar/baz", then
      --  to "foo/bar/baz" and then to "/foo/bar/baz".

      --------------
      -- Add_File --
      --------------

      procedure Add_File
        (Key  : String_Access;
         File : File_Info_Access;
         Map  : in out Alias_Maps.Map)
      is
         V_Key    : constant Virtual_File := Create (+Key.all);
         Cur      : constant Cursor := Map.Find (V_Key);
         File_Set : File_Set_Access;
      begin
         if Cur = No_Element then
            File_Set := new File_Sets.Set;
            Map.Insert (V_Key, File_Set);
         else
            File_Set := Element (Cur);
         end if;
         File_Set.Insert (File);
      end Add_File;

      -----------
      -- Clear --
      -----------

      procedure Clear (Map : in out Alias_Maps.Map) is
      begin
         for File_Set of Map loop
            Free (File_Set);
         end loop;
      end Clear;

      ----------------------
      -- Grow_Unique_Name --
      ----------------------

      procedure Grow_Unique_Name (File : File_Info_Access) is
         Full : String renames File.Full_Name.all;

         procedure Get_Previous_Separator (Index : in out Positive);
         --  Assuming index is a valid index in Full, decrease it until either
         --  it meets the start of Full or until it meets a path separator
         --  (slash or backslash). Note that consecutive path separators are
         --  considered as a single separator.

         ----------------------------
         -- Get_Previous_Separator --
         ----------------------------

         procedure Get_Previous_Separator (Index : in out Positive) is
            function Is_Sep (Index : Positive) return Boolean is
              (Full (Index) in '/' | '\');
            --  Return whether the character in Full at Index is a path
            --  separator.

            Crossed_Non_Sep : Boolean := False;
         begin
            --  To simplify the next steps, prepare the initial case (when
            --  Index is at the end of Full) to look like we are already on
            --  the last separator character to skip.

            if Full'Last = Index then
               while Full'First < Index and then not Is_Sep (Index) loop
                  Index := Index - 1;
               end loop;

            --  When resuming in the middle of a path, we start from the first
            --  character of a subdirectory. In such a case, first get to the
            --  path separator.

            elsif Full'First < Index
              and then not Is_Sep (Index)
              and then Is_Sep (Index - 1)
            then
               Index := Index - 1;
            end if;

            --  Skip all consecutive path separators

            while Full'First < Index and then Is_Sep (Index) loop
               Index := Index - 1;
            end loop;

            --  At this point, either we are already at the beginning of Full,
            --  either we are over a non-separator character. In the latter
            --  case, get to the next separator.

            while Full'First < Index and then not Is_Sep (Index) loop
               Crossed_Non_Sep := True;
               Index := Index - 1;
            end loop;

            --  And now, if we crossed a non-separator character, we stopped
            --  either because we met a separator or because we reached the
            --  beginning of Full. In the former case, we do not want to
            --  include the separator character in the output.

            if Crossed_Non_Sep and then Is_Sep (Index) then
               Index := Index + 1;
            end if;
         end Get_Previous_Separator;

         First : Positive :=
           (if File.Unique_Name = null
            then Full'Last
            else Full'Last - File.Unique_Name.all'Length + 1);

      --  Start of processing for Grow_Unique_Name

      begin
         --  Get the substring in Full that will make the next Unique_Name

         Get_Previous_Separator (First);

         --  Assign it to Unique_Name

         declare
            New_Unique_Name : String renames Full (First .. Full'Last);
         begin
            if File.Unique_Name /= null then

               --  Complain if the unique name did not grow

               if New_Unique_Name'Length = File.Unique_Name.all'Length then
                  raise Program_Error
                    with "Multiple files have the same full name";
               end if;

               Free (File.Unique_Name);
            end if;
            File.Unique_Name := new String'(New_Unique_Name);
         end;
      end Grow_Unique_Name;

      Alias_Map : Alias_Maps.Map;
      --  Mapping: simple name to set of files that have this simple name

   --  Start of processing for Freeze_Files_Table

   begin
      if Files_Table_Frozen then
         return;
      end if;

      --  First, build the alias map: conflicting files will get grouped under
      --  a single alias set.

      for File of Files_Table loop
         if File.Kind = Source_File then
            Add_File (File.Simple_Name, File, Alias_Map);
         end if;
      end loop;

      --  Then, for each set of aliased files, find the shortest unique suffix.
      --  Note that the loops below will not get executed ofter (aliases are
      --  rare), so efficiency should not be an issue.

      for Alias_Set of Alias_Map loop

         --  The loop below will not run at all if this alias set contains a
         --  single file: in such a case, make sure the file gets an unique
         --  name.

         if Alias_Set.Length = 1 then
            declare
               File : File_Info renames Alias_Set.First_Element.all;
            begin
               File.Unique_Name := new String'(File.Simple_Name.all);
            end;
         end if;

         --  While there are conflicts between candidate unique names, make the
         --  suffix longer for all. When one candidate has no longer conflicts
         --  with other files, remove it from the set.

         while Alias_Set.Length > 1 loop
            declare
               Unique_Name_Map : Alias_Maps.Map;
            begin
               --  Make the suffix longer and sort file in a dedicated map,
               --  so that we can see what files still conflict with new
               --  Unique_Name attributes.

               for File of Alias_Set.all loop
                  Grow_Unique_Name (File);
                  Add_File (File.Unique_Name, File, Unique_Name_Map);
               end loop;

               --  Remove from Alias_Set all the files that now have an
               --  Unique_Name that is really unique.

               for File_Set of Unique_Name_Map loop
                  if File_Set.Length = 1 then
                     Alias_Set.Delete (File_Set.First_Element);
                  end if;
               end loop;

               Clear (Unique_Name_Map);
            end;
         end loop;
      end loop;

      --  Fill Sorted_Files_Table with all indexes from the files table, and
      --  then sort these indexes according to the referenced file names.

      declare

         function Appears_Before
           (Left, Right : Valid_Source_File_Index) return Boolean;
         --  Return whether Left should appear before Right in
         --  Sorted_Files_Table.

         --------------------
         -- Appears_Before --
         --------------------

         function Appears_Before
           (Left, Right : Valid_Source_File_Index) return Boolean
         is
            L : File_Info renames Files_Table.Element (Left).all;
            R : File_Info renames Files_Table.Element (Right).all;
         begin
            if L.Simple_Name.all = R.Simple_Name.all then
               return L.Full_Name /= null
                      and then R.Full_Name /= null
                      and then L.Full_Name.all < R.Full_Name.all;
            else
               return L.Simple_Name.all < R.Simple_Name.all;
            end if;
         end Appears_Before;

         package Sorting is new File_Index_Vectors.Generic_Sorting
           ("<" => Appears_Before);

      begin
         Sorted_Files_Table.Reserve_Capacity (Files_Table.Length);
         for Index in Files_Table.First_Index .. Files_Table.Last_Index loop
            Sorted_Files_Table.Append (Index);
         end loop;
         Sorting.Sort (Sorted_Files_Table);
      end;

      Clear (Alias_Map);
      Files_Table_Frozen := True;
   end Freeze_Files_Table;

   ---------------------
   -- Get_Unique_Name --
   ---------------------

   function Get_Unique_Name (Index : Source_File_Index) return String is
      File : File_Info renames Get_File (Index).all;
   begin
      Freeze_Files_Table;
      return File.Unique_Name.all;
   end Get_Unique_Name;

   ----------------------
   -- Create_File_Info --
   ----------------------

   function Create_File_Info
     (Kind                   : File_Kind;
      Full_Name, Simple_Name : String_Access;
      Indexed_Simple_Name    : Boolean)
      return File_Info_Access
   is
      Result : constant File_Info_Access := new File_Info (Kind);
   begin
      Result.Full_Name := Full_Name;
      Result.Simple_Name := Simple_Name;
      Result.Indexed_Simple_Name := Indexed_Simple_Name;
      if Result.Kind = Source_File then
         Result.Lines := (Source_Line_Vectors.Empty_Vector with null record);
         Result.Unit := (Known => False);
      end if;
      return Result;
   end Create_File_Info;

   ---------------------------
   -- Consolidate_File_Kind --
   ---------------------------

   procedure Consolidate_File_Kind
     (Index : Valid_Source_File_Index;
      Kind  : File_Kind)
   is
      FI : File_Info_Access := Get_File (Index);
   begin
      if Kind = Stub_File then

         --  Kind does not bring anything new to what we know about FI and
         --  there is no possible inconsistency: do nothing.

         null;

      elsif FI.Kind = Stub_File then

         --  We have a more specific kind of file: change the kind of FI

         pragma Assert (not Files_Table_Frozen);

         if Switches.Debug_File_Table then
            declare
               Name : constant String_Access :=
                 (if FI.Full_Name = null
                  then FI.Simple_Name
                  else FI.Full_Name);
            begin
               Put_Line
                 ("Promoting " & Name.all & " from stub to source file");
            end;
         end if;

         declare
            New_FI : constant File_Info_Access :=
              Create_File_Info
                (Kind, FI.Full_Name, FI.Simple_Name, FI.Indexed_Simple_Name);
         begin
            New_FI.Has_Source := FI.Has_Source;
            Free (FI);
            Files_Table.Replace_Element (Index, New_FI);
         end;

      elsif FI.Kind /= Kind then
         Outputs.Fatal_Error
           ("Trying to use " & FI.Simple_Name.all & " both as a "
            & Kind_Name (FI.Kind) & " and as a "
            & Kind_Name (Kind));
      end if;
   end Consolidate_File_Kind;

   ---------------------------
   -- Invalidate_Line_Cache --
   ---------------------------

   procedure Invalidate_Line_Cache (FI : File_Info_Access) is

      procedure Free_Line_Cache (Index : Positive);
      --  Free cached line in FI at Index

      ----------------------
      -- Free_Cached_Line --
      ----------------------

      procedure Free_Line_Cache (Index : Positive) is
         LI : constant Line_Info_Access := Get_Line (FI, Index);
      begin
         if LI = null then
            return;
         end if;

         if LI.Line_Cache /= null then
            Free (LI.Line_Cache);
         end if;
      end Free_Line_Cache;

      --  Start of processing for Free_Line_Cache

   begin
      Iterate_On_Lines (FI, Free_Line_Cache'Access);
   end Invalidate_Line_Cache;

   ----------------------------
   -- Is_Multistatement_Line --
   ----------------------------

   function Is_Multistatement_Line (Sloc : Source_Location) return Boolean is
      LI : constant Line_Info_Access :=
             Get_Line (Get_File (Sloc.Source_File), Sloc.L.Line);
   begin
      if LI = null then
         return False;
      else
         return Is_Multistatement_Line (LI.all);
      end if;
   end Is_Multistatement_Line;

   function Is_Multistatement_Line (LI : in out Line_Info) return Boolean is
   begin
      --  Count statements once, and then cache the result in
      --  LI.Statement_Count.

      if LI.Is_Multistatement = Unknown then
         declare
            Count : Natural := 0;
         begin
            Count := 0;
            if LI.SCOs /= null then
               for SCO of LI.SCOs.all loop
                  if Kind (SCO) = Statement then
                     Count := Count + 1;
                  end if;
               end loop;
            end if;
            LI.Is_Multistatement := To_Tristate (Count > 1);
         end;
      end if;

      return To_Boolean (LI.Is_Multistatement);
   end Is_Multistatement_Line;

   ----------------------
   -- Iterate_On_Lines --
   ----------------------

   procedure Iterate_On_Lines
     (File    : File_Info_Access;
      Process : not null access procedure (Index : Positive)) is
   begin
      for Index in File.Lines.First_Index .. File.Lines.Last_Index loop
         Process (Index);
      end loop;
   end Iterate_On_Lines;

   ----------
   -- Open --
   ----------

   procedure Open
     (File    : in out File_Type;
      FI      : File_Info_Access;
      Success : out Boolean)
   is
      procedure Try_Open
        (File    : in out File_Type;
         Name    : String;
         Success : out Boolean);
      --  Try to open Name, with no rebase/search information. If it fails,
      --  set Success to False; otherwise, set it to True and return the
      --  handle in File.

      --------------
      -- Try_Open --
      --------------

      procedure Try_Open
        (File    : in out Ada.Text_IO.File_Type;
         Name    : String;
         Success : out Boolean) is
      begin
         Open (File, In_File, Name);
         Success := True;
      exception
         when Name_Error | Use_Error =>
            Success := False;
      end Try_Open;

      --  Local variables

      Name : String_Access;

   --  Start of processing for Open

   begin
      if not FI.Has_Source then
         Success := False;
         return;
      end if;

      Name := FI.Full_Name;

      if Name = null then
         Name := FI.Simple_Name;
      end if;

      --  Try original path

      Try_Open (File, Name.all, Success);

      if FI.Full_Name = null then

         if Success then

            --  Found using simple name (in current directory)

            FI.Full_Name :=
              Build_Filename
                (Ada.Directories.Current_Directory,
                 FI.Simple_Name.all);
         else
            --  If previous attempt failed, try again to locate the source file
            --  with the help of the project manager

            FI.Full_Name :=
              Project.Find_Source_File (FI.Simple_Name.all);
            if FI.Full_Name /= null then
               Try_Open (File, FI.Full_Name.all, Success);
               if not Success then
                  Free (FI.Full_Name);
               end if;
            end if;
         end if;

         --  If we could not find the source file in his original path or using
         --  the project information, try to use the rebase and souce search
         --  mechanism on the simple name.

         if not Success then
            declare
               Candidate_File : Virtual_File := Create (+FI.Simple_Name.all);
            begin
               Candidate_File := Locate_Source (Candidate_File);
               if Candidate_File /= No_File then
                  Try_Open (File, +Candidate_File.Full_Name, Success);
                  if Success then
                     FI.Full_Name := new String'(+Candidate_File.Full_Name);
                  end if;
               end if;
            end;
         end if;
      end if;

      --  We do not need to use the rebase mechanism on full names, as this was
      --  done when inserting the file in the file table. If Try_Open does not
      --  succeed with a full name, then either the file does really not exist,
      --  even after searching, or there is another problem when openning.

      FI.Has_Source := Success;
   end Open;

   -------------------
   -- Locate_Source --
   -------------------

   function Locate_Source (File : Virtual_File) return Virtual_File is
      Candidate : Virtual_File;
   begin
      --  If the file exists at his original full path then great!

      if File.Is_Regular_File then
         return File;
      end if;

      --  Try to rebase

      declare
         use GNAT.Regpat;
         E     : Source_Rebase_Entry_Acc := First_Source_Rebase_Entry;
         Name  : constant String := +File.Full_Name;

         Match_Res : GNAT.Regpat.Match_Array (0 .. 0);
      begin
         while E /= null loop
            GNAT.Regpat.Match (E.Old_Prefix.all, Name, Match_Res);
            if Match_Res (0) /= GNAT.Regpat.No_Match then
               Candidate := Create
                 (+(E.New_Prefix.all
                    & Name (Match_Res (0).Last + 1 .. Name'Last)));
               if Candidate.Is_Readable then
                  return Candidate;
               end if;
            end if;

            E := E.Next;
         end loop;
      end;

      --  Try source path

      declare
         E : Source_Search_Entry_Acc := First_Source_Search_Entry;
      begin
         while E /= null loop
            Candidate := Create ((+(E.Prefix.all & '/')) & File.Base_Name);
            if Candidate.Is_Readable then
               return Candidate;
            end if;
            E := E.Next;
         end loop;
      end;

      return No_File;

   end Locate_Source;

   -------------------------------
   -- Writeable_Sloc_To_SCO_Map --
   -------------------------------

   function Writeable_Sloc_To_SCO_Map
     (Index : Source_File_Index;
      Kind  : SCO_Kind) return access Sloc_To_SCO_Maps.Map
   is
      FI : constant File_Info_Access := Get_File (Index);
   begin
      if FI.Sloc_To_SCO_Maps = null then
         FI.Sloc_To_SCO_Maps := new Sloc_To_SCO_Map_Array;
      end if;
      return FI.Sloc_To_SCO_Maps (Kind)'Access;
   end Writeable_Sloc_To_SCO_Map;

   ---------------------
   -- Sloc_To_SCO_Map --
   ---------------------

   function Sloc_To_SCO_Map
     (Index : Source_File_Index;
      Kind  : SCO_Kind) return access constant Sloc_To_SCO_Maps.Map is
   begin
      if Get_File (Index).Kind = Source_File then
         return Writeable_Sloc_To_SCO_Map (Index, Kind);
      else
         return Empty_Sloc_To_SCO_Map'Access;
      end if;
   end Sloc_To_SCO_Map;

   ----------------
   -- To_Display --
   ----------------

   function To_Display (File : File_Info_Access) return Boolean is
   begin
      if Object_Coverage_Enabled then
         return File.Has_Object_Coverage_Info;
      else
         return File.Has_Source_Coverage_Info;
      end if;
   end To_Display;

   -----------------------
   -- Warn_File_Missing --
   -----------------------

   procedure Warn_File_Missing (File : File_Info) is
   begin
      if File.Full_Name /= null then
         Outputs.Warn ("can't open " & File.Full_Name.all);
      else
         Outputs.Warn ("can't find " & File.Simple_Name.all
                       & " in source path");
      end if;
   end Warn_File_Missing;

   ---------------------------------
   -- Checkpoint streaming format --
   ---------------------------------

   --  1) Header:
   --
   --     * [First : Source_File_Index], index of the first file entry;
   --     * [Last : Source_File_Index], index of the last file entry;
   --
   --     Note that, for instance, First => 1, Last => 10 does not mean that 10
   --     file entries are streamed: some may be skipped because they don't
   --     contribute to the source coverage analysis.
   --
   --  2) File table: one group of the following fields times the number of
   --     streamed entries
   --
   --     * [SFI : Source_File_Index], the index of the file entry or
   --       No_Source_File if this marks the end of the sequence (in this case,
   --       there is no further content to parse);
   --     * [Name : String], the name of the file;
   --     * [Kind : File_Kind], the kind of the file;
   --     * [Indexed_Simple_Name : Boolean], whether the simple name for this
   --       file was indexed in the simple name map.
   --     * if Kind = Library_File, [Main_Source : Source_File_Index]
   --     * if Kind = Source_Files, [Ignore_Status : Ignores_Status]
   --                               [Unit.Known : Boolean]
   --                if Unit.Known, [Unit.Name : String]

   ---------------------
   -- Checkpoint_Save --
   ---------------------

   procedure Checkpoint_Save (CSS : access Checkpoint_Save_State) is
      S : access Ada.Streams.Root_Stream_Type'Class renames CSS.Stream;
   begin
      --  1) Output first and last SFIs

      Source_File_Index'Write (S, Files_Table.First_Index);
      Source_File_Index'Write (S, Files_Table.Last_Index);

      --  2) Output file table info for each file
      --  Note that we need LI files there, not just source files with
      --  coverage info.

      for FI_C in Files_Table.Iterate loop
         declare
            FI : File_Info_Access renames File_Vectors.Element (FI_C);
         begin
            Source_File_Index'Write (S, File_Vectors.To_Index (FI_C));
            String'Output (S, (if FI.Full_Name /= null
                               then FI.Full_Name.all
                               else FI.Simple_Name.all));
            File_Kind'Write (S, FI.Kind);
            Boolean'Write (S, FI.Indexed_Simple_Name);
            if FI.Kind = Library_File then
               pragma Assert (FI.Main_Source /= No_Source_File);
               Source_File_Index'Write (S, FI.Main_Source);
            elsif FI.Kind = Source_File then
               Any_Ignore_Status'Write (S, FI.Ignore_Status);
               Boolean'Write (S, FI.Unit.Known);
               if FI.Unit.Known then
                  String'Output
                    (S, Ada.Strings.Unbounded.To_String (FI.Unit.Name));
               end if;
            end if;
         end;
      end loop;

      --  No_Source_File marks end of file table info

      Source_File_Index'Write (S, No_Source_File);
   end Checkpoint_Save;

   ----------------------
   -- Checkpoint_Clear --
   ----------------------

   procedure Checkpoint_Clear is
   begin
      Sorted_Files_Table.Clear;
      for FI of Files_Table loop
         Free (FI.Full_Name);
         Free (FI.Simple_Name);
         if FI.Kind = Source_File then

            --  Line_Info records are allocated in chunks, so we free them
            --  separately: see below

            Free (FI.Sloc_To_SCO_Maps);
         end if;
         Free (FI);
      end loop;
      Files_Table.Clear;
      Files_Table_Frozen := False;

      --  Free Line_Info records

      declare
         Next : Line_Info_Chunk_Access;
      begin
         while Current_LI_Chunk /= null loop
            Next := Current_LI_Chunk.Previous_Chunk;
            for LI of Current_LI_Chunk.Lines
              (Current_LI_Chunk.Lines'First .. Current_LI_Chunk_Next_Line - 1)
            loop
               Free (LI.Obj_Infos);
               Free (LI.SCOs);
               Free (LI.Messages);
               Free (LI.Line_Cache);
            end loop;
            Free (Current_LI_Chunk);
            Current_LI_Chunk := Next;
            Current_LI_Chunk_Next_Line := Line_Info_Chunk_Size;
         end loop;
      end;

      Simple_Name_Map.Clear;
      Full_Name_Map.Clear;
   end Checkpoint_Clear;

   ---------------------
   -- Checkpoint_Load --
   ---------------------

   procedure Checkpoint_Load
     (CLS                  : access Checkpoint_Load_State;
      Ignored_Source_Files : access GNAT.Regexp.Regexp)
   is
      pragma Assert
        (CLS.Purpose = Instrumentation or else Ignored_Source_Files = null);
      S      : access Ada.Streams.Root_Stream_Type'Class renames CLS.Stream;
      Relocs : Checkpoint_Relocations renames CLS.Relocations;

      --  1) Read header

      CP_First_SFI : constant Source_File_Index := Source_File_Index'Input (S);
      CP_Last_SFI  : constant Source_File_Index := Source_File_Index'Input (S);
      CP_SFI       : Source_File_Index;

      --  2) Read file table entries

      type File_Entry (Kind : File_Kind := Source_File) is record
         Name                : String_Access;
         Indexed_Simple_Name : Boolean;
         case Kind is
            when Stub_File =>
               null;
            when Library_File =>
               Main_Source : Source_File_Index;
            when Source_File =>
               Ignore_Status : Any_Ignore_Status;
               Unit          : Owning_Unit;
         end case;
      end record;

      CP_Entries : array (CP_First_SFI .. CP_Last_SFI) of File_Entry;
      --  File_Entries read from checkpoint, with their original (checkpoint)
      --  indices.

      pragma Warnings (Off, CP_SFI);
      --  Kill bogus infinite loop warning (P324-050)

   begin
      Allocate_SFI_Maps (Relocs, CP_First_SFI, CP_Last_SFI);

      --  We first load all file entries, and then import them into the
      --  current context. The reason for this two pass design is that
      --  whether or not we import a given LI file entry depends on info
      --  from the entry for its main source.

      --  Pass 1: load all file entries from checkpoint

      loop
         Source_File_Index'Read (S, CP_SFI);
         exit when CP_SFI = No_Source_File;

         declare
            FE                  : File_Entry renames CP_Entries (CP_SFI);
            Name                : constant String := String'Input (S);
            Kind                : constant File_Kind := File_Kind'Input (S);
            Indexed_Simple_Name : constant Boolean := Boolean'Input (S);
         begin
            case Kind is
               when Stub_File =>
                  FE := (Kind => Stub_File, others => <>);
               when Source_File =>
                  FE := (Kind => Source_File, others => <>);
                  if not Checkpoints.Version_Less (CLS, Than => 5) then

                  --  Dumping ingored source files is only supported from
                  --  Checkpoint version 5.

                     FE.Ignore_Status := Any_Ignore_Status'Input (S);
                     declare
                        use Ada.Strings.Unbounded;
                        Unit_Known : constant Boolean := Boolean'Input (S);
                     begin
                        if Unit_Known then
                           FE.Unit :=
                             (Known => True,
                              Name  => To_Unbounded_String (String'Input (S)));
                        else
                           FE.Unit := (Known => False);
                        end if;
                     end;
                  else
                     FE.Ignore_Status := Unknown;
                     FE.Unit := (Known => False);
                  end if;
               when Library_File =>
                  FE := (Kind => Library_File, others => <>);
                  FE.Main_Source := Source_File_Index'Input (S);
                  pragma Assert (FE.Main_Source /= No_Source_File);
            end case;
            FE.Name := new String'(Name);
            FE.Indexed_Simple_Name := Indexed_Simple_Name;
         end;
      end loop;

      --  Pass 2: process entries, importing them as appropriate

      for CP_SFI in CP_Entries'Range loop
         declare
            FE  : File_Entry renames CP_Entries (CP_SFI);
            SFI : Source_File_Index := No_Source_File;

         begin
            if FE.Name /= null
              and then FE.Kind = Source_File
              and then Ignored_Source_Files /= null
              and then GNAT.Regexp.Match (+Create (+FE.Name.all).Base_Name,
                                          Ignored_Source_Files.all)
            then
               if Switches.Verbose then
                  Put_Line ("Ignored SFI from SID file:" & CP_SFI'Image
                            & " (" & FE.Name.all & ")");
               end if;
               Ignore_SFI (Relocs, CP_SFI);

               --  Still insert this source file in the files table to keep
               --  track of all the files that were ignored.

               if FE.Indexed_Simple_Name then
                  SFI := Get_Index_From_Generic_Name
                     (FE.Name.all, FE.Kind, Indexed_Simple_Name => True);
               else
                  SFI := Get_Index_From_Full_Name
                     (FE.Name.all, FE.Kind, Indexed_Simple_Name => False);
               end if;

               --  But mark it as always ignored

               Consolidate_Ignore_Status (SFI, Always);
               if FE.Unit.Known then
                  Consolidate_Source_File_Unit (SFI, FE.Unit.Name);
               end if;

            else
               if FE.Kind = Library_File then
                  --  Optimization:  If we can find a source file that matches
                  --  the main unit for the library file to import, consider
                  --  they are the same source file and consolidate this
                  --  library with with the one already loaded. This eliminates
                  --  duplicate file entries for library files and thus avoids
                  --  checkpoint files bloat.

                  declare
                     Main_Entry : File_Entry renames
                       CP_Entries (FE.Main_Source);
                     Main_SFI         : constant Source_File_Index :=
                       Get_Index_From_Generic_Name
                         (Name   => Main_Entry.Name.all,
                          Kind   => Source_File,
                          Insert => False);
                  begin
                     if Main_SFI /= No_Source_File then
                        SFI := Get_File (Main_SFI).LI;
                        FE.Main_Source := No_Source_File;
                     end if;
                  end;
               end if;

               if SFI = No_Source_File then
                  --  Delicate circuitry here: if in the original run, a simple
                  --  name was passed to Get_Index_From_Full_Name, then it must
                  --  not be indexed as a simple name here (which is what
                  --  Get_Index_From_Generic_Name would do). This can happen
                  --  when a relative ALI file path is passed to --scos=.

                  if FE.Indexed_Simple_Name then
                     SFI := Get_Index_From_Generic_Name
                        (FE.Name.all, FE.Kind, Indexed_Simple_Name => True);
                  else
                     SFI := Get_Index_From_Full_Name
                        (FE.Name.all, FE.Kind, Indexed_Simple_Name => False);
                  end if;
               end if;

               Set_SFI_Map (Relocs, CP_SFI, SFI);
               if FE.Kind = Source_File then
                  Consolidate_Ignore_Status (SFI, FE.Ignore_Status);
                  if FE.Unit.Known then
                     Consolidate_Source_File_Unit (SFI, FE.Unit.Name);
                  end if;
               end if;

               Set_SFI_Map (Relocs, CP_SFI, SFI);
               if Switches.Verbose then
                  Put_Line ("Remap " & FE.Name.all & ":" & CP_SFI'Img
                            & " ->" & Remap_SFI (Relocs, CP_SFI)'Img);
               end if;
            end if;
         end;
      end loop;

      --  For library files loaded during this execution of gnatcov (not the
      --  merged ones: see the optimization above), propagate the main source
      --  information.

      for CP_SFI in CP_Entries'Range loop
         if not SFI_Ignored (Relocs, CP_SFI) then
            declare
               FE : File_Entry renames CP_Entries (CP_SFI);
               FI : File_Info renames Get_File
                 (Remap_SFI (Relocs, CP_SFI)).all;
            begin
               case FE.Kind is
               when Stub_File | Source_File =>
                  null;

               when Library_File =>
                  if FE.Main_Source /= No_Source_File then
                     FI.Main_Source := FE.Main_Source;
                     Remap_SFI (Relocs, FI.Main_Source);
                  end if;
               end case;
            end;
         end if;
      end loop;

      --  Release the names table

      for FE of CP_Entries loop
         Free (FE.Name);
      end loop;
   end Checkpoint_Load;

end Files_Table;
