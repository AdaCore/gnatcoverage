------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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
with Ada.Unchecked_Deallocation;

with System;
with System.Address_To_Access_Conversions;

with GNAT.OS_Lib;
with Osint;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with Outputs;
with Perf_Counters; use Perf_Counters;
with Project;

package body Files_Table is

   subtype Valid_Source_File_Index is
     Source_File_Index range First_Source_File .. Source_File_Index'Last;
   package File_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Valid_Source_File_Index,
      Element_Type => File_Info_Access);

   Files_Table           : File_Vectors.Vector;

   Unique_Names_Computed : Boolean := False;
   --  Whether Unique_Name fields for element of Files_Table have been
   --  computed. It is invalid to add files to the table after this is set
   --  to True.

   procedure Build_Unique_Names;
   --  Compute unique names for all files in the table. Also take care of
   --  setting Unique_Names_Computed.

   procedure Expand_Line_Table (FI : File_Info_Access; Line : Positive);

   package Filename_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Virtual_File,
      Element_Type    => Source_File_Index,
      Hash            => Full_Name_Hash,
      Equivalent_Keys => "=",
      "="             => "=");

   type Simple_Name_Info is record
      Matches : Positive;
      --  Number of files that have this base name

      File    : Source_File_Index;
      --  The registered file, if any
   end record;
   --  Information about a source file base name registered in Simple_Name_Map

   package Simple_Name_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Virtual_File,
      Element_Type    => Simple_Name_Info,
      Hash            => Full_Name_Hash,
      Equivalent_Keys => "=",
      "="             => "=");

   Simple_Name_Map : Simple_Name_Maps.Map;
   Full_Name_Map   : Filename_Maps.Map;

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

   type Source_Rebase_Entry;
   type Source_Rebase_Entry_Acc is access Source_Rebase_Entry;
   type Source_Rebase_Entry is record
      Old_Prefix : String_Access;
      New_Prefix : String_Access;
      Next : Source_Rebase_Entry_Acc;
   end record;

   First_Source_Rebase_Entry : Source_Rebase_Entry_Acc := null;
   Last_Source_Rebase_Entry  : Source_Rebase_Entry_Acc := null;

   First_Source_Search_Entry : Source_Search_Entry_Acc := null;
   Last_Source_Search_Entry  : Source_Search_Entry_Acc := null;

   --  Executable search prefix

   Exec_Search_Prefix : Virtual_File;

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
      E : Source_Rebase_Entry_Acc;
   begin
      E := new Source_Rebase_Entry'(Old_Prefix => new String'(Old_Prefix),
                                    New_Prefix => new String'(New_Prefix),
                                    Next => null);

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
      E := new Source_Search_Entry'(Prefix => new String'(Prefix),
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

   ---------------------------
   -- Canonicalize_Filename --
   ---------------------------

   function Canonicalize_Filename (Filename : String) return String_Access
   is
      use Ada.Characters.Handling;
      Res : String := Filename;
   begin
      if Res'Length > 2 and then Res (Res'First + 1) = ':' then
         --  Looks like a Windows file name

         --  Capitalize the driver letter

         Res (Res'First) := To_Upper (Res (Res'First));

         --  Lower case letters, back-slashify

         for I in Res'First + 2 .. Res'Last loop
            if Is_Upper (Res (I)) then
               Res (I) := To_Lower (Res (I));
            elsif Res (I) = '/' then
               Res (I) := '\';
            end if;
         end loop;
      end if;
      return new String'(Res);
   end Canonicalize_Filename;

   --------------------
   -- Build_Filename --
   --------------------

   function Build_Filename
     (Dir      : String;
      Filename : String) return String_Access
   is
   begin
      return Canonicalize_Filename (Dir & '/' & Filename);
   end Build_Filename;

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

   -----------------------
   -- Expand_Line_Table --
   -----------------------

   procedure Expand_Line_Table (FI : File_Info_Access; Line : Positive) is
      New_Lines : Source_Line_Array_Acc;

   begin
      if Line <= FI.Lines.Last_Index then
         return;
      end if;

      New_Lines :=
        new Source_Line_Array'
          (FI.Lines.Last_Index + 1 .. Line =>
             (State => (others => No_Code), others => <>));

      Bump (Line_Table_Alloc);
      Bump (Line_Table_Alloc_Size, How_Many => New_Lines'Length);

      FI.Lines.Reserve_Capacity (Ada.Containers.Count_Type (Line));
      for J in New_Lines'Range loop
         FI.Lines.Append (New_Lines (J)'Access);
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
      for Index in Files_Table.First_Index .. Files_Table.Last_Index loop
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

   ---------------------
   -- Fill_Line_Cache --
   ---------------------

   procedure Fill_Line_Cache (FI : File_Info_Access) is
      F          : File_Type;
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

   function Get_Full_Name (Index : Source_File_Index) return String is
      Full_Name : constant String_Access :=
        Files_Table.Element (Index).Full_Name;
   begin
      if Full_Name /= null then
         return Full_Name.all;
      else
         Outputs.Fatal_Error ("No full path name for "
                              & Files_Table.Element (Index).Simple_Name.all);
         return "";
      end if;
   end Get_Full_Name;

   ------------------------------
   -- Get_Index_From_Full_Name --
   ------------------------------

   function Get_Index_From_Full_Name
     (Full_Name         : String;
      Insert            : Boolean := True;
      Index_Simple_Name : Boolean := True) return Source_File_Index
   is
      use Filename_Maps;
      use Simple_Name_Maps;

      Full_Path : constant Virtual_File := Create (+Full_Name);

      Cur         : Filename_Maps.Cursor;
      Res         : Source_File_Index;
      Info        : File_Info_Access;
      Info_Simple : File_Info_Access;

   begin
      Cur := Full_Name_Map.Find (Full_Path);

      if Cur /= Filename_Maps.No_Element then
         Res := Element (Cur);
         return Res;
      end if;

      --  Here if full name not found, try again with simple name

      declare
         Simple_Path : constant Virtual_File := Create (Base_Name (Full_Path));
         Simple_Cur  : Simple_Name_Maps.Cursor :=
            Simple_Name_Map.Find (Simple_Path);
      begin

         if Simple_Cur /= Simple_Name_Maps.No_Element
           and then Element (Simple_Cur).File /= No_Source_File
         then
            Res := Element (Simple_Cur).File;

            --  If we are not allowed to insert something, do not modify
            --  existing entries.

            if not Insert then
               return Res;
            end if;

            Info_Simple := Files_Table.Element (Res);

            if Info_Simple.Full_Name = null then
               Info_Simple.Full_Name :=
                  new String'(+GNATCOLL.VFS.Full_Name (Full_Path));
               Full_Name_Map.Insert (Full_Path, Res);
               return Res;
            else
               if Create (+Info_Simple.Full_Name.all) /= Full_Path then
                  Put_Line ("Warning: same base name for files:");
                  Put_Line ("  " & (+GNATCOLL.VFS.Full_Name (Full_Path)));
                  Put_Line ("  " & Info_Simple.Full_Name.all);
               end if;
            end if;

         --  Here if not found by simple name, either

         elsif not Insert then
            return No_Source_File;
         end if;

         --  If we reach this point, we are inserting a new file into the table

         pragma Assert (not Unique_Names_Computed);

         Info := new File_Info'
           (Full_Name                =>
               new String'(+GNATCOLL.VFS.Full_Name (Full_Path)),
            Simple_Name              =>
               new String'(+GNATCOLL.VFS.Full_Name (Simple_Path)),
            Unique_Name              => null,
            Has_Source               => True,
            Lines                    => (Source_Line_Vectors.Empty_Vector
                                           with null record),
            Stats                    => (others => 0),
            Sloc_To_SCO_Maps         => null,
            Has_Source_Coverage_Info => False,
            Has_Object_Coverage_Info => False);

         Files_Table.Append (Info);
         Res := Files_Table.Last_Index;

         --  If needed, add an entry into the simple name map. It will help
         --  aliasing computation. Do not register the file itself if not
         --  told to.

         if Simple_Cur = Simple_Name_Maps.No_Element then
            declare
               Inserted : Boolean;
               File_Index : constant Source_File_Index :=
                 (if Index_Simple_Name then Res else No_Source_File);
            begin
               Simple_Name_Map.Insert
                 (Simple_Path,
                  (Matches => 1, File => File_Index),
                  Simple_Cur,
                  Inserted);
               pragma Assert (Inserted);
            end;
            --  The alias number already contains the correct value

         else
            --  The entry already exists: just update its Match count and set
            --  the correct value to the alias number to the current file.

            declare
               Simple_Entry : Simple_Name_Info := Element (Simple_Cur);
            begin
               Simple_Entry.Matches := Simple_Entry.Matches + 1;
               Simple_Name_Map.Replace_Element (Simple_Cur, Simple_Entry);
            end;
         end if;

         Full_Name_Map.Insert (Full_Path, Res);
         return Res;
      end;
   end Get_Index_From_Full_Name;

   --------------------------------
   -- Get_Index_From_Simple_Name --
   --------------------------------

   function Get_Index_From_Simple_Name
     (Simple_Name : String;
      Insert      : Boolean := True) return Source_File_Index
   is
      use Filename_Maps;
      use Simple_Name_Maps;

      Simple_Path : constant Virtual_File := Create (+Simple_Name);

      Cur  : constant Simple_Name_Maps.Cursor :=
        Simple_Name_Map.Find (Simple_Path);
      Res  : Source_File_Index;
      Info : File_Info_Access;
   begin
      if Cur /= Simple_Name_Maps.No_Element then
         return Element (Cur).File;
      end if;

      if not Insert then
         return No_Source_File;
      end if;

      pragma Assert (not Unique_Names_Computed);

      Info := new File_Info'(Simple_Name              =>
                                new String'(+Full_Name (Simple_Path)),
                             Full_Name                => null,
                             Unique_Name              => null,
                             Has_Source               => True,
                             Lines                    =>
                                (Source_Line_Vectors.Empty_Vector
                                    with null record),
                             Stats                    => (others => 0),
                             Sloc_To_SCO_Maps         => null,
                             Has_Source_Coverage_Info => False,
                             Has_Object_Coverage_Info => False);

      Files_Table.Append (Info);
      Res := Files_Table.Last_Index;
      Simple_Name_Map.Insert (Simple_Path, (Matches => 1, File => Res));

      return Res;
   end Get_Index_From_Simple_Name;

   ---------------------------------
   -- Get_Index_From_Generic_Name --
   ---------------------------------

   function Get_Index_From_Generic_Name
     (Name : String) return Source_File_Index
   is
      File_Name : constant Virtual_File := Create (+Name);
   begin
      if Is_Absolute_Path (File_Name) then

         --  Library files are the only source of file names that may
         --  contain base names (as opposed to absolute file names). Thus,
         --  if we find an absolute path here, do not bother index its base
         --  name, since it will potentially introduce base name clashes for
         --  no benefit.

         return Get_Index_From_Full_Name (Name, Index_Simple_Name => False);
      else
         return Get_Index_From_Simple_Name (Name);
      end if;
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
      if Index in File.Lines.First_Index .. File.Lines.Last_Index then
         return File.Lines.Element (Index);
      else
         --  Get_Line may be called with no source information loaded, for
         --  example when emitting a diagnostic with sloc information based on
         --  SCOs only. In that case return a null pointer, since we do not
         --  have any available Line_Info structure.

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
   -- Build_Unique_Names --
   ------------------------

   procedure Build_Unique_Names is

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
         V_Key       : constant Virtual_File :=
           Create (+Key.all);
         Cur         : constant Cursor := Map.Find (V_Key);
         File_Set   : File_Set_Access;
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

      --  Start of processing for Build_Unique_Names

   begin
      --  First, build the alias map: conflicting files will get grouped under
      --  a single alias set.

      for File of Files_Table loop
         Add_File (File.Simple_Name, File, Alias_Map);
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
               --  so that we can see what files still confilct with new
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

      Clear (Alias_Map);
      Unique_Names_Computed := True;
   end Build_Unique_Names;

   ---------------------
   -- Get_Unique_Name --
   ---------------------

   function Get_Unique_Name (Index : Source_File_Index) return String is
      File : File_Info renames Get_File (Index).all;

   begin
      if not Unique_Names_Computed then
         Build_Unique_Names;
      end if;

      return File.Unique_Name.all;
   end Get_Unique_Name;

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
        (File    : in out File_Type;
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

      end if;

      --  Try to rebase

      if not Success then
         declare
            E : Source_Rebase_Entry_Acc := First_Source_Rebase_Entry;
            First : constant Positive := Name'First;
         begin
            while E /= null loop
               if Name'Length > E.Old_Prefix'Length
                 and then (Name (First .. First + E.Old_Prefix'Length - 1)
                           = E.Old_Prefix.all)
               then
                  Try_Open (File,
                            E.New_Prefix.all
                            & Name (First + E.Old_Prefix'Length
                                    .. Name'Last),
                            Success);
                  exit when Success;
               end if;

               E := E.Next;
            end loop;
         end;
      end if;

      --  Try source path

      if not Success then
         declare
            E : Source_Search_Entry_Acc := First_Source_Search_Entry;
         begin
            while E /= null loop
               Try_Open (File, E.Prefix.all & '/' & FI.Simple_Name.all,
                         Success);

               if Success then
                  FI.Full_Name :=
                    new String'(E.Prefix.all & '/' & FI.Simple_Name.all);
                  exit;
               end if;

               E := E.Next;
            end loop;
         end;
      end if;

      FI.Has_Source := Success;
   end Open;

   ---------------------
   -- Sloc_To_SCO_Map --
   ---------------------

   function Sloc_To_SCO_Map
     (Index : Source_File_Index;
      Kind  : SCO_Kind) return access Sloc_To_SCO_Maps.Map
   is
      FI : constant File_Info_Access := Get_File (Index);
   begin
      if FI.Sloc_To_SCO_Maps = null then
         FI.Sloc_To_SCO_Maps := new Sloc_To_SCO_Map_Array;
      end if;
      return FI.Sloc_To_SCO_Maps (Kind)'Access;
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

end Files_Table;
