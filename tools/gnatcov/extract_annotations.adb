------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2026, AdaCore                     --
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

with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Streams.Stream_IO;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with GNATCOLL.Mmap;
with GNATCOLL.VFS; use GNATCOLL.VFS;
with GPR2.Build.Source;
with GPR2.Project.View;

with Stable_Sloc;

with Command_Line;
with Coverage_Options;    use Coverage_Options;
with Files_Handling;      use Files_Handling;
with Instrument.Ada_Preprocessing;
with Instrument.Ada_Unit; use Instrument.Ada_Unit;
with Instrument.Ada_Unit_Provider;
with Instrument.C;
with Outputs;             use Outputs;
with Project;
with SC_Obligations;      use SC_Obligations;
with Slocs;               use Slocs;
with SS_Annotations;      use SS_Annotations;
with Strings;             use Strings;
with Switches;            use Switches;
with Switches_GPR;
with Temp_Dirs;

procedure Extract_Annotations (Args : Command_Line.Parser.Parsed_Arguments) is

   use Command_Line;
   use type Unbounded_String;

   ------------------------------
   -- In-source annotation set --
   ------------------------------

   type Raw_Annotation is record
      Annot : ALI_Annotation;
      --  Decoded annotation

      Span : Local_Source_Location_Range;
      --  Extent of the text that materializes it (pragma or comment). Its
      --  Last_Sloc is exclusive.
   end record;

   package Annotation_Vectors is new
     Ada.Containers.Indefinite_Vectors (Positive, Raw_Annotation);

   type Annotation_Layout is record
      Del_First, Del_Last : Natural := 0;
      --  Extent, as buffer indexes, of the text that materializes the
      --  annotation: the pragma for Ada, the comment for C/C++. This is the
      --  slice that --in-place removes. Unlike Raw_Annotation.Span.Last_Sloc,
      --  Del_Last is inclusive.
      --
      --  Del_First is 0 when the annotation could not be located in the
      --  buffer, in which case the annotation is skipped altogether.

      Region_Start, Region_End : Natural := 0;
      --  Buffer indexes to anchor a region annotation on, once the text above
      --  is gone

      Point_After, Point_Before : Natural := 0;
      --  Likewise, for the annotations that designate a point rather than a
      --  region

      Partner : Natural := 0;
      --  Index of the annotation that closes the region this one opens, or
      --  opens the region this one closes. 0 when unpaired.

      --  All the indexes above refer to the *original* text. A 0 anchor means
      --  there is no such anchor, which makes the annotation impossible to
      --  relocate.
   end record;

   -----------------------------
   -- Source text bookkeeping --
   -----------------------------

   package Index_Vectors is new Ada.Containers.Vectors (Positive, Positive);

   package Sloc_Range_Vectors is new
     Ada.Containers.Vectors (Positive, Local_Source_Location_Range);

   type Boolean_Array is array (Positive range <>) of Boolean;

   --  Textual model of one source file: enough to convert between source
   --  locations and buffer indexes, to tell code apart from whitespace and
   --  comments, and to rewrite the file without its annotations.

   type Source_Text (Length : Natural) is record
      File   : GNATCOLL.Mmap.Mapped_File;
      Region : GNATCOLL.Mmap.Mapped_Region;
      Mapped : Boolean := True;
      Data   : GNATCOLL.Mmap.Str_Access;
      --  Contents of the file. These are left memory mapped rather than copied
      --  into a String of our own: sources can be big, and we only ever read
      --  them. Unmap below releases the mapping, which must happen before the
      --  file is rewritten.

      Is_Code : Boolean_Array (1 .. Length);
      --  Whether the character at each index is "code", i.e. neither
      --  whitespace, nor part of a comment, nor part of a removed annotation.

      Line_Of : Index_Vectors.Vector;
      --  Line number of the character at each index

      Line_Start : Index_Vectors.Vector;
      --  Index of the first character of each line
   end record;

   type Source_Text_Access is access Source_Text;

   procedure Free_Source is new
     Ada.Unchecked_Deallocation (Source_Text, Source_Text_Access);

   function Load (Filename : String) return Source_Text_Access;
   --  Map Filename in memory and build its textual model. Return null if it
   --  cannot be read.

   procedure Unmap (Self : in out Source_Text);
   --  Release the memory mapping that Self holds on its source file. Does
   --  nothing if it was already released. No Data access is valid afterwards.

   procedure Mark_Blanks (Self : in out Source_Text);
   --  Clear Self.Is_Code for every whitespace character

   procedure Mark_Comment
     (Self : in out Source_Text; Span : Local_Source_Location_Range);
   --  Clear Self.Is_Code over Span, which is expected to designate a comment.
   --
   --  Comments are reported by the language's own lexer rather than looked for
   --  here: gnatcov must consider exactly the same comments as the
   --  instrumenter does, and a hand-written scanner would eventually disagree
   --  with it (C++ raw string literals, for one).

   function To_Index
     (Self : Source_Text; Sloc : Local_Source_Location) return Natural;
   --  Return the index in Self.Data of Sloc, or 0 if it is out of range

   function To_Sloc
     (Self : Source_Text; Index : Positive) return Local_Source_Location;
   --  Reverse of To_Index

   procedure Emit_Annotations
     (File      : Virtual_File;
      Lang      : Any_Language;
      Comments  : Sloc_Range_Vectors.Vector;
      Collected : Annotation_Vectors.Vector;
      In_Place  : Boolean;
      Count     : in out Natural);
   --  Turn the in-source annotations Collected, found in File, into external
   --  annotations added to SS_Annotations.Ext_Annotation_DB, bumping Count for
   --  each one created.
   --
   --  If In_Place, first remove the annotation text from File, and anchor the
   --  created entries on the surrounding code as it stands afterwards.

   ----------
   -- Load --
   ----------

   function Load (Filename : String) return Source_Text_Access is
      use GNATCOLL.Mmap;

      File   : Mapped_File;
      Region : Mapped_Region;
   begin
      begin
         File := Open_Read (Filename);
      exception
         when others =>
            Warn
              ("Could not open "
               & Filename
               & ": no annotation extracted from it");
            return null;
      end;
      Region := Read (File);

      declare
         Length : constant Natural := Last (Region);
         Result : constant Source_Text_Access := new Source_Text (Length);
         Line   : Positive := 1;
      begin
         Result.File := File;
         Result.Region := Region;
         Result.Data := Data (Region);
         Result.Is_Code := (others => True);

         --  Build the line tables. Line_Start (1) is the first character, and
         --  a new line starts right after each line feed.

         Result.Line_Start.Append (1);
         for I in 1 .. Length loop
            Result.Line_Of.Append (Line);
            if Result.Data (I) = ASCII.LF then
               Line := Line + 1;
               Result.Line_Start.Append (I + 1);
            end if;
         end loop;

         return Result;
      end;
   end Load;

   -----------
   -- Unmap --
   -----------

   procedure Unmap (Self : in out Source_Text) is
      use GNATCOLL.Mmap;
   begin
      if Self.Mapped then
         Free (Self.Region);
         Close (Self.File);
         Self.Mapped := False;
      end if;
   end Unmap;

   -----------------
   -- Mark_Blanks --
   -----------------

   procedure Mark_Blanks (Self : in out Source_Text) is
   begin
      for I in 1 .. Self.Length loop
         if Self.Data (I) in ' ' | ASCII.HT | ASCII.CR | ASCII.LF then
            Self.Is_Code (I) := False;
         end if;
      end loop;
   end Mark_Blanks;

   ------------------
   -- Mark_Comment --
   ------------------

   procedure Mark_Comment
     (Self : in out Source_Text; Span : Local_Source_Location_Range)
   is
      First : constant Natural := To_Index (Self, Span.First_Sloc);
      Last  : constant Natural := To_Index (Self, Span.Last_Sloc);
   begin
      if First = 0 then
         return;
      end if;
      for I in First .. (if Last = 0 then Self.Length else Last - 1) loop
         Self.Is_Code (I) := False;
      end loop;
   end Mark_Comment;

   --------------
   -- To_Index --
   --------------

   function To_Index
     (Self : Source_Text; Sloc : Local_Source_Location) return Natural
   is
      Result : Natural;
   begin
      if Sloc.Line not in 1 .. Natural (Self.Line_Start.Length) then
         return 0;
      end if;
      Result := Self.Line_Start (Sloc.Line) + Natural'Max (Sloc.Column, 1) - 1;
      return (if Result in 1 .. Self.Length then Result else 0);
   end To_Index;

   -------------
   -- To_Sloc --
   -------------

   function To_Sloc
     (Self : Source_Text; Index : Positive) return Local_Source_Location
   is
      Line : constant Positive := Self.Line_Of (Index);
   begin
      return (Line => Line, Column => Index - Self.Line_Start (Line) + 1);
   end To_Sloc;

   ----------------------
   -- Emit_Annotations --
   ----------------------

   procedure Emit_Annotations
     (File      : Virtual_File;
      Lang      : Any_Language;
      Comments  : Sloc_Range_Vectors.Vector;
      Collected : Annotation_Vectors.Vector;
      In_Place  : Boolean;
      Count     : in out Natural)
   is
      Filename : constant String := File.Display_Full_Name;

      Src : Source_Text_Access := Load (Filename);

      Layout : array (1 .. Natural (Collected.Length)) of Annotation_Layout;
      --  Buffer indexes computed for each annotation in Collected, in lockstep
      --  with it

      Keep : Boolean_Array (1 .. (if Src = null then 0 else Src.Length)) :=
        (others => True);
      --  Whether each character survives the rewriting

      New_Line_Of   : Index_Vectors.Vector;
      New_Column_Of : Index_Vectors.Vector;
      --  For each original index, its line and column in the rewritten text.
      --  Only meaningful for kept characters.

      function Anchor_Sloc
        (Index : Natural; At_Line_Start : Boolean := False)
         return Local_Source_Location;
      --  Convert an anchor index in the original text into a source
      --  location in the text as it stands after rewriting.
      --
      --  If At_Line_Start, designate the first column of that line rather than
      --  the anchor itself. This matters for the start of an exempted region:
      --  gnatcov rejects an Exempt_On annotation that lies within a statement
      --  obligation, and region bounds are handled line by line anyway, so
      --  pointing at the indentation of the first exempted statement conveys
      --  the same region without overlapping its obligation.

      procedure Compute_Extents;
      --  Fill Del_First / Del_Last from the collected spans

      procedure Compute_Partners;
      --  Pair each region-opening annotation with the one that closes it

      procedure Compute_Anchors;
      --  Fill the anchors of every annotation, assuming its text is gone

      function Free_Sibling (Suffix : String) return String;
      --  Return a filename next to Filename, made of Filename and Suffix, that
      --  no file uses yet. Renaming through such a name must not clobber
      --  anything, and it has to sit in the same directory as Filename so that
      --  the rename stays within one filesystem.

      procedure Rewrite;
      --  Drop the annotation text from the source, write the result back to
      --  Filename, and fill New_Line_Of / New_Column_Of. Releases the mapping
      --  that Src holds on Filename.

      procedure Emit
        (Kind         : Any_Annotation_Kind;
         Annot        : ALI_Annotation;
         First, Last  : Natural;
         Insert_After : Boolean := False);
      --  Create the external annotation of the given Kind spanning the anchors
      --  First .. Last, or warn and skip it if those anchors are missing.

      -----------------
      -- Anchor_Sloc --
      -----------------

      function Anchor_Sloc
        (Index : Natural; At_Line_Start : Boolean := False)
         return Local_Source_Location
      is
         Result : Local_Source_Location;
      begin
         if In_Place then
            Result :=
              (Line => New_Line_Of (Index), Column => New_Column_Of (Index));
         else
            Result := To_Sloc (Src.all, Index);
         end if;

         if At_Line_Start then
            Result.Column := 1;
         end if;
         return Result;
      end Anchor_Sloc;

      ---------------------
      -- Compute_Extents --
      ---------------------

      procedure Compute_Extents is
      begin
         for I in Layout'Range loop
            declare
               L_I  : Annotation_Layout renames Layout (I);
               Span : constant Local_Source_Location_Range :=
                 Collected (I).Span;
               F    : constant Natural := To_Index (Src.all, Span.First_Sloc);
               L    : constant Natural := To_Index (Src.all, Span.Last_Sloc);
            begin
               L_I.Del_First := F;

               --  Span.Last_Sloc is exclusive. When it falls past the end of
               --  the buffer, To_Index returns 0: the annotation then runs to
               --  the very end of the file.

               L_I.Del_Last := (if L = 0 then Src.Length else L - 1);

               if F = 0 or else L_I.Del_Last < F then
                  Warn
                    (Filename
                     & ":"
                     & Image (Span.First_Sloc)
                     & ": could not locate the annotation text, ignoring it");
                  L_I.Del_First := 0;
               end if;
            end;
         end loop;
      end Compute_Extents;

      ----------------------
      -- Compute_Partners --
      ----------------------

      procedure Compute_Partners is
         Pending_Exempt : Natural := 0;
         Pending_Cov    : Natural := 0;

         procedure Close (Pending : in out Natural; I : Positive);
         --  Pair I with Pending, if there is such a pending annotation

         -----------
         -- Close --
         -----------

         procedure Close (Pending : in out Natural; I : Positive) is
         begin
            if Pending /= 0 then
               Layout (I).Partner := Pending;
               Layout (Pending).Partner := I;
               Pending := 0;
            end if;
         end Close;

      begin
         for I in Layout'Range loop
            if Layout (I).Del_First /= 0 then
               case Collected (I).Annot.Kind is
                  when Exempt_On  =>
                     if Pending_Exempt /= 0 then
                        Warn
                          (Filename
                           & ":"
                           & Image (Collected (I).Span.First_Sloc)
                           & ": nested exemption region, ignoring the"
                           & " enclosing one");
                     end if;
                     Pending_Exempt := I;

                  when Exempt_Off =>
                     Close (Pending_Exempt, I);

                  when Cov_Off    =>
                     Pending_Cov := I;

                  when Cov_On     =>
                     Close (Pending_Cov, I);

                  when others     =>
                     null;
               end case;
            end if;
         end loop;
      end Compute_Partners;

      ---------------------
      -- Compute_Anchors --
      ---------------------

      procedure Compute_Anchors is
      begin
         --  The annotation text is about to disappear, so it cannot serve as
         --  an anchor: mark it as non-code before looking for anchors.

         for L_I of Layout loop
            if L_I.Del_First /= 0 then
               for J in L_I.Del_First .. L_I.Del_Last loop
                  Src.Is_Code (J) := False;
                  Keep (J) := False;
               end loop;
            end if;
         end loop;

         for I in Layout'Range loop
            if Layout (I).Del_First /= 0 then
               declare
                  L_I : Annotation_Layout renames Layout (I);

                  First_Line : constant Positive :=
                    Src.Line_Of (L_I.Del_First);
                  Last_Line  : constant Positive := Src.Line_Of (L_I.Del_Last);

                  Line_First : constant Positive :=
                    Src.Line_Start (First_Line);
                  Line_Last  : Natural := Src.Length;

                  Opens : constant Boolean :=
                    Collected (I).Annot.Kind in Exempt_On | Cov_Off;

                  --  A region anchor must stay within the region being
                  --  migrated. Without this, an empty region (an opening
                  --  annotation immediately followed by its closing one)
                  --  would anchor its start on the code that *follows* it and
                  --  its end on the code that *precedes* it, yielding a
                  --  reversed span. Bounding the search leaves both anchors
                  --  at 0 instead, and Emit then reports the region as
                  --  having nothing to attach to.

                  Fwd_Last  : constant Natural :=
                    (if Opens and then L_I.Partner /= 0
                     then Layout (L_I.Partner).Del_First - 1
                     else Src.Length);
                  Bwd_First : constant Positive :=
                    (if not Opens and then L_I.Partner /= 0
                     then Layout (L_I.Partner).Del_Last + 1
                     else 1);
               begin
                  --  Exemption and coverage-disabling regions are line
                  --  granular, and the line holding the annotation is part of
                  --  the region. So look for the region anchors from the start
                  --  of the annotation's first line and up to the end of its
                  --  last line: this keeps the region on the same lines
                  --  whenever those lines hold other code.

                  if Last_Line < Natural (Src.Line_Start.Length) then
                     Line_Last := Src.Line_Start (Last_Line + 1) - 1;
                  end if;

                  for J in Line_First .. Fwd_Last loop
                     if Src.Is_Code (J) then
                        L_I.Region_Start := J;
                        exit;
                     end if;
                  end loop;

                  for J in reverse Bwd_First .. Line_Last loop
                     if Src.Is_Code (J) then
                        L_I.Region_End := J;
                        exit;
                     end if;
                  end loop;

                  --  Fine grained exemptions and buffer annotations designate
                  --  a point rather than a region, and count obligations from
                  --  it, so they must anchor strictly past the annotation.

                  for J in L_I.Del_Last + 1 .. Src.Length loop
                     if Src.Is_Code (J) then
                        L_I.Point_After := J;
                        exit;
                     end if;
                  end loop;

                  for J in reverse 1 .. L_I.Del_First - 1 loop
                     if Src.Is_Code (J) then
                        L_I.Point_Before := J;
                        exit;
                     end if;
                  end loop;
               end;
            end if;
         end loop;
      end Compute_Anchors;

      ------------------
      -- Free_Sibling --
      ------------------

      function Free_Sibling (Suffix : String) return String is
      begin
         for Attempt in 0 .. Natural'Last loop
            declare
               Candidate : constant String :=
                 Filename
                 & Suffix
                 & (if Attempt = 0 then "" else Trim (Attempt'Image, Both));
            begin
               if not Create (+Candidate).Is_Regular_File then
                  return Candidate;
               end if;
            end;
         end loop;
         raise Program_Error;
      end Free_Sibling;

      -------------
      -- Rewrite --
      -------------

      procedure Rewrite is
         use Ada.Streams.Stream_IO;

         Out_File : File_Type;
         S        : Stream_Access;

         Line   : Positive := 1;
         Column : Positive := 1;
      begin
         --  Drop whole lines that hold nothing but removed annotations:
         --  leaving blank lines behind would be needless noise in the
         --  user's sources.

         for L in 1 .. Natural (Src.Line_Start.Length) loop
            declare
               First   : constant Positive := Src.Line_Start (L);
               Last    : Natural := Src.Length;
               Blank   : Boolean := True;
               Dropped : Boolean := False;
            begin
               if L < Natural (Src.Line_Start.Length) then
                  Last := Src.Line_Start (L + 1) - 1;
               end if;

               for J in First .. Last loop
                  if not Keep (J) then
                     Dropped := True;
                  elsif Src.Data (J)
                        not in ' ' | ASCII.HT | ASCII.CR | ASCII.LF
                  then
                     Blank := False;
                  end if;
               end loop;

               if Dropped and then Blank then
                  for J in First .. Last loop
                     Keep (J) := False;
                  end loop;
               end if;
            end;
         end loop;

         --  Now compute the post-rewriting position of every kept character,
         --  and emit the new contents.

         for J in 1 .. Src.Length loop
            New_Line_Of.Append (Line);
            New_Column_Of.Append (Column);
            if Keep (J) then
               if Src.Data (J) = ASCII.LF then
                  Line := Line + 1;
                  Column := 1;
               else
                  Column := Column + 1;
               end if;
            end if;
         end loop;

         declare
            New_Filename : constant String := Free_Sibling (".gnatcov-new");
            New_File     : constant Virtual_File := Create (+New_Filename);

            Old_Filename : constant String := Free_Sibling (".gnatcov-old");
            Old_File     : constant Virtual_File := Create (+Old_Filename);

            Success : Boolean;
         begin
            --  Write the new contents aside, then release the mapping before
            --  swapping the files: truncating a file that is still mapped in
            --  would leave us reading unmapped pages.

            Create (Out_File, Ada.Streams.Stream_IO.Out_File, New_Filename);
            S := Stream (Out_File);
            for J in 1 .. Src.Length loop
               if Keep (J) then
                  Character'Write (S, Src.Data (J));
               end if;
            end loop;
            Close (Out_File);

            Unmap (Src.all);

            --  Now swap the files. Keep the original under Old_Filename
            --  until the new one is in place, so that a failure at any point
            --  leaves the user with their source rather than with nothing:
            --  Rename refuses to overwrite an existing file on some platforms,
            --  so we cannot simply rename over the original.

            File.Rename (Full_Name => Old_File, Success => Success);
            if not Success then
               New_File.Delete (Success);
               Fatal_Error
                 ("Could not rename "
                  & Filename
                  & " out of the way: it has been left untouched");
            end if;

            New_File.Rename (Full_Name => File, Success => Success);
            if not Success then

               --  Put the original back before giving up

               Old_File.Rename (Full_Name => File, Success => Success);
               Fatal_Error
                 ("Could not write back "
                  & Filename
                  & " without its annotations"
                  & (if Success
                     then ": it has been left untouched"
                     else ": the original is left in " & Old_Filename));
            end if;

            Old_File.Delete (Success);
         end;
      end Rewrite;

      ----------
      -- Emit --
      ----------

      procedure Emit
        (Kind         : Any_Annotation_Kind;
         Annot        : ALI_Annotation;
         First, Last  : Natural;
         Insert_After : Boolean := False) is
      begin
         if First = 0 or else Last = 0 then
            Warn
              (Filename
               & ": no source construct left to attach the "
               & Kind_Image (Kind)
               & " annotation to, ignoring it");
            return;
         end if;

         Add_Extracted_Annotation
           (DB           => Ext_Annotation_DB,
            Kind         => Kind,
            Annot        => Annot,
            File         => File,
            Lang         => Lang,
            Span         =>
              (First_Sloc =>
                 Anchor_Sloc
                   (First,
                    At_Line_Start =>
                      In_Place and then Kind in Exempt_Region | Exempt_On),
               Last_Sloc  => Anchor_Sloc (Last)),
            Insert_After => Insert_After);
         Count := Count + 1;
      end Emit;

      --  Start of processing for Emit_Annotations

   begin
      if Src = null then
         return;
      end if;

      Mark_Blanks (Src.all);
      for Span of Comments loop
         Mark_Comment (Src.all, Span);
      end loop;
      Compute_Extents;
      Compute_Partners;

      --  When rewriting, the annotations must be created against the sources
      --  as they stand *after* the rewriting: self-relocating backends hash
      --  the text of the enclosing declaration, so an entry computed on the
      --  original text would no longer match. Hence rewrite first, then emit.

      if In_Place then
         Compute_Anchors;
         Rewrite;
      end if;

      for I in Layout'Range loop
         if Layout (I).Del_First /= 0 then
            declare
               L_I  : Annotation_Layout renames Layout (I);
               A    : constant ALI_Annotation := Collected (I).Annot;
               Span : constant Local_Source_Location_Range :=
                 Collected (I).Span;

               --  Without rewriting, the annotation text stays in place and is
               --  the most faithful anchor there is.

               Own_First : constant Natural :=
                 (if In_Place then L_I.Region_Start else L_I.Del_First);
               Own_Last  : constant Natural :=
                 (if In_Place then L_I.Region_End else L_I.Del_Last);

               Own_Point : constant Natural :=
                 (if In_Place then L_I.Point_After else L_I.Del_First);
               --  Anchor for the annotations that designate a point: strictly
               --  past the removed text, so that the obligations counted from
               --  the annotation are still the same ones.
            begin
               case A.Kind is
                  when Exempt_On                    =>

                     --  The pair is emitted as a single Exempt_Region when its
                     --  Exempt_Off is reached, so there is nothing to do here
                     --  beyond reporting an unterminated region.

                     if L_I.Partner = 0 then
                        Warn
                          (Filename
                           & ":"
                           & Image (Span.First_Sloc)
                           & ": Exempt_On with no matching Exempt_Off,"
                           & " ignoring it");
                     end if;

                  when Exempt_Off                   =>
                     if L_I.Partner = 0 then
                        Warn
                          (Filename
                           & ":"
                           & Image (Span.First_Sloc)
                           & ": Exempt_Off with no matching Exempt_On,"
                           & " ignoring it");
                     else
                        --  Render the pair as a single Exempt_Region, which is
                        --  what external annotations are meant to express.

                        Emit
                          (Kind  => Exempt_Region,
                           Annot => Collected (L_I.Partner).Annot,
                           First =>
                             (if In_Place
                              then Layout (L_I.Partner).Region_Start
                              else Layout (L_I.Partner).Del_First),
                           Last  => Own_Last);
                     end if;

                  when Cov_Off                      =>
                     Emit (Cov_Off, A, Own_First, Own_First);

                  when Cov_On                       =>
                     Emit (Cov_On, A, Own_Last, Own_Last);

                  when Fine_Grained_Annotation_Kind =>
                     Emit (A.Kind, A, Own_Point, Own_Point);

                  when Dump_Buffers | Reset_Buffers =>

                     --  These insert code where the annotation was. Anchor on
                     --  the following construct and insert before it, or, when
                     --  the annotation closes a sequence, on the preceding one
                     --  and insert after it.

                     if not In_Place or else L_I.Point_After /= 0 then
                        Emit
                          (A.Kind,
                           A,
                           Own_Point,
                           Own_Point,
                           Insert_After => False);
                     else
                        Emit
                          (A.Kind,
                           A,
                           L_I.Point_Before,
                           L_I.Point_Before,
                           Insert_After => True);
                     end if;
               end case;
            end;
         end if;
      end loop;

      Unmap (Src.all);
      Free_Source (Src);
   end Emit_Annotations;

   In_Place : constant Boolean := Args.Bool_Args (Opt_In_Place);

   Output_File : Virtual_File;

   Ada_Instrumenter : Ada_Instrumenter_Type;
   Ada_Ready        : Boolean := False;
   --  The Ada instrumenter is only set up if we actually have Ada sources
   --  to process, as setting it up requires a project.

   Temp_Dir : Temp_Dirs.Temporary_Directory;

   Files : File_Vectors.Vector;
   --  Sources to extract annotations from

   Count : Natural := 0;
   --  Number of annotations extracted so far

   procedure Collect_Files;
   --  Fill Files from the command line, or from the project sources

   procedure Setup_Ada_Instrumenter;
   --  Initialize Ada_Instrumenter, or error out if that is not possible

   function Language_Of (File : Virtual_File) return Any_Language;
   --  Return the language of File, from the project if one is loaded and
   --  knows about File, and from its extension otherwise.

   procedure Process_File (File : Virtual_File; Lang : Any_Language);
   --  Extract the annotations of File, rewriting it first if In_Place

   -------------------
   -- Collect_Files --
   -------------------

   procedure Collect_Files is

      procedure Add_Source
        (View : GPR2.Project.View.Object; File : GPR2.Build.Source.Object);
      --  Callback for Enumerate_Sources

      ----------------
      -- Add_Source --
      ----------------

      procedure Add_Source
        (View : GPR2.Project.View.Object; File : GPR2.Build.Source.Object)
      is
         pragma Unreferenced (View);
      begin
         Files.Append (Create (+String (File.Path_Name.Value)));
      end Add_Source;

   begin
      if Args.Remaining_Args.Is_Empty then
         if not Project.Is_Project_Loaded then
            Fatal_Error
              ("Missing -P switch or positional FILES: nothing to extract"
               & " annotations from");
         end if;
         Project.Enumerate_Sources (Add_Source'Access, All_Languages);
      else
         for F of Args.Remaining_Args loop
            declare
               File : constant Virtual_File := Create (+(+F));
            begin
               if not File.Is_Regular_File then
                  Fatal_Error (File.Display_Full_Name & ": no such file");
               end if;
               Files.Append (File);
            end;
         end loop;
      end if;
   end Collect_Files;

   ----------------------------
   -- Setup_Ada_Instrumenter --
   ----------------------------

   procedure Setup_Ada_Instrumenter is
   begin
      if Ada_Ready then
         return;
      end if;

      --  Analyzing Ada sources requires a unit provider, the configuration
      --  pragmas and the preprocessor configuration, all of which we can
      --  only get from a project.

      if not Project.Is_Project_Loaded then
         Fatal_Error
           ("Extracting annotations from Ada sources requires a project"
            & " file: please pass -P");
      end if;

      Temp_Dirs.Create_Temporary_Directory
        (Temp_Dir, "gnatcov_extract_annotations");

      declare
         Dir            : constant String :=
           Temp_Dirs.Directory_Name (Temp_Dir);
         Mapping        : constant String := Dir & "/instr-mapping.json";
         Config_Pragmas : constant String := Dir & "/config-pragmas.json";
         Prep_Data      : constant String := Dir & "/prep-data.json";
      begin
         Instrument.Ada_Unit_Provider.Create_Mapping_File (Mapping);
         Instrument.Ada_Unit.Save_Config_Pragmas_Mapping (Config_Pragmas);
         Instrument.Ada_Preprocessing.Create_Preprocessor_Data_File
           (Prep_Data);

         Ada_Instrumenter :=
           Create_Ada_Instrumenter
             (Default_Charset            =>
                Parser.Value_Or_Null
                  (Args.String_Args (Opt_Ada_Default_Charset)),
              Tag                        => Null_Unbounded_String,
              Config_Pragmas_Mapping     => Config_Pragmas,
              Mapping_Filename           => Mapping,
              Preprocessor_Data_Filename => Prep_Data);
      end;
      Ada_Ready := True;
   end Setup_Ada_Instrumenter;

   -----------------
   -- Language_Of --
   -----------------

   function Language_Of (File : Virtual_File) return Any_Language is
   begin
      if Project.Is_Project_Loaded then
         declare
            Source : constant GPR2.Build.Source.Object :=
              Project.Lookup_Source
                (Create (File.Full_Name, Normalize => True).Display_Full_Name);
         begin
            if Source.Is_Defined then
               return Switches_GPR.To_Language_Or_All (Source.Language);
            end if;
         end;
      end if;

      return Guess_Language (File);
   end Language_Of;

   ------------------
   -- Process_File --
   ------------------

   procedure Process_File (File : Virtual_File; Lang : Any_Language) is
      Filename : constant String := File.Display_Full_Name;

      Collected : Annotation_Vectors.Vector;
      Comments  : Sloc_Range_Vectors.Vector;

      procedure Record_Annotation
        (Annot : ALI_Annotation; Span : Local_Source_Location_Range);
      --  Callback for the language-specific annotation scanners

      procedure Record_Comment (Span : Local_Source_Location_Range);
      --  Callback for the language-specific comment scanners

      -----------------------
      -- Record_Annotation --
      -----------------------

      procedure Record_Annotation
        (Annot : ALI_Annotation; Span : Local_Source_Location_Range) is
      begin
         Collected.Append (Raw_Annotation'(Annot, Span));
      end Record_Annotation;

      --------------------
      -- Record_Comment --
      --------------------

      procedure Record_Comment (Span : Local_Source_Location_Range) is
      begin
         Comments.Append (Span);
      end Record_Comment;

      procedure Record_C_Comment
        (Comment : Unbounded_String; First, Last : Source_Location);
      --  Adapter for Instrument.C.Iterate_Comments, which also hands over
      --  the comment text and full source locations

      ----------------------
      -- Record_C_Comment --
      ----------------------

      procedure Record_C_Comment
        (Comment : Unbounded_String; First, Last : Source_Location)
      is
         pragma Unreferenced (Comment);
      begin
         Comments.Append ((First_Sloc => First.L, Last_Sloc => Last.L));
      end Record_C_Comment;

   begin
      --  First gather the in-source annotations

      case Lang is
         when Ada_Language              =>
            Setup_Ada_Instrumenter;
            Iterate_Source_Annotations
              (Ada_Instrumenter, File, Record_Annotation'Access);

         when C_Language | CPP_Language =>
            Instrument.C.Iterate_Source_Annotations
              (Filename, Lang, Record_Annotation'Access);

         when All_Languages             =>
            return;
      end case;

      if Collected.Is_Empty then
         return;
      end if;

      --  Anchoring the annotations on the surrounding code requires telling
      --  code apart from comments, so ask the language's lexer where the
      --  comments are. Only needed when rewriting.

      if In_Place then
         case Lang is
            when Ada_Language              =>
               Iterate_Comments
                 (Ada_Instrumenter, File, Record_Comment'Access);

            when C_Language | CPP_Language =>
               Instrument.C.Iterate_Comments
                 (Filename, Lang, Record_C_Comment'Access);

            when All_Languages             =>
               null;
         end case;
      end if;

      Emit_Annotations
        (File      => File,
         Lang      => Lang,
         Comments  => Comments,
         Collected => Collected,
         In_Place  => In_Place,
         Count     => Count);
   end Process_File;

   --  Start of processing for Extract_Annotations

begin
   --  Require the -o/--output switch: this is where the extracted
   --  annotations go.

   if not Args.String_Args (Opt_Output).Present then
      Fatal_Error ("Missing --output switch");
   end if;
   Output_File := Create (+(+Args.String_Args (Opt_Output).Value));

   Collect_Files;

   --  Refuse to write the annotations over one of the sources they are
   --  extracted from: that would destroy the source, and with --in-place it
   --  would do so after having rewritten part of the tree.
   declare
      function Key (F : Virtual_File) return String
      is (Create (F.Full_Name, Normalize => True).Display_Full_Name);

      Output_Key : constant String := Key (Output_File);
   begin
      for F of Files loop
         if Key (F) = Output_Key then
            Fatal_Error
              ("--output would overwrite the source " & F.Display_Full_Name);
         end if;
      end loop;
   end;

   for File of Files loop
      Process_File (File, Language_Of (File));
   end loop;

   --  Write the extracted annotations, together with the pre-existing ones
   --  loaded through --external-annotations.

   Stable_Sloc.Write_Entries (Ext_Annotation_DB, Output_File);

   if Count = 0 then
      Warn
        ("No annotation extracted: "
         & Output_File.Display_Full_Name
         & " only holds the annotations passed through"
         & " --external-annotations, if any");
   elsif not Switches.Quiet then
      Ada.Text_IO.Put_Line
        (Count'Image
         & " annotation(s) written to "
         & Output_File.Display_Full_Name);
   end if;
end Extract_Annotations;
