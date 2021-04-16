------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2021, AdaCore                     --
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

with Ada.Characters.Handling;
with Ada.Strings.Hash;

with GNAT.OS_Lib;
with GNAT.Regexp;
with GNAT.Regpat;

package body Strings is

   ----------
   -- Hash --
   ----------

   function Hash (El : String_Access) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (El.all);
   end Hash;

   -----------
   -- Equal --
   -----------

   function Equal (L, R : String_Access) return Boolean is
   begin
      pragma Assert (L /= null and then R /= null);
      return L.all = R.all;
   end Equal;

   ---------
   -- Img --
   ---------

   function Img (I : Integer) return String is
      Result : constant String := Integer'Image (I);
      First  : Positive;
   begin
      if I >= 0 then
         First := Result'First + 1;
      else
         First := Result'First;
      end if;
      return Result (First .. Result'Last);
   end Img;

   ---------
   -- "<" --
   ---------

   function "<" (L, R : String_Access) return Boolean is
   begin
      pragma Assert (L /= null and then R /= null);
      return L.all < R.all;
   end "<";

   ----------------
   -- Has_Prefix --
   ----------------

   function Has_Prefix (S : String; Prefix : String) return Boolean is
      Length : constant Integer := Prefix'Length;
   begin
      return S'Length > Length
        and then S (S'First .. S'First + Length - 1) = Prefix;
   end Has_Prefix;

   ----------------
   -- Has_Suffix --
   ----------------

   function Has_Suffix (S : String; Suffix : String) return Boolean is
      Length : constant Integer := Suffix'Length;
   begin
      return S'Length > Length
        and then S (S'Last - Length + 1 .. S'Last) = Suffix;
   end Has_Suffix;

   --------------------
   -- Vector_To_List --
   --------------------

   function Vector_To_List
     (V : String_Vectors.Vector)
      return String_List_Access
   is
      Result : constant String_List_Access :=
        new String_List (1 .. Natural (V.Length));
      I      : Positive := 1;
   begin
      for S of V loop
         Result (I) := new String'(+S);
         I := I + 1;
      end loop;
      return Result;
   end Vector_To_List;

   --------------------
   -- Glob_To_Regexp --
   --------------------

   function Glob_To_Regexp (Pattern : String) return String is
      use Ada.Strings.Unbounded;
      use GNAT.Regpat;

      Sep : constant Character := GNAT.OS_Lib.Directory_Separator;
      --  Directory separator on this OS. Will also be used to determine if we
      --  should canonicalize the pattern (only done on Windows).

      Escaped_Sep : constant String := (if Sep = '\' then "\\" else "" & Sep);
      --  Same as Sep, but escaped so that we can use it in a regexp

      function Canonicalize_Pattern (Pattern : String) return String;
      --  Canonicalizes a globbing pattern under Windows to capitalize the
      --  drive letter (if it exists) and turn all other characters to lower
      --  case. It also converts all forward-slashes to back-slashes.

      --------------------------
      -- Canonicalize_Pattern --
      --------------------------

      function Canonicalize_Pattern (Pattern : String) return String is
         use Ada.Characters.Handling;

         Res         : String := Pattern;
         Start_Index : Natural := Res'First;
      begin

         --  We only need to canonicalize patterns under Windows

         if Sep = '\' then
            if Res'Length >= 2 and then Res (Start_Index + 1) = ':' then
               Res (Start_Index) := To_Upper (Res (Start_Index));
               Start_Index := Start_Index + 2;
            end if;

            for J in Start_Index .. Res'Last loop
               if Res (J) = '/' then
                  Res (J) := '\';
               else
                  Res (J) := To_Lower (Res (J));
               end if;
            end loop;
         end if;

         return Res;
      end Canonicalize_Pattern;

      Pat : constant String := Canonicalize_Pattern (Pattern);
      Res : Unbounded_String;
      I   : Natural := Pat'First;

   begin
      --  Glob to Regexp translation largely inspired from Python's fnmatch
      --  (https://github.com/python/cpython/blob/3.6/Lib/fnmatch.py)
      --  module, with some tweaks here and there.
      --
      --  The biggest differences are the handling of '*', which is translated
      --  as '.*' in fnmatch, but not here (see bellow), and the fact that both
      --  [!abc] and [^abc] can be used to match anything but a,b or c.
      --  The first form comes from fnmatch, while the other was added for
      --  consistency with the globbing patterns accepted by GNAT.Regexp.

      while I <= Pat'Last loop

         case Pat (I) is

            --  '*' cannot be directly transalted as '.*' as you do not expect
            --  '*' to match on more than one directory level:
            --  you would expect 'src_*/' to match 'src_1/' and 'src_2/', but
            --  not 'src_1/subdir/'.

            when '*' => Append (Res, "[^" & Escaped_Sep & "]*");
            when '?' => Append (Res, ".");
            when '[' =>
               declare
                  End_Cur : Natural := I + 1;
                  --  Will hold the position of the next closing bracket. Note
                  --  that this means that using ']' in a [...] item is not
                  --  allowed.

               begin
                  while End_Cur <= Pat'Last
                    and then Pat (End_Cur) /= ']'
                  loop
                     End_Cur := End_Cur + 1;
                  end loop;
                  if End_Cur > Pat'Last then
                     Append (Res, "\[");
                  else
                     for J in I .. End_Cur loop
                        case Pat (J) is
                        when '\' =>
                           Append (Res, "\\");
                        when '-' =>
                           Append (Res, "\-");
                        when '!' | '^' =>
                           if J = I + 1 then
                              Append (Res, '^');
                           else
                              Append (Res, "\" & Pat (J));
                           end if;
                        when others =>
                           Append (Res, Pat (J));
                        end case;
                     end loop;
                     I := End_Cur;
                  end if;
               end;
            when others =>
               Append (Res, Quote ("" & Pat (I)));
         end case;
         I := I + 1;
      end loop;
      return To_String (Res);
   end Glob_To_Regexp;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Stream : in out Unbounded_String_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
      use Ada.Streams;
      use Ada.Strings.Unbounded;

      Last_Index : constant Natural :=
        Natural'Min (Length (Stream.S.all),
                     Stream.Read_Index + Item'Length - 1);
      Read_Length : constant Natural := Last_Index - Stream.Read_Index + 1;
      Item_S : String (1 .. Read_Length);
      for Item_S'Address use Item'Address;
      pragma Import (Ada, Item_S);
   begin
      Item_S := Slice
        (Stream.S.all,
         Low  => Stream.Read_Index,
         High => Last_Index);
      Stream.Read_Index := Last_Index + 1;
      Last := Item'First + Stream_Element_Offset (Read_Length) - 1;
   end Read;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Stream : in out Unbounded_String_Stream;
      Item   : Ada.Streams.Stream_Element_Array)
   is
      use Ada.Strings.Unbounded;

      Item_S : String (1 .. Item'Length);
      for Item_S'Address use Item'Address;
      pragma Import (Ada, Item_S);
   begin
      Append (Stream.S.all, Item_S);
   end Write;

   ------------------------
   -- Match_Pattern_List --
   ------------------------

   procedure Match_Pattern_List
     (Patterns_List        : String_Vectors.Vector;
      Strings_List         : in out String_Vectors.Vector;
      Patterns_Not_Covered : out String_Vectors.Vector)
   is
      use Ada.Characters.Handling;
      use Ada.Strings.Unbounded;
      use String_Vectors;

      Pattern_Length : constant Natural := Natural (Patterns_List.Length);
      Regexps        : array (0 .. Pattern_Length - 1) of GNAT.Regexp.Regexp;
      --  List of regexps, one for each pattern in Patterns_List

      Regexps_Covered : array (0 .. Pattern_Length - 1) of Boolean :=
        (others => False);
      --  Record which Regexp of Regexps matched at least once

      Matching_Strings : String_Vectors.Vector;
      --  Result holder for Strings_List

      procedure Process_String (C : Cursor);
      --  Try matching the string in Strings_List referenced by C against each
      --  pattern. If there is a match, add the String to Matching_Strings.
      --  Also mark as covered every pattern that matched.

      --------------------
      -- Process_String --
      --------------------

      procedure Process_String (C : Cursor)
      is
         Str       : constant Unbounded_String := +To_Lower (+Element (C));
         Str_Added : Boolean := False;
      begin
         for I in Regexps'Range loop
            if GNAT.Regexp.Match (+Str, Regexps (I)) then
               if not Str_Added then
                  Matching_Strings.Append (Str);
                  Str_Added := True;
               end if;

               --  A unit matching this pattern was found; the pattern is
               --  covered.

               Regexps_Covered (I) := True;

               --  Continue the search in case other patterns match Str so that
               --  we can mark them as covered as well.

            end if;
         end loop;
      end Process_String;

   --  Start of processing for Match_Pattern_List

   begin
      for I in Regexps'Range loop
         Regexps (I) :=
           GNAT.Regexp.Compile
             (Glob_To_Regexp (To_Lower (+(Patterns_List.Element (I)))));
      end loop;

      Strings_List.Iterate (Process_String'Access);

      --  Return the matched strings and the patterns not covered

      Strings_List := Matching_Strings;

      for I in Regexps_Covered'Range loop
         if not Regexps_Covered (I) then
            Patterns_Not_Covered.Append (Patterns_List.Element (I));
         end if;
      end loop;

   end Match_Pattern_List;

end Strings;
