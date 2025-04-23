------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2021-2024, AdaCore                     --
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

with GNAT.OS_Lib;
with GNAT.Regpat;

with Strings; use Strings;

package body Paths is

   use all type Unbounded_String;

   On_Windows : constant Boolean := GNAT.OS_Lib.Directory_Separator = '\';

   function Likely_Windows_Path (Path : String) return Boolean;
   --  Whether the provided Path features Windows typical markers

   function Normalize_Windows_Pattern
     (Pattern : String;
      Lower   : Boolean := True) return String;
   --  Assuming Pattern is a Windows file name or globbing pattern, return a
   --  minimally normalized version of it, with all chars converted to lower
   --  case and \ used consistently as the dir separator. Pattern may actually
   --  also be a unit name pattern, in which case the adjustments are expected
   --  not to have an effect.

   function Starts_With_Drive_Pattern (Path : String) return Boolean;
   --  Whether the provided Path string starts with a "<letter>:" sequence

   --------------------
   -- Build_Filename --
   --------------------

   function Build_Filename
     (Dir      : String;
      Filename : String) return String
   is
   begin
      return Dir & GNAT.OS_Lib.Directory_Separator & Filename;
   end Build_Filename;

   function Build_Filename
     (Dir      : String;
      Filename : String) return String_Access
   is
   begin
      return new String'(Build_Filename (Dir, Filename));
   end Build_Filename;

   --------------------------
   -- Fold_Filename_Casing --
   --------------------------

   function Fold_Filename_Casing (Filename : String) return String is
   begin
      if On_Windows then
         return Ada.Characters.Handling.To_Lower (Filename);
      else
         return Filename;
      end if;
   end Fold_Filename_Casing;

   ---------------------------
   -- Canonicalize_Filename --
   ---------------------------

   function Canonicalize_Filename
     (Filename : String;
      Lower    : Boolean := True) return String
   is
      --  Start with a very basic normalization step as for globbing
      --  patterns. If we have a Windows path but are not on a Windows
      --  host, stop there as there's not much more we can reasonably
      --  do. Otherwise, leave it to the GNAT runtime.

      Likely_Windows : constant Boolean := Likely_Windows_Path (Filename);

      Name : constant String :=
        (if Likely_Windows
         then Normalize_Windows_Pattern (Filename, Lower)
         else Filename);
   begin
      if Likely_Windows and then not On_Windows then
         return Name;
      else
         return GNAT.OS_Lib.Normalize_Pathname (Name);
      end if;
   end Canonicalize_Filename;

   function Canonicalize_Filename
     (Filename : String;
      Lower    : Boolean := True) return String_Access is
   begin
      return new String'(Canonicalize_Filename (Filename, Lower));
   end Canonicalize_Filename;

   --------------------
   -- Glob_To_Regexp --
   --------------------

   function Glob_To_Regexp (Pattern : String) return String is
      use GNAT.Regpat;

      Pat : constant String :=
        (if On_Windows
         then Normalize_Windows_Pattern (Pattern)
         else Pattern);

      I   : Natural := Pat'First;
      Res : Unbounded_String;

   begin
      --  Glob to Regexp translation largely inspired from Python's fnmatch
      --  (https://github.com/python/cpython/blob/3.6/Lib/fnmatch.py)
      --  module, with some tweaks here and there.
      --
      --  The biggest differences are the handling of '*', which is translated
      --  as '.*' in fnmatch, but not here (see below), and the fact that both
      --  [!abc] and [^abc] can be used to match anything but a,b or c.
      --  The first form comes from fnmatch, while the other was added for
      --  consistency with the globbing patterns accepted by GNAT.Regexp.
      --
      --  We also automatically translate '\' (backslashes) to forward slashes
      --  ('/'), so that we can transparently match Windows-style file names on
      --  Unix systems and conversely.

      while I <= Pat'Last loop

         case Pat (I) is

            --  '*' cannot be directly translated as '.*' as you do not expect
            --  '*' to match on more than one directory level:
            --  you would expect 'src_*/' to match 'src_1/' and 'src_2/', but
            --  not 'src_1/subdir/'.

            when '*' => Append (Res, "[^/]*");
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
                           Append (Res, "/");
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

            when '/' | '\' =>

               --  De-duplicate directory separators so that "a//b" can match
               --  "a/b".

               if Res = "" or else Element (Res, Length (Res)) /= '/' then
                  Append (Res, '/');
               end if;

            when others =>
               Append (Res, Quote ("" & Pat (I)));
         end case;
         I := I + 1;
      end loop;
      return +Res;
   end Glob_To_Regexp;

   --------------------------
   -- Normalize_For_Regexp --
   --------------------------

   function Normalize_For_Regexp (Filename : String) return String is
   begin
      return Result : String := Canonicalize_Filename (Filename) do
         if On_Windows then
            for C of Result loop
               if C = '\' then
                  C := '/';
               else
                  C := Ada.Characters.Handling.To_Lower (C);
               end if;
            end loop;
         end if;
      end return;
   end Normalize_For_Regexp;

   ----------------------
   -- Is_Absolute_Path --
   ----------------------

   function Is_Absolute_Path (Path : String) return Boolean is

      function Is_Absolute_Unix_Path (Path : String) return Boolean;
      function Is_Absolute_Windows_Path (Path : String) return Boolean;
      --  Predicates to determine if Path looks like a Unix/Windows  absolute
      --  filename.

      ---------------------------
      -- Is_Absolute_Unix_Path --
      ---------------------------

      function Is_Absolute_Unix_Path (Path : String) return Boolean is
      begin
         return Path'Length >= 1 and then Path (Path'First) = '/';
      end Is_Absolute_Unix_Path;

      ------------------------------
      -- Is_Absolute_Windows_Path --
      ------------------------------

      function Is_Absolute_Windows_Path (Path : String) return Boolean is
      begin
         return Path'Length >= 3
                and then Starts_With_Drive_Pattern (Path)
                and then Path (Path'First + 2) in '\' | '/';
      end Is_Absolute_Windows_Path;

   --  Start of processing for Is_Absolute_Path

   begin
      return Is_Absolute_Unix_Path (Path)
             or else Is_Absolute_Windows_Path (Path);
   end Is_Absolute_Path;

   -------------------------
   -- Likely_Windows_Path --
   -------------------------

   function Likely_Windows_Path (Path : String) return Boolean is
   begin
      return Starts_With_Drive_Pattern (Path);
   end Likely_Windows_Path;

   -------------------------------
   -- Normalize_Windows_Pattern --
   -------------------------------

   function Normalize_Windows_Pattern
     (Pattern : String;
      Lower   : Boolean := True) return String is

      --  Force lower case and Backslashify, paying attention not to add
      --  multiple backslashes in a row.
      --
      --  Folding to all lower case maximizes the mapping unicity from
      --  different casing on input, notably useful for path components that
      --  might be coming from the command line, e.g. as a --source-rebase
      --  argument.
      --
      --  At least for dirsep purposes, we craft the new value incrementally.

      use Ada.Characters.Handling;
      Res : Unbounded_String;

      Newchar    : Character;
      New_Is_Sep : Boolean;
      --  The new character we take from Pattern, and whether it
      --  is a dir separator.

      Last_Was_Sep : Boolean := False;
      --  Whether the last character we added to our result was a dir
      --  separator.
   begin
      for I in Pattern'Range loop
         Newchar := (if Pattern (I) = '/' then '\' else Pattern (I));
         New_Is_Sep := Newchar = '\';

         if Lower then
            Newchar := To_Lower (Newchar);
         end if;

         if not New_Is_Sep or else not Last_Was_Sep then
            Append (Res, Newchar);
            Last_Was_Sep := New_Is_Sep;
         end if;
      end loop;

      return +Res;
   end Normalize_Windows_Pattern;

   -------------------------------
   -- Starts_With_Drive_Pattern --
   -------------------------------

   function Starts_With_Drive_Pattern (Path : String) return Boolean is
   begin
      return Path'Length >= 2
             and then Path (Path'First) in 'A' .. 'Z' | 'a' ..  'z'
             and then Path (Path'First + 1) = ':';
   end Starts_With_Drive_Pattern;

   ----------------------------
   -- Workaround_Simple_Name --
   ----------------------------

   function Workaround_Simple_Name (Path : String) return String is
   begin
      --  Return the Path suffix that precedes the first directory separator
      --  according to the current platform. Return the full string if there is
      --  no separator.

      for I in reverse Path'Range loop
         if Path (I) = '/' or else (On_Windows and then Path (I) = '\') then
            return Path (I + 1 .. Path'Last);
         end if;
      end loop;
      return Path;
   end Workaround_Simple_Name;

   ----------------------------
   -- Has_Relative_Component --
   ----------------------------

   function Has_Relative_Component (Path : String) return Boolean is
   begin
      for I in Path'First .. Path'Last - 1 loop
         if Path (I) = '.'
           and then Path (I + 1) in '.' | GNAT.OS_Lib.Directory_Separator
         then
            return False;
         end if;
      end loop;
      return True;
   end Has_Relative_Component;

end Paths;
