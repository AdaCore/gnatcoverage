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

with Ada.Directories;

with GNAT.Strings; use GNAT.Strings;

package Paths is

   subtype File_Name is String_Access;

   function Build_Filename
     (Dir      : String;
      Filename : String) return String;
   function Build_Filename
     (Dir      : String;
      Filename : String) return String_Access;
   --  Create a filename from a directory name and a filename. The directory
   --  name is expected to be not empty and the result is _not_ canonicalized
   --  (left to the callers to decide).

   function "/" (Dir, Name : String) return String is
     (Ada.Directories.Compose (Dir, Name));
   --  Likewise, without the "dir shouldn't be empty" constraint but checking
   --  that the path components are valid when not empty.

   function Fold_Filename_Casing (Filename : String) return String;
   --  If this is a Windows host, return the lower-cased filename, otherwise
   --  leave it unchanged.

   function Canonicalize_Filename
     (Filename : String;
      Lower    : Boolean := True) return String;
   function Canonicalize_Filename
     (Filename : String;
      Lower    : Boolean := True) return String_Access;
   --  Assuming Filename is a full pathname to a file, return a normalized
   --  version of it such that different references to the same file map to the
   --  same canonical string as much as possible.
   --
   --  On Windows, if the Lower argument is true, the path will also be
   --  lowercased. If it's false, the original case is kept.

   function Glob_To_Regexp (Pattern : String) return String;
   --  Convert the provided globbing Pattern to a regular expression.  This
   --  might be used for file name or unit name patterns.

   function Normalize_For_Regexp (Filename : String) return String;
   --  Assuming Filename is a full pathname to a file, return a normalized
   --  version of it that is suitable to match against patterns created with
   --  Glob_To_Regexp: de-duplicated directory separators, backslashes
   --  converted to forward slashes, case folded for Windows pathnames.

   function Is_Absolute_Path (Path : String) return Boolean;
   --  Return whether Path is an absolute path. Unlike the GNAT runtime version
   --  of the service, this one always matches both Windows or Unix file path
   --  flavors.

   function Platform_Independent_Basename (Filename : String) return String;
   --  Regardless of the platfrom on which gnatcov runs, extract the basename
   --  of the given absolute filename, which may be a Windows-style filename or
   --  a Unix-style one.

   --  TODO??? Handle Unicode file names

   function Workaround_Simple_Name (Path : String) return String;
   --  TODO??? (eng/toolchain/gnat#603) The native GNAT runtime has a bug on
   --  Unix systems: Ada.Directories.Simple_Name misbehaves in the presence of
   --  backslashes. Provide our own implementation as a workaround.

   function Has_Relative_Component (Path : String) return Boolean;
   --  Returns whether Path contains relative components, i.e. './' or '../',
   --  adjusted for the platform directory separator.

end Paths;
