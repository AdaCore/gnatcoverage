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

--  Source locations

with Files_Table; use Files_Table;
with Strings;     use Strings;

package body Slocs is

   function Abridged_Image
     (Sloc        : Source_Location;
      Ref         : Source_Location;
      Unique_Name : Boolean := False) return String;
   --  Return the image of Sloc, omitting elements that are common with Ref. If
   --  Unique_Name then use unique filenames, simple ones otherwise.

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Local_Source_Location) return Boolean is
   begin
      if L = No_Local_Location then
         return False;

      elsif R = No_Local_Location then
         return True;
      end if;

      if L.Line < R.Line then
         return True;

      elsif L.Line > R.Line then
         return False;
      end if;

      return L.Column < R.Column;
   end "<";

   function "<" (L, R : Local_Source_Location_Range) return Boolean is
     --  Earlier sorts lower...

     (L.First_Sloc < R.First_Sloc
       or else

     --  For ranges starting at the same point, outer sorts lower: note
     --  the intentionally reversed comparison on Last_Sloc.

     (L.First_Sloc = R.First_Sloc
        and then
      not (L.Last_Sloc <= R.Last_Sloc)));

   function "<" (L, R : Source_Location) return Boolean is
   begin
      --  Note: No_Location must sort higher than any specific source location,
      --  and No_Source_File sorts *lower* than any specific source file index,
      --  so the comparison between L.Source_File and R.Source_File is
      --  intentionally reversed.

      return (L.Source_File > R.Source_File
              or else (L.Source_File = R.Source_File and then L.L < R.L));
   end "<";

   function "<" (L, R : Source_Location_Range) return Boolean is
     (L.Source_File < R.Source_File
       or else
     (L.Source_File = R.Source_File and then (L.L < R.L)));

   ----------
   -- "<=" --
   ----------

   function "<=" (L, R : Local_Source_Location) return Boolean is
     (L < R or else L = R);

   function "<=" (L, R : Source_Location) return Boolean is
     (L < R or else L = R);

   --------------------
   -- Abridged_Image --
   --------------------

   function Abridged_Image
     (Sloc        : Source_Location;
      Ref         : Source_Location;
      Unique_Name : Boolean := False) return String
   is
      function Get_Name (File : Source_File_Index) return String is
        (if Unique_Name
         then Get_Unique_Name (File)
         else Get_Simple_Name (File));

      Show_File, Show_Line, Show_Column : Boolean;

   begin
      if Sloc.L = No_Local_Location then
         return "<no loc>";
      end if;

      Show_File   := Sloc.Source_File /= Ref.Source_File;
      Show_Line   := Show_File
                       or else Sloc.L.Line /= Ref.L.Line;
      Show_Column := Show_Line
                       or else Sloc.L.Column /= Ref.L.Column;

      return
        (if Show_File then Get_Name (Sloc.Source_File) & ":" else "")
        &
        (if Show_Line then Img (Sloc.L.Line) & ":" else "")
        &
        (if Show_Column then Img (Sloc.L.Column) else "");
   end Abridged_Image;

   ----------------
   -- First_Sloc --
   ----------------

   function First_Sloc (R : Source_Location_Range) return Source_Location is
     (To_Sloc (R.Source_File, R.L.First_Sloc));

   -----------
   -- Image --
   -----------

   function Image (Sloc : Source_Location;
                   Unique_Name : Boolean := False) return String is
   begin
      return Abridged_Image
        (Sloc        => Sloc,
         Ref         => No_Location,
         Unique_Name => Unique_Name);
   end Image;

   function Image (Sloc_Range : Source_Location_Range) return String is
      First_Sloc : constant Source_Location :=
                     (Sloc_Range.Source_File, Sloc_Range.L.First_Sloc);
      Last_Sloc  : constant Source_Location :=
                     (Sloc_Range.Source_File, Sloc_Range.L.Last_Sloc);
   begin
      if Sloc_Range.L.First_Sloc = Sloc_Range.L.Last_Sloc then
         return Abridged_Image (Sloc => First_Sloc, Ref  => No_Location);
      else
         return Abridged_Image (Sloc => First_Sloc, Ref => No_Location)
           & "-" & Abridged_Image (Sloc => Last_Sloc, Ref => First_Sloc);
      end if;
   end Image;

   --------------
   -- In_Range --
   --------------

   function In_Range
      (Sloc       : Source_Location;
       Sloc_Range : Source_Location_Range) return Boolean
   is
   begin
      return First_Sloc (Sloc_Range) <= Sloc
               and then
             Sloc <= Last_Sloc (Sloc_Range);
   end In_Range;

   ---------------
   -- Last_Sloc --
   ---------------

   function Last_Sloc (R : Source_Location_Range) return Source_Location is
     (To_Sloc (R.Source_File, R.L.Last_Sloc));

   --------------
   -- To_Range --
   --------------

   function To_Range
     (First_Sloc, Last_Sloc : Source_Location) return Source_Location_Range
   is
      Source_File : Source_File_Index;
   begin
      if First_Sloc.Source_File /= No_Source_File then
         pragma Assert (Last_Sloc.Source_File = First_Sloc.Source_File
                        or else
                        Last_Sloc.Source_File = No_Source_File);
         Source_File := First_Sloc.Source_File;
      else
         Source_File := Last_Sloc.Source_File;
      end if;

      return (Source_File => Source_File,
              L           => (First_Sloc => First_Sloc.L,
                              Last_Sloc  => Last_Sloc.L));
   end To_Range;

   -------------
   -- To_Sloc --
   -------------

   function To_Sloc
     (Source_File : Source_File_Index;
      Local_Sloc  : Local_Source_Location) return Source_Location
   is
   begin
      if Local_Sloc = No_Local_Location then
         return No_Location;
      else
         pragma Assert (Source_File /= No_Source_File);
         return (Source_File, Local_Sloc);
      end if;
   end To_Sloc;

end Slocs;
