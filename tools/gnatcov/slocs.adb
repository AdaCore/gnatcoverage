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
     (Sloc : Source_Location;
      Ref  : Source_Location) return String;
   --  Return the image of Sloc, omitting elements that are common with Ref

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Source_Location) return Boolean is
   begin
      if L = No_Location then
         return False;
      elsif R = No_Location then
         return True;
      end if;

      if L.Source_File < R.Source_File then
         return True;

      elsif L.Source_File > R.Source_File then
         return False;
      end if;

      if L.Line < R.Line then
         return True;

      elsif L.Line > R.Line then
         return False;
      end if;

      return L.Column < R.Column;
   end "<";

   function "<" (L, R : Source_Location_Range) return Boolean is
   begin
      return L.First_Sloc < R.First_Sloc
               or else
             (L.First_Sloc = R.First_Sloc and then L.Last_Sloc < R.Last_Sloc);
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (L, R : Source_Location) return Boolean is
   begin
      return L < R or else L = R;
   end "<=";

   --------------------
   -- Abridged_Image --
   --------------------

   function Abridged_Image
     (Sloc : Source_Location;
      Ref  : Source_Location) return String
   is
      Show_File, Show_Line, Show_Column : Boolean;

   begin
      if Sloc = No_Location then
         return "<no loc>";
      end if;

      Show_File   := Sloc.Source_File /= Ref.Source_File;
      Show_Line   := Show_File or else Sloc.Line /= Ref.Line;
      Show_Column := Show_Line or else Sloc.Column /= Ref.Column;

      return
        (if Show_File then Get_Simple_Name (Sloc.Source_File) & ":" else "")
        &
        (if Show_Line then Img (Sloc.Line) & ":" else "")
        &
        (if Show_Column then Img (Sloc.Column) else "");
   end Abridged_Image;

   -----------
   -- Image --
   -----------

   function Image (Sloc : Source_Location) return String is
   begin
      return Abridged_Image (Sloc, Ref => No_Location);
   end Image;

   function Image (Sloc_Range : Source_Location_Range) return String is
   begin
      if Sloc_Range.First_Sloc = Sloc_Range.Last_Sloc then
         return Abridged_Image (Sloc_Range.First_Sloc, Ref => No_Location);
      else
         return Abridged_Image
           (Sloc_Range.First_Sloc, Ref => No_Location)
           & "-" & Abridged_Image
                     (Sloc_Range.Last_Sloc, Ref => Sloc_Range.First_Sloc);
      end if;
   end Image;

end Slocs;
