------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                     Copyright (C) 2008-2009, AdaCore                     --
--                                                                          --
-- Couverture is free software; you can redistribute it  and/or modify it   --
-- under terms of the GNU General Public License as published by the Free   --
-- Software Foundation; either version 2, or (at your option) any later     --
-- version.  Couverture is distributed in the hope that it will be useful,  --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details. You  should  have  received a copy of the GNU --
-- General Public License  distributed with GNAT; see file COPYING. If not, --
-- write  to  the Free  Software  Foundation,  59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
------------------------------------------------------------------------------

--  Source locations

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Files_Table; use Files_Table;

package body Sources is

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
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Ada.Strings.Unbounded;

      Result : Unbounded_String;
      Show_Line, Show_Column : Boolean;
   begin
      if Sloc = No_Location then
         return "<no loc>";
      end if;

      if Sloc.Source_File /= Ref.Source_File then
         Result := To_Unbounded_String (Get_Name (Sloc.Source_File) & ":");
         Show_Line   := True;
         Show_Column := True;
      else
         Show_Line   := Sloc.Line /= Ref.Line;
         Show_Column := Show_Line or else Sloc.Column /= Ref.Column;
      end if;

      if Show_Line then
         Result := Result & Trim (Sloc.Line'Img, Both) & ":";
      end if;

      if Show_Column then
         Result := Result & Trim (Sloc.Column'Img, Both);
      end if;

      return To_String (Result);
   end Abridged_Image;

   -----------
   -- Image --
   -----------

   function Image (Sloc : Source_Location) return String is
   begin
      return Abridged_Image (Sloc, Ref => No_Location);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (First_Sloc, Last_Sloc : Source_Location) return String is
   begin
      if First_Sloc = Last_Sloc then
         return Abridged_Image (First_Sloc, Ref => No_Location);
      else
         return Abridged_Image (First_Sloc, Ref => No_Location)
           & "-" & Abridged_Image (Last_Sloc, Ref => First_Sloc);
      end if;
   end Image;

end Sources;
