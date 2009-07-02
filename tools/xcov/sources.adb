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

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Fixed;

package body Sources is

   Filenames : Filename_Vectors.Vector;

   package Filename_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => String_Acc,
      Element_Type    => Source_File_Index,
      Hash            => Hash,
      Equivalent_Keys => Equal,
      "="             => "=");

   Filename_Map : Filename_Maps.Map;

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

   ---------------
   -- Get_Index --
   ---------------

   function Get_Index (Name : String) return Source_File_Index is
      use Filename_Maps;
      Nam : aliased String := Name;
      Cur : constant Cursor := Filename_Map.Find (Nam'Unrestricted_Access);
   begin
      if Cur /= No_Element then
         return Element (Cur);

      else
         declare
            New_Name : constant String_Acc := new String'(Name);
         begin
            Filenames.Append (New_Name);
            Filename_Map.Insert (New_Name, Filenames.Last_Index);
            return Filenames.Last_Index;
         end;
      end if;
   end Get_Index;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Index : Source_File_Index) return String is
   begin
      return Filenames.Element (Index).all;
   end Get_Name;

   -----------
   -- Image --
   -----------

   function Image (Sloc : Source_Location) return String is
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      if Sloc = No_Location then
         return "<no loc>";
      else
         return Get_Name (Sloc.Source_File)
           & ":" & Trim (Sloc.Line'Img, Both)
           & ":" & Trim (Sloc.Column'Img, Both);
      end if;
   end Image;

end Sources;
