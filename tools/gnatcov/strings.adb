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

with Ada.Strings.Hash;

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
   -----------------

   function Has_Prefix (S : String; Prefix : String) return Boolean is
      Length : constant Integer := Prefix'Length;
   begin
      return S'Length > Length
        and then S (S'First .. S'First + Length - 1) = Prefix;
   end Has_Prefix;

end Strings;
