------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2006-2024, AdaCore                     --
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
with Ada.Unchecked_Conversion;

package body Hex_Images is
   type Hex_Str_Type is array (0 .. 15) of Character;
   Hexdigits : constant Hex_Str_Type := "0123456789abcdef";

   function Hex_Image (B : Unsigned_8) return String is
      Res : String (1 .. 2);
   begin
      for I in 1 .. 2 loop
         Res (I) := Hexdigits
           (Natural (Shift_Right (B, 8 - 4 * I) and 16#0f#));
      end loop;
      return Res;
   end Hex_Image;

   function Conv is new Ada.Unchecked_Conversion
     (Source => Integer_32, Target => Unsigned_32);

   function Hex_Image (W : Unsigned_32) return String is
      Res : String (1 .. 8);
   begin
      for I in 1 .. 8 loop
         Res (I) := Hexdigits
           (Natural (Shift_Right (W, 32 - 4 * I) and 16#0f#));
      end loop;
      return Res;
   end Hex_Image;

   function Hex_Image (W : Unsigned_64) return String is
      Res : String (1 .. 16);
   begin
      for I in 1 .. 16 loop
         Res (I) := Hexdigits
           (Natural (Shift_Right (W, 64 - 4 * I) and 16#0f#));
      end loop;
      return Res;
   end Hex_Image;

   function Hex_Image (W : Unsigned_16) return String is
      Res : String (1 .. 4);
   begin
      for I in 1 .. 4 loop
         Res (I) := Hexdigits
           (Natural (Shift_Right (W, 16 - 4 * I) and 16#0f#));
      end loop;
      return Res;
   end Hex_Image;

   function Hex_Image (W : Integer_32) return String is
   begin
      return Hex_Image (Conv (W));
   end Hex_Image;

   ------------------------
   -- Strip_Zero_Padding --
   ------------------------

   function Strip_Zero_Padding (Image : String) return String is
      First_Meaningful_Digit : Natural := Image'First;
   begin
      if Image'Length = 0 then
         return Image;
      end if;

      loop
         if First_Meaningful_Digit >= Image'Last then
            return (1 => Image (Image'Last));

         elsif Image (First_Meaningful_Digit) /= '0' then
            return Image (First_Meaningful_Digit .. Image'Last);
         end if;

         First_Meaningful_Digit := First_Meaningful_Digit + 1;
      end loop;
   end Strip_Zero_Padding;

end Hex_Images;
