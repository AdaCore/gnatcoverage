------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2006-2017, AdaCore                     --
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
with Interfaces; use Interfaces;

package Hex_Images is
   function Hex_Image (W : Integer_32) return String;
   function Hex_Image (W : Unsigned_32) return String;
   function Hex_Image (B : Unsigned_8) return String;
   function Hex_Image (W : Unsigned_16) return String;
   function Hex_Image (W : Unsigned_64) return String;

   function Strip_Zero_Padding (Image : String) return String;
   --  Assuming Image is a left zero padded hexadecimal image (e.g. 01) of a
   --  number, return the same image with unnecessary leading zeros stripped
   --  (e.g. 1).
end Hex_Images;
