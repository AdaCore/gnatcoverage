------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2024, AdaCore                     --
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

--  Helpers to compute hashes (used to implement hash tables)

with Ada.Containers; use Ada.Containers;

package Hashes is

   function Shift_Left (Value : Hash_Type; Amount : Natural) return Hash_Type;
   pragma Import (Intrinsic, Shift_Left);

   function Shift_Right (Value : Hash_Type; Amount : Natural) return Hash_Type;
   pragma Import (Intrinsic, Shift_Right);

   function Combine (Left, Right : Hash_Type) return Hash_Type
   is (Left
       xor (Right
            + 16#9e3779b9#
            + Shift_Left (Left, 6)
            + Shift_Right (Left, 2)));
   --  Combine two hashes into a single hash value. Used to compute the hash of
   --  a container from the hashes of its components.
   --
   --  The implementation is inspired from Boost's hash_combine function
   --  (https://www.boost.org/doc/libs/latest/libs/container_hash/doc/html
   --  /hash.html#notes_hash_combine).

end Hashes;
