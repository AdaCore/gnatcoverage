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

with GNAT.SHA1;

with Logging; use Logging;
with Strings; use Strings;

package Hashes is

   function Shift_Left (Value : Hash_Type; Amount : Natural) return Hash_Type;
   pragma Import (Intrinsic, Shift_Left);

   function Shift_Right (Value : Hash_Type; Amount : Natural) return Hash_Type;
   pragma Import (Intrinsic, Shift_Right);

   function Combine (Left, Right : Hash_Type) return Hash_Type
   is (Left
       xor
         (Right
          + 16#9e3779b9#
          + Shift_Left (Left, 6)
          + Shift_Right (Left, 2)));
   --  Combine two hashes into a single hash value. Used to compute the hash of
   --  a container from the hashes of its components.
   --
   --  The implementation is inspired from Boost's hash_combine function
   --  (https://www.boost.org/doc/libs/latest/libs/container_hash/doc/html
   --  /hash.html#notes_hash_combine).

   type Tracing_Hash is private;
   --  Wrapper around GNAT.SHA1.Context to add the ability to log about the
   --  hashed content.

   function Start_Hash
     (Label : String; Trace : GNATCOLL_Trace) return Tracing_Hash;
   --  Start computing a hash.
   --
   --  Label must be a human readable description of what is hashed, for
   --  logging purposes.
   --
   --  Trace is used to emit logging for this hash computation.

   procedure Update_Hash (Self : in out Tracing_Hash; S : String);
   --  Append S to the hashed content

   function Digest
     (Self : in out Tracing_Hash) return GNAT.SHA1.Binary_Message_Digest;
   --  Return the digest for this hash

private

   type Tracing_Hash is record
      Ctx : GNAT.SHA1.Context;
      --  Actual hash computation context

      Trace : GNATCOLL_Trace;
      --  Trace that controls whether to keep the content used for hashing:
      --  preserving may be costly, so do it only when the relevant trace is
      --  enabled.

      Label : Unbounded_String;
      --  Label for what is hashed

      Buffer : Unbounded_String;
      --  Content used for hashing
   end record;

end Hashes;
