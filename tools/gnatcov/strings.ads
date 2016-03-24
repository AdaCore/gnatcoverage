------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2015, AdaCore                     --
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

with Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Streams;
with Ada.Strings.Unbounded;

with GNAT.Strings; use GNAT.Strings;

package Strings is

   subtype File_Name is String_Access;

   function Hash (El : String_Access) return Ada.Containers.Hash_Type;
   --  Compute a hash from El.all

   function Equal (L, R : String_Access) return Boolean;
   --  Assuming that L and R are not null, return true iff L and R designate
   --  identical strings.
   --  We do not redefine "=" here, so that a String_Access can be compared to
   --  null.

   function "<" (L, R : String_Access) return Boolean;
   --  Assuming that L and R are not null, return true iff L.all < R.all

   function Img (I : Integer) return String;
   --  Same as Integer'Image without the starting space character

   function Has_Prefix (S : String; Prefix : String) return Boolean;
   --  True if S starts with Prefix

   function Has_Suffix (S : String; Suffix : String) return Boolean;
   --  True if S ends with Suffix

   function "+"
     (S : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "+"
     (S : Ada.Strings.Unbounded.Unbounded_String)
      return String
      renames Ada.Strings.Unbounded.To_String;

   package String_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Ada.Strings.Unbounded.Unbounded_String,
      "="          => Ada.Strings.Unbounded."=");

   function Vector_To_List
     (V : String_Vectors.Vector)
      return String_List_Access;

   type Unbounded_String_Stream
     (S : access Ada.Strings.Unbounded.Unbounded_String)
   is new Ada.Streams.Root_Stream_Type with record
      Read_Index : Positive := 1;
   end record;

   overriding procedure Read
     (Stream : in out Unbounded_String_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);
   --  Read from position Read_Index in string

   overriding procedure Write
     (Stream : in out Unbounded_String_Stream;
      Item   : Ada.Streams.Stream_Element_Array);
   --  Append to string

private
   pragma Inline (Hash);
   pragma Inline (Equal);
   pragma Inline ("<");
end Strings;
