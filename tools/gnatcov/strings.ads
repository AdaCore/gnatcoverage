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

with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Streams;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Equal_Case_Insensitive;
with Ada.Strings.Unbounded.Hash;
with Ada.Strings.Unbounded.Less_Case_Insensitive;

with GNAT.Strings; use GNAT.Strings;

limited with Checkpoints;

package Strings is

   package US renames Ada.Strings.Unbounded;
   subtype Unbounded_String is US.Unbounded_String;
   use all type Unbounded_String;
   Null_Unbounded_String : Unbounded_String renames US.Null_Unbounded_String;
   function Hash (S : Unbounded_String) return Ada.Containers.Hash_Type
   renames US.Hash;
   function Equal_Case_Insensitive
     (Left, Right : Unbounded_String) return Boolean
   renames US.Equal_Case_Insensitive;
   function Less_Case_Insensitive
     (Left, Right : Unbounded_String) return Boolean
   renames US.Less_Case_Insensitive;

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
   function Img (H : Ada.Containers.Hash_Type) return String;
   --  Same as <type>'Image without the starting space character

   function Has_Prefix (S : String; Prefix : String) return Boolean;
   --  True if S starts with Prefix

   function Has_Suffix (S : String; Suffix : String) return Boolean;
   --  True if S ends with Suffix

   function "+" (S : String) return Unbounded_String
   renames To_Unbounded_String;

   function "+" (S : Unbounded_String) return String renames To_String;

   package String_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => Unbounded_String,
        "="          => "=");

   procedure Read
     (CLS   : in out Checkpoints.Checkpoint_Load_State;
      Value : out String_Vectors.Vector);
   --  Read a String_Vectors.Vector from CLS

   procedure Write
     (CSS   : in out Checkpoints.Checkpoint_Save_State;
      Value : String_Vectors.Vector);
   --  Write a String_Vectors.Vector to CSS

   package String_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Unbounded_String,
        Element_Type => Unbounded_String,
        "<"          => "<",
        "="          => "=");

   procedure Read
     (CLS   : in out Checkpoints.Checkpoint_Load_State;
      Value : out String_Maps.Map);
   --  Read a String_Maps.Map from CLS

   procedure Write
     (CSS : in out Checkpoints.Checkpoint_Save_State; Value : String_Maps.Map);
   --  Write a String_Maps.Map to CSS

   package String_Sets is new
     Ada.Containers.Ordered_Sets
       (Element_Type => Unbounded_String,
        "<"          => Less_Case_Insensitive,
        "="          => Equal_Case_Insensitive);
   --  Case insensitive string set

   package String_Vectors_Maps is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => Unbounded_String,
        Element_Type    => String_Vectors.Vector,
        Equivalent_Keys => "=",
        Hash            => Hash,
        "="             => String_Vectors."=");

   function To_String_Set (V : String_Vectors.Vector) return String_Sets.Set;
   --  Convert the given strings vector to a strings set

   function Vector_To_List
     (V : String_Vectors.Vector) return String_List_Access;

   --  The stream type below allows arbitrary objects to be streamed
   --  from/to an unbounded string, which is used as a buffer of bytes.

   procedure Match_Pattern_List
     (Patterns_List        : String_Vectors.Vector;
      Strings_List         : in out String_Vectors.Vector;
      Patterns_Not_Covered : in out String_Sets.Set);
   --  Try matching each pattern of Patterns_List against each item of
   --  Strings_List (case-insensitively). Every item not matched is removed
   --  from Strings_List. Also, each pattern that matched at least once is
   --  removed from Patterns_Not_Covered.

   type Unbounded_String_Stream (S : access Unbounded_String) is
     new Ada.Streams.Root_Stream_Type
   with record
      Read_Index : Positive := 1;
   end record;

   overriding
   procedure Read
     (Stream : in out Unbounded_String_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);
   --  Read from position Read_Index in string

   overriding
   procedure Write
     (Stream : in out Unbounded_String_Stream;
      Item   : Ada.Streams.Stream_Element_Array);
   --  Append to string

   --  TODO??? Handle Unicode source texts

   procedure Append_From_String
     (Vec : in out String_Vectors.Vector; From : Unbounded_String);
   --  Process From as if it was a comma separated list, and append each
   --  element to Vec.

   function Interpret_Escape_Sequence (Str : String) return String;
   --  Return Str but with every occurrence of an escape sequence
   --  replaced by the corresponding character. Only single character sequences
   --  are supported (no octal nor hexadecimal escape sequences).
   --
   --  Raise Constraint_Error on invalid or unsupported escape sequences.

   function Contains (Str, Substr : String) return Boolean
   is (Ada.Strings.Fixed.Index (Str, Substr) /= 0);
   --  Return whether Str contains the given Substr

   function Index_Non_Blank (Str : String) return Positive;
   --  Overload of Ada.Strings.Fixed.Index_Non_Blank working for any space-
   --  like character, including for instance tabs.

private
   pragma Inline (Hash);
   pragma Inline (Equal);
   pragma Inline ("<");
end Strings;
