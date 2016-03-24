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
   ----------------

   function Has_Prefix (S : String; Prefix : String) return Boolean is
      Length : constant Integer := Prefix'Length;
   begin
      return S'Length > Length
        and then S (S'First .. S'First + Length - 1) = Prefix;
   end Has_Prefix;

   ----------------
   -- Has_Suffix --
   ----------------

   function Has_Suffix (S : String; Suffix : String) return Boolean is
      Length : constant Integer := Suffix'Length;
   begin
      return S'Length > Length
        and then S (S'Last - Length + 1 .. S'Last) = Suffix;
   end Has_Suffix;

   --------------------
   -- Vector_To_List --
   --------------------

   function Vector_To_List
     (V : String_Vectors.Vector)
      return String_List_Access
   is
      Result : constant String_List_Access :=
        new String_List (1 .. Natural (V.Length));
      I      : Positive := 1;
   begin
      for S of V loop
         Result (I) := new String'(+S);
         I := I + 1;
      end loop;
      return Result;
   end Vector_To_List;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Stream : in out Unbounded_String_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
      use Ada.Streams;
      use Ada.Strings.Unbounded;

      Last_Index : constant Natural :=
        Natural'Min (Length (Stream.S.all),
                     Stream.Read_Index + Item'Length - 1);
      Read_Length : constant Natural := Last_Index - Stream.Read_Index + 1;
      Item_S : String (1 .. Read_Length);
      for Item_S'Address use Item'Address;
      pragma Import (Ada, Item_S);
   begin
      Item_S := Slice
        (Stream.S.all,
         Low  => Stream.Read_Index,
         High => Last_Index);
      Stream.Read_Index := Last_Index + 1;
      Last := Item'First + Stream_Element_Offset (Read_Length) - 1;
   end Read;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Stream : in out Unbounded_String_Stream;
      Item   : Ada.Streams.Stream_Element_Array)
   is
      use Ada.Strings.Unbounded;

      Item_S : String (1 .. Item'Length);
      for Item_S'Address use Item'Address;
      pragma Import (Ada, Item_S);
   begin
      Append (Stream.S.all, Item_S);
   end Write;

end Strings;
