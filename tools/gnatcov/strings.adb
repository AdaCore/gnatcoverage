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

with Ada.Characters.Handling;
with Ada.Strings.Hash;

with GNAT.Regexp;

with Checkpoints; use Checkpoints;
with Paths;

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

   function Img (H : Ada.Containers.Hash_Type) return String is
      Result : constant String := Ada.Containers.Hash_Type'Image (H);
   begin
      return Result (Result'First + 1 .. Result'Last);
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
      return
        S'Length >= Length
        and then S (S'First .. S'First + Length - 1) = Prefix;
   end Has_Prefix;

   ----------------
   -- Has_Suffix --
   ----------------

   function Has_Suffix (S : String; Suffix : String) return Boolean is
      Length : constant Integer := Suffix'Length;
   begin
      return
        S'Length >= Length and then S (S'Last - Length + 1 .. S'Last) = Suffix;
   end Has_Suffix;

   ----------
   -- Read --
   ----------

   procedure Read
     (CLS   : in out Checkpoints.Checkpoint_Load_State;
      Value : out String_Vectors.Vector)
   is
      procedure Read is new
        Read_Vector
          (Index_Type   => Natural,
           Element_Type => Unbounded_String,
           "="          => "=",
           Vectors      => String_Vectors,
           Read_Element => Read);
   begin
      Read (CLS, Value);
   end Read;

   procedure Read
     (CLS   : in out Checkpoints.Checkpoint_Load_State;
      Value : out String_Maps.Map)
   is
      procedure Read is new
        Read_Map
          (Key_Type     => Unbounded_String,
           Element_Type => Unbounded_String,
           Map_Type     => String_Maps.Map,
           Clear        => String_Maps.Clear,
           Insert       => String_Maps.Insert,
           Read_Key     => Read,
           Read_Element => Read);
   begin
      Read (CLS, Value);
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (CSS   : in out Checkpoints.Checkpoint_Save_State;
      Value : String_Vectors.Vector)
   is
      procedure Write is new
        Write_Vector
          (Index_Type    => Natural,
           Element_Type  => Unbounded_String,
           "="           => "=",
           Vectors       => String_Vectors,
           Write_Element => Write);
   begin
      Write (CSS, Value);
   end Write;

   procedure Write
     (CSS : in out Checkpoints.Checkpoint_Save_State; Value : String_Maps.Map)
   is
      procedure Write is new
        Write_Map
          (Key_Type      => Unbounded_String,
           Element_Type  => Unbounded_String,
           Map_Type      => String_Maps.Map,
           Cursor_Type   => String_Maps.Cursor,
           Length        => String_Maps.Length,
           Iterate       => String_Maps.Iterate,
           Query_Element => String_Maps.Query_Element,
           Write_Key     => Write,
           Write_Element => Write);
   begin
      Write (CSS, Value);
   end Write;

   --------------------
   -- To_String_Sets --
   --------------------

   function To_String_Set (V : String_Vectors.Vector) return String_Sets.Set is
      Result : String_Sets.Set;
   begin
      for Elem of V loop
         Result.Include (Elem);
      end loop;
      return Result;
   end To_String_Set;

   --------------------
   -- Vector_To_List --
   --------------------

   function Vector_To_List
     (V : String_Vectors.Vector) return String_List_Access
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

   overriding
   procedure Read
     (Stream : in out Unbounded_String_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
      use Ada.Streams;

      Last_Index  : constant Natural :=
        Natural'Min
          (Length (Stream.S.all), Stream.Read_Index + Item'Length - 1);
      Read_Length : constant Natural := Last_Index - Stream.Read_Index + 1;
      Item_S      : String (1 .. Read_Length);
      for Item_S'Address use Item'Address;
      pragma Import (Ada, Item_S);
   begin
      Item_S :=
        Slice (Stream.S.all, Low => Stream.Read_Index, High => Last_Index);
      Stream.Read_Index := Last_Index + 1;
      Last := Item'First + Stream_Element_Offset (Read_Length) - 1;
   end Read;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write
     (Stream : in out Unbounded_String_Stream;
      Item   : Ada.Streams.Stream_Element_Array)
   is
      Item_S : String (1 .. Item'Length);
      for Item_S'Address use Item'Address;
      pragma Import (Ada, Item_S);
   begin
      Append (Stream.S.all, Item_S);
   end Write;

   ------------------------
   -- Match_Pattern_List --
   ------------------------

   procedure Match_Pattern_List
     (Patterns_List        : String_Vectors.Vector;
      Strings_List         : in out String_Vectors.Vector;
      Patterns_Not_Covered : in out String_Sets.Set)
   is
      use Ada.Characters.Handling;
      use String_Vectors;

      Pattern_Length : constant Natural := Natural (Patterns_List.Length);
      Regexps        : array (0 .. Pattern_Length - 1) of GNAT.Regexp.Regexp;
      --  List of regexps, one for each pattern in Patterns_List

      Matching_Strings : String_Vectors.Vector;
      --  Result holder for Strings_List

      procedure Process_String (C : Cursor);
      --  Try matching the string in Strings_List referenced by C against each
      --  pattern. If there is a match, add the String to Matching_Strings.
      --  Also mark as covered every pattern that matched.

      --------------------
      -- Process_String --
      --------------------

      procedure Process_String (C : Cursor) is
         Str       : constant Unbounded_String := +To_Lower (+Element (C));
         Str_Added : Boolean := False;
      begin
         for I in Regexps'Range loop
            if GNAT.Regexp.Match (+Str, Regexps (I)) then
               if not Str_Added then
                  Matching_Strings.Append (Str);
                  Str_Added := True;
               end if;

               --  A unit matching this pattern was found; the pattern is
               --  covered. Remove it from Patterns_Not_Covered.

               declare
                  Pattern : constant Unbounded_String :=
                    Patterns_List.Element (I);
                  Cur     : String_Sets.Cursor :=
                    Patterns_Not_Covered.Find (Pattern);
               begin
                  if String_Sets.Has_Element (Cur) then
                     Patterns_Not_Covered.Delete (Cur);
                  end if;
               end;

            --  Continue the search in case other patterns match Str so that
            --  we can mark them as covered as well.

            end if;
         end loop;
      end Process_String;

      --  Start of processing for Match_Pattern_List

   begin
      for I in Regexps'Range loop
         Regexps (I) :=
           GNAT.Regexp.Compile
             (Paths.Glob_To_Regexp (To_Lower (+(Patterns_List.Element (I)))));
      end loop;

      Strings_List.Iterate (Process_String'Access);

      --  Return the matched strings and the patterns not covered

      Strings_List := Matching_Strings;

   end Match_Pattern_List;

   ------------------------
   -- Append_From_String --
   ------------------------

   procedure Append_From_String
     (Vec : in out String_Vectors.Vector; From : Unbounded_String)
   is
      Last            : constant Natural := Length (From);
      Arg_Start_Index : Natural := 1;
   begin
      --  Add a value for all slices before commas

      for I in 1 .. Last loop
         if Element (From, I) = ',' then
            Vec.Append (Unbounded_Slice (From, Arg_Start_Index, I - 1));
            Arg_Start_Index := I + 1;
         end if;
      end loop;

      --  Do not forget to add the slice after the comma

      Vec.Append (Unbounded_Slice (From, Arg_Start_Index, Last));
   end Append_From_String;

   -------------------------------
   -- Interpret_Escape_Sequence --
   -------------------------------

   function Interpret_Escape_Sequence (Str : String) return String is
      use US;
      Res   : Unbounded_String;
      Index : Positive := Str'First;
      C     : Character;
   begin
      while Index <= Str'Last loop
         if Str (Index) = '\' then
            if Index = Str'Last then
               raise Constraint_Error with "stray trailing backslash";
            end if;
            C :=
              (case Str (Index + 1) is
                 when 'a'                   => ASCII.BEL,
                 when 'b'                   => ASCII.BS,
                 when 'e'                   => ASCII.ESC,
                 when 'f'                   => ASCII.FF,
                 when 'n'                   => ASCII.LF,
                 when 'r'                   => ASCII.CR,
                 when 't'                   => ASCII.HT,
                 when 'v'                   => ASCII.VT,
                 when ''' | '"' | '?' | '\' => Str (Index + 1),
                 when others                =>
                   raise Constraint_Error
                     with
                       "invalid or unknown escape sequence: "
                       & Str (Index .. Index + 1));
            Index := Index + 2;
         else
            C := Str (Index);
            Index := Index + 1;
         end if;
         Append (Res, C);
      end loop;
      return +Res;
   end Interpret_Escape_Sequence;

end Strings;
