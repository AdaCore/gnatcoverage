------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2014, AdaCore                       --
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

with Ada.Strings.Unbounded;

package body Highlighting is

   function Get_Token_Last
     (Buffer : Buffer_Type;
      First : Natural) return Natural;
   --  Return the last character index in Buffer of the token that starts at
   --  index First.

   --------------------
   -- Get_Token_Last --
   --------------------

   function Get_Token_Last
     (Buffer : Buffer_Type;
      First : Natural) return Natural
   is
      Position : Natural := First + 1;
   begin
      while Position <= Buffer.Last
        and then Buffer.Tokens (Position) = None
      loop
         Position := Position + 1;
      end loop;
      return Position - 1;
   end Get_Token_Last;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Object : in out Buffer_Type) is
   begin
      Object.Reset;
   end Initialize;

   procedure Reset (Buffer : in out Buffer_Type) is
   begin
      Buffer.Tokens := (others => None);
      Buffer.Last := 0;
   end Reset;

   -----------------
   -- Start_Token --
   -----------------

   procedure Start_Token
     (Buffer : in out Buffer_Type;
      Token : Some_Token_Kind) is
   begin
      if Buffer.Last + 1 <= Buffer.Length then
         Buffer.Tokens (Buffer.Last + 1) := Token;
      end if;
   end Start_Token;

   ---------
   -- Put --
   ---------

   procedure Put (Buffer : in out Buffer_Type; Chr : Character) is
   begin
      Buffer.Put ((1 => Chr));
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Buffer : in out Buffer_Type; Str : String) is
      Available_Buf : String renames
        Buffer.Str (Buffer.Last + 1 .. Buffer.Str'Last);
      Copy_Length : constant Natural :=
        Natural'Min (Str'Length, Available_Buf'Length);
   begin
      Available_Buf
        (Available_Buf'First .. Available_Buf'First + Copy_Length - 1) :=
        Str (Str'First .. Str'First + Copy_Length - 1);
      Buffer.Last := Buffer.Last + Copy_Length;
   end Put;

   -----------
   -- First --
   -----------

   function First (Buffer : Buffer_Type) return Cursor is
   begin
      if Buffer.Last < 1 then
         return No_Element;
      else
         return
           (Buffer => Buffer'Unrestricted_Access,
            First  => 1,
            Last   => Get_Token_Last (Buffer, 1));
      end if;
   end First;

   ----------
   -- Next --
   ----------

   function Next (Position : Cursor) return Cursor is
      Buffer : Buffer_Access renames Position.Buffer;
   begin
      if Position = No_Element
        or else Position.Last >= Buffer.Last
      then
         return No_Element;
      else
         return
           (Buffer => Buffer,
            First  => Position.Last + 1,
            Last   => Get_Token_Last (Buffer.all, Position.Last + 1));
      end if;
   end Next;

   ----------
   -- Next --
   ----------

   procedure Next (Position : in out Cursor) is
   begin
      Position := Next (Position);
   end Next;

   ----------
   -- Text --
   ----------

   function Text (Position : Cursor) return String is
   begin
      if Position = No_Element then
         raise No_Token;
      else
         return Position.Buffer.Str (Position.First .. Position.Last);
      end if;
   end Text;

   -----------
   -- Token --
   -----------

   function Token (Position : Cursor) return Some_Token_Kind is
   begin
      if Position = No_Element then
         raise No_Token;
      else
         return Position.Buffer.Tokens (Position.First);
      end if;
   end Token;

   ------------------
   -- Format_Token --
   ------------------

   function Format_Token (Text : String; Kind : Some_Token_Kind) return String
   is
      use Ada.Strings.Unbounded;

      Hex_Digits : constant array (0 .. 15) of Character := "0123456789abcdef";
      Result     : Unbounded_String;

   begin
      Append (Result, Some_Token_Kind'Image (Kind));
      Append (Result, ':');

      --  Append printable characters (except '\') as-is and put other ones as
      --  "\XX" escape sequences.
      for C of Text loop
         declare
            Pos : constant Integer := Character'Pos (C);
         begin
            if C < ' ' or else C > '~' or else C = '\' then
               Append (Result, '\');
               Append (Result, Hex_Digits (Pos / 16));
               Append (Result, Hex_Digits (Pos mod 16));
            else
               Append (Result, C);
            end if;
         end;
      end loop;
      return To_String (Result);
   end Format_Token;

end Highlighting;
