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

with Ada.Finalization; use Ada.Finalization;

package Highlighting is

   type Token_Kind is
     (None,
      Error,
      Text,
      Punctuation,
      Prefix,
      Mnemonic,
      Mnemonic_Call,
      Mnemonic_Jump,
      Mnemonic_Branch,
      Literal,
      Register,
      Name);
   subtype Some_Token_Kind is Token_Kind range Error .. Token_Kind'Last;
   --  Category for tokens. None is used internally only.

   type Buffer_Type (Length : Natural) is new Controlled with private;
   --  A buffer one can write tokens into. It can contain at most Length
   --  characters.

   type Cursor is private;
   No_Element : constant Cursor;
   --  Cursor to iterate on the tokens contained in a buffer.

   overriding procedure Initialize (Object : in out Buffer_Type);

   procedure Reset (Buffer : in out Buffer_Type);
   --  Empty a buffer

   procedure Start_Token
     (Buffer : in out Buffer_Type;
      Token : Some_Token_Kind);
   --  Make the next characters written into Buffer belong to a new token, of
   --  kind Token.

   procedure Put (Buffer : in out Buffer_Type; Chr : Character);
   pragma Inline (Put);
   --  Append a character to Buffer, appending it to the current token

   procedure Put (Buffer : in out Buffer_Type; Str : String);
   --  Append a string to Buffer, appending it to the current token

   function Last_Index (Buffer : Buffer_Type) return Natural;
   --  Return the index of the last character of the last token in Buffer.
   --  Indices starts at 1, so return 0 if there is no token yet.

   No_Token : exception;

   function First (Buffer : Buffer_Type) return Cursor;
   --  Return a cursor that points to the first token in Buffer, or No_Element
   --  if the buffer is empty.

   function Next (Position : Cursor) return Cursor;
   procedure Next (Position : in out Cursor);
   --  Advance a cursor to then next token in the associated buffer, or to
   --  No_Element if Position points to the last token.

   function Text (Position : Cursor) return String;
   --  Return the text associated to the token pointed to by Position. Raise a
   --  No_Token exception if Position is No_Element.

   function Token (Position : Cursor) return Some_Token_Kind;
   --  Return the kind of the token pointed to by Position. Raise a No_Token
   --  exception if Position is No_Element.

   function Get_Raw (Buffer : Buffer_Type; Index : Natural) return Character;
   --  Considering the stream of tokens as a flat stream of characters, return
   --  the character at Index in Buffer.

   function Get_Raw (Buffer : Buffer_Type) return String;
   --  Return the stream of tokens as a flat stream of characters

   subtype Color_Type is String (1 .. 6);
   --  Color in hexadecimal notation

   type Token_Style_Type is record
      Color                    : Color_Type;
      Bold, Italic, Underlined : Boolean;
   end record;

   type Style_Type is array (Token_Kind) of Token_Style_Type;

   Style_Default : constant Style_Type :=
     (None            => ("000000", False, False, False),
      Error           => ("a00000", True,  False, False),
      Text            => ("202020", False, False, False),
      Punctuation     => ("202020", False, False, False),
      Prefix          => ("4080c0", False, False, False),
      Mnemonic        => ("0e84b5", False, False, False),
      Mnemonic_Call   => ("0e84b5", False, False, False),
      Mnemonic_Jump   => ("0e84b5", False, False, False),
      Mnemonic_Branch => ("cd9129", True,  False, False),
      Literal         => ("0e84b5", False, False, False),
      Register        => ("902000", False, False, False),
      Name            => ("007020", False, False, False));

private

   type Token_Stream is array (Natural range <>) of Token_Kind;

   type Buffer_Type (Length : Natural) is new Controlled with record
      Str    : String (1 .. Length);
      --  Tokens as a flat stream of characters

      Tokens : Token_Stream (1 .. Length);
      --  Each cell maps to a character in Str. All are None except for
      --  the cells that start a token. In such a case, the cell contains the
      --  corresponding token type.

      Last   : Natural;
      --  Index of the last character written to the buffer, or zero when the
      --  buffer is empty.
   end record;

   type Buffer_Access is access all Buffer_Type;

   type Cursor is record
      Buffer      : Buffer_Access;
      --  The buffer this cursor is associated to

      First, Last : Natural;
      --  First and last index in Buffer of the characters that make up the
      --  current token.
   end record;

   No_Element : constant Cursor := (null, 0, 0);

   function Last_Index (Buffer : Buffer_Type) return Natural is
     (Buffer.Last);

   function Get_Raw (Buffer : Buffer_Type; Index : Natural) return Character is
     (Buffer.Str (Index));

   function Get_Raw (Buffer : Buffer_Type) return String is
     (Buffer.Str (1 .. Buffer.Last));

end Highlighting;
