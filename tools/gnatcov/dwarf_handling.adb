------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2006-2021, AdaCore                     --
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

with Dwarf;

package body Dwarf_Handling is

   procedure Write_Byte (Addr : Address; Val : Unsigned_8);

   ----------------------
   -- Build_Abbrev_Map --
   ----------------------

   procedure Build_Abbrev_Map (Base : Address; Res : out Abbrev_Map_Acc) is
      use Dwarf;
      Max   : Unsigned_32;
      Off   : Storage_Offset;
      V     : Unsigned_32;
      Form  : Unsigned_32;
      Cst   : Unsigned_32;
      N_Res : Abbrev_Map_Acc;
   begin
      Off := 0;
      Max := 0;
      Res := new Abbrev_Map_Type (0 .. 128);
      Res.all := (others => Null_Address);

      loop
         Read_ULEB128 (Base, Off, V);

         if V > Max then
            Max := V;
         end if;

         exit when V = 0;

         if Max > Res.all'Last then
            N_Res := new Abbrev_Map_Type (0 .. 2 * Max);
            N_Res (Res'Range) := Res.all;
            N_Res (Res'Last + 1 .. N_Res'Last) := (others => Null_Address);
            Free (Res);
            Res := N_Res;
         end if;

         if Res (V) /= Null_Address then
            --  Put_Line ("!! abbrev override !!");
            return;
         end if;

         Res (V) := Base + Off;
         Read_ULEB128 (Base, Off, V);

         --  Skip child flag

         Off := Off + 1;

         loop
            Read_ULEB128 (Base, Off, V);
            Read_ULEB128 (Base, Off, Form);

            --  DW_FORM_implicit_const takes its value from the table

            if Form = DW_FORM_implicit_const then
               Read_SLEB128 (Base, Off, Cst);
            end if;

            exit when V = 0 and then Form = 0;
         end loop;
      end loop;
   end Build_Abbrev_Map;

   ---------------
   -- Read_Byte --
   ---------------

   function Read_Byte (Addr : Address) return Unsigned_8 is
      Result : Unsigned_8;
      for Result'Address use Addr;
      pragma Import (Ada, Result);
   begin
      return Result;
   end Read_Byte;

   ------------------
   -- Read_ULEB128 --
   ------------------

   procedure Read_ULEB128 (Base : Address;
                           Off  : in out Storage_Offset;
                           Res  : out Unsigned_32)
   is
      B     : Unsigned_8;
      Shift : Integer;
   begin
      Res   := 0;
      Shift := 0;

      loop
         B := Read_Byte (Base + Off);
         Off := Off + 1;
         Res := Res or Shift_Left (Unsigned_32 (B and 16#7f#), Shift);
         exit when (B and 16#80#) = 0;

         Shift := Shift + 7;
      end loop;
   end Read_ULEB128;

   ------------------
   -- Read_SLEB128 --
   ------------------

   procedure Read_SLEB128 (Base : Address;
                           Off  : in out Storage_Offset;
                           Res  : out Unsigned_32)
   is
      B     : Unsigned_8;
      Shift : Integer;
   begin
      Res   := 0;
      Shift := 0;

      loop
         B := Read_Byte (Base + Off);
         Off := Off + 1;
         Res := Res or Shift_Left (Unsigned_32 (B and 16#7f#), Shift);
         Shift := Shift + 7;

         exit when (B and 16#80#) = 0;
      end loop;

      if Shift < 32 and (Res and Shift_Left (1, Shift - 1)) /= 0 then
         Res := Res or Shift_Left (-1, Shift);
      end if;
   end Read_SLEB128;

   -------------------
   -- Read_Word8_Le --
   -------------------

   procedure Read_Word8_Le (Base : Address;
                            Off  : in out Storage_Offset;
                            Res  : out Unsigned_64)
   is
      B : Unsigned_8;
   begin
      Res := 0;
      for I in 0 .. 7 loop
         B := Read_Byte (Base + Off + Storage_Offset (I));
         Res := Res or Shift_Left (Unsigned_64 (B), 8 * I);
      end loop;
      Off := Off + 8;
   end Read_Word8_Le;

   -------------------
   -- Read_Word4_Le --
   -------------------

   procedure Read_Word4_Le (Base : Address;
                            Off : in out Storage_Offset;
                            Res : out Unsigned_32)
   is
      B0, B1, B2, B3 : Unsigned_8;
   begin
      B0 := Read_Byte (Base + Off + 0);
      B1 := Read_Byte (Base + Off + 1);
      B2 := Read_Byte (Base + Off + 2);
      B3 := Read_Byte (Base + Off + 3);
      Res := Shift_Left (Unsigned_32 (B3), 24)
        or Shift_Left (Unsigned_32 (B2), 16)
        or Shift_Left (Unsigned_32 (B1), 8)
        or Shift_Left (Unsigned_32 (B0), 0);
      Off := Off + 4;
   end Read_Word4_Le;

   -------------------
   -- Read_Word2_Le --
   -------------------

   procedure Read_Word2_Le (Base : Address;
                            Off : in out Storage_Offset;
                            Res : out Unsigned_16)
   is
      B0, B1 : Unsigned_8;
   begin
      B0 := Read_Byte (Base + Off + 0);
      B1 := Read_Byte (Base + Off + 1);
      Res := Shift_Left (Unsigned_16 (B1), 8)
        or Shift_Left (Unsigned_16 (B0), 0);
      Off := Off + 2;
   end Read_Word2_Le;

   -------------------
   -- Read_Word8_Be --
   -------------------

   procedure Read_Word8_Be (Base : Address;
                            Off : in out Storage_Offset;
                            Res : out Unsigned_64)
   is
      B : Unsigned_8;
   begin
      Res := 0;
      for I in 0 .. 7 loop
         B := Read_Byte (Base + Off + Storage_Offset (I));
         Res := Res or Shift_Left (Unsigned_64 (B), 56 - 8 * I);
      end loop;
      Off := Off + 8;
   end Read_Word8_Be;

   -------------------
   -- Read_Word4_Be --
   -------------------

   procedure Read_Word4_Be (Base : Address;
                            Off : in out Storage_Offset;
                            Res : out Unsigned_32)
   is
      B0, B1, B2, B3 : Unsigned_8;
   begin
      B0 := Read_Byte (Base + Off + 0);
      B1 := Read_Byte (Base + Off + 1);
      B2 := Read_Byte (Base + Off + 2);
      B3 := Read_Byte (Base + Off + 3);
      Res := Shift_Left (Unsigned_32 (B0), 24)
        or Shift_Left (Unsigned_32 (B1), 16)
        or Shift_Left (Unsigned_32 (B2), 8)
        or Shift_Left (Unsigned_32 (B3), 0);
      Off := Off + 4;
   end Read_Word4_Be;

   -------------------
   -- Read_Word2_Be --
   -------------------

   procedure Read_Word2_Be (Base : Address;
                         Off : in out Storage_Offset;
                         Res : out Unsigned_16)
   is
      B0, B1 : Unsigned_8;
   begin
      B0 := Read_Byte (Base + Off + 0);
      B1 := Read_Byte (Base + Off + 1);
      Res := Shift_Left (Unsigned_16 (B0), 8)
        or Shift_Left (Unsigned_16 (B1), 0);
      Off := Off + 2;
   end Read_Word2_Be;

   ---------------
   -- Read_Byte --
   ---------------

   procedure Read_Byte (Base : Address;
                        Off : in out Storage_Offset;
                        Res : out Unsigned_8)
   is
   begin
      Res := Read_Byte (Base + Off);
      Off := Off + 1;
   end Read_Byte;

   -----------------
   -- Read_String --
   -----------------

   procedure Read_String (Base : Address; Off : in out Storage_Offset)
   is
      B : Unsigned_8;
   begin
      loop
         Read_Byte (Base, Off, B);
         exit when B = 0;
      end loop;
   end Read_String;

   -----------------
   -- Read_String --
   -----------------

   function Read_String (Addr : Address) return String
   is
      function C_Strlen (Addr : Address) return Integer;
      pragma Import (C, C_Strlen, "strlen");
      Len : Integer;

      subtype Fat_String is String (Positive);
      type Fat_String_Acc is access Fat_String;
      function To_Fat_String is new Ada.Unchecked_Conversion
        (Address, Fat_String_Acc);
      Str : constant Fat_String_Acc := To_Fat_String (Addr);
   begin
      if Addr = Null_Address then
         return "";
      end if;
      Len := C_Strlen (Addr);
      return Str (1 .. Len);
   end Read_String;

   ----------------
   -- Write_Byte --
   ----------------

   procedure Write_Byte (Addr : Address; Val : Unsigned_8)
   is
      type Unsigned_8_Acc is access all Unsigned_8;
      function To_Unsigned_8_Acc is new Ada.Unchecked_Conversion
        (Address, Unsigned_8_Acc);
   begin
      To_Unsigned_8_Acc (Addr).all := Val;
   end Write_Byte;

   --------------------
   -- Write_Word4_Le --
   --------------------

   procedure Write_Word4_Le (Base : Address;
                             Off : in out Storage_Offset;
                             Val : Unsigned_32)
   is
      B0, B1, B2, B3 : Unsigned_8;
   begin
      B0 := Unsigned_8 (Shift_Right (Val, 0) and 16#Ff#);
      B1 := Unsigned_8 (Shift_Right (Val, 8) and 16#Ff#);
      B2 := Unsigned_8 (Shift_Right (Val, 16) and 16#Ff#);
      B3 := Unsigned_8 (Shift_Right (Val, 24) and 16#Ff#);

      Write_Byte (Base + Off + 0, B0);
      Write_Byte (Base + Off + 1, B1);
      Write_Byte (Base + Off + 2, B2);
      Write_Byte (Base + Off + 3, B3);
      Off := Off + 4;
   end Write_Word4_Le;

   --------------------
   -- Write_Word4_Be --
   --------------------

   procedure Write_Word4_Be (Base : Address;
                             Off : in out Storage_Offset;
                             Val : Unsigned_32)
   is
      B0, B1, B2, B3 : Unsigned_8;
   begin
      B0 := Unsigned_8 (Shift_Right (Val, 24) and 16#Ff#);
      B1 := Unsigned_8 (Shift_Right (Val, 16) and 16#Ff#);
      B2 := Unsigned_8 (Shift_Right (Val, 8) and 16#Ff#);
      B3 := Unsigned_8 (Shift_Right (Val, 0) and 16#Ff#);

      Write_Byte (Base + Off + 0, B0);
      Write_Byte (Base + Off + 1, B1);
      Write_Byte (Base + Off + 2, B2);
      Write_Byte (Base + Off + 3, B3);
      Off := Off + 4;
   end Write_Word4_Be;

   --------------------
   -- Write_Word8_Le --
   --------------------

   procedure Write_Word8_Le (Base : Address;
                             Off : in out Storage_Offset;
                             Val : Unsigned_64)
   is
      B : Unsigned_8;
   begin
      for I in 0 .. 7 loop
         B := Unsigned_8 (Shift_Right (Val, 8 * I) and 16#ff#);
         Write_Byte (Base + Off + Storage_Offset (I), B);
      end loop;
      Off := Off + 8;
   end Write_Word8_Le;

   --------------------
   -- Write_Word8_Be --
   --------------------

   procedure Write_Word8_Be (Base : Address;
                             Off : in out Storage_Offset;
                             Val : Unsigned_64)
   is
      B : Unsigned_8;
   begin
      for I in 0 .. 7 loop
         B := Unsigned_8 (Shift_Right (Val, 64 - 8 * I - 8) and 16#ff#);
         Write_Byte (Base + Off + Storage_Offset (I), B);
      end loop;
      Off := Off + 8;
   end Write_Word8_Be;

end Dwarf_Handling;
