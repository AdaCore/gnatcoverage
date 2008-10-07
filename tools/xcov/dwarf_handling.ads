------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                     Copyright (C) 2006 Tristan Gingold                   --
--                        Copyright (C) 2008, AdaCore                       --
--                                                                          --
-- Couverture is free software; you can redistribute it  and/or modify it   --
-- under terms of the GNU General Public License as published by the Free   --
-- Software Foundation; either version 2, or (at your option) any later     --
-- version.  Couverture is distributed in the hope that it will be useful,  --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details. You  should  have  received a copy of the GNU --
-- General Public License  distributed with GNAT; see file COPYING. If not, --
-- write  to  the Free  Software  Foundation,  59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Ada.Unchecked_Deallocation;

package Dwarf_Handling is
   type Abbrev_Map_Type is array (Unsigned_32 range <>) of Address;
   type Abbrev_Map_Acc is access Abbrev_Map_Type;
   procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
     (Abbrev_Map_Type, Abbrev_Map_Acc);

   procedure Build_Abbrev_Map (Base : Address; Res : out Abbrev_Map_Acc);

   procedure Read_Word8_Le (Base : Address;
                            Off : in out Storage_Offset;
                            Res : out Unsigned_64);

   procedure Read_Word8_Be (Base : Address;
                            Off : in out Storage_Offset;
                            Res : out Unsigned_64);

   procedure Read_Word4_Le (Base : Address;
                            Off : in out Storage_Offset;
                            Res : out Unsigned_32);

   procedure Read_Word4_Be (Base : Address;
                            Off : in out Storage_Offset;
                            Res : out Unsigned_32);

   procedure Read_Word2_Le (Base : Address;
                            Off : in out Storage_Offset;
                            Res : out Unsigned_16);

   procedure Read_Word2_Be (Base : Address;
                            Off : in out Storage_Offset;
                            Res : out Unsigned_16);

   procedure Read_Byte (Base : Address;
                        Off : in out Storage_Offset;
                        Res : out Unsigned_8);
   function Read_Byte (Addr : Address) return Unsigned_8;

   procedure Read_ULEB128 (Base : Address;
                           Off : in out Storage_Offset;
                           Res : out Unsigned_32);
   procedure Read_SLEB128 (Base : Address;
                           Off : in out Storage_Offset;
                           Res : out Unsigned_32);

   procedure Read_String (Base : Address; Off : in out Storage_Offset);
   function Read_String (Addr : Address) return String;

   procedure Write_Word4_Le (Base : Address;
                             Off : in out Storage_Offset;
                             Val : Unsigned_32);

   procedure Write_Word4_Be (Base : Address;
                             Off : in out Storage_Offset;
                             Val : Unsigned_32);

end Dwarf_Handling;
