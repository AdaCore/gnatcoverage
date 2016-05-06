------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2016, AdaCore                     --
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

with Interfaces.C; use Interfaces.C;

with System;

package Dis_Opcodes is

   ---------------
   -- BFD Types --
   ---------------

   type BFD_VMA is new Interfaces.C.unsigned_long;
   --  Type of virtual memory addresses of the disassembled program

   type BFD_Byte is new Interfaces.C.unsigned_char;
   --  Type holding the bytes of the disassembled program

   type BFD_Byte_Array is array (Natural range <>) of BFD_Byte;

   type BFD_Endian is
     (BFD_ENDIAN_BIG, BFD_ENDIAN_LITTLE, BFD_ENDIAN_UNKNOWN)
     with Convention => C;
   --  Possible endianness values of the program to disassemble

   ---------------
   -- Interface --
   ---------------

   type Disassemble_Handle is private;
   --  Opaque object used to disassemble binary code

   function Create_Arm_Disassembler return Disassemble_Handle
     with Import        => True,
          Convention    => C,
          External_Name => "create_arm_disassembler";
   --  Returns a handle to disassemble ARM binary code

   function Create_Thumb_Disassembler return Disassemble_Handle
     with Import        => True,
          Convention    => C,
          External_Name => "create_thumb_disassembler";
   --  Returns a handle to disassemble Thumb binary code

   function Create_X86_Disassembler return Disassemble_Handle
     with Import        => True,
          Convention    => C,
          External_Name => "create_x86_disassembler";
   --  Returns a handle to disassemble x86 binary code

   function Create_Visium_Disassembler return Disassemble_Handle
     with Import        => True,
          Convention    => C,
          External_Name => "create_visium_disassembler";
   --  Returns a handle to disassemble Visium binary code

   function Create_Ppc_Disassembler return Disassemble_Handle
     with Import        => True,
          Convention    => C,
          External_Name => "create_ppc_disassembler";
   --  Returns a handle to disassemble PowerPC binary code

   function Create_Sparc_Disassembler return Disassemble_Handle
     with Import        => True,
          Convention    => C,
          External_Name => "create_sparc_disassembler";
   --  Returns a handle to disassemble SPARC binary code

   procedure Delete_Disassembler (DH : Disassemble_Handle)
     with Import        => True,
          Convention    => C,
          External_Name => "delete_disassembler";
   --  Frees any memory used by the handle

   function Disassemble_To_Text
     (DH          : Disassemble_Handle;
      Pc          : BFD_VMA;
      Dest        : out char_array;
      Dest_Size   : unsigned;
      Insn_Buffer : BFD_Byte_Array;
      Ib_Size     : unsigned;
      Endian      : BFD_Endian) return int
     with Import        => True,
          Convention    => C,
          External_Name => "disassemble_to_text";
   --  Disassemble the instruction in Insn_Buffer of size Ib_Size.
   --  The instruction is considered to be located at address Pc and
   --  represented in endianness Endian.
   --  The disassembled code is stored in Dest of capacity Dest_Size.
   --  Returns the size (in bytes) of the binary instruction that was
   --  disassembled.

   type Print_Symbol_Cb is access function
     (Addr       : BFD_VMA;
      Symbolizer : System.Address;
      Buff_Addr  : System.Address;
      Buff_Size  : int) return int with Convention => C;
   --  Symbolizer should denote the address of a Symbolizer'Class object.
   --  Functions of this type should return the number of characters written
   --  by the function.
   --  Functions of this type should not add the '\0'

   procedure Set_Disassembler_Symbolizer
     (DH         : Disassemble_Handle;
      Symbolizer : System.Address;
      Address_Cb : Print_Symbol_Cb)
     with Import        => True,
          Convention    => C,
          External_Name => "set_disassembler_symbolizer";
   --  Sets the callback to print symbols for the given handle.
   --  Symbolizer is the argument that will be given to Address_Cb for its
   --  formal parameter of the same name.

private
   type Disassemble_Handle is new System.Address;
end Dis_Opcodes;
