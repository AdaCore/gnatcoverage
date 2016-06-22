------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2013, AdaCore                     --
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

with Interfaces; use Interfaces;
with System;
with System.Storage_Elements;

with Arch;         use Arch;
with Disa_Common;
with Dwarf_Handling;
with Highlighting; use Highlighting;
with Ppc_Descs;    use Ppc_Descs;
with Outputs;

package body Disa_Ppc is

   APU_E500_SPE      : constant := 16#0100#;
   APU_E500_SPFP_EFS : constant := 16#0101#;

   function Extract_APU_Info
     (Filename         : String;
      Is_Big_Endian    : Boolean;
      APU_Info_Section : Binary_Content)
      return Traces.Machine_Type
   is
      Section : Binary_Content renames APU_Info_Section;
      Cursor  : Arch_Addr := Section.First;

      procedure Skip (Bytes : Arch_Addr);
      --  Skip "Bytes" bytes in the section. Raise an error if there isn't
      --  enough bytes left.

      function Read_Word return Unsigned_32;
      --  Read the next 4 bytes in the section and return it as an unsigned
      --  4-byte word. Raise an error

      procedure Fatal_Error (Msg : String);
      --  Shortcut for Outputs.Fatal_Error (Filename & ": " & Msg);

      ----------
      -- Skip --
      ----------

      procedure Skip (Bytes : Arch_Addr) is
      begin
         if Cursor + Bytes > Section.Last + 1 then
            Fatal_Error ("invalid APU info section");
         end if;
         Cursor := Cursor + Bytes;
      end Skip;

      ---------------
      -- Read_Word --
      ---------------

      function Read_Word return Unsigned_32 is
         Base   : constant System.Address := Address_Of (Section, Cursor);
         Off    : System.Storage_Elements.Storage_Offset := 0;
         Result : Unsigned_32;
      begin
         Skip (4);
         if Is_Big_Endian then
            Dwarf_Handling.Read_Word4_Be (Base, Off, Result);
         else
            Dwarf_Handling.Read_Word4_Le (Base, Off, Result);
         end if;
         return Result;
      end Read_Word;

      -----------------
      -- Fatal_Error --
      -----------------

      procedure Fatal_Error (Msg : String) is
      begin
         Outputs.Fatal_Error
           (Filename & ": " & Msg);
      end Fatal_Error;

      Name_Length : constant Unsigned_32 := Read_Word;
      Data_Length : constant Unsigned_32 := Read_Word;
      Info_Type   : constant Unsigned_32 := Read_Word;

      Name_Base : constant System.Address := Address_Of (Section, Cursor);
   begin
      if Info_Type /= 2 then
         Fatal_Error ("invalid type for APU info section");
      end if;

      Skip (Arch_Addr (Name_Length));
      declare
         Name : String (1 .. Natural (Name_Length));
         for Name'Address use Name_Base;
      begin
         if Name_Length /= 8 or else Name /= "APUinfo" & ASCII.NUL then
            Fatal_Error ("invalid name for APU info section");
         end if;
      end;

      if Data_Length mod 4 /= 0 then
         Fatal_Error
           ("data length for APU info section is not a multiple of word size");
      end if;

      while Cursor /= Section.Last + 1 loop
         declare
            Data_Entry     : constant Unsigned_32 := Read_Word;
            APU_Identifier : constant Unsigned_16 :=
               Unsigned_16 (Shift_Right (Data_Entry, 16));
         begin
            if APU_Identifier in APU_E500_SPE | APU_E500_SPFP_EFS then
               return Traces.E500;
            end if;
         end;
      end loop;

      return Traces.PPC;
   end Extract_APU_Info;

   function To_Insn (Insn_Bin : Binary_Content) return Unsigned_32 is
     (Disa_Common.To_Big_Endian_U32
        (Slice (Insn_Bin, Insn_Bin.First, Insn_Bin.First + 3)));

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Object : in out PPC_Disassembler) is
   begin
      Object.Handle := Dis_Opcodes.Create_Ppc_Disassembler;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (Object : in out PPC_Disassembler) is
   begin
      Dis_Opcodes.Delete_Disassembler (Object.Handle);
   end Finalize;

   ---------------------
   -- Get_Insn_Length --
   ---------------------

   function Get_Insn_Length
     (Self     : PPC_Disassembler;
      Insn_Bin : Binary_Content) return Positive
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Insn_Bin);
   begin
      return 4;
   end Get_Insn_Length;

   ----------------------
   -- Disassemble_Insn --
   ----------------------

   procedure Disassemble_Insn
     (Self     : PPC_Disassembler;
      Insn_Bin : Binary_Content;
      Pc       : Traces.Pc_Type;
      Buffer   : in out Highlighting.Buffer_Type;
      Insn_Len : out Natural;
      Sym      : Symbolizer'Class) is
   begin
      Disa_Common.Opcodes_Disassemble_Insn
        (Self.Handle, Insn_Bin, Pc, Buffer, Insn_Len, Sym, 4);
   end Disassemble_Insn;

   -------------------------
   -- Get_Insn_Properties --
   -------------------------

   procedure Get_Insn_Properties
     (Self        : PPC_Disassembler;
      Insn_Bin    : Binary_Content;
      Pc          : Pc_Type;
      Branch      : out Branch_Kind;
      Flag_Indir  : out Boolean;
      Flag_Cond   : out Boolean;
      Branch_Dest : out Dest;
      FT_Dest     : out Dest)
   is
      Insn : constant Unsigned_32 := To_Insn (Insn_Bin);

      Opc, Xo, Bo : Unsigned_32;
      D : Pc_Type;
   begin
      Branch_Dest := (No_PC, No_PC);
      FT_Dest     := (No_PC, No_PC);

      Flag_Indir := False;
      Flag_Cond  := False;

      Opc := Get_Field (F_OPC, Insn);
      Xo  := Get_Field (F_XO, Insn);

      --  To be overriden for non-common cases

      if Get_Field (F_LK, Insn) = 1 then
         Branch := Br_Call;
      else
         Branch := Br_Jmp;
      end if;

      if Opc = 18 then
         --  Opc = 18: b, ba, bl and bla

         D := Pc_Type (Shift_Left (Get_Signed_Field (F_LI, Insn), 2));
         if Get_Field (F_AA, Insn) = 1 then
            Branch_Dest.Target := D;
         else
            Branch_Dest.Target := Pc + D;
         end if;
         return;

      elsif Opc = 16 then
         --  bcx

         D := Pc_Type (Shift_Left (Get_Signed_Field (F_BD, Insn), 2));
         if Get_Field (F_AA, Insn) = 1 then
            Branch_Dest.Target := D;
         else
            Branch_Dest.Target := Pc + D;
         end if;

      elsif Opc = 19 and Xo = 16 then
         --  bclrx

         Flag_Indir := True;
         if Branch = Br_Jmp then
            Branch := Br_Ret;
         end if;

      elsif Opc = 19 and Xo = 528 then
         --  bcctrx

         Flag_Indir := True;

      else
         Branch := Br_None;
         return;
      end if;

      Bo := Get_Field (F_BO, Insn);
      Flag_Cond := not ((Bo and 2#10100#) = 2#10100#);
      FT_Dest.Target := Pc + Pc_Type (Get_Insn_Length (Self, Insn_Bin));
   end Get_Insn_Properties;

   ----------------
   -- Is_Padding --
   ----------------

   function Is_Padding
     (Self     : PPC_Disassembler;
      Insn_Bin : Binary_Content;
      Pc       : Pc_Type) return Boolean
   is
      pragma Unreferenced (Self, Insn_Bin, Pc);
   begin
      return False;
   end Is_Padding;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Object : in out E500_Disassembler) is
   begin
      Object.Handle := Dis_Opcodes.Create_E500_Disassembler;
   end Initialize;

end Disa_Ppc;
