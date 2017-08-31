------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2017, AdaCore                     --
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

with Disa_Common;

package body Disa_Thumb is

   function To_Insn16 (Insn_Bin : Binary_Content) return Unsigned_16 is
     (Disa_Common.ELF_To_U16
        (Slice (Insn_Bin, Insn_Bin.First, Insn_Bin.First + 1)));

   function To_Insn32 (Insn_Bin : Binary_Content) return Unsigned_32 is
     (Unsigned_32 (Disa_Common.ELF_To_U16
                   (Slice (Insn_Bin,
                           Insn_Bin.First + 2,
                           Insn_Bin.First + 3)))
      + Shift_Left (Unsigned_32 (Disa_Common.ELF_To_U16
                                 (Slice (Insn_Bin,
                                         Insn_Bin.First + 0,
                                         Insn_Bin.First + 1))), 16));

   function Is_32bit_Insn (Insn_Bin : Binary_Content) return Boolean is
     (case Shift_Right (To_Insn16 (Insn_Bin), 11) is
         when 2#00000# .. 2#11100# => False,
         when others               => True);

   function Slice (U32   : Unsigned_32;
                   Shift : Natural;
                   Mask  : Unsigned_32) return Unsigned_32 is
     (Shift_Right (U32, Shift) and Mask);

   function Sign_Extend
     (U32   : Unsigned_32;
      Width : Natural) return Unsigned_32
   is (Shift_Right_Arithmetic (Shift_Left (U32, 32 - Width), 32 - Width));

   function Get_Target (Immediate : Unsigned_32;
                        Shift     : Natural;
                        Pc        : Unsigned_32) return Unsigned_32
   is (Pc + Shift_Left (Immediate, Shift) + 4);
   --  The PC is always 2 instructions beyond the currently executing
   --  instruction, hence the +4 offset.

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Object : in out Thumb_Disassembler) is
   begin
      Object.Handle := Dis_Opcodes.Create_Thumb_Disassembler;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (Object : in out Thumb_Disassembler) is
   begin
      Dis_Opcodes.Delete_Disassembler (Object.Handle);
   end Finalize;

   ---------------------
   -- Get_Insn_Length --
   ---------------------

   function Get_Insn_Length
     (Self     : Thumb_Disassembler;
      Insn_Bin : Binary_Content) return Positive
   is
      pragma Unreferenced (Self);

   begin
      return (if Is_32bit_Insn (Insn_Bin) then 4 else 2);
   end Get_Insn_Length;

   ----------------------
   -- Disassemble_Insn --
   ----------------------

   procedure Disassemble_Insn
     (Self     : Thumb_Disassembler;
      Insn_Bin : Binary_Content;
      Pc       : Traces.Pc_Type;
      Buffer   : in out Highlighting.Buffer_Type;
      Insn_Len : out Natural;
      Sym      : Symbolizer'Class)
   is
   begin
      Disa_Common.Opcodes_Disassemble_Insn
        (Self.Handle, Insn_Bin, Pc, Buffer, Insn_Len, Sym, 4);
   end Disassemble_Insn;

   -------------------------
   -- Get_Insn_Properties --
   -------------------------

   procedure Get_Insn_Properties
     (Self        : Thumb_Disassembler;
      Insn_Bin    : Binary_Content;
      Pc          : Pc_Type;
      Branch      : out Branch_Kind;
      Flag_Indir  : out Boolean;
      Flag_Cond   : out Boolean;
      Branch_Dest : out Dest;
      FT_Dest     : out Dest)
   is
      PC32 : constant Unsigned_32 := Unsigned_32 (Pc);
   begin
      Branch      := Br_None;
      Branch_Dest := (No_PC, No_PC);
      FT_Dest     := (No_PC, No_PC);

      Flag_Indir := False;
      Flag_Cond  := False;

      if Is_32bit_Insn (Insn_Bin) then
         declare
            Insn32 : constant Unsigned_32 := To_Insn32 (Insn_Bin);
            Opcode : constant Unsigned_32 :=
              Shift_Right (Insn32, 11 + 16) and 2#11#;
         begin
            case Opcode is
            when 2#00# =>
               null;

            when 2#01# =>
               if (Insn32 and 16#fff0_ffe0#) = 16#e8d0_f000# then

                  --  TBB/TBH(T15)

                  Branch := Br_Jmp;
                  Flag_Indir := True;

               elsif (Insn32 and 16#ffd0_2000#) = 16#e890_0000# then

                  --  LDM/LDMIA/LDMFD (T2)

                  if (Insn32 and 2**15) = 2**15 then

                     --  This instruction writes in PC, so it's a control-flow
                     --  instruction.

                     Branch := Br_Jmp;
                     Flag_Indir := True;
                  end if;
               end if;

            when 2#10# =>
               declare
                  Discr : constant Unsigned_32 :=
                    (Slice (Insn32, 12, 1)
                     or Shift_Left (Slice (Insn32, 14, 2#11#), 1));

                  S     : constant Unsigned_32 := Slice (Insn32, 26, 1);
                  J1    : constant Unsigned_32 := Slice (Insn32, 13, 1);
                  J2    : constant Unsigned_32 := Slice (Insn32, 11, 1);

                  XJ1   : constant Unsigned_32 := J1 xor S xor 1;
                  XJ2   : constant Unsigned_32 := J2 xor S xor 1;

                  Imm6  : constant Unsigned_32 := Slice (Insn32, 16, 16#3f#);
                  Imm10 : constant Unsigned_32 := Slice (Insn32, 16, 16#3ff#);
                  Imm11 : constant Unsigned_32 := Slice (Insn32, 0,  16#7ff#);

                  Immediate : Unsigned_32;
               begin
                  case Discr is
                  when 2#111# => --  BL(T1)
                     Branch := Br_Call;
                     Immediate := Sign_Extend
                       ((Shift_Left (Imm11, 1)
                        or Shift_Left (Imm10, 12)
                        or Shift_Left (XJ1, 22)
                        or Shift_Left (XJ2, 23)
                        or Shift_Left (S, 24)),
                        25);

                  when 2#110# => --  BLX(T2)
                     Branch := Br_Call;
                     Immediate := Sign_Extend
                       ((Shift_Left (Imm11, 1)
                        or Shift_Left (Imm6, 12)
                        or Shift_Left (J1, 19)
                        or Shift_Left (J2, 20)
                        or Shift_Left (S, 21)),
                        22);

                  when 2#100# => --  B(T3)

                     --  TODO??? This is BXJ when cond = 1111

                     Branch := Br_Jmp;
                     Flag_Cond := True;
                     Immediate := Sign_Extend
                       ((Shift_Left (Imm11, 1)
                        or Shift_Left (Imm6, 12)
                        or Shift_Left (J1, 19)
                        or Shift_Left (J2, 20)
                        or Shift_Left (S, 21)),
                        22);

                  when 2#101# => --  B(T4)
                     Branch := Br_Jmp;
                     Immediate := Sign_Extend
                       ((Shift_Left (Imm11, 1)
                        or Shift_Left (Imm10, 12)
                        or Shift_Left (XJ1, 22)
                        or Shift_Left (XJ2, 23)
                        or Shift_Left (S, 24)),
                        25);
                     --  B(T3)

                  when others =>
                     null;
                  end case;

                  if Branch /= Br_None then
                     Branch_Dest.Target :=
                       Pc_Type (Get_Target (Immediate, 0, PC32));
                  end if;
               end;

            when others => --  2#11#
               if (Insn32 and 16#ffff_2000#) = 16#e8bd_0000# then

                  --  POP (T2)

                  if (Insn32 and 16#0000_8000#) = 16#0000_8000# then

                     --  This instruction writes in PC, so it's a control-flow
                     --  instruction.

                     Branch := Br_Jmp;
                     Flag_Indir := True;
                  end if;

               elsif (Insn32 and 16#ffff_0fff#) in
                     16#f85d_0b04#
                   | 16#f8d0_0000#
               then

                  --  POP (T3), LDR (immediate, T3)

                  if (Insn32 and 16#0000_f000#) = 16#0000_f000# then

                     --  This instruction writes in PC, so it's a control-flow
                     --  instruction.

                     Branch := Br_Jmp;
                     Flag_Indir := True;
                  end if;
               end if;
            end case;
         end;

      else
         declare
            Insn16 : constant Unsigned_16 := To_Insn16 (Insn_Bin);
            Opcode  : constant Unsigned_16 := Shift_Right (Insn16, 10);
         begin
            case Opcode is
            when 2#010001# =>
               if (Insn16 and 16#ff87#) = 16#4780# then

                  --  BLX(T1)

                  Branch := Br_Call;
                  Flag_Indir := True;

               elsif (Insn16 and 16#ff87#) = 16#4700# then

                  --  BX(T1)

                  Branch := Br_Jmp;
                  Flag_Indir := False;

               end if;

            when 2#101100# .. 2#101111# =>
               if (Insn16 and 16#0500#) = 16#0100# then

                  --  CBZ/CBNZ

                  Flag_Cond := True;
                  Branch := Br_Jmp;
                  declare
                     I    : constant Unsigned_32 :=
                       Unsigned_32 (Shift_Right (Insn16, 9) and 1);
                     Imm5 : constant Unsigned_32 :=
                       Unsigned_32 (Shift_Right (Insn16, 3) and 16#1f#);
                     Immediate : constant Unsigned_32 :=
                       (Shift_Left (Imm5, 1)
                        or Shift_Left (I, 6));
                  begin
                     Branch_Dest.Target :=
                       Pc_Type (Get_Target (Immediate, 0, PC32));
                  end;

               elsif (Shift_Right (Insn16, 8) and 16#f#) = 2#1101# then

                  --  POP (T1, P is set: write to PC = branch)

                  Branch := Br_Jmp;
                  Flag_Indir := True;

               end if;
            when 2#110100# .. 2#110111# =>

               --  B(T1)

               Flag_Cond := True;
               Branch := Br_Jmp;
               declare
                  Immediate : constant Unsigned_32 :=
                    Sign_Extend (Unsigned_32 (Insn16), 8);
               begin
                  Branch_Dest.Target :=
                    Pc_Type (Get_Target (Immediate, 1, PC32));
               end;

            when 2#111000# .. 2#111001# =>

               --  B(T2)

               Flag_Cond := False;
               Branch := Br_Jmp;
               declare
                  Immediate : constant Unsigned_32 :=
                    Sign_Extend (Unsigned_32 (Insn16), 11);
               begin
                  Branch_Dest.Target :=
                    Pc_Type (Get_Target (Immediate, 1, PC32));
               end;

            when others =>
                  null;
            end case;
         end;
      end if;

      FT_Dest.Target := Pc + Pc_Type (Get_Insn_Length (Self, Insn_Bin));
   end Get_Insn_Properties;

   ----------------
   -- Is_Padding --
   ----------------

   function Is_Padding
     (Self     : Thumb_Disassembler;
      Insn_Bin : Binary_Content;
      Pc       : Pc_Type) return Boolean
   is
      pragma Unreferenced (Self, Insn_Bin, Pc);
   begin
      return False;
   end Is_Padding;

end Disa_Thumb;
