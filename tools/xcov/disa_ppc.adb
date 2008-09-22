------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
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
with System; use System;
with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;
with System.Storage_Elements;
with Hex_Images; use Hex_Images;

package body Disa_Ppc is
   function Get_Insn_Length (Addr : System.Address) return Positive
   is
      pragma Unreferenced (Addr);
   begin
      return 4;
   end Get_Insn_Length;

   function Read_Byte (Addr : Address) return Unsigned_8
   is
      type Unsigned_8_Acc is access all Unsigned_8;
      function To_Unsigned_8_Acc is new Ada.Unchecked_Conversion
        (Address, Unsigned_8_Acc);
   begin
      return To_Unsigned_8_Acc (Addr).all;
   end Read_Byte;

   function Get_Insn (Addr : Address) return Unsigned_32
   is
      use System.Storage_Elements;
      B0, B1, B2, B3 : Unsigned_8;
   begin
      B0 := Read_Byte (Addr + 0);
      B1 := Read_Byte (Addr + 1);
      B2 := Read_Byte (Addr + 2);
      B3 := Read_Byte (Addr + 3);
      return Shift_Left (Unsigned_32 (B0), 24)
        or Shift_Left (Unsigned_32 (B1), 16)
        or Shift_Left (Unsigned_32 (B2), 8)
        or Shift_Left (Unsigned_32 (B3), 0);
   end Get_Insn;

   function Bin_Image (Val : Unsigned_32; Len : Natural := 32) return String
   is
      Res : String (1 .. 32);
   begin
      for I in 0 .. Len - 1 loop
         Res (32 - I) := Character'Val (Character'Pos ('0') +
                                        (Shift_Right (Val, I) and 1));
      end loop;
      return Res (32 + 1 - Len .. 32);
   end Bin_Image;

   --  Some well-known spr
   --Spr_Xer : constant Unsigned_32 := 2#00001_00000#;
   Spr_Lr  : constant Unsigned_32 := 2#01000_00000#;
   Spr_Ctr : constant Unsigned_32 := 2#01001_00000#;

   procedure Disassemble_Insn (Addr : System.Address;
                               Pc : Traces.Pc_Type;
                               Line : out String;
                               Line_Pos : out Natural;
                               Insn_Len : out Natural;
                               Proc_Cb : Symbol_Proc_Type)
   is
      W : Unsigned_32;
      Opcd : Unsigned_8;

      --  Add CHAR to the line.
      procedure Add (C : Character);
      pragma Inline (Add);

      procedure Add (C : Character) is
      begin
         if Line_Pos <= Line'Last then
            Line (Line_Pos) := C;
            Line_Pos := Line_Pos + 1;
         end if;
      end Add;

      --  Add STR to the line.
      procedure Add (Str : String) is
      begin
         if Line_Pos + Str'Length <= Line'Last then
            Line (Line_Pos .. Line_Pos + Str'Length - 1) := Str;
            Line_Pos := Line_Pos + Str'Length;
         else
            for I in Str'Range loop
               Add (Str (I));
            end loop;
         end if;
      end Add;

      procedure Add_Ht is
      begin
         Add (' ');
         while Line_Pos - Line'First < 7 loop
            Add (' ');
         end loop;
      end Add_HT;

      subtype Natural_0_9 is Natural range 0 .. 9;
      procedure Add_Digit (N : Natural_0_9) is
      begin
         Add (Character'Val (Character'Pos ('0') + N));
      end Add_Digit;

      procedure Add_Num2 (Reg : Unsigned_32)
      is
         H, L : Unsigned_32;
      begin
         L := Reg mod 10;
         H := Reg / 10;
         if H /= 0 then
            Add_Digit (Natural (H));
         end if;
         Add_Digit (Natural (L));
      end Add_Num2;

      procedure Add_Reg (Reg : Unsigned_32) is
      begin
         Add ('r');
         Add_Num2 (Reg);
      end Add_Reg;

      procedure Add_Fp (Reg : Unsigned_32) is
      begin
         Add ('f');
         Add_Num2 (Reg);
      end Add_Fp;

      procedure Add_Cr (Reg : Unsigned_32) is
      begin
         Add ("cr");
         Add_Num2 (Reg);
      end Add_Cr;

      procedure Add_Crb (Reg : Unsigned_32) is
      begin
         Add ("crb");
         Add_Num2 (Reg);
      end Add_Crb;

      function Get_Field (Pos, Len : Natural) return Unsigned_32
      is
      begin
         return Shift_Right (Shift_Left (W, Pos), 32 - Len);
      end Get_Field;

      function Get_Field_D return Unsigned_32 is
      begin
         return Get_Field (6, 5);
      end Get_Field_D;

      function Get_Field_S return Unsigned_32 renames Get_Field_D;

      function Get_Field_A return Unsigned_32 is
      begin
         return Get_Field (11, 5);
      end Get_Field_A;

      function Get_Field_B return Unsigned_32 is
      begin
         return Get_Field (16, 5);
      end Get_Field_B;

      function Get_Field_C return Unsigned_32 is
      begin
         return Get_Field (21, 5);
      end Get_Field_C;

      function Get_Field_Sh return Unsigned_32 is
      begin
         return Get_Field (16, 5);
      end Get_Field_Sh;

      function Get_Field_Mb return Unsigned_32 is
      begin
         return Get_Field (21, 5);
      end Get_Field_Mb;

      function Get_Field_Me return Unsigned_32 is
      begin
         return Get_Field (26, 5);
      end Get_Field_Me;

      function Get_Field_Crfd return Unsigned_32 is
      begin
         return Get_Field (6, 3);
      end Get_Field_Crfd;

      function Get_Field_Crfs return Unsigned_32 is
      begin
         return Get_Field (11, 3);
      end Get_Field_Crfs;

      function Get_Field_Bo return Unsigned_8 is
      begin
         return Unsigned_8 (Get_Field (6, 5));
      end Get_Field_Bo;

      function Get_Field_Bi return Unsigned_8 is
      begin
         return Unsigned_8 (Get_Field (11, 5));
      end Get_Field_Bi;

      function Get_Field_Lk return Unsigned_8 is
      begin
         return Unsigned_8 (Get_Field (31, 1));
      end Get_Field_Lk;

      procedure Add_Simm
      is
         V : constant Unsigned_16 := Unsigned_16 (Get_Field (16, 16));
      begin
         if (V and 16#8000#) /= 0 then
            Add ("-0x");
            Add (Hex_Image (-V));
         else
            Add ("0x");
            Add (Hex_Image (V));
         end if;
      end Add_Simm;

      procedure Add_Uimm
      is
         V : constant Unsigned_16 := Unsigned_16 (Get_Field (16, 16));
      begin
         Add ("0x");
         Add (Hex_Image (V));
      end Add_Uimm;

      procedure Add_Load_Store (Name : String; Reg : Character) is
      begin
         Add (Name);
         if Get_Field (5, 1) /= 0 then
            Add ('u');
         end if;
         Add_HT;
         Add (Reg);
         Add_Num2 (Get_Field_D);
         Add (',');
         Add_Simm;
         Add ('(');
         Add_Reg (Get_Field_A);
         Add (')');
      end Add_Load_Store;

      procedure Add_Int_Load_Store (Name : String) is
      begin
         Add_Load_Store (Name, 'r');
      end Add_Int_Load_Store;

      procedure Add_Fp_Load_Store (Name : String) is
      begin
         Add_Load_Store (Name, 'f');
      end Add_Fp_Load_Store;

      procedure Add_Rc_Ht (Name : String) is
      begin
         Add (Name);
         if Get_Field (31, 1) = 1 then
            Add ('.');
         end if;
         Add_Ht;
      end Add_Rc_Ht;

      procedure Add_D_A_Rc (Name : String) is
      begin
         Add_Rc_Ht (Name);
         Add_Reg (Get_Field_D);
         Add (',');
         Add_Reg (Get_Field_A);
      end Add_D_A_Rc;

      procedure Add_D (Name : String) is
      begin
         Add (Name);
         Add_Ht;
         Add_Reg (Get_Field_D);
      end Add_D;

      procedure Add_D_A_Simm (Name : String) is
      begin
         Add_D (Name);
         Add (',');
         Add_Reg (Get_Field_A);
         Add (',');
         Add_Simm;
      end Add_D_A_Simm;

      procedure Add_D_A_B_Rc (Name : String) is
      begin
         Add_D_A_Rc (Name);
         Add (',');
         Add_Reg (Get_Field_B);
      end Add_D_A_B_Rc;

      procedure Add_A_S_Rc (Name : String) is
      begin
         Add_Rc_HT (Name);
         Add_Reg (Get_Field_A);
         Add (',');
         Add_Reg (Get_Field_S);
      end Add_A_S_Rc;

      procedure Add_A_S_B_Rc (Name : String) is
      begin
         Add_A_S_Rc (Name);
         Add (',');
         Add_Reg (Get_Field_B);
      end Add_A_S_B_Rc;

      procedure Add_S_A_Uimm (Name : String) is
      begin
         Add (Name);
         Add_HT;
         Add_Reg (Get_Field_S);
         Add (',');
         Add_Reg (Get_Field_A);
         Add (',');
         Add_Uimm;
      end Add_S_A_Uimm;

      procedure Add_A_S_Sh_Mb_Me_Rc (Name : String) is
      begin
         Add_A_S_Rc (Name);
         Add (',');
         Add_Num2 (Get_Field_Sh);
         Add (',');
         Add_Num2 (Get_Field_Mb);
         Add (',');
         Add_Num2 (Get_Field_Me);
      end Add_A_S_Sh_Mb_Me_Rc;

      procedure Add_A_B (Name : String) is
      begin
         Add (Name);
         Add_HT;
         Add_Reg (Get_Field_A);
         Add (',');
         Add_Reg (Get_Field_B);
      end Add_A_B;

      procedure Add_Crfd (Name : String) is
      begin
         Add (Name);
         Add_Ht;
         Add_Cr (Get_Field_Crfd);
         Add (',');
      end Add_Crfd;

      procedure Add_Crfd_A (Name : String) is
      begin
         Add_Crfd (Name);
         Add_Reg (Get_Field_A);
         Add (',');
      end Add_Crfd_A;

      procedure Add_Crbd_Crba_Crbb (Name : String) is
      begin
         Add (Name);
         Add_Ht;
         Add_Crb (Get_Field_D);
         Add (',');
         Add_Crb (Get_Field_A);
         Add (',');
         Add_Crb (Get_Field_B);
      end Add_Crbd_Crba_Crbb;

      procedure Add_Fd_Rc (Name : String) is
      begin
         Add_Rc_Ht (Name);
         Add_Fp (Get_Field_D);
      end Add_Fd_Rc;

      procedure Add_Fd_Fb_Rc (Name : String) is
      begin
         Add_Fd_Rc (Name);
         Add (',');
         Add_Fp (Get_Field_B);
      end Add_Fd_Fb_Rc;

      procedure Add_Fd_Fa_Fb_Rc (Name : String) is
      begin
         Add_Fd_Rc (Name);
         Add (',');
         Add_Fp (Get_Field_A);
         Add (',');
         Add_Fp (Get_Field_B);
      end Add_Fd_Fa_Fb_Rc;

      procedure Add_Fd_Fa_Fb_Fc_Rc (Name : String) is
      begin
         Add_Fd_Fa_Fb_Rc (Name);
         Add (',');
         Add_Fp (Get_Field_C);
      end Add_Fd_Fa_Fb_Fc_Rc;

   begin
      W := Get_Insn (Addr);
      Insn_Len := 4;
      Line_Pos := Line'First;

      Opcd := Unsigned_8 (Shift_Right (W, 26));
      case Opcd is
         when 16#7# =>
            Add_D_A_Simm ("mulli");
         when 16#8# =>
            Add_D_A_Simm ("subfic");
         when 16#A# =>
            Add_Crfd_A ("cmpli");
            Add_Uimm;
         when 16#B# =>
            Add_Crfd_A ("cmpi");
            Add_Simm;
         when 16#C# =>
            Add_D_A_Simm ("addic");
         when 16#D# =>
            Add_D_A_Simm ("addic.");
         when 16#E# =>
            declare
               D : constant Unsigned_32 := Get_Field_D;
               A : constant Unsigned_32 := Get_Field_A;
            begin
               if A = 0 then
                  Add ("li");
                  Add_HT;
                  Add_Reg (D);
               else
                  Add ("addi");
                  Add_HT;
                  Add_Reg (D);
                  Add (',');
                  Add_Reg (A);
               end if;
               Add (',');
               Add_Simm;
            end;
         when 16#F# =>
            declare
               D : constant Unsigned_32 := Get_Field_D;
               A : constant Unsigned_32 := Get_Field_A;
            begin
               if A = 0 then
                  Add ("lis");
                  Add_HT;
                  Add_Reg (D);
               else
                  Add ("addis");
                  Add_HT;
                  Add_Reg (D);
                  Add (',');
                  Add_Reg (A);
               end if;
               Add (',');
               Add_Uimm;
            end;
         when 16#10# =>
            declare
               Bd : Unsigned_32 := Get_Field (16, 14);
               Bo : constant Unsigned_8 := Get_Field_Bo;
               Bi : constant Unsigned_8 := Get_Field_Bi;
               Aa : constant Unsigned_32 := Get_Field (30, 1);
               Lk : constant Unsigned_8 := Get_Field_Lk;
            begin
               Add ("bc");
               if Lk = 1 then
                  Add ('l');
               end if;
               if Aa = 1 then
                  Add ('a');
               end if;
               Add_HT;
               Add (Hex_Image (Bo));
               Add (',');
               Add (Hex_Image (Bi));
               Add (',');
               Bd := Shift_Left (Bd, 2);
               if (Bd or 16#8000#) /= 0 then
                  Bd := Bd or 16#Ffff0000#;
               end if;
               Add (Hex_Image (Pc + Bd));
            end;
         when 16#11# =>
            Add ("sc");
         when 16#12# =>
            declare
               Aa : constant Unsigned_8 := Unsigned_8 (Get_Field (30, 1));
               Lk : constant Unsigned_8 := Unsigned_8 (Get_Field (31, 1));
               Li : Unsigned_32 := Get_Field (6, 24);
               Target : Unsigned_32;
            begin
               if (Li and 16#800000#) /= 0 then
                  Li := Li or 16#Ff000000#;
               end if;
               Target := Shift_Left (Li, 2);
               Add ('b');
               if Lk = 1 then
                  Add ('l');
               end if;
               if Aa = 1 then
                  Add ('a');
               else
                  Target := Target + Pc;
               end if;
               Add_HT;
               Add ("0x");
               Add (Hex_Image (Target));
               if Proc_Cb /= null then
                  Proc_Cb.all (Target, Line, Line_Pos);
               end if;
            end;
         when 16#13# =>
            declare
               Xo : constant Unsigned_32 := Get_Field (21, 10);
               Bo, Bi, Lk : Unsigned_8;
            begin
               case Xo is
                  when 2#0000000000# =>
                     Add_Crfd ("mcrf");
                     Add_Cr (Get_Field_Crfs);
                  when 2#0000010000# => -- bclrx
                     Bo := Get_Field_Bo;
                     Bi := Get_Field_Bi;
                     Lk := Get_Field_Lk;
                     if Bo = 16#14# and Bi = 0 and Lk = 0 then
                        Add ("blr");
                     else
                        Add ("bclr");
                        Add_HT;
                        Add (Hex_Image (Bo));
                        Add (',');
                        Add (Hex_Image (Bi));
                        Add (',');
                        Add_Digit (Natural (Lk));
                     end if;
                  when 2#0000100001# =>
                     Add_Crbd_Crba_Crbb ("crnor");
                  when 2#000110010# =>
                     Add ("rfi");
                  when 2#0010010110# =>
                     Add ("isync");
                  when 2#0011000001# =>
                     Add_Crbd_Crba_Crbb ("crxor");
                  when 2#0100100001# =>
                     Add_Crbd_Crba_Crbb ("creqv");
                  when 2#0111000001# =>
                     Add_Crbd_Crba_Crbb ("cror");
                  when 2#1000010000# =>
                     Add ("bcctr");
                     if Get_Field_Lk /= 0 then
                        Add ('l');
                     end if;
                     Add_Ht;
                     Add (Hex_Image (Get_Field_Bo));
                     Add (',');
                     Add (Hex_Image (Get_Field_Bi));
                  when others =>
                     Add ("unknown op: 13-xo=");
                     Add (Bin_Image (Xo, 10));
               end case;
            end;
         when 16#14# =>
            Add_A_S_Sh_Mb_Me_Rc ("rlwimi");
         when 16#15# =>
            Add_A_S_Sh_Mb_Me_Rc ("rlwinm");
         when 16#17# =>
            Add_A_S_B_Rc ("rlwnm");
            Add (',');
            Add_Reg (Get_Field_Mb);
            Add (',');
            Add_Reg (Get_Field_Me);
         when 16#18# =>
            Add_S_A_Uimm ("ori");
         when 16#19# =>
            Add_S_A_Uimm ("oris");
         when 16#1A# =>
            Add_S_A_Uimm ("xori");
         when 16#1B# =>
            Add_S_A_Uimm ("xoris");
         when 16#1C# =>
            Add_S_A_Uimm ("andi.");
         when 16#1D# =>
            Add_S_A_Uimm ("andis.");
         when 16#1f# =>
            declare
               Xo : constant Unsigned_32 := Get_Field (21, 10);
               Spr : Unsigned_32;
               Rc : Unsigned_32;
               S, A, B : Unsigned_32;
               Crfd : Unsigned_8;
            begin
               case Xo is
                  when 2#0000000000# =>
                     Crfd := Unsigned_8 (Get_Field (6, 3));
                     Add ("cmpw");
                     Add_Ht;
                     Add ("cr");
                     Add_Digit (Natural (Crfd));
                     Add (',');
                     --Add_Digit (Natural (Get_Field (9, 1)));
                     --Add (',');
                     Add_Reg (Get_Field_A);
                     Add (',');
                     Add_Reg (Get_Field_B);
                  when 2#0000001000# =>
                     Add_D_A_B_Rc ("subfc");
                  when 2#1000001000# =>
                     Add_D_A_B_Rc ("subfco");
                  when 2#0000001010# =>
                     Add_D_A_B_Rc ("addc");
                  when 2#1000001010# =>
                     Add_D_A_B_Rc ("addco");
                  when 2#0000001011# =>
                     Add_D_A_B_Rc ("mulhwu");
                  when 2#0000010011# =>
                     Add ("mfcr");
                     Add_Ht;
                     Add_Reg (Get_Field_A);
                  when 2#0000010100# =>
                     Add_D_A_B_Rc ("lwarx"); -- Rc = 0
                  when 2#0000010111# =>
                     Add_D_A_B_Rc ("lwzx"); -- Rc = 0
                  when 2#0000011000# =>
                     Add_A_S_B_Rc ("slw");
                  when 2#0000011010# =>
                     Add_A_S_Rc ("cntlzw");
                  when 2#0000011100# =>
                     Add_A_S_B_Rc ("and");
                  when 2#0000100000# =>
                     Add_Crfd_A ("cmpl");
                     Add_Reg (Get_Field_B);
                  when 2#0000101000# =>
                     Add_D_A_B_Rc ("subf");
                  when 2#1000101000# =>
                     Add_D_A_B_Rc ("subfo");
                  when 2#0000110110# =>
                     Add_A_B ("dcbst");
                  when 2#0000111100# =>
                     Add_A_S_B_Rc ("andc");
                  when 2#0001001011# =>
                     Add_D_A_B_Rc ("mulhw");
                  when 2#0001010011# =>
                     Add ("mfmsr");
                     Add_Ht;
                     Add_Reg (Get_Field_D);
                  when 2#0001010110# =>
                     Add_A_B ("dcbf");
                  when 2#0001010111# =>
                     Add_D_A_B_Rc ("lbzx"); -- Rc = 0
                  when 2#0001101000# =>
                     Add_D_A_Rc ("neg");
                  when 2#1001101000# =>
                     Add_D_A_Rc ("nego");
                  when 2#0001110111# =>
                     Add_D_A_B_Rc ("lbzux"); -- Rc = 0
                  when 2#0001111100# =>
                     Add_A_S_B_Rc ("nor");
                  when 2#0010001000# =>
                     Add_D_A_B_Rc ("subfe");
                  when 2#1010001000# =>
                     Add_D_A_B_Rc ("subfeo");
                  when 2#0010001010# =>
                     Add_D_A_B_Rc ("adde");
                  when 2#1010001010# =>
                     Add_D_A_B_Rc ("addeo");
                  when 2#0010010000# =>
                     Add ("mtcrf");
                     Add_Ht;
                     Add ("0x");
                     Add (Hex_Image (Unsigned_8 (Get_Field (12, 8))));
                     Add (',');
                     Add_Reg (Get_Field_S);
                  when 2#0010010010# =>
                     Add ("mtmsr");
                     Add_HT;
                     Add_Reg (Get_Field_S);
                  when 2#0010010110# =>
                     Add_D_A_B_Rc ("stwcx"); -- S = D, Rc = 1
                  when 2#0010010111# =>
                     Add_D_A_B_Rc ("stwu"); -- S = D, Rc = 0
                  when 2#0010110111# =>
                     Add_D_A_B_Rc ("stwux"); -- S = D, Rc = 0
                  when 2#0011001000# =>
                     Add_D_A_Rc ("subfz");
                  when 2#1011001000# =>
                     Add_D_A_Rc ("subfzo");
                  when 2#0011001010# =>
                     Add_D_A_Rc ("addze");
                  when 2#1011001010# =>
                     Add_D_A_Rc ("addzeo");
                  when 2#0011010010# =>
                     Add ("mtsr");
                     Add_HT;
                     Add_Num2 (Get_Field (12, 4));
                     Add (',');
                     Add_Reg (Get_Field_S);
                  when 2#0011010111# =>
                     Add_D_A_B_Rc ("stbx"); -- S = D, Rc = 0
                  when 2#0011101000# =>
                     Add_D_A_Rc ("subfme");
                  when 2#1011101000# =>
                     Add_D_A_Rc ("subfmeo");
                  when 2#0011101010# =>
                     Add_D_A_Rc ("addme");
                  when 2#1011101010# =>
                     Add_D_A_Rc ("addmeo");
                  when 2#0011101011# =>
                     Add_D_A_B_Rc ("mullw");
                  when 2#1011101011# =>
                     Add_D_A_B_Rc ("mullwo");
                  when 2#0011110010# =>
                     Add_D ("mtsrin");  -- S = D
                     Add (',');
                     Add_Reg (Get_Field_B);
                  when 2#0011110111# =>
                     Add_D_A_B_Rc ("stbux"); -- S=D, Rc = 0
                  when 2#0100001010# =>
                     Add_D_A_B_Rc ("add");
                  when 2#1100001010# =>
                     Add_D_A_B_Rc ("addo");
                  when 2#0100010111# =>
                     Add_D_A_B_Rc ("lhzx"); -- Rc = 0
                  when 2#0100110010# =>
                     Add ("tlbie");
                     Add_Ht;
                     Add_Reg (Get_Field_B);
                  when 2#0100111100# =>
                     Add_A_S_B_Rc ("xor");
                  when 2#0101010011# =>
                     Spr := Get_Field (11, 10);
                     if Spr = Spr_Lr then
                        Add ("mflr");
                        Add_Ht;
                        Add_Reg (Get_Field_D);
                     else
                        Add ("mfspr");
                        Add_HT;
                        Add_Reg (Get_Field_D);
                        Add (",0x" );
                        Add (Hex_Image (Unsigned_16 (Spr)));
                     end if;
                  when 2#0101010111# =>
                     Add_D_A_B_Rc ("lhax"); -- Rc = 0
                  when 2#0101110011# =>
                     Add ("mftb");
                     Add_HT;
                     Add_Reg (Get_Field_D);
                     Add (",0x" );
                     Add (Hex_Image (Unsigned_16 (Get_Field (11, 10))));
                  when 2#0110010111# =>
                     Add_D_A_B_Rc ("sthx"); -- D = S, Rc = 0
                  when 2#0110011100# =>
                     Add_A_S_B_Rc ("orc");
                  when 2#0110111100# =>
                     S := Get_Field_S;
                     A := Get_Field_A;
                     B := Get_Field_B;
                     Rc := Get_Field (31, 1);
                     if S = B and Rc = 0 then
                        Add ("mov");
                        Add_Ht;
                        Add_Reg (A);
                        Add (',');
                        Add_Reg (S);
                     else
                        Add_Rc_HT ("or");
                        Add_Reg (A);
                        Add (',');
                        Add_Reg (S);
                        Add (',');
                        Add_Reg (B);
                     end if;
                  when 2#0111001011# =>
                     Add_D_A_B_Rc ("divwu");
                  when 2#1111001011# =>
                     Add_D_A_B_Rc ("divwuo");
                  when 2#0111010011# =>
                     Spr := Get_Field (11, 10);
                     if Spr = Spr_Lr then
                        Add ("mtlr");
                        Add_HT;
                        Add_Reg (Get_Field_D);
                     elsif Spr = Spr_Ctr then
                        Add ("mtctr");
                        Add_HT;
                        Add_Reg (Get_Field_D);
                     else
                        Add ("mtspr");
                        Add_HT;
                        Add_Reg (Get_Field_D);
                        Add (",0x");
                        Add (Hex_Image (Unsigned_16 (Spr)));
                     end if;
                  when 2#0111011100# =>
                     Add_A_S_B_Rc ("nand");
                  when 2#0111101011# =>
                     Add_D_A_B_Rc ("divw");
                  when 2#1111101011# =>
                     Add_D_A_B_Rc ("divwo");
                  when 2#1000010110# =>
                     Add_D_A_B_Rc ("lwbrx"); -- Rc = 0
                  when 2#1000010111# =>
                     Add_D_A_B_Rc ("lfsx"); -- Rc = 0
                  when 2#1000011000# =>
                     Add_A_S_B_Rc ("srw");
                  when 2#1000110110# =>
                     Add ("tlbsync");
                  when 2#1000110111# =>
                     Add_D_A_B_Rc ("lfsux"); -- Rc = 0
                  when 2#1001010011# =>
                     Add ("mfsr");
                     Add_HT;
                     Add_Reg (Get_Field_D);
                     Add (',');
                     Add_Num2 (Get_Field (12,3));
                  when 2#1001010110# =>
                     Add ("sync");
                  when 2#1001010111# =>
                     Add_D_A_B_Rc ("lfdx"); -- Rc = 0;
                  when 2#1001110111# =>
                     Add_D_A_B_Rc ("lfdxu"); -- Rc = 0;
                  when 2#1010010011# =>
                     Add_D ("mfsrin");
                     Add (',');
                     Add_Reg (Get_Field_B);
                  when 2#1010010110# =>
                     Add_D_A_B_Rc ("stwbrx"); -- S = D, Rc
                  when 2#1010010111# =>
                     Add_D_A_B_Rc ("stfsx"); -- S = R, Rc = 0
                  when 2#1011010111# =>
                     Add_D_A_B_Rc ("stfdx"); -- S = R, Rc = 0
                  when 2#1011110111# =>
                     Add_D_A_B_Rc ("stfdux"); -- S = R, Rc = 0
                  when 2#1100010110# =>
                     Add_D_A_B_Rc ("lhbrx"); -- Rc = 0
                  when 2#1100011000# =>
                     Add_A_S_B_Rc ("sraw");
                  when 2#1100111000# =>
                     Add_A_S_Rc ("srawi");
                     Add (',');
                     Add_Num2 (Get_Field_Sh);
                  when 2#1101010110# =>
                     Add ("eieio");
                  when 2#1110010110# =>
                     Add_D_A_B_Rc ("sthbrx"); -- S = R, Rc = 0
                  when 2#1110011010# =>
                     Add_A_S_Rc ("extsh");
                  when 2#1110111010# =>
                     Add_A_S_Rc ("extsb");
                  when 2#1111010110# =>
                     Add_A_B ("ibci");
                  when others =>
                     Add ("unknown op: 1f-xo=");
                     Add (Bin_Image (Xo, 10));
               end case;
            end;
         when 16#20# | 16#21#=>
            Add_Int_Load_Store ("lwz");
         when 16#22# | 16#23# =>
            Add_Int_Load_Store ("lbz");
         when 16#24# | 16#25# =>
            Add_Int_Load_Store ("stw");
         when 16#26# | 16#27# =>
            Add_Int_Load_Store ("stb");
         when 16#28# | 16#29# =>
            Add_Int_Load_Store ("lhz");
         when 16#2a# | 16#2b# =>
            Add_Int_Load_Store ("lha");
         when 16#2c# | 16#2d# =>
            Add_Int_Load_Store ("sth");
         when 16#30# | 16#31# =>
            Add_Fp_Load_Store ("lfs");
         when 16#32# | 16#33# =>
            Add_Fp_Load_Store ("lfd");
         when 16#34# | 16#35# =>
            Add_Fp_Load_Store ("stfs");
         when 16#36# | 16#37# =>
            Add_Fp_Load_Store ("stfd");
         when 16#3B# =>
            declare
               Xo : constant Unsigned_32 := Get_Field (26, 5);
            begin
               case Xo is
                  when 2#10010# =>
                     Add_Fd_Fa_Fb_Rc ("fdivs");
                  when 2#10100# =>
                     Add_Fd_Fa_Fb_Rc ("fsubs");
                  when 2#10101# =>
                     Add_Fd_Fa_Fb_Rc ("fadds");
                  when 2#11001# =>
                     Add_Fd_Rc ("fmuls");
                     Add (',');
                     Add_Fp (Get_Field_A);
                     Add (',');
                     Add_Fp (Get_Field_C);
                  when 2#11100# =>
                     Add_Fd_Fa_Fb_Fc_Rc ("fmsubs");
                  when 2#11101# =>
                     Add_Fd_Fa_Fb_Fc_Rc ("fmadds");
                  when 2#11110# =>
                     Add_Fd_Fa_Fb_Fc_Rc ("fnmsubs");
                  when 2#11111# =>
                     Add_Fd_Fa_Fb_Fc_Rc ("fnmadds");
                  when others =>
                     Add ("unknown op: 3b-xo=");
                     Add (Bin_Image (Xo, 5));
               end case;
            end;
         when 16#3F# =>
            declare
               Xo : constant Unsigned_32 := Get_Field (21, 10);
            begin
               case Xo and 2#11111# is
                  when 2#10010# =>
                     Add_Fd_Fa_Fb_Rc ("fdiv");
                     return;
                  when 2#10100# =>
                     Add_Fd_Fa_Fb_Rc ("fsub");
                     return;
                  when 2#10101# =>
                     Add_Fd_Fa_Fb_Rc ("fadd");
                     return;
                  when 2#11001# =>
                     Add_Fd_Rc ("fmul");
                     Add (',');
                     Add_Fp (Get_Field_A);
                     Add (',');
                     Add_Fp (Get_Field_C);
                     return;
                  when 2#11100# =>
                     Add_Fd_Fa_Fb_Fc_Rc ("fmsub");
                     return;
                  when 2#11101# =>
                     Add_Fd_Fa_Fb_Fc_Rc ("fmadd");
                     return;
                  when 2#11110# =>
                     Add_Fd_Fa_Fb_Fc_Rc ("fnmsub");
                     return;
                  when 2#11111# =>
                     Add_Fd_Fa_Fb_Fc_Rc ("fnmadd");
                     return;
                  when others =>
                     null;
               end case;
               case Xo is
                  when 2#0000000000# =>
                     Add_Crfd ("fcmpu");
                     Add_Fp (Get_Field_A);
                     Add (',');
                     Add_Fp (Get_Field_B);
                  when 2#0000001100# =>
                     Add_Fd_fb_Rc ("frsp");
                  when 2#0000001110# =>
                     Add_Fd_Fb_Rc ("fctiw");
                  when 2#0000001111# =>
                     Add_Fd_Fb_Rc ("fctiwz");
                  when 2#0000100000# =>
                     Add_Crfd ("fcmpo");
                     Add_Fp (Get_Field_A);
                     Add (',');
                     Add_Fp (Get_Field_B);
                  when 2#0000100110# =>
                     Add_Rc_Ht ("mtfsb1");
                     Add_Crb (Get_Field_D);
                  when 2#0001000000# =>
                     Add_Crfd ("mcrfs");
                     Add_Cr (Get_Field_Crfs);
                  when 2#0001000110# =>
                     Add_Rc_Ht ("mtfsb0");
                     Add_Crb (Get_Field_D);
                  when 2#0000101000# =>
                     Add_Fd_fb_Rc ("fneg");
                  when 2#0001001000# =>
                     Add_Fd_Fb_Rc ("fmr");
                  when 2#0010000110# =>
                     Add_Rc_HT ("mtfsfi");
                     Add_Cr (Get_Field_Crfd);
                     Add (",0x");
                     Add (Hex_Image (Unsigned_8 (Get_Field (16, 4))));
                  when 2#0010001000# =>
                     Add_Fd_Fb_Rc ("fnabs");
                  when 2#0100001000# =>
                     Add_Fd_Fb_Rc ("fabs");
                  when 2#1001000111# =>
                     Add_Rc_HT ("mffs");
                     Add_Fp (Get_Field_D);
                  when 2#1011000111# =>
                     Add_Rc_Ht ("mtfsf");
                     Add ("0x");
                     Add (Hex_Image (Unsigned_8 (Get_Field (7, 8))));
                     Add (',');
                     Add_Fp (Get_Field_B);
                  when others =>
                     Add ("unknown op: 3f-xo=");
                     Add (Bin_Image (Xo, 10));
               end case;
            end;

         when 16#00# | 16#01# | 16#02# | 16#1e# =>
            Add (".long");
            Add_HT;
            Add ("0x");
            Add (Hex_Image (W));

         when others =>
            Add ("unknown op: ");
            Add (Hex_Image (Opcd));
      end case;
   end Disassemble_Insn;
end Disa_Ppc;

