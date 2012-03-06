------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2012, AdaCore                     --
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

--  This program generates a GNATcoverage trace file from data created
--  by running a program on a target system under control of an iSystem
--  probe, which collects NEXUS realtime program trace data from the target.

with Ada.Command_Line; use Ada.Command_Line;
with Text_IO;          use Text_IO;
with Interfaces;       use Interfaces;
with Ada.Exceptions;   use Ada.Exceptions;

with Nexus_Rep; use Nexus_Rep;
with Isys2nex;  use Isys2nex;

with Elf_Files;    use Elf_Files;
with Elf_Common;   use Elf_Common;
with Elf32;        use Elf32;
with Elf_Arch32;   use Elf_Arch32;
with Swaps;        use Swaps;
with Traces_Dbase; use Traces_Dbase;
with Traces;       use Traces;
with Qemu_Traces;  use Qemu_Traces;
with Traces_Files; use Traces_Files;

procedure Nexus_Trace_Gen is
   Usage : constant String := "usage: Nexus_Trace_Gen binary_file ocd_file";

   type String_Ptr is access String;

   Executable_Filename : String_Ptr;
   OCD_Filename        : String_Ptr;

   Executable_File  : Elf_File;
   Ehdr             : Elf_Ehdr;
   Entry_Point      : Elf32_Addr;
   Text_Shdr_Idx    : Elf_Half;
   Text_Shdr_Ptr    : Elf_Shdr_Acc;
   Text_Section_Len : Elf_Size;
   Text_First_Addr  : Unsigned_32;

   type Insns_Array is array (Positive range <>) of Unsigned_32;
   Insns_Ptr        : access Insns_Array;
   N_Insns          : Positive;

   type Insn_Flags_T is record
      Been_Executed : Boolean;
      Is_Branch     : Boolean;
      Br_Taken      : Boolean;
      Br_Not_Taken  : Boolean;
   end record;
   type Insn_Flags_Array_T is array (Positive range <>) of Insn_Flags_T;
   Insn_Flags                 : access Insn_Flags_Array_T;
   Op_Code                    : Unsigned_32;
   Insn_Idx                   : Positive;
   Block_Begin_Idx            : Positive;
   Block_End_Idx              : Positive;
   New_Addr                   : Unsigned_32;
   Br_Insn                    : Unsigned_32;
   Br_Addr                    : Unsigned_32;
   AA_Bit                     : Unsigned_32;
   U_Addr                     : Unsigned_32;
   Last_Indirect_Or_Sync_Addr : Unsigned_32 := 0;

   Nexus_Msg_List_Elem : Nexus_Message_List_Elem_Ptr_T;
   Nexus_Msg           : Nexus_Message_Rec_Ptr_T;
   Message_List        : Nexus_Message_List_T;

   At_Trace_Start : Boolean;

   Exe_Exception : exception;
   procedure Chk_Exe (Msg : String);
   procedure Chk_Exe (Msg : String) is
   begin
      if Get_Status (Executable_File) /= Status_Ok then
         raise Exe_Exception with Msg;
      end if;
   end Chk_Exe;

   function To_Hex_Word_Str (Val : Unsigned_32) return String;
   function To_Hex_Word_Str (Val : Unsigned_32) return String is
      S   : String (1 .. 13) := "16#0000_0000#";
      Idx : Positive := 12;
      V   : Unsigned_32 := Val;

      D : constant array (Unsigned_32 range 0 .. 15) of Character
        := ('0', '1', '2', '3', '4', '5', '6', '7',
            '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
   begin
      while V /= 0 loop
         S (Idx) := D (V mod 16);
         Idx := Idx - 1;
         if Idx = 8 then
            Idx := 7;
         end if;
         V := V / 16;
      end loop;
      return S;
   end To_Hex_Word_Str;

begin
   if Argument_Count /= 2 then
      Put_Line (Standard_Error, Usage);
      Set_Exit_Status (1);
      return;
   end if;

   Executable_Filename := new String'(Argument (1));
   Open_File (Executable_File, Executable_Filename.all);

   Chk_Exe ("Error Opening");
   Ehdr := Get_Ehdr (Executable_File);
   Chk_Exe ("Error reading file header");
   Entry_Point := Ehdr.E_Entry;
   --  Open the executable ELF file, get the ELF header from
   --  it, and read the entry point of the program.
   --  For now, we are presuming the OCD trace data starts
   --  from the entry point (not explicit in the trace output),
   --  though that could change with the use of triggers.

   Load_Shdr (Executable_File);
   Chk_Exe ("Error retrieving section headers");
   Text_Shdr_Idx := Get_Shdr_By_Name (Executable_File, ".text");
   Chk_Exe ("Error finding "".text"" section");
   Text_Shdr_Ptr := Get_Shdr (Executable_File, Text_Shdr_Idx);
   Chk_Exe ("Error getting "".text"" section header");
   Text_First_Addr  := Text_Shdr_Ptr.Sh_Addr;
   Text_Section_Len := Text_Shdr_Ptr.Sh_Size;
   if Text_Section_Len < 4 then
      Close_File (Executable_File);
      raise Program_Error with ".text section too small.";
   end if;
   N_Insns := Positive (Text_Section_Len / 4);
   Insns_Ptr := new Insns_Array (1 .. N_Insns);
   Load_Section (Executable_File, Text_Shdr_Idx,
                 Insns_Ptr (Insns_Ptr'First)'Address);
   Chk_Exe ("Error reading code from executable");
   for J in Insns_Ptr'Range loop
      Insns_Ptr (J) := Swap (Insns_Ptr (J));
   end loop;
   --  All the above is to extract the code from the executable into
   --  an array of instructions.
   --  Two generalization are probably advisable:  ???
   --    - go through all sections and find code (not juse .text)
   --    - determine if swapping is needed (PPC -> x86 presumed now).

   Close_File (Executable_File);

   Insn_Flags := new Insn_Flags_Array_T (1 .. N_Insns);
   for J in Insn_Flags'Range loop
      Op_Code := Shift_Right (Insns_Ptr (J), 26);
      if Op_Code = 16 or else Op_Code = 18 or else
        (Op_Code = 19 and then ((Insns_Ptr (J) and 16#3fe#) = 16#20#)) then
         Insn_Flags (J) := (Is_Branch => True, others => False);
      else
         Insn_Flags (J) := (others => False);
      end if;
   end loop;
   --  ??? Only the instructions whose mnemonics begin with 'b' are
   --  dealt with. Many other instuctions (particularly with major
   --  opcode = 19) will cause a change in flow, and will generate
   --  NEXUS program trace messages.

   --  Put_Line (To_Hex_Word_Str (Entry_Point));
   --  Put_Line (To_Hex_Word_Str (Text_First_Addr));

   OCD_Filename := new String'(Argument (2));
   Message_List := Ocdfile_To_Nexus_List (OCD_Filename.all);
   Nexus_Msg_List_Elem := Message_List.First;
   if Nexus_Msg_List_Elem = null then
      raise Program_Error with "Empty Nexus message list";
   end if;

   Nexus_Msg := Nexus_Msg_List_Elem.Message;
   if Nexus_Msg.Tcode /= Debug_Status then
      raise Program_Error with "Expected first Nexus message to be Status";
   elsif Nexus_Msg.Debug_Status_V.STATUS /= 0 then
      raise Program_Error with "Expected initial Status value to be 0";
   end if;

   --  In Insn_Flags we have a list of flags set for each instruction.
   --  We have the sequence of Nexus messages in Message_List.
   --  We now go trhough the Message_List and modify the flags in
   --  Insn_Flags to indicate which instructions have been executed,
   --  and which direction(s) were taken on executed branch instructions.
   --  Later the Insn_Flags array will be processed to generate GNATcoverage
   --  trace data.  This works for Flat traces. Needs reconsideration for
   --  other kinds of trace data files.  ???

   Block_Begin_Idx := Positive (Entry_Point - Text_First_Addr) / 4 + 1;
   At_Trace_Start := True;
   --  I-CNT from first Program Trace Message after the start of trace
   --  is 1 less than other cases. Flag that we are at the start of the
   --  trace, and at the end of the loop check that it is turned off
   --  (i.e. processing of first subquent message should make false).
   loop
      New_Addr := Text_First_Addr + Unsigned_32 (Block_Begin_Idx - 1) * 4;
      Put_Line ("Block starting address: " & To_Hex_Word_Str (New_Addr));
      Nexus_Msg_List_Elem := Nexus_Msg_List_Elem.Next;
      if Nexus_Msg_List_Elem = null then
         raise Program_Error with "Unexpected end of message list";
      end if;

      Nexus_Msg := Nexus_Msg_List_Elem.Message;
      case Nexus_Msg.Tcode is
         when Debug_Status                                 =>
            raise Program_Error with "Unexpected TCODE";

         when Ownership_Trace_Message                      =>
            raise Program_Error with "Unexpected TCODE";

         when Prog_Trace_Direct_Branch_Message             =>
            N_Insns := Positive
              (Nexus_Msg.Prog_Trace_Direct_Branch_Message_V.I_CNT + 1);
            Block_End_Idx := Block_Begin_Idx + N_Insns - 1;
            Insn_Idx := Block_Begin_Idx;
            loop
               Insn_Flags (Insn_Idx).Been_Executed := True;
               if Insn_Idx = Block_End_Idx then
                  if not Insn_Flags (Insn_Idx).Is_Branch then
                     raise Program_Error with "End of block not a branch";
                  end if;
                  Insn_Flags (Insn_Idx).Br_Taken := True;
                  exit;
               elsif Insn_Flags (Insn_Idx).Is_Branch then
                  Insn_Flags (Insn_Idx).Br_Not_Taken := True;
               end if;
               Insn_Idx := Insn_Idx + 1;
            end loop;
            Br_Insn := Insns_Ptr (Block_End_Idx);
            Br_Addr := Text_First_Addr + Unsigned_32 (Block_End_Idx - 1) * 4;
            Op_Code := Shift_Right (Br_Insn, 26);
            AA_Bit := Shift_Right ((Br_Insn and 2#10#), 1);
            if Op_Code = 18 then
               New_Addr := Shift_Right_Arithmetic (Shift_Left (Br_Insn, 6), 6);
               if AA_Bit = 0 then
                  New_Addr := New_Addr + Br_Addr;
               end if;
            elsif Op_Code = 16 then
               New_Addr :=
                 Shift_Right_Arithmetic (Shift_Left (Br_Insn, 16), 16);
               if AA_Bit = 0 then
                  New_Addr := New_Addr + Br_Addr;
               end if;
            else
               raise Program_Error with "Unexpected opcode for Direct Branch";
            end if;
            Block_Begin_Idx := Positive (New_Addr - Text_First_Addr) / 4 + 1;

         when Prog_Trace_Indirect_Branch_Message           =>
            N_Insns := Positive
              (Nexus_Msg.Prog_Trace_Indirect_Branch_Message_V.I_CNT + 1);
            Block_End_Idx := Block_Begin_Idx + N_Insns - 1;
            Insn_Idx := Block_Begin_Idx;
            loop
               Insn_Flags (Insn_Idx).Been_Executed := True;
               if Insn_Idx = Block_End_Idx then
                  if not Insn_Flags (Insn_Idx).Is_Branch then
                     raise Program_Error with "End of block not a branch";
                  end if;
                  Insn_Flags (Insn_Idx).Br_Taken := True;
                  exit;
               elsif Insn_Flags (Insn_Idx).Is_Branch then
                  Insn_Flags (Insn_Idx).Br_Not_Taken := True;
               end if;
               Insn_Idx := Insn_Idx + 1;
            end loop;
            U_Addr := Unsigned_32
              (Nexus_Msg.Prog_Trace_Indirect_Branch_Message_V.U_ADDR);
            --  Br_Addr:= Text_First_Addr + Unsigned_32(Block_End_Idx - 1) * 4;
            New_Addr := Last_Indirect_Or_Sync_Addr xor U_Addr;
            Block_Begin_Idx := Positive (New_Addr - Text_First_Addr) / 4 + 1;
            Last_Indirect_Or_Sync_Addr := New_Addr;

         when Data_Trace_Data_Write_Message                =>
            raise Program_Error with "Unexpected TCODE";

         when Data_Trace_Data_Read_Message                 =>
            raise Program_Error with "Unexpected TCODE";

         when Error_Message                                =>
            raise Program_Error with "Unexpected TCODE";

         when Prog_Trace_Direct_Branch_Message_Sync        =>
            N_Insns := Positive
              (Nexus_Msg.Prog_Trace_Direct_Branch_Message_Sync_V.I_CNT + 1);
            if At_Trace_Start then
               N_Insns := N_Insns + 1;
               At_Trace_Start := False;
            end if;
            Block_End_Idx := Block_Begin_Idx + N_Insns - 1;
            Insn_Idx := Block_Begin_Idx;
            loop
               Insn_Flags (Insn_Idx).Been_Executed := True;
               if Insn_Idx = Block_End_Idx then
                  if not Insn_Flags (Insn_Idx).Is_Branch then
                     raise Program_Error with "End of block not a branch";
                  end if;
                  Insn_Flags (Insn_Idx).Br_Taken := True;
                  exit;
               elsif Insn_Flags (Insn_Idx).Is_Branch then
                  Insn_Flags (Insn_Idx).Br_Not_Taken := True;
               end if;
               Insn_Idx := Insn_Idx + 1;
            end loop;
            New_Addr := Unsigned_32
              (Nexus_Msg.Prog_Trace_Direct_Branch_Message_Sync_V.F_ADDR);
            Block_Begin_Idx := Positive (New_Addr - Text_First_Addr) / 4 + 1;
            Last_Indirect_Or_Sync_Addr := New_Addr;

         when Prog_Trace_Indirect_Branch_Message_Sync      =>
            null;
         when Data_Trace_Data_Write_Message_Sync           =>
            raise Program_Error with "Unexpected TCODE";

         when Data_Trace_Data_Read_Message_Sync            =>
            raise Program_Error with "Unexpected TCODE";

         when Watchpoint_Message                           =>
            exit;
         when Resource_Full_Message                        =>
            raise Program_Error with "Unexpected TCODE";

         when Prog_Trace_Indirect_Branch_Hist_Message      =>
            null;
         when Prog_Trace_Indirect_Branch_Hist_Message_Sync =>
            null;
         when Prog_Trace_Program_Correlation_Message       =>
            raise Program_Error with "Unexpected TCODE";

         when  1 | 7 | 9 | 10 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23
            |  24 | 25 | 26 | 30 | 31 | 32 =>
            raise Program_Error with "Invalid TCODE";

      end case;
      if At_Trace_Start then
         raise Program_Error with "Trace Start not cleared";
      end if;
   end loop;

   declare
      --  This declare block contains the code for creating the trace file
      --  from the Insn_Flags array.

      In_Block : Boolean;

      Block_Start, Block_End : Unsigned_32;

      Tbase          : Traces_Base;
      Tfile          : Trace_File_Type;
      Trace_Filename : String_Ptr;
      Op             : Unsigned_8;
   begin
      Init_Base (Tbase);
      Trace_Filename := new String'(Executable_Filename.all & ".trace");

      In_Block := False;
      for J in Insn_Flags'Range loop
         if Insn_Flags (J).Been_Executed then
            if not In_Block then
               In_Block := True;
               Block_Start := Text_First_Addr + Unsigned_32 (J - 1) * 4;
            end if;
            if Insn_Flags (J).Is_Branch or else J = Insn_Flags'Last then
               In_Block := False;
               Block_End := Text_First_Addr + Unsigned_32 (J - 1) * 4;
               Put (To_Hex_Word_Str (Block_Start) & ' ');
               Put (To_Hex_Word_Str (Block_End) & ' ');
               if J /= Insn_Flags'Last then
                  Put (Boolean'Image (Insn_Flags (J).Br_Taken) & ' ');
                  Put (Boolean'Image (Insn_Flags (J).Br_Not_Taken));
                  Op := Trace_Op_Block;
                  if Insn_Flags (J).Br_Taken then
                     Op := Op or Trace_Op_Br0;
                  end if;
                  if Insn_Flags (J).Br_Not_Taken then
                     Op := Op or Trace_Op_Br1;
                  end if;
                  if Block_End > 16#4000_01e0# then
                     Add_Entry (Tbase, Block_Start, Block_End, Op);
                  end if;

                  New_Line;
               else
                  New_Line;
                  exit;
               end if;
            end if;
         end if;
      end loop;
      Traces.Machine := 20;
      Create_Trace_File (Flat, Tfile);
      Append_Info (Tfile, Exec_File_Name, Executable_Filename.all);
      Write_Trace_File (Trace_Filename.all, Tfile, Tbase);

   end;

exception
   when E : Isys2nex_Error =>
      Put_Line (Standard_Error, Exception_Message (E));
      Set_Exit_Status (1);
   when E : Program_Error =>
      Put_Line (Standard_Error, Exception_Message (E));
      Set_Exit_Status (1);
   when E : Exe_Exception =>
      Put_Line (Standard_Error, "In " & Executable_Filename.all & ':'
                & Exception_Message (E));
      Close_File (Executable_File);
      Set_Exit_Status (1);
end Nexus_Trace_Gen;
