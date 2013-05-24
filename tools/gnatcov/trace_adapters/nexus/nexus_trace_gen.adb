------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2013, AdaCore                     --
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
--
--  This program is only intended to be called from the driver program
--  isys_drv. The 2 programs, and the python program also called by
--  isys_drv to run the target executable, may all be integrated eventually.
--  Checking of errors from arguments or inputs is not thorough since this
--  is not a user-run program.
--
--  The program requires arguments on the command line naming the
--  processor, the executable file, the nexus trace file (aka the
--  On Chip Debug file), a history file name (or the argument
--  "--nohist") and the name of the initialized qemu trace
--  file which is to be filled in. Also, there are arguments
--  that describe the HW trigger settings that were used when
--  the traces were generated. These are the IAC register that
--  was used for the start trigger, the address set in that IAC
--  register, and the IAC register used for the stop tirgger or
--  the constant '0' if no stop trigger was set. This trigger information
--  is used for anchoring BTMs as Nexus tracing is turned on and
--  off by the triggers.
--
--  Currently this works only for 32 bit PowerPC Book E processors and
--  presumption of that constraint is hard-coded in, in many places
--  (particularly the 32 bit word size, machine code of 20, swap to
--  little endian host).

with Ada.Unchecked_Deallocation;
with Ada.Characters.Handling; use Ada.Characters.Handling;

with Ada.Command_Line; use Ada.Command_Line;
with Text_IO;          use Text_IO;
with Interfaces;       use Interfaces;
with Ada.Exceptions;   use Ada.Exceptions;
with GNAT.OS_Lib;      use GNAT.OS_Lib;
with System;           use System;

with System.Storage_Elements; use System.Storage_Elements;

with Nexus_Rep; use Nexus_Rep;
with Isys2nex;  use Isys2nex;

with Elf_Files;    use Elf_Files;
with Elf_Common;   use Elf_Common;
with Elf32;        use Elf32;
with Elf_Arch;     use Elf_Arch;
with Swaps;        use Swaps;
with Traces;       use Traces;
with Qemu_Traces;  use Qemu_Traces;
with Traces_Files; use Traces_Files;
with Strings;      use Strings;

procedure Nexus_Trace_Gen is

   procedure Usage;

   procedure Usage is
      procedure P (S : String) renames Put_Line;
   begin
      P ("usage: Nexus_Trace_Gen proc_id binary_file ocd_file" &
           " --nohist|hist_file trace_file");
      P ("  PT_Start_IAC_# PT_Start_Addr never|PT_Stop_IAC");
   end Usage;

   type String_Ptr is access String;

   Tracefile_Path      : String_Ptr;
   Histfile_Path       : String_Ptr;
   Executable_Filename : String_Ptr;
   Processor_ID        : String_Ptr;

   OCD_Filename        : String_Ptr;
   --  On Chip Debug file: file containing the Nexus trace messages.

   Executable_File  : Elf_File;
   Ehdr             : Elf_Ehdr;
   Text_Shdr_Idx    : Elf_Half;
   Text_Shdr_Ptr    : Elf_Shdr_Acc;
   Text_Section_Len : Elf_Addr;
   Text_First_Addr  : Unsigned_32;
   --  Exe_Sym          : Elf_Sym;

   type Watchpoint_T is mod 2 ** 8;
   Watchpoints_Seen : Watchpoint_T;

   J                 : Positive;
   Under_OK          : Boolean;
   PT_Start_IAC_Bit  : Watchpoint_T;
   PT_Stop_IAC_Bit   : Watchpoint_T;
   PT_Start_Address  : Unsigned_32;
   PT_Running        : Boolean;
   --  Vars used for trace start address options procesing and use.

   type Insns_Array is array (Positive range <>) of Unsigned_32;
   Insns_Ptr        : access Insns_Array;
   N_Insns          : Positive;
   Insn_Count       : Natural;

   EV_Code : Nexus_Packet_T;

   ICNT_Adj_Val     : Nexus_Packet_T;
   ICNT_Proc_Adj    : Nexus_Packet_T;
   function ICNT_Adj return Nexus_Packet_T;
   --  The Insrtruction Count value returned in various BT messages needs
   --  some adjustement to consistently include all the intrsuctions since
   --  that last BT message (or since PT is started).
   --
   --  One source of difference between the value returned and the value
   --  desired, is differences in implementations of Nexus modules. E.g. the
   --  value returned by the module on the 5554 is one short (as if the
   --  branch instruction is not counted), whereas the value returned by
   --  the 5634 module is the desired count. The variable ICNT_Proc_Adj is
   --  set to the processor specific adjustment that is needed.
   --
   --  The processor specific adjustment is always needed, but other
   --  adjustments result from dynamic events, and only apply to the
   --  next BT message that occurs. ICNT_Adj_Val is the actual value
   --  that should be applied to the next BT message, and it is the
   --  value that is returned by the function ICNT_Adj. ICNT_Adj also
   --  resets ICNT_Adj_Val to ICNT_Proc_Adj.
   --
   --  Thus, the events that require changes to the adjustment value
   --  modify ICNT_Adj_Val, and the function ICNT_Adj is called during
   --  BT message processing to get the adjustment needed.
   --
   --  One dynamic event is the startup of PT message generation,
   --  either when the CPU starts (if DC1 enabling is used -- not allowed
   --  at this point) or when the watchpoint programmed into the Watchpoint
   --  Trigger register is hit -- this is discovered in Watchpoint Message
   --  processing. The ICNT in the initial BTM after this is one to low.
   --
   --  The other dynamic event is overflow of the instruction counter between
   --  BT messages. This is indicated by a Resource Full message.

   function ICNT_Adj return Nexus_Packet_T is
      R : Nexus_Packet_T := ICNT_Adj_Val;
   begin
      ICNT_Adj_Val := ICNT_Proc_Adj;
      return R;
   end ICNT_Adj;

   type Insn_Flags_T is record
      Been_Executed : Boolean;
      Is_Branch     : Boolean;
      Br_Taken      : Boolean;
      Br_Not_Taken  : Boolean;
      Historical    : Boolean;
   end record;
   type Insn_Flags_Array_T is array (Positive range <>) of Insn_Flags_T;
   Insn_Flags                 : access Insn_Flags_Array_T;

   Op_Code                    : Unsigned_32;
   Ext_Op_Code                : Unsigned_32;
   Insn_Idx                   : Positive;
   Idx2                       : Positive;
   Block_Begin_Idx            : Positive;
   Block_End_Idx              : Positive;
   Trace_Start_Idx            : Positive;
   New_Addr                   : Unsigned_32;
   Br_Insn                    : Unsigned_32;
   Br_Addr                    : Unsigned_32;
   AA_Bit                     : Unsigned_32;
   U_Addr                     : Unsigned_32;
   Last_Indirect_Or_Sync_Addr : Unsigned_32 := 0;

   Nexus_Msg_List_Elem : Nexus_Message_List_Elem_Ptr_T;
   Nexus_Msg           : Nexus_Message_Rec_Ptr_T;
   Message_List        : Nexus_Message_List_T;

   Exe_Exception : exception;
   procedure Chk_Exe (Msg : String);
   procedure Chk_Exe (Msg : String) is
   begin
      if Get_Status (Executable_File) /= Status_Ok then
         raise Exe_Exception with Msg;
      end if;
   end Chk_Exe;

   function Exe_Address_From_Arg (Arg : String) return Unsigned_32;
   --  Some command line options can specify an address from the
   --  executable. The address can be given either by a symbol name,
   --  or a hexadecimal value using either the C 0xHHHH syntax or
   --  the Ada 16#HHHH# syntax. A symbol name cannot have "0x" or
   --  "16#" as a proper prefix.
   --
   --  If a valid address is not found, a message will be output to
   --  Standard_Error and a Constraint_Error will be raised.
   --
   --  Presumes "Executable_File" has been opened, "Ehdr" retrieved and
   --  Section Headers loaded.

   function Exe_Address_From_Arg (Arg : String) return Unsigned_32 is
      A : Unsigned_32 := 0;
      Invalid_Insn_Addr : constant Unsigned_32 := 1;

      function Lookup_Sym_Value (S : String) return Unsigned_32;

      function Lookup_Sym_Value (S : String) return Unsigned_32 is
         Sym : Elf_Sym;
         N_Syms  : Natural;
         N_Bytes : Natural;
         Str_Idx : Natural;
         K       : Natural;
         Symtab_Shdr_Idx : Elf_Half;
         Symtab_Shdr_Ptr : Elf_Shdr_Acc;
         type Sym_Array_T is array (Positive range <>) of Elf_Sym;
         type Sym_Array_Ptr_T is access all Sym_Array_T;
         procedure Free is
           new Ada.Unchecked_Deallocation (Sym_Array_T, Sym_Array_Ptr_T);
         Sym_Array_Ptr : Sym_Array_Ptr_T;
         Strtab_Shdr_Idx : Elf_Half;
         Strtab_Shdr_Ptr : Elf_Shdr_Acc;
         type String_Table_T is array (Natural range <>) of Character;
         type String_Table_Ptr_T is access all String_Table_T;
         procedure Free is
           new Ada.Unchecked_Deallocation (String_Table_T, String_Table_Ptr_T);
         String_Table_Ptr : String_Table_Ptr_T;
      begin
         Symtab_Shdr_Idx := Get_Shdr_By_Name (Executable_File, ".symtab");
         Chk_Exe ("Error finding "".symtab"" section");
         Symtab_Shdr_Ptr := Get_Shdr (Executable_File, Symtab_Shdr_Idx);
         Chk_Exe ("Error getting "".symtab"" section header");
         N_Syms :=
           Natural (Symtab_Shdr_Ptr.Sh_Size / Symtab_Shdr_Ptr.Sh_Entsize);
         if N_Syms = 0 then
            Put_Line (Standard_Error, "Symbol table is empty.");
            raise Constraint_Error;
         end if;
         Sym_Array_Ptr := new Sym_Array_T (1 .. N_Syms);
         Load_Section (Executable_File, Symtab_Shdr_Idx,
                       Sym_Array_Ptr (Sym_Array_Ptr'First)'Address);
         Chk_Exe ("Error loading symbol table");
         --  Load the symbol table

         Strtab_Shdr_Idx := Elf_Half (Symtab_Shdr_Ptr.Sh_Link);
         Strtab_Shdr_Ptr := Get_Shdr (Executable_File, Strtab_Shdr_Idx);
         Chk_Exe ("Error getting symbol table strings section header");
         N_Bytes := Natural (Strtab_Shdr_Ptr.Sh_Size);
         if N_Bytes = 0 then
            Put_Line (Standard_Error, "Symbols string table is empty.");
            raise Constraint_Error;
         end if;
         String_Table_Ptr := new String_Table_T (0 .. N_Bytes - 1);
         Load_Section (Executable_File, Strtab_Shdr_Idx,
                       String_Table_Ptr (String_Table_Ptr'First)'Address);
         Chk_Exe ("Error loading symbols string table");
         --  Load the string table

         for J in Sym_Array_Ptr'Range loop
            Sym := Get_Sym (Executable_File, Sym_Array_Ptr (J)'Address);
            Str_Idx := Natural (Sym.St_Name);
            if Str_Idx > String_Table_Ptr'Last then
               Put_Line (Standard_Error, "String table index too large");
               raise Constraint_Error;
            end if;
            K := S'First;
            loop
               exit when S (K) /= String_Table_Ptr (Str_Idx);
               Str_Idx := Str_Idx + 1;
               if K = S'Last then
                  if Character'Pos (String_Table_Ptr (Str_Idx)) = 0 then
                     Free (Sym_Array_Ptr);
                     Free (String_Table_Ptr);
                     return Sym.St_Value;
                  else
                     exit;
                  end if;
               end if;
               K := K + 1;
            end loop;

         end loop;

         Put_Line (Standard_Error, "Symbol """ & S & """ not found");
         raise Constraint_Error;
      end Lookup_Sym_Value;

   begin
      if Arg'Length > 2 and Has_Prefix (Arg, "0x") then
         J := Arg'First + 2;
         while J <= Arg'Last loop
            A := A * 16;
            if Arg (J) in '0' .. '9' then
               A := A + (Character'Pos (Arg (J)) - Character'Pos ('0'));
            elsif Arg (J) in 'a' .. 'f' then
               A := A + (Character'Pos (Arg (J)) - Character'Pos ('a') + 10);
            elsif Arg (J) in 'A' .. 'F' then
               A := A + (Character'Pos (Arg (J)) - Character'Pos ('A') + 10);
            else
               Put_Line (Standard_Error, "Invalid Adress: " & Arg);
               raise Constraint_Error;
            end if;
            J := J + 1;
         end loop;
         return A;
      elsif Arg'Length > 3 and Has_Prefix (Arg, "16#") then
         J := Arg'First + 3;
         Under_OK := False;
         while J <= Arg'Last loop
            if J = Arg'Last and then Arg (J) = '#' then
               return A;
            end if;
            if J = Arg'Last then
               Put_Line (Standard_Error, "Malformed Address: " & Arg);
               raise Constraint_Error;
            end if;
            A := A * 16;
            if Arg (J) in '0' .. '9' then
               A := A + (Character'Pos (Arg (J)) - Character'Pos ('0'));
               Under_OK := True;
            elsif Arg (J) in 'a' .. 'f' then
               A := A + (Character'Pos (Arg (J)) - Character'Pos ('a') + 10);
               Under_OK := True;
            elsif Arg (J) in 'A' .. 'F' then
               A := A + (Character'Pos (Arg (J)) - Character'Pos ('A') + 10);
               Under_OK := True;
            elsif Arg (J) = '_' then
               if not Under_OK or else J + 1 = Arg'Last then
                  Put_Line (Standard_Error, "Malformed Address: " & Arg);
                  raise Constraint_Error;
               end if;
               Under_OK := False;
               A := A / 16;
            else
               Put_Line (Standard_Error, "Invalid Adress: " & Arg);
               raise Constraint_Error;
            end if;
            J := J + 1;
         end loop;
         return A;
      else
         --  In the symbol name case.... look up in Executable
         return Lookup_Sym_Value (Arg);
      end if;
   end Exe_Address_From_Arg;

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
   --  Keep around for debugging.

   Do_History : Boolean;
   History_FD : File_Descriptor;
   Trace_FD   : File_Descriptor;
   Tr_Header  : Trace_Header;
   Trace_Header_Size : constant Natural :=
     Trace_Header'Size / System.Storage_Unit;
   Entry32 : Trace_Entry32;
   E32_Size : constant Natural := Trace_Entry32'Size / System.Storage_Unit;
   Writing_Trace     : Boolean;
   File_Len          : Long_Integer;
   N_History_Entries : Integer;
   Decision_Addr     : Unsigned_32;

   In_Block               : Boolean;
   Block_Start, Block_End : Unsigned_32;
   Op                     : Unsigned_8;
   --  Variables used when making final pass over instructions flags
   --  array to write out traces.

begin

   for J in 1 .. Argument_Count - 1 loop
      Put (Argument (J) & ' ');
   end loop;
   Put_Line (Argument (Argument_Count));

   Processor_ID := new String'(Argument (1));
   Executable_Filename := new String'(Argument (2));
   OCD_Filename := new String'(Argument (3));
   if Argument (4) = "--nohist" then
      Histfile_Path := new String'("");
      Do_History := False;
   else
      Histfile_Path := new String'(Argument (4));
      Do_History := True;
   end if;

   Tracefile_Path := new String'(Argument (5));

   Open_File (Executable_File, Executable_Filename.all);
   Chk_Exe ("Error Opening");
   Ehdr := Get_Ehdr (Executable_File);
   Chk_Exe ("Error reading file header");
   Load_Shdr (Executable_File);
   Chk_Exe ("Error retrieving section headers");
   --  Open the executable file before processing of PT_Start_Addr, in
   --  case a symbol name is used for the address, which requires
   --  processing of the executable.

   if To_Upper (Argument (6)) = "IAC1" then
      PT_Start_IAC_Bit := 1;
   elsif To_Upper (Argument (6)) = "IAC2" then
      PT_Start_IAC_Bit := 2;
   elsif To_Upper (Argument (6)) = "IAC3" then
      PT_Start_IAC_Bit := 4;
   elsif To_Upper (Argument (6)) = "IAC4" then
      PT_Start_IAC_Bit := 8;
   else
      Put_Line (Standard_Error, "PT_Start_IAC out of range 1 .. 4");
      Set_Exit_Status (1);
      return;
   end if;

   PT_Start_Address := Exe_Address_From_Arg (Argument (7));

   if Argument (8) = "0" then
      PT_Stop_IAC_Bit := 0;
   elsif To_Upper (Argument (8)) = "IAC1" then
      PT_Stop_IAC_Bit := 1;
   elsif To_Upper (Argument (8)) = "IAC2" then
      PT_Stop_IAC_Bit := 2;
   elsif To_Upper (Argument (8)) = "IAC3" then
      PT_Stop_IAC_Bit := 4;
   elsif To_Upper (Argument (8)) = "IAC4" then
      PT_Stop_IAC_Bit := 8;
   else
      Put_Line (Standard_Error,
                "PT_Stop_IAC must be 'never' or in range 1 .. 4");
      Set_Exit_Status (1);
      return;
   end if;

   if Processor_ID.all = "5634" then
      ICNT_Proc_Adj := 0;
   elsif Processor_ID.all = "5554" then
      ICNT_Proc_Adj := 1;
   else
      Put_Line (Standard_Error, "Unrecognized Processor ID (procid).");
      Usage;
      Set_Exit_Status (1);
      return;
   end if;
   ICNT_Adj_Val := ICNT_Proc_Adj;

   Text_Shdr_Idx := Get_Shdr_By_Name (Executable_File, ".text");
   Chk_Exe ("Error finding "".text"" section");
   Text_Shdr_Ptr := Get_Shdr (Executable_File, Text_Shdr_Idx);
   Chk_Exe ("Error getting "".text"" section header");
   Text_First_Addr  := Unsigned_32 (Text_Shdr_Ptr.Sh_Addr);
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
   --  Handle gaps in a code segment ???

   Close_File (Executable_File);

   Insn_Flags := new Insn_Flags_Array_T (1 .. N_Insns);
   for J in Insn_Flags'Range loop
      Op_Code := Shift_Right (Insns_Ptr (J), 26);
      if Op_Code = 16 or else Op_Code = 18 or else
        (Op_Code = 19 and then ((Insns_Ptr (J) and 16#3fe#) = 16#20#)) or else
        (Op_Code = 19 and then ((Insns_Ptr (J) and 16#3fe#) = 16#12c#)) or else
        (Op_Code = 31 and then ((Insns_Ptr (J) and 16#3fe#) = 16#008#)) then
         Insn_Flags (J) := (Is_Branch => True, others => False);
      else
         Insn_Flags (J) := (others => False);
      end if;
   end loop;
   --  ??? Only the instructions whose mnemonics begin with 'b' are
   --  dealt with. Many other instuctions (particularly with major
   --  opcode = 19) will cause a change in flow, and will generate
   --  NEXUS program trace messages.

   --  If doing History traces, read the addresses that are "interesting"
   --  from the history file. For each such address, mark all instructions
   --  in the same basic block (go backwards over all non-branches, and
   --  forward to the first branch) as Historical in the flags array.

   if Do_History then

      History_FD := Open_Read (Histfile_Path.all, Binary);
      if History_FD = Invalid_FD then
         raise Program_Error with "Error opening history file.";
      end if;
      if Read (History_FD, Tr_Header'Address, Trace_Header_Size)
        /= Trace_Header_Size
      then
         Close (History_FD);
         raise Program_Error with "Error reading history file.";
      end if;
      --  Do checks on header ???
      if Natural (Tr_Header.Sizeof_Target_Pc) /= 4 then
         Close (History_FD);
         raise Program_Error with "Only handle 32 bit history files.";
      end if;

      File_Len := File_Length (History_FD) - Long_Integer (Trace_Header_Size);
      if File_Len mod Long_Integer (E32_Size) /= 0 then
         Close (History_FD);
         raise Program_Error with "Unexpected size of history file.";
      end if;

      N_History_Entries := Integer (File_Len / Long_Integer (E32_Size));

      for J in 1 .. N_History_Entries loop
         if Read (History_FD, Entry32'Address, E32_Size) /= E32_Size then
            Close (History_FD);
            raise Program_Error with "Histmap read failure.";
         end if;

         Decision_Addr := Entry32.Pc;
         if Decision_Addr < Text_First_Addr or else
           Decision_Addr >= Text_First_Addr + Unsigned_32 (N_Insns * 4)
         then
            Close (History_FD);
            raise Program_Error with "History map address out of range.";
         end if;

         Insn_Idx := Positive (((Decision_Addr - Text_First_Addr) / 4))
           + Insn_Flags'First;
         Idx2 := Insn_Idx;
         while Idx2 > Insn_Flags'First loop
            Idx2 := Idx2 - 1;
            exit when Insn_Flags (Idx2).Is_Branch;
            Insn_Flags (Idx2).Historical := True;
         end loop;
         Idx2 := Insn_Idx;
         loop
            Insn_Flags (Idx2).Historical := True;
            exit when Idx2 = Insn_Flags'Last or else
              Insn_Flags (Idx2).Is_Branch;
         end loop;

      end loop;
      Close (History_FD);

   end if;

   --  Open the trace file and set things up to write trace entries.
   --  In the case of 'Flat' traces, nothing will be written until
   --  all of the Nexus data is processed, which fills in the
   --  Insn_Flags_Array. In the case of 'History' traces, basic blocks
   --  which include an address found in the history file, will be written
   --  out on the fly, during the processing of the Nexus data (in historical
   --  order), with the rest of the trace data (basic blocks not of
   --  historical interest) will be processed afterwards -- like 'Flat' traces.

   Tr_Header :=
     (Magic            => Qemu_Trace_Magic,
      Version          => Qemu_Trace_Version,
      Kind             => Flat,
      Sizeof_Target_Pc => Pc_Type_Size,
      Big_Endian       => Big_Endian_Host,
      Machine_Hi       => Unsigned_8 (Shift_Right (Unsigned_16 (20), 8)),
      Machine_Lo       => Unsigned_8 (Unsigned_16 (20) and 16#Ff#),
      --  Note hardcoding of machine. Fix ???
      Padding          => 0);
   if Do_History then
      Tr_Header.Kind := History;
   end if;

   Trace_FD := Open_Read_Write (Tracefile_Path.all, Binary);
   if Trace_FD = Invalid_FD then
      raise Program_Error with "Error opening trace file to append.";
   end if;
   Lseek (Trace_FD, 0, Seek_End);

   if Write (Trace_FD, Tr_Header'Address, Trace_Header_Size)
     /= Trace_Header_Size
   then
      Close (Trace_FD);
      raise Program_Error with "Error appending to trace file.";
   end if;

   --  In Insn_Flags we have a list of flags set for each instruction.
   --  We have the sequence of Nexus messages in Message_List.
   --  We now go trhough the Message_List writing out History entries
   --  if requested, and modifying flags in Insn_Flags to indicate which
   --  instructions have been executed, and which direction(s) were taken
   --  on executed branch instructions. Later the Insn_Flags array is
   --  processed to generate GNATcoverage trace data: all of it in the
   --  "Flat" case, or non-histoical blocks in the "History" case.

   Message_List := Ocdfile_To_Nexus_List (OCD_Filename.all);
   Nexus_Msg_List_Elem := Message_List.First;
   if Nexus_Msg_List_Elem = null then
      raise Program_Error with "Empty Nexus message list";
   end if;

   PT_Running := False;

   loop
      Nexus_Msg := Nexus_Msg_List_Elem.Message;
      case Nexus_Msg.Tcode is
         when Watchpoint_Message                           =>
            --  Processing needed for WPs that are used for the start
            --  and stop of PT message generation. The transition from
            --  stopped state to running state needs to be handled since
            --  the instruction count for the subsequent BTM can differ
            --  if the WP addr is between branches.
            Watchpoints_Seen :=
              Watchpoint_T (Nexus_Msg.Watchpoint_Message_V.WPHIT);
            if (Watchpoints_Seen and PT_Start_IAC_Bit) /= 0 then
               if (Watchpoints_Seen and PT_Stop_IAC_Bit) /= 0 then
                  raise Program_Error with
                    "Cannot handle simultaneous Start and Stop WP msg";
               end if;
               if not PT_Running then
                  PT_Running := True;
                  Block_Begin_Idx :=
                    Integer (PT_Start_Address - Text_First_Addr) / 4 + 1;
                  ICNT_Adj_Val := ICNT_Proc_Adj + 1;
                  --  ICNT_Adj_Val := ICNT_Proc_Adj;
               end if;
            elsif (Watchpoints_Seen and PT_Stop_IAC_Bit) /= 0 then
               PT_Running := False;
            end if;

         when Resource_Full_Message                        =>
            --  An RCODE of 0 indicates that the Instruction count
            --  has wrapped around and another 255 needs to be added
            --  to the I_CNT value returned by the next BTM.
            if Nexus_Msg.Resource_Full_Message_V.RCODE = 0 then
               ICNT_Adj_Val := ICNT_Adj_Val + 255;
            else
               raise Program_Error with
                 "Resource Full Message unexpected RCODE received";
            end if;

         when Prog_Trace_Program_Correlation_Message       =>
            --  There are 2 expected events that generate Program Correlation
            --  messages: a breakpoint is set to end execution; the PT
            --  stop watchpoint is hit.
            EV_Code :=
              Nexus_Msg.Prog_Trace_Program_Correlation_Message_V.EVCODE;
            if EV_Code = 4 or else EV_Code = 0 then
               if EV_Code = 4 and then PT_Running then
                  raise Program_Error with
                    "Correlation message EVCODE of 4 while PT running";
                  --  Watchpoint should have occured to set PT_Running False
               end if;

               --  For either Stop Trigger or entering of debug mode (BP),
               --  an instruction count is provided for instructions since
               --  the last BTM. Here, we mark those instructions as
               --  executed.

               Insn_Count := Natural
                 (Nexus_Msg.Prog_Trace_Program_Correlation_Message_V.I_CNT
                  + ICNT_Adj);

               if Insn_Count > 0 then
                  N_Insns := Positive (Insn_Count);
                  Block_End_Idx := Block_Begin_Idx + N_Insns - 1;
                  Insn_Idx := Block_Begin_Idx;
                  if Do_History and then Insn_Flags (Insn_Idx).Historical then
                     Writing_Trace := True;
                     Trace_Start_Idx := Insn_Idx;
                  else
                     Writing_Trace := False;
                  end if;

                  loop
                     Insn_Flags (Insn_Idx).Been_Executed := True;
                     if Insn_Idx = Block_End_Idx then
                        --  The last instruction should be a branch taken.
                        if not Insn_Flags (Insn_Idx).Is_Branch then
                           raise Program_Error with "End of blk not a branch";
                        end if;
                        Insn_Flags (Insn_Idx).Br_Taken := True;
                        if Writing_Trace then
                           Entry32.Pc :=
                             Unsigned_32 ((Trace_Start_Idx - Insn_Flags'First)
                                          * 4) + Text_First_Addr;
                           Entry32.Size :=
                             Unsigned_16 ((Insn_Idx - Trace_Start_Idx + 1)
                                          * 4);
                           Entry32.Op := Trace_Op_Br0;
                           Entry32.Pad0 := 0;
                           if
                             Write (Trace_FD, Entry32'Address, E32_Size)
                             /= E32_Size
                           then
                              Close (Trace_FD);
                              raise Program_Error
                                with "Error writing trace entry.";
                           end if;
                        end if;

                        exit;
                     elsif Insn_Flags (Insn_Idx).Is_Branch then
                        --  A branch may appear in the middle of the I_CNT
                        --  instructions since the last message. That is a
                        --  branch that was not taken.
                        Insn_Flags (Insn_Idx).Br_Not_Taken := True;

                        if Writing_Trace then
                           Entry32.Pc :=
                             Unsigned_32 ((Trace_Start_Idx - Insn_Flags'First)
                                          * 4) + Text_First_Addr;
                           Entry32.Size :=
                             Unsigned_16 ((Insn_Idx - Trace_Start_Idx + 1)
                                          * 4);
                           Entry32.Op := Trace_Op_Br1;
                           Entry32.Pad0 := 0;
                           if
                             Write (Trace_FD, Entry32'Address, E32_Size)
                             /= E32_Size
                           then
                              Close (Trace_FD);
                              raise Program_Error
                                with "Error writing trace entry.";
                           end if;
                        end if;
                        if
                          Do_History and then
                          Insn_Flags (Insn_Idx + 1).Historical
                        then
                           Writing_Trace := True;
                           Trace_Start_Idx := Insn_Idx + 1;
                        else
                           Writing_Trace := False;
                        end if;
                     end if;
                     Insn_Idx := Insn_Idx + 1;
                  end loop;
                  --  If BP, then exit below.
                  --  If Stop trigger, then Block_Begin_Index will be
                  --  calculated, when/if Start trigger is seen.

                  if EV_Code = 0 then
                     --  BP
                     exit;
                  end if;
               end if;
            else
               raise Program_Error with
                 "Correlation message with unxpected EVCODE";
            end if;

         when Prog_Trace_Direct_Branch_Message             =>
            N_Insns := Positive
              (Nexus_Msg.Prog_Trace_Direct_Branch_Message_V.I_CNT + ICNT_Adj);
            Block_End_Idx := Block_Begin_Idx + N_Insns - 1;
            Insn_Idx := Block_Begin_Idx;
            if Do_History and then Insn_Flags (Insn_Idx).Historical then
               Writing_Trace := True;
               Trace_Start_Idx := Insn_Idx;
            else
               Writing_Trace := False;
            end if;

            loop
               Insn_Flags (Insn_Idx).Been_Executed := True;
               if Insn_Idx = Block_End_Idx then
                  --  The last instruction should be a branch taken.
                  if not Insn_Flags (Insn_Idx).Is_Branch then
                     raise Program_Error with "End of block not a branch";
                  end if;
                  Insn_Flags (Insn_Idx).Br_Taken := True;
                  if Writing_Trace then
                     Entry32.Pc :=
                       Unsigned_32 ((Trace_Start_Idx - Insn_Flags'First) * 4)
                       + Text_First_Addr;
                     Entry32.Size :=
                       Unsigned_16 ((Insn_Idx - Trace_Start_Idx + 1) * 4);
                     Entry32.Op := Trace_Op_Br0;
                     Entry32.Pad0 := 0;
                     if
                       Write (Trace_FD, Entry32'Address, E32_Size) /= E32_Size
                     then
                        Close (Trace_FD);
                        raise Program_Error
                          with "Error writing trace entry.";
                     end if;
                  end if;

                  exit;
               elsif Insn_Flags (Insn_Idx).Is_Branch then
                  --  A branch may appear in the middle of the I_CNT
                  --  instructions since the last message. That is a
                  --  branch that was not taken.
                  Insn_Flags (Insn_Idx).Br_Not_Taken := True;

                  if Writing_Trace then
                     Entry32.Pc :=
                       Unsigned_32 ((Trace_Start_Idx - Insn_Flags'First) * 4)
                       + Text_First_Addr;
                     Entry32.Size :=
                       Unsigned_16 ((Insn_Idx - Trace_Start_Idx + 1) * 4);
                     Entry32.Op := Trace_Op_Br1;
                     Entry32.Pad0 := 0;
                     if
                       Write (Trace_FD, Entry32'Address, E32_Size) /= E32_Size
                     then
                        Close (Trace_FD);
                        raise Program_Error
                          with "Error writing trace entry.";
                     end if;
                  end if;
                  if
                    Do_History and then Insn_Flags (Insn_Idx + 1).Historical
                  then
                     Writing_Trace := True;
                     Trace_Start_Idx := Insn_Idx + 1;
                  else
                     Writing_Trace := False;
                  end if;
               end if;
               Insn_Idx := Insn_Idx + 1;
            end loop;

            --  For Direct Branch messages, the new address is calucluated
            --  by analysis of the instruction.

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
            elsif Op_Code = 19 then
               Ext_Op_Code := Br_Insn and 16#0000_03fe#;
               if Ext_Op_Code = 16#12c# then
                  --  This is an isync op. New_Addr is next instruction. ??
                  New_Addr := Br_Addr + 4;
               else
                  raise Program_Error with
                    "Unexpected Extended opcode for opcode 19 Direct_Branch";
               end if;
            else
               raise Program_Error with "Unexpected opcode for Direct Branch";
            end if;
            Block_Begin_Idx := Positive (New_Addr - Text_First_Addr) / 4 + 1;

         when Prog_Trace_Indirect_Branch_Message           =>
            N_Insns := Positive
              (Nexus_Msg.Prog_Trace_Indirect_Branch_Message_V.I_CNT
               + ICNT_Adj);
            Block_End_Idx := Block_Begin_Idx + N_Insns - 1;
            Insn_Idx := Block_Begin_Idx;
            if Do_History and then Insn_Flags (Insn_Idx).Historical then
               Writing_Trace := True;
               Trace_Start_Idx := Insn_Idx;
            else
               Writing_Trace := False;
            end if;

            loop
               Insn_Flags (Insn_Idx).Been_Executed := True;
               if Insn_Idx = Block_End_Idx then
                  if not Insn_Flags (Insn_Idx).Is_Branch then
                     raise Program_Error with "End of block not a branch";
                  end if;
                  Insn_Flags (Insn_Idx).Br_Taken := True;
                  if Writing_Trace then
                     Entry32.Pc :=
                       Unsigned_32 ((Trace_Start_Idx - Insn_Flags'First) * 4)
                       + Text_First_Addr;
                     Entry32.Size :=
                       Unsigned_16 ((Insn_Idx - Trace_Start_Idx + 1) * 4);
                     Entry32.Op := Trace_Op_Br0;
                     Entry32.Pad0 := 0;
                     if
                       Write (Trace_FD, Entry32'Address, E32_Size) /= E32_Size
                     then
                        Close (Trace_FD);
                        raise Program_Error
                          with "Error writing trace entry.";
                     end if;
                  end if;

                  exit;
               elsif Insn_Flags (Insn_Idx).Is_Branch then
                  --  As in the direct Branch case, there can be branches
                  --  not taken since the last message.
                  Insn_Flags (Insn_Idx).Br_Not_Taken := True;

                  if Writing_Trace then
                     Entry32.Pc :=
                       Unsigned_32 ((Trace_Start_Idx - Insn_Flags'First) * 4)
                       + Text_First_Addr;
                     Entry32.Size :=
                       Unsigned_16 ((Insn_Idx - Trace_Start_Idx + 1) * 4);
                     Entry32.Op := Trace_Op_Br1;
                     Entry32.Pad0 := 0;
                     if
                       Write (Trace_FD, Entry32'Address, E32_Size) /= E32_Size
                     then
                        Close (Trace_FD);
                        raise Program_Error
                          with "Error writing trace entry.";
                     end if;
                  end if;
                  if
                    Do_History and then Insn_Flags (Insn_Idx + 1).Historical
                  then
                     Writing_Trace := True;
                     Trace_Start_Idx := Insn_Idx + 1;
                  else
                     Writing_Trace := False;
                  end if;
               end if;
               Insn_Idx := Insn_Idx + 1;
            end loop;
            U_Addr := Unsigned_32
              (Nexus_Msg.Prog_Trace_Indirect_Branch_Message_V.U_ADDR);
            --  Br_Addr:= Text_First_Addr + Unsigned_32(Block_End_Idx - 1) * 4;
            New_Addr := Last_Indirect_Or_Sync_Addr xor U_Addr;
            Block_Begin_Idx := Positive (New_Addr - Text_First_Addr) / 4 + 1;
            Last_Indirect_Or_Sync_Addr := New_Addr;

         when Prog_Trace_Direct_Branch_Message_Sync        =>
            N_Insns := Positive
              (Nexus_Msg.Prog_Trace_Direct_Branch_Message_Sync_V.I_CNT
               + ICNT_Adj);
            Block_End_Idx := Block_Begin_Idx + N_Insns - 1;
            Insn_Idx := Block_Begin_Idx;
            if Do_History and then Insn_Flags (Insn_Idx).Historical then
               Writing_Trace := True;
               Trace_Start_Idx := Insn_Idx;
            else
               Writing_Trace := False;
            end if;

            loop
               Insn_Flags (Insn_Idx).Been_Executed := True;
               if Insn_Idx = Block_End_Idx then
                  if not Insn_Flags (Insn_Idx).Is_Branch then
                     raise Program_Error with "End of block not a branch";
                  end if;
                  Insn_Flags (Insn_Idx).Br_Taken := True;
                  if Writing_Trace then
                     Entry32.Pc :=
                       Unsigned_32 ((Trace_Start_Idx - Insn_Flags'First) * 4)
                       + Text_First_Addr;
                     Entry32.Size :=
                       Unsigned_16 ((Insn_Idx - Trace_Start_Idx + 1) * 4);
                     Entry32.Op := Trace_Op_Br0;
                     Entry32.Pad0 := 0;
                     if
                       Write (Trace_FD, Entry32'Address, E32_Size) /= E32_Size
                     then
                        Close (Trace_FD);
                        raise Program_Error
                          with "Error writing trace entry.";
                     end if;
                  end if;

                  exit;
               elsif Insn_Flags (Insn_Idx).Is_Branch then
                  Insn_Flags (Insn_Idx).Br_Not_Taken := True;

                  if Writing_Trace then
                     Entry32.Pc :=
                       Unsigned_32 ((Trace_Start_Idx - Insn_Flags'First) * 4)
                       + Text_First_Addr;
                     Entry32.Size :=
                       Unsigned_16 ((Insn_Idx - Trace_Start_Idx + 1) * 4);
                     Entry32.Op := Trace_Op_Br1;
                     Entry32.Pad0 := 0;
                     if
                       Write (Trace_FD, Entry32'Address, E32_Size) /= E32_Size
                     then
                        Close (Trace_FD);
                        raise Program_Error
                          with "Error writing trace entry.";
                     end if;
                  end if;
                  if
                    Do_History and then Insn_Flags (Insn_Idx + 1).Historical
                  then
                     Writing_Trace := True;
                     Trace_Start_Idx := Insn_Idx + 1;
                  else
                     Writing_Trace := False;
                  end if;
               end if;
               Insn_Idx := Insn_Idx + 1;
            end loop;

            --  In Sync messages, the full address is provided.
            New_Addr := Unsigned_32
              (Nexus_Msg.Prog_Trace_Direct_Branch_Message_Sync_V.F_ADDR);
            Block_Begin_Idx := Positive (New_Addr - Text_First_Addr) / 4 + 1;
            Last_Indirect_Or_Sync_Addr := New_Addr;

         when Prog_Trace_Indirect_Branch_Message_Sync      =>
            N_Insns := Positive
              (Nexus_Msg.Prog_Trace_Indirect_Branch_Message_Sync_V.I_CNT
               + ICNT_Adj);
            Block_End_Idx := Block_Begin_Idx + N_Insns - 1;
            Insn_Idx := Block_Begin_Idx;
            if Do_History and then Insn_Flags (Insn_Idx).Historical then
               Writing_Trace := True;
               Trace_Start_Idx := Insn_Idx;
            else
               Writing_Trace := False;
            end if;

            loop
               Insn_Flags (Insn_Idx).Been_Executed := True;
               if Insn_Idx = Block_End_Idx then
                  if not Insn_Flags (Insn_Idx).Is_Branch then
                     raise Program_Error with "End of block not a branch";
                  end if;
                  Insn_Flags (Insn_Idx).Br_Taken := True;
                  if Writing_Trace then
                     Entry32.Pc :=
                       Unsigned_32 ((Trace_Start_Idx - Insn_Flags'First) * 4)
                       + Text_First_Addr;
                     Entry32.Size :=
                       Unsigned_16 ((Insn_Idx - Trace_Start_Idx + 1) * 4);
                     Entry32.Op := Trace_Op_Br0;
                     Entry32.Pad0 := 0;
                     if
                       Write (Trace_FD, Entry32'Address, E32_Size) /= E32_Size
                     then
                        Close (Trace_FD);
                        raise Program_Error
                          with "Error writing trace entry.";
                     end if;
                  end if;

                  exit;
               elsif Insn_Flags (Insn_Idx).Is_Branch then
                  Insn_Flags (Insn_Idx).Br_Not_Taken := True;

                  if Writing_Trace then
                     Entry32.Pc :=
                       Unsigned_32 ((Trace_Start_Idx - Insn_Flags'First) * 4)
                       + Text_First_Addr;
                     Entry32.Size :=
                       Unsigned_16 ((Insn_Idx - Trace_Start_Idx + 1) * 4);
                     Entry32.Op := Trace_Op_Br1;
                     Entry32.Pad0 := 0;
                     if
                       Write (Trace_FD, Entry32'Address, E32_Size) /= E32_Size
                     then
                        Close (Trace_FD);
                        raise Program_Error
                          with "Error writing trace entry.";
                     end if;
                  end if;
                  if
                    Do_History and then Insn_Flags (Insn_Idx + 1).Historical
                  then
                     Writing_Trace := True;
                     Trace_Start_Idx := Insn_Idx + 1;
                  else
                     Writing_Trace := False;
                  end if;
               end if;
               Insn_Idx := Insn_Idx + 1;
            end loop;

            --  In Sync messages, the full address is provided.
            New_Addr := Unsigned_32
              (Nexus_Msg.Prog_Trace_Indirect_Branch_Message_Sync_V.F_ADDR);
            Block_Begin_Idx := Positive (New_Addr - Text_First_Addr) / 4 + 1;
            Last_Indirect_Or_Sync_Addr := New_Addr;

         when Debug_Status                                 =>
            null;

         when Data_Trace_Data_Write_Message                =>
            raise Program_Error with "Unexpected Data Trace TCODE";

         when Data_Trace_Data_Read_Message                 =>
            raise Program_Error with "Unexpected Data Trace TCODE";

         when Error_Message                                =>
            raise Program_Error with "Unexpected Error Message TCODE";

         when Ownership_Trace_Message                      =>
            raise Program_Error with "Unexpected Ownership Trace TCODE";

         when Data_Trace_Data_Write_Message_Sync           =>
            raise Program_Error with "Unexpected Data Trace TCODE";

         when Data_Trace_Data_Read_Message_Sync            =>
            raise Program_Error with "Unexpected Data Trace TCODE";

         when Prog_Trace_Indirect_Branch_Hist_Message      =>
            raise Program_Error with "Unexpected Branch History TCODE";
         when Prog_Trace_Indirect_Branch_Hist_Message_Sync =>
            raise Program_Error with "Unexpected Branch History TCODE";

         when  1 | 7 | 9 | 10 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23
            |  24 | 25 | 26 | 30 | 31 | 32 =>
            raise Program_Error with "Invalid TCODE";

      end case;
      Nexus_Msg_List_Elem := Nexus_Msg_List_Elem.Next;
      exit when Nexus_Msg_List_Elem = null;
   end loop;

   --  Loop over Insn_Flags, calculating basic blocks and writing
   --  trace entries.

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
            if J /= Insn_Flags'Last then
               if not Do_History or else not Insn_Flags (J).Historical then
                  Op := Trace_Op_Block;
                  if Insn_Flags (J).Br_Taken then
                     Op := Op or Trace_Op_Br0;
                  end if;
                  if Insn_Flags (J).Br_Not_Taken then
                     Op := Op or Trace_Op_Br1;
                  end if;
                  Entry32.Pc := Block_Start;
                  Entry32.Size := Unsigned_16 (Block_End - Block_Start + 4);
                  Entry32.Op := Op;
                  Entry32.Pad0 := 0;
                  if
                    Write (Trace_FD, Entry32'Address, E32_Size) /= E32_Size
                  then
                     Close (Trace_FD);
                     raise Program_Error with "Error appending trace entry.";
                  end if;
               end if;

            else
               exit;
            end if;
         end if;
      end if;
   end loop;
   Close (Trace_FD);

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
