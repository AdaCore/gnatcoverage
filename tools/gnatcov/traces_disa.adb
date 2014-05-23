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

with Ada.Text_IO; use Ada.Text_IO;

with Disassemblers;     use Disassemblers;
with Elf_Arch;          use Elf_Arch;
with Interfaces;        use Interfaces;
with Hex_Images;        use Hex_Images;
with Highlighting;      use Highlighting;
with Elf_Disassemblers; use Elf_Disassemblers;
with Switches;
with Traces_Files;

package body Traces_Disa is

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label
     (Sym : Symbolizer'Class; Info : Address_Info_Acc) return String
   is
      Buffer : Highlighting.Buffer_Type (64);
   begin
      --  Display address

      Symbolize (Sym, Info.First, Buffer);

      if Buffer.Last_Index < 1 then
         return "";
      else
         return Buffer.Get_Raw (2 .. Buffer.Last_Index) & ':';
      end if;
   end Get_Label;

   -----------------
   -- Disassemble --
   -----------------

   function Disassemble
     (Insn : Binary_Content;
      Pc   : Pc_Type;
      Sym  : Symbolizer'Class) return String
   is
      Buffer : Buffer_Type (128);
      Insn_Len : Natural;
   begin
      Disa_For_Machine (Machine).
        Disassemble_Insn_Or_Abort (Insn, Pc, Buffer, Insn_Len, Sym);

      if Elf_Arch.Elf_Addr (Insn_Len) /= Length (Insn) then
         raise Constraint_Error;
      end if;
      return Buffer.Get_Raw;
   end Disassemble;

   ---------------------------
   -- Textio_Disassemble_Cb --
   ---------------------------

   procedure Textio_Disassemble_Cb
     (Addr  : Pc_Type;
      State : Insn_State;
      Insn  : Binary_Content;
      Sym   : Symbolizer'Class)
   is
      Off : Pc_Type := Addr;
      I : Pc_Type;
   begin
      if Switches.Debug_Break_Long_Instructions then
         while
            --  Make sure we process each byte of the given instruction.
            Off <= Insn.Last
            and then Off >= Addr --  And handle overflow
         loop
            --  Each dump line must start with indentation, the memory address
            --  of the first byte we are dumping and the state char.
            Put ("  ");
            Put (Hex_Image (Off));
            Put (' ');
            Disp_State_Char (State);
            Put (":  ");

            --  There must not be more than 8 bytes on each line, and the
            --  disassembly must appear on the first line.

            I := 0;
            while I < 8 and then Off - Addr + I < Length (Insn) loop
               Put (Hex_Image (Get (Insn, Off + I)));
               Put (' ');
               I := I + 1;
            end loop;

            --  Pad "missing" bytes with spaces
            for P in I .. 8 loop
               Put ("   ");
            end loop;

            --  And display the disassembly only in the first line
            if Off = Addr then
               Put (Disassemble (Insn, Addr, Sym));
            end if;
            New_Line;

            Off := Off + 8;
         end loop;

      else
         Put (Hex_Image (Addr));
         Put (' ');
         Disp_State_Char (State);
         Put (":  ");
         for I in Insn.First .. Insn.Last loop
            Put (Hex_Image (Get (Insn, I)));
            Put (' ');
         end loop;
         for I in Length (Insn) .. 4 loop
            Put ("   ");
         end loop;
         Put ("  ");
         Put (Disassemble (Insn, Addr, Sym));
         New_Line;
         return;
      end if;

   end Textio_Disassemble_Cb;

   -------------------
   -- For_Each_Insn --
   -------------------

   procedure For_Each_Insn
     (Insns : Binary_Content;
      State : Insn_State;
      Cb    : Disassemble_Cb;
      Sym  : Symbolizer'Class)
   is
      Pc : Pc_Type;
      Insn_Len : Natural := 0;
   begin
      Pc := Insns.First;
      while Pc <= Insns.Last loop
         Insn_Len :=
           Disa_For_Machine (Machine).Get_Insn_Length_Or_Abort
                                        (Slice (Insns, Pc, Insns.Last));
         Cb.all
           (Pc, State, Slice (Insns, Pc, Pc + Pc_Type (Insn_Len - 1)), Sym);
         Pc := Pc + Pc_Type (Insn_Len);

         --  Handle wrap around.
         exit when Pc = 0;
      end loop;
   exception
      --  Catch everything except Program_Errors, since Get_Insn_Length may
      --  have already caught one and aborted.

      when Program_Error =>
         raise;
      when Error : others =>
         Abort_Disassembler_Error (Pc, Slice (Insns, Pc, Insns.Last), Error);
   end For_Each_Insn;

   -------------------------
   -- Disp_Assembly_Lines --
   -------------------------

   procedure Disp_Assembly_Lines
     (Insns : Binary_Content;
      Base  : Traces_Base;
      Cb    : access procedure (Addr  : Pc_Type;
                                State : Insn_State;
                                Insn  : Binary_Content;
                                Sym   : Symbolizer'Class);
      Sym   : Symbolizer'Class)
   is
      It : Entry_Iterator;
      E : Trace_Entry;
      Addr : Pc_Type;
      Next_Addr : Pc_Type;
      State : Insn_State;
   begin
      Init (Base, It, Insns.First);
      Get_Next_Trace (E, It);
      Addr := Insns.First;

      loop
         Next_Addr := Insns.Last;

         --  Find matching trace
         --  Note: object coverage data is piggy-backed in the traces database

         while E /= Bad_Trace and then Addr > E.Last loop
            Get_Next_Trace (E, It);
         end loop;

         if E /= Bad_Trace and then Addr in E.First .. E.Last then
            State := E.State;
            if E.Last < Next_Addr then
               Next_Addr := E.Last;
            end if;

         else
            State := Not_Covered;
            if E /= Bad_Trace and then E.First < Next_Addr then
               Next_Addr := E.First - 1;
            end if;
         end if;

         For_Each_Insn (Slice (Insns, Addr, Next_Addr), State, Cb, Sym);
         exit when Next_Addr >= Insns.Last;
         Addr := Next_Addr + 1;
      end loop;
   end Disp_Assembly_Lines;

   --------------------------
   -- Dump_Traces_With_Asm --
   --------------------------

   procedure Dump_Traces_With_Asm
    (Exe : Exe_File_Type; Trace_Filename : String)
   is
      use Traces_Files;
      Addr : Address_Info_Acc := null;

      procedure Disp_Entry (E : Trace_Entry);
      --  Comment needed???

      ----------------
      -- Disp_Entry --
      ----------------

      procedure Disp_Entry (E : Trace_Entry) is
         use Traces_Disa;
         Sec : Address_Info_Acc;
         Buffer : Highlighting.Buffer_Type (128);
      begin
         Dump_Entry (E);
         if Addr = null
           or else E.First not in Addr.First .. Addr.Last
         then
            Addr := Get_Symbol (Exe, E.First);
         end if;

         if Addr = null then
            Put_Line ("(not in the executable)");

         else
            Symbolize (Exe, E.First, Buffer);
            Buffer.Put (':');
            Put_Line (Buffer.Get_Raw (2 .. Buffer.Last_Index));

            Sec := Addr.Parent;
            while Sec.Kind /= Section_Addresses loop
               Sec := Sec.Parent;
            end loop;

            Load_Section_Content (Exe, Sec);
            For_Each_Insn (Slice (Sec.Section_Content, E.First, E.Last),
                           Covered, Textio_Disassemble_Cb'Access, Exe);
         end if;
      end Disp_Entry;

      File : Trace_File_Type;

   --  Start of processing for Dump_Traces_Wth_Asm

   begin
      Read_Trace_File (Trace_Filename, File, null, Disp_Entry'Access);
      Free (File);
   end Dump_Traces_With_Asm;

end Traces_Disa;
