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

with Ada.Text_IO;    use Ada.Text_IO;

with Interfaces;        use Interfaces;

with Arch;              use Arch;
with Disassemblers;     use Disassemblers;
with Execs_Dbase;
with Hex_Images;        use Hex_Images;
with Highlighting;      use Highlighting;
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
     (Insn     : Binary_Content;
      Pc       : Pc_Type;
      Insn_Set : Insn_Set_Type;
      Sym      : Symbolizer'Class) return String
   is
      Buffer : Buffer_Type (128);
      Insn_Len : Natural;
   begin
      Disa_For_Machine (Machine, Insn_Set).
        Disassemble_Insn (Insn, Pc, Buffer, Insn_Len, Sym);

      if Arch.Arch_Addr (Insn_Len) /= Length (Insn) then
         raise Constraint_Error;
      end if;
      return Buffer.Get_Raw;
   end Disassemble;

   ---------------------------
   -- Textio_Disassemble_Cb --
   ---------------------------

   procedure Textio_Disassemble_Cb
     (Addr     : Pc_Type;
      State    : Insn_State;
      Insn     : Binary_Content;
      Insn_Set : Insn_Set_Type;
      Sym      : Symbolizer'Class)
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
               Put (Disassemble (Insn, Addr, Insn_Set, Sym));
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
         Put (Disassemble (Insn, Addr, Insn_Set, Sym));
         New_Line;
         return;
      end if;

   end Textio_Disassemble_Cb;

   -------------------
   -- For_Each_Insn --
   -------------------

   procedure For_Each_Insn
     (Insns    : Binary_Content;
      I_Ranges : Insn_Set_Ranges;
      State    : Insn_State;
      Cb       : Disassemble_Cb;
      Sym      : Symbolizer'Class)
   is
      Pc       : Pc_Type;
      Insn_Len : Natural := 0;

      Cache    : Insn_Set_Cache := Empty_Cache;
      Insn_Set : Insn_Set_Type;

   begin
      Pc := Insns.First;
      while Iterate_Over_Insns (I_Ranges, Cache, Insns.Last, Pc, Insn_Set) loop
         Insn_Len := Disa_For_Machine (Machine, Insn_Set).
           Get_Insn_Length (Slice (Insns, Pc, Insns.Last));

         Cb.all
           (Pc,
            State,
            Slice (Insns, Pc, Pc + Pc_Type (Insn_Len - 1)),
            Insn_Set,
            Sym);
         Pc := Pc + Pc_Type (Insn_Len);

         --  Handle wrap around.
         exit when Pc = 0;
      end loop;
   end For_Each_Insn;

   -------------------------
   -- Disp_Assembly_Lines --
   -------------------------

   procedure Disp_Assembly_Lines
     (Insns    : Binary_Content;
      I_Ranges : Insn_Set_Ranges;
      Base     : Traces_Base;
      Cb       : access procedure (Addr     : Pc_Type;
                                   State    : Insn_State;
                                   Insn     : Binary_Content;
                                   Insn_Set : Insn_Set_Type;
                                   Sym      : Symbolizer'Class);
      Sym      : Symbolizer'Class)
   is
      It        : Entry_Iterator;
      E         : Trace_Entry;
      Addr      : Pc_Type;
      Next_Addr : Pc_Type;
      State     : Insn_State;
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

         For_Each_Insn
           (Slice (Insns, Addr, Next_Addr), I_Ranges, State, Cb, Sym);
         exit when Next_Addr >= Insns.Last;
         Addr := Next_Addr + 1;
      end loop;
   end Disp_Assembly_Lines;

   --------------------------
   -- Dump_Traces_With_Asm --
   --------------------------

   procedure Dump_Traces_With_Asm
    (Exe : Exe_File_Type'Class; Trace_Filename : String)
   is
      use Traces_Files;
      Addr : Address_Info_Acc := null;

      function Load_Shared_Object
         (Trace_File  : Trace_File_Type;
          Filename    : String;
          Signature   : Binary_File_Signature;
          First, Last : Traces.Pc_Type) return Exe_File_Acc;

      procedure Disp_Entry
        (Trace_File : Trace_File_Type;
         Shared_Obj : Exe_File_Acc;
         E          : Trace_Entry);
      --  Comment needed???

      procedure Read_Trace_File is new Read_Trace_File_Gen
        (Shared_Object_Type   => Exe_File_Acc,
         No_Shared_Object     => Exe'Unrestricted_Access,
         Process_Info_Entries => Check_Trace_File_From_Exec,
         Load_Shared_Object   => Load_Shared_Object,
         Process_Trace_Entry  => Disp_Entry);

      ------------------------
      -- Load_Shared_Object --
      ------------------------

      function Load_Shared_Object
         (Trace_File  : Trace_File_Type;
          Filename    : String;
          Signature   : Binary_File_Signature;
          First, Last : Traces.Pc_Type) return Exe_File_Acc
      is
         pragma Unreferenced (Trace_File);
         pragma Unreferenced (First);
         pragma Unreferenced (Last);

         SO : Exe_File_Acc;
      begin
         Execs_Dbase.Open_Exec_For_Trace
           (Filename, 0, Trace_Filename, Signature, SO);
         return SO;
      end Load_Shared_Object;

      ----------------
      -- Disp_Entry --
      ----------------

      procedure Disp_Entry
        (Trace_File : Trace_File_Type;
         Shared_Obj : Exe_File_Acc;
         E          : Trace_Entry)
      is
         pragma Unreferenced (Trace_File);
         use Traces_Disa;
         SO     : Exe_File_Type'Class renames Shared_Obj.all;
         Sec    : Address_Info_Acc;
         Buffer : Highlighting.Buffer_Type (128);
      begin
         Dump_Entry (E);
         if Addr = null
           or else E.First not in Addr.First .. Addr.Last
         then
            Addr := Get_Symbol (SO, E.First);
         end if;

         if Addr = null then
            Put_Line ("(not in the executable)");

         else
            Symbolize (SO, E.First, Buffer);
            Buffer.Put (':');
            Put_Line (Buffer.Get_Raw (2 .. Buffer.Last_Index));

            Sec := Addr.Parent;
            while Sec.Kind /= Section_Addresses loop
               Sec := Sec.Parent;
            end loop;

            Load_Section_Content (SO, Sec);
            For_Each_Insn (Slice (Sec.Section_Content, E.First, E.Last),
                           Get_Insn_Set_Ranges (SO, Sec.Section_Sec_Idx).all,
                           Covered, Textio_Disassemble_Cb'Access, SO);
         end if;
      end Disp_Entry;

      File : Trace_File_Type;

   --  Start of processing for Dump_Traces_With_Asm

   begin
      Read_Trace_File (Trace_Filename, File);
      Free (File);
   end Dump_Traces_With_Asm;

end Traces_Disa;
