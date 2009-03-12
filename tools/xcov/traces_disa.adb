------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                      Copyright (C) 2008-2009, AdaCore                    --
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
with Ada.Text_IO; use Ada.Text_IO;
with System; use System;
with Interfaces; use Interfaces;
with Hex_Images; use Hex_Images;
with Elf_Disassemblers; use Elf_Disassemblers;

package body Traces_Disa is
   function Get_Label (Sym : Symbolizer'Class; Info : Addresses_Info_Acc)
                      return String
   is
      Line : String (1 .. 64);
      Line_Pos : Natural;
   begin
      --  Display address.
      Line_Pos := Line'First;
      Symbolize (Sym, Info.First, Line, Line_Pos);
      if Line_Pos > Line'First then
         if Line_Pos > Line'Last then
            Line_Pos := Line'Last;
         end if;
         Line (Line_Pos) := ':';
         return Line (Line'First + 1 .. Line_Pos);
      else
         return "";
      end if;
   end Get_Label;

   --  INSN is exactly one instruction.
   --  Generate the disassembly for INSN.
   function Disassemble
     (Insn : Binary_Content; Pc : Pc_Type; Sym : Symbolizer'Class)
     return String
   is
      Addr : Address;
      Line_Pos : Natural;
      Line : String (1 .. 128);
      Insn_Len : Natural := 0;
   begin
      Addr := Insn (Insn'First)'Address;
      Disa_For_Machine (Machine).
        Disassemble_Insn (Addr, Pc, Line, Line_Pos, Insn_Len, Sym);

      if Insn_Len /= Insn'Length then
         raise Constraint_Error;
      end if;
      return Line (1 .. Line_Pos - 1);
   end Disassemble;

   procedure Textio_Disassemble_Cb (Addr : Pc_Type;
                                    State : Trace_State;
                                    Insn : Binary_Content;
                                    Sym : Symbolizer'Class)
   is
   begin
      Put (Hex_Image (Addr));
      Put (' ');
      Disp_State_Char (State);
      Put (":");
      Put (ASCII.HT);
      for I in Insn'Range loop
         Put (Hex_Image (Insn (I)));
         Put (' ');
      end loop;
      Put ("  ");
      Put (Disassemble (Insn, Addr, Sym));
      New_Line;
   end Textio_Disassemble_Cb;

   procedure For_Each_Insn (Insns : Binary_Content;
                            State : Trace_State;
                            Cb : Disassemble_Cb;
                            Sym : Symbolizer'Class)
   is
      Pc : Pc_Type;
      Insn_Len : Natural := 0;
   begin
      Pc := Insns'First;
      while Pc < Insns'Last loop
         Insn_Len :=
           Disa_For_Machine (Machine).Get_Insn_Length (Insns (Pc)'Address);
         Cb.all (Pc, State, Insns (Pc .. Pc + Pc_Type (Insn_Len - 1)), Sym);
         Pc := Pc + Pc_Type (Insn_Len);

         exit when Pc = 0;
         --  When is this supposed to happen???
      end loop;
   end For_Each_Insn;

   procedure Disp_Assembly_Lines
     (Insns : Binary_Content;
      Base : Traces_Base;
      Cb : access procedure (Addr : Pc_Type;
                             State : Trace_State;
                             Insn : Binary_Content;
                             Sym : Symbolizer'Class);
      Sym : Symbolizer'Class)
   is
      It : Entry_Iterator;
      E : Trace_Entry;
      Addr : Pc_Type;
      Next_Addr : Pc_Type;
      State : Trace_State;
   begin
      --  Disp_Address (Info);
      Init (Base, It, Insns'First);
      Get_Next_Trace (E, It);
      Addr := Insns'First;

      loop
         Next_Addr := Insns'Last;

         --  Find matching trace.
         while E /= Bad_Trace and then Addr > E.Last loop
            Get_Next_Trace (E, It);
         end loop;
         --  Dump_Entry (E);
         if E /= Bad_Trace and then (Addr >= E.First and Addr <= E.Last) then
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
         For_Each_Insn (Insns (Addr .. Next_Addr), State, Cb, Sym);
         exit when Next_Addr >= Insns'Last;
         Addr := Next_Addr + 1;
      end loop;
   end Disp_Assembly_Lines;
end Traces_Disa;
