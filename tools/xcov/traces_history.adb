------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                        Copyright (C) 2009, AdaCore                       --
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
with Traces; use Traces;
with Traces_Files;
with Traces_Disa;

package body Traces_History is
   procedure Dump_Traces_With_Asm (Exe : Exe_File_Type;
                                   Trace_Filename : String)
   is
      Addr : Addresses_Info_Acc := null;

      procedure Disp_Entry (E : Trace_Entry)
      is
         use Traces_Disa;
         Sec : Addresses_Info_Acc;
         Line : String (1 .. 128);
         Line_Pos : Natural := Line'First;
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
            Symbolize (Exe, E.First, Line, Line_Pos);
            Line (Natural'Min (Line'Last, Line_Pos)) := ':';
            Put_Line (Line (Line'First + 1 .. Line_Pos));
            Sec := Addr.Parent;
            while Sec.Kind /= Section_Addresses loop
               Sec := Sec.Parent;
            end loop;
            Load_Section_Content (Exe, Sec);
            For_Each_Insn (Sec.Section_Content (E.First .. E.Last), Covered,
                           Textio_Disassemble_Cb'Access, Exe);
         end if;
      end Disp_Entry;
   begin
      Traces_Files.Read_Trace_File (Trace_Filename, Disp_Entry'Access);
   end Dump_Traces_With_Asm;

end Traces_History;
