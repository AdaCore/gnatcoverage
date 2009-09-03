------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                     Copyright (C) 2008-2009, AdaCore                     --
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

with Traces_Disa;
with Hex_Images;
with Traces_Names; use Traces_Names;
with Strings; use Strings;
with Traces_Dbase; use Traces_Dbase;
with Traces_Lines; use Traces_Lines;
with Traces_Elf; use Traces_Elf;
with Disa_Symbolize;

package body Traces_Dump is

   --------------------------
   -- Dump_Routines_Traces --
   --------------------------

   procedure Dump_Routines_Traces
   is
      use Traces_Disa;

      procedure Process_One
        (Name : String_Acc;
         Info : in out Subprogram_Info);
      --  Display traces for one routine

      procedure Process_One
        (Name : String_Acc;
         Info : in out Subprogram_Info)
      is
         use Hex_Images;
      begin
         Put (Name.all);

         if Info.Traces /= null then
            Put (' ');
            Put (State_Char (Compute_Routine_State (Info.Insns, Info.Traces)));
         end if;

         if Info.Insns /= null then
            Put (": " & Hex_Image (Info.Insns'First)
                 & '-' & Hex_Image (Info.Insns'Last));
         end if;
         New_Line;

         if Info.Traces /= null then
            if Flag_Show_Asm then
               if Info.Exec = null then
                  Disp_Assembly_Lines
                    (Info.Insns.all,
                     Info.Traces.all,
                     Textio_Disassemble_Cb'Access,
                     Disa_Symbolize.Nul_Symbolizer);

               else
                  Disp_Assembly_Lines
                    (Info.Insns.all,
                     Info.Traces.all,
                     Textio_Disassemble_Cb'Access,
                     Info.Exec.all);
               end if;
            end if;
         end if;
      end Process_One;

   --  Start of processing for Dump_Routines_Traces

   begin
      Iterate (Process_One'Access);
   end Dump_Routines_Traces;

   -----------------------------
   -- Dump_Uncovered_Routines --
   -----------------------------

   procedure Dump_Uncovered_Routines (Report : File_Access) is
      use Traces_Disa;

      procedure Process_One
        (Name : String_Acc;
         Info : in out Subprogram_Info);
      --  Report information for the given routine

      procedure Process_One
        (Name : String_Acc;
         Info : in out Subprogram_Info)
      is
         Routine_State : constant Line_State :=
                           Compute_Routine_State (Info.Insns, Info.Traces);
      begin
         if Info.Insns = null then
            Put_Line (Report.all, Name.all & " not found in executable(s)");

         elsif Routine_State /= Covered
           and then Routine_State /= No_Code
         then
            Put (Report.all, Name.all & " not fully covered : ");
            Put (Report.all, State_Char (Routine_State));
            New_Line (Report.all);
         end if;
      end Process_One;

   --  Start of processing for Dump_Uncovered_Routines

   begin
      Put_Line (Report.all, "ERRORS BY ROUTINE:");
      New_Line (Report.all);
      Iterate (Process_One'Access);
      New_Line (Report.all);
   end Dump_Uncovered_Routines;

end Traces_Dump;
