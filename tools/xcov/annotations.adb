------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                       Copyright (C) 2009, AdaCore                        --
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
with Traces_Disa;
with Slocs; use Slocs;
with Coverage;
with Coverage.Object;
with Coverage.Source;

package body Annotations is

   procedure Disp_File_Line_State
     (Pp         : in out Pretty_Printer'Class;
      File       : Files_Table.File_Info_Access);
   --  Comment needed???

   --------------------------
   -- Disp_File_Line_State --
   --------------------------

   procedure Disp_File_Line_State (Pp : in out Pretty_Printer'Class;
                                   File : File_Info_Access)
   is
      use Traces_Disa;

      procedure Disassemble_Cb
        (Addr  : Pc_Type;
         State : Insn_State;
         Insn  : Binary_Content;
         Sym   : Symbolizer'Class);
      --  Comment needed???

      --------------------
      -- Disassemble_Cb --
      --------------------

      procedure Disassemble_Cb
        (Addr  : Pc_Type;
         State : Insn_State;
         Insn  : Binary_Content;
         Sym   : Symbolizer'Class) is
      begin
         Pretty_Print_Insn (Pp, Addr, State, Insn, Sym);
      end Disassemble_Cb;

      F                : File_Type;
      Last             : Natural := 1;
      Has_Source       : Boolean;

      procedure Process_One_Line (Index : Positive);

      procedure Process_One_Line (Index : Positive)
      is
         Instruction_Set  : Addresses_Info_Acc;
         Info             : Files_Table.Object_Coverage_Info_Acc;
         Sec_Info         : Addresses_Info_Acc;
         LI               : constant Line_Info_Access :=
           Get_Line_Info (File, Index);
         Ls               : constant Line_State := LI.State;
         In_Symbol        : Boolean;
         In_Insn_Set      : Boolean;
      begin
         if Has_Source then
            Pretty_Print_Start_Line (Pp, Index, LI, Get_Line (F));
         else
            Pretty_Print_Start_Line (Pp, Index, LI, "");
         end if;

         if Pp.Show_Asm then
            Info := LI.Obj_First;

            if Info /= null then
               Pretty_Print_Start_Instruction_Set (Pp, Ls);
               In_Insn_Set := True;
            else
               In_Insn_Set := False;
            end if;

            --  Iterate over each insn block for the source line

            while Info /= null loop
               Instruction_Set := Info.Instruction_Set;
               declare
                  Label : constant String :=
                    Get_Label (Info.Exec.all, Instruction_Set);
                  Symbol : constant Addresses_Info_Acc :=
                    Get_Symbol (Info.Exec.all, Instruction_Set.First);
               begin
                  if Label'Length > 0 and Symbol /= null then
                     In_Symbol := True;
                     Pretty_Print_Start_Symbol (Pp,
                                                Symbol.Symbol_Name.all,
                                                Symbol.First,
                                                Info.State);
                  else
                     In_Symbol := False;
                  end if;
               end;

               Sec_Info := Instruction_Set.Parent;

               while Sec_Info /= null
                 and then Sec_Info.Kind /= Section_Addresses
               loop
                  Sec_Info := Sec_Info.Parent;
               end loop;
               Disp_Assembly_Lines
                 (Sec_Info.Section_Content
                    (Instruction_Set.First .. Instruction_Set.Last),
                  Info.Base.all, Disassemble_Cb'Access, Info.Exec.all);

               if In_Symbol then
                  Pretty_Print_End_Symbol (Pp);
               end if;

               Info := Info.Next;
            end loop;

            if In_Insn_Set then
               Pretty_Print_End_Instruction_Set (Pp);
            end if;
         end if;

         Pretty_Print_End_Line (Pp);
         Last := Index;
      end Process_One_Line;

      Line       : Natural;
      Skip       : Boolean;

      --  Start of processing for Disp_File_Line_State

   begin
      Files_Table.Open (F, File, Has_Source);

      Pretty_Print_Start_File (Pp, File, File.Stats, Has_Source, Skip);
      if Skip then
         if Has_Source then
            Close (F);
         end if;
         return;
      end if;

      Iterate_On_Lines (File, Process_One_Line'Access);

      if Has_Source then
         Line := Last + 1;
         while not End_Of_File (F) loop
            Pretty_Print_Start_Line (Pp, Line, Empty_Line_Info, Get_Line (F));
            Line := Line + 1;
         end loop;

         Close (F);
      end if;

      Pretty_Print_End_File (Pp);
   end Disp_File_Line_State;

   -----------------------
   -- Disp_File_Summary --
   -----------------------

   procedure Disp_File_Summary
   is
      procedure Disp_One_File (File : Files_Table.File_Info);
      --  Display summary for the given file

      -------------------
      -- Disp_One_File --
      -------------------

      procedure Disp_One_File (File : Files_Table.File_Info) is
      begin
         Put (File.Simple_Name.all);
         Put (": ");
         Put (Get_Stat_String (File.Stats));
         New_Line;
      end Disp_One_File;

   --  Start of processing for Disp_File_Summary

      procedure Process_One_File (FI : File_Info_Access);

      procedure Process_One_File (FI : File_Info_Access) is
      begin
         if To_Display (FI) then
            Disp_One_File (FI.all);
         end if;
      end Process_One_File;

   begin
      Files_Table_Iterate (Process_One_File'Access);
   end Disp_File_Summary;

   ---------------------
   -- Generate_Report --
   ---------------------

   procedure Generate_Report
     (Pp       : in out Pretty_Printer'Class;
      Show_Asm : Boolean)
   is
      procedure Compute_File_State (FI : File_Info_Access);

      procedure Process_One_File (FI : File_Info_Access);

      procedure Compute_File_State (FI : File_Info_Access) is
         procedure Compute_Line_State (L : Positive);

         procedure Compute_Line_State (L : Positive) is
            use Coverage;
            LI : constant Line_Info_Access := Get_Line_Info (FI, L);
            S : Line_State;
         begin
            case Get_Coverage_Level is
               when Insn
                 | Branch =>
                  Coverage.Object.Compute_Line_State (LI);
               when Stmt =>
                  Coverage.Source.Compute_Line_State (LI, FI, L);
               when Decision
                 | MCDC =>
                  raise Program_Error
                    with "source coverage not yet implemented";
               when Unknown =>
                  raise Program_Error;
            end case;
            S := LI.State;
            FI.Stats (S) := FI.Stats (S) + 1;
         end Compute_Line_State;
      begin
         FI.Stats := (others => 0);
         Iterate_On_Lines (FI, Compute_Line_State'Access);
         for I in Line_State'Range loop
            Global_Stats (I) := Global_Stats (I) + FI.Stats (I);
         end loop;
      end Compute_File_State;

      procedure Process_One_File (FI : File_Info_Access) is
      begin
         if To_Display (FI) then
            Disp_File_Line_State (Pp, FI);
         end if;
      end Process_One_File;

   begin
      Pp.Show_Asm := Show_Asm;

      --  Compute lines state, files and global statistics.
      Global_Stats := (others => 0);
      Files_Table_Iterate (Compute_File_State'Access);

      --  Print.
      Pretty_Print_Start (Pp);
      Files_Table_Iterate (Process_One_File'Access);
      Pretty_Print_End (Pp);
   end Generate_Report;

   --------------------------
   -- To_Annotation_Format --
   --------------------------

   function To_Annotation_Format (Option : String) return Annotation_Format is
   begin
      if Option = "asm" then
         return Annotate_Asm;
      elsif Option = "xcov" then
         return Annotate_Xcov;
      elsif Option = "html" then
         return Annotate_Html;
      elsif Option = "xml" then
         return Annotate_Xml;
      elsif Option = "xcov+asm" then
         return Annotate_Xcov_Asm;
      elsif Option = "html+asm" then
         return Annotate_Html_Asm;
      elsif Option = "report" then
         return Annotate_Report;
      else
         return Annotate_Unknown;
      end if;
   end To_Annotation_Format;

end Annotations;
