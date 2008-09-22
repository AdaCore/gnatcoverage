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
with Ada.Strings.Hash;
with Ada.Text_Io;
with Ada.Directories;

package body Traces_Sources is
   function Hash (El : String_Acc) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (El.all);
   end Hash;

   function Equivalent (L, R : String_Acc) return Boolean is
   begin
      return L.all = R.all;
   end Equivalent;

   function Equal (L, R : Source_Lines) return Boolean is
      pragma Unreferenced (L, R);
   begin
      return False;
   end Equal;

   --  Contains all the files.
   Filenames : Filenames_Maps.Map;

   function Find_File (Filename : String_Acc) return Filenames_Maps.Cursor
   is
      use Filenames_Maps;
      Res : Cursor;
      T : Source_Lines;
      Ok : Boolean;
   begin
      Res := Find (Filenames, Filename);
      if Res = No_Element then
         Source_Lines_Vectors.Init (T);
         Insert (Filenames, Filename, T, Res, Ok);
         if Ok = False then
            raise Program_Error;
         end if;
      end if;
      return Res;
   end Find_File;

   procedure Add_Line_State (File : Filenames_Maps.Cursor;
                             Line : Natural;
                             State : Traces.Trace_State)
   is
      procedure Process (Key : String_Acc; Element : in out Source_Lines)
      is
         pragma Unreferenced (Key);
         use Source_Lines_Vectors;
         L : constant Natural := Last (Element);
         Ls : Line_State;
      begin
         if L < Line then
            Set_Last (Element, Line);
            for I in L + 1 .. Line loop
               Element.Table (I) := (State => No_Code, others => <>);
            end loop;
         end if;
         if State = Unknown then
            raise Program_Error;
         end if;
         Ls := Element.Table (Line).State;
         Ls := Update_Table (Ls, State);
         Element.Table (Line).State := Ls;
      end Process;
   begin
      Filenames_Maps.Update_Element (Filenames, File, Process'Access);
   end Add_Line_State;

   procedure Add_Line (File : Filenames_Maps.Cursor;
                       Line : Natural;
                       Info : Addresses_Info_Acc)
   is
      procedure Process (Key : String_Acc; Element : in out Source_Lines)
      is
         pragma Unreferenced (Key);
         use Source_Lines_Vectors;
         L : constant Natural := Last (Element);
      begin
         if L < Line then
            Set_Last (Element, Line);
            for I in L + 1 .. Line loop
               Element.Table (I) := (State => No_Code, others => <>);
            end loop;
         end if;
         Append (Element.Table (Line).Lines, Info);
      end Process;
   begin
      Filenames_Maps.Update_Element (Filenames, File, Process'Access);
   end Add_Line;

--     procedure Set_Color (State : Line_State)
--     is
--        use Display;
--     begin
--        case State is
--           when No_Code =>
--              Set_Color (Black);
--           when Not_Covered =>
--              Set_Color (Red);
--           when Partially_Covered =>
--              Set_Color (Magenta);
--           when Covered | Branch_Taken | Branch_Fallthrough =>
--              Set_Color (Cyan);
--           when Covered_No_Branch | Branch_Covered =>
--              Set_Color (Green);
--        end case;
--     end Set_Color;

   type State_Map_Array is
     array (DO178B_Level_Type, Line_State) of Line_State;
   State_Map : constant State_Map_Array :=
     (Level_Raw => (No_Code => No_Code,
                    Not_Covered => Not_Covered,
                    Partially_Covered => Partially_Covered,
                    Covered => Covered,
                    Covered_No_Branch => Covered_No_Branch,
                    Branch_Taken => Branch_Taken,
                    Branch_Fallthrough => Branch_Fallthrough,
                    Branch_Covered => Branch_Covered),
      Level_A   => (No_Code => No_Code,
                    Not_Covered => Not_Covered,
                    Partially_Covered => Partially_Covered,
                    Covered => Partially_Covered,
                    Covered_No_Branch => Covered_No_Branch,
                    Branch_Taken => Partially_Covered,
                    Branch_Fallthrough => Partially_Covered,
                    Branch_Covered => Covered_No_Branch),
      Level_C   => (No_Code => No_Code,
                    Not_Covered => Not_Covered,
                    Partially_Covered => Covered_No_Branch,
                    Covered => Covered_No_Branch,
                    Covered_No_Branch => Covered_No_Branch,
                    Branch_Taken => Covered_No_Branch,
                    Branch_Fallthrough => Covered_No_Branch,
                    Branch_Covered => Covered_No_Branch));

   function Get_Stat_String (Stats : Stat_Array) return String
   is
      Total : Natural := 0;
   begin
      for J in Stats'Range loop
         Total := Total + Stats (J);
      end loop;
      Total := Total - Stats (No_Code);

      if Total = 0 then
         return "no code";
      else
         declare
            Res : constant String :=
              Natural'Image (Stats (Covered_No_Branch) * 100 / Total)
              & "% of" & Natural'Image (Total) & " lines covered";
         begin
            return Res (Res'First + 1 .. Res'Last);
         end;
      end if;
   end Get_Stat_String;

   function Get_Pourcentage (Stats : Stat_Array) return Pourcentage
   is
      Total : Natural := 0;
   begin
      for J in Stats'Range loop
         Total := Total + Stats (J);
      end loop;
      Total := Total - Stats (No_Code);

      return (Nbr => Stats (Covered_No_Branch), Total => Total);
   end Get_Pourcentage;

   procedure Disp_File_Line_State (Pp : in out Pretty_Printer'class;
                                   Filename : String;
                                   File : Source_Lines)
   is
      use Source_Lines_Vectors;
      use Ada.Text_IO;

      procedure Disassemble_Cb (Addr : Pc_Type;
                                State : Trace_State;
                                Insn : System.Address;
                                Insn_Len : Natural;
                                Res : String)
      is
      begin
         Pretty_Print_Insn (Pp, Addr, State, Insn, Insn_Len, Res);
      end Disassemble_Cb;

      procedure Print_Label (Label : String) is
      begin
         Pretty_Print_Label (Pp, Label);
      end Print_Label;

      F : File_Type;
      Has_Source : Boolean;
      Line : Natural;

      Info : Addresses_Info_Acc;
      Ls : Line_State;

      Stats : Stat_Array := (others => 0);
      Skip : Boolean;
   begin
      for I in Integer range First .. Last (File) loop
         Ls := File.Table (I).State;
         Ls := State_Map (DO178B_Level, Ls);
         Stats (Ls) := Stats (Ls) + 1;
      end loop;

      Pretty_Print_File (Pp, Filename, Stats, Skip);
      if Skip then
         return;
      end if;

      begin
         Open (F, In_File, Filename);
         Has_Source := True;
      exception
         when Ada.Text_IO.Name_Error =>
            Put_Line (Filename & ": (can't open)");
            if not Flag_Show_Missing then
               return;
            end if;
            Has_Source := False;
      end;

      for I in Integer range First .. Last (File) loop
         Ls := File.Table (I).State;
         Ls := State_Map (DO178B_Level, Ls);
         if Has_Source then
            Pretty_Print_Line (Pp, I, Ls, Get_Line (F));
         else
            Pretty_Print_Line (Pp, I, Ls, "");
         end if;

         if Flag_Show_Asm then
            Info := Get_First (File.Table (I).Lines);
            while Info /= null loop
               Disp_Line (Info, Print_Label'Access, Disassemble_Cb'Unrestricted_Access);
               Info := Get_Line_Next (Info);
            end loop;
         end if;
      end loop;
      if Has_Source then
         Line := Last (File) + 1;
         while not End_Of_File (F) loop
            Pretty_Print_Line (Pp, Line, No_Code, Get_Line (F));
            Line := Line + 1;
         end loop;
         Close (F);
      end if;

      Pretty_Print_End_File (Pp);
   end Disp_File_Line_State;

   procedure Disp_Line_State (Pp : in out Pretty_Printer'Class)
   is
      use Filenames_Maps;
      use Ada.Text_IO;
      use Ada.Directories;

      procedure Process (Key : String_Acc; Element : in Source_Lines) is
      begin
         Disp_File_Line_State (Pp, Key.all, Element);
      end Process;
      Cur : Cursor;
   begin
      Pretty_Print_Start (Pp);

      --  Iterates on all files.
      Cur := First (Filenames);
      while Cur /= No_Element loop
         Query_Element (Cur, Process'Access);
         Next (Cur);
      end loop;

      Pretty_Print_Finish (Pp);
   end Disp_Line_State;

   procedure Disp_File_Summary
   is
      use Ada.Text_IO;
      use Ada.Directories;

      procedure Process (Key : String_Acc; File : in Source_Lines)
      is
         use Source_Lines_Vectors;

         Stats : Stat_Array := (others => 0);
         State : Line_State;
      begin
         for I in Integer range First .. Last (File) loop
            State := File.Table (I).State;
            State := State_Map (DO178B_Level, State);
            Stats (State) := Stats (State) + 1;
         end loop;

         Put (Simple_Name (Key.all));
         Put (": ");
         Put (Get_Stat_String (Stats));
         New_Line;
      end Process;

      use Filenames_Maps;
      Cur : Cursor;
   begin
      Cur := First (Filenames);
      while Cur /= No_Element loop
         Query_Element (Cur, Process'Access);
         Next (Cur);
      end loop;
   end Disp_File_Summary;

end Traces_Sources;
