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
with Ada.Integer_Text_IO;
with Display;

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
   begin
      return False;
   end Equal;

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

   use Traces;

   subtype Known_Trace_State is
     Trace_State range Not_Covered .. Trace_State'Last;
   type State_Update_Table_Type is array (Line_State, Known_Trace_State)
     of Line_State;

   Update_Table : constant State_Update_Table_Type :=
     (
      No_Code =>
        (Not_Covered => Not_Covered,
         Covered => Covered_No_Branch,
         Branch_Taken => Branch_Taken,
         Fallthrough_Taken => Branch_Fallthrough,
         Both_Taken => Branch_Covered),
      Not_Covered =>
        (Not_Covered => Not_Covered,
         others => Partially_Covered),
      Partially_Covered =>
        (others => Partially_Covered),
      Covered =>
        (Not_Covered => Partially_Covered,
         others => Covered),
      Covered_No_Branch =>
        (Not_Covered => Partially_Covered,
         Covered => Covered_No_Branch,
         Branch_Taken => Branch_Taken,
         Fallthrough_Taken => Branch_Fallthrough,
         Both_Taken => Branch_Covered),
      Branch_Taken =>
        (Not_Covered => Partially_Covered,
         Covered => Branch_Taken,
         Branch_Taken => Covered,
         Fallthrough_Taken => Covered,
         Both_Taken => Covered),
      Branch_Fallthrough =>
        (Not_Covered => Partially_Covered,
         Covered => Branch_Fallthrough,
         Branch_Taken => Covered,
         Fallthrough_Taken => Covered,
         Both_Taken => Covered),
      Branch_Covered =>
        (Not_Covered => Partially_Covered,
         Covered | Branch_Taken | Fallthrough_Taken => Covered,
         Both_Taken => Branch_Covered)
      );

   procedure Add_Line_State (File : Filenames_Maps.Cursor;
                             Line : Natural;
                             State : Traces.Trace_State)
   is
      procedure Process (Key : String_Acc; Element : in out Source_Lines)
      is
         use Source_Lines_Vectors;
         use Traces;
         L : Natural := Last (Element);
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
         Element.Table (Line).State :=
           Update_Table (Element.Table (Line).State, State);
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
         use Source_Lines_Vectors;
         use Traces;
         L : Natural := Last (Element);
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

   procedure Set_Color (State : Line_State)
   is
      use Display;
   begin
      case State is
         when No_Code =>
            Set_Color (Black);
         when Not_Covered =>
            Set_Color (Red);
         when Partially_Covered =>
            Set_Color (Magenta);
         when Covered | Branch_Taken | Branch_Fallthrough =>
            Set_Color (Cyan);
         when Covered_No_Branch | Branch_Covered =>
            Set_Color (Green);
      end case;
   end Set_Color;

   procedure Disp_File_Line_State (Filename : String; File : Source_Lines)
   is
      use Source_Lines_Vectors;
      use Ada.Integer_Text_IO;
      use Ada.Text_IO;
      use Display;

      type State_Map_Array is array (Line_State) of Character;
      Map : constant State_Map_Array :=
        (No_Code => '.',
         Not_Covered => '-',
         Partially_Covered => '!',
         Covered => 'x',
         Covered_No_Branch => '+',
         Branch_Taken => '>',
         Branch_Fallthrough => 'v',
         Branch_Covered => '*');
      F : File_Type;
      Has_Source : Boolean;
      Line : Natural;

      Info : Addresses_Info_Acc;
   begin
      begin
         Open (F, In_File, Filename);
         Put_Line (Filename & ':');
         Has_Source := True;
      exception
         when Name_Error =>
            Put_Line (Filename & ": (can't open)");
            if not Flag_Show_Missing then
               return;
            end if;
            Has_Source := False;
      end;
      for I in Integer range First .. Last (File) loop
         Set_Color (File.Table (I).State);
         Put (I, 4);
         Put (' ');
         Put (Map (File.Table (I).State));
         Put (": ");
         if Has_Source then
            Put (Get_Line (F));
         end if;
         New_Line;

         if Flag_Show_Asm then
            Info := Get_First (File.Table (I).Lines);
            while Info /= null loop
               Disp_Line (Info);
               Info := Get_Line_Next (Info);
            end loop;
         end if;
      end loop;
      Set_Color (Black);
      if Has_Source then
         Line := Last (File) + 1;
         while not End_Of_File (F) loop
            Put (Line, 4);
            Put (" .: ");
            Put (Get_Line (F));
            New_Line;
            Line := Line + 1;
         end loop;
         Close (F);
      end if;
   end Disp_File_Line_State;

   procedure Disp_Line_State
   is
      use Filenames_Maps;
      use Ada.Text_IO;

      procedure Process (Key : String_Acc; Element : in Source_Lines) is
      begin
         Disp_File_Line_State (Key.all, Element);
      end Process;
      Cur : Cursor;
   begin
      Put_Line ("0: unknown, .: no code");
      Put_Line ("-: not covered, !: partially covered");
      Put_Line ("+: covered, *: branch covered");
      Cur := First (Filenames);
      while Cur /= No_Element loop
         Query_Element (Cur, Process'Access);
         Next (Cur);
      end loop;
   end Disp_Line_State;

   procedure Disp_File_Summary
   is
      use Ada.Text_IO;

      procedure Process (Key : String_Acc; File : in Source_Lines)
      is
         use Source_Lines_Vectors;
         use Display;

         State : Line_State;
         Result : Line_State := No_Code;
      begin
         for I in Integer range First .. Last (File) loop
            State := File.Table (I).State;
            case State is
               when No_Code =>
                  null;
               when Not_Covered
                 | Partially_Covered
                 | Covered
                 | Covered_No_Branch
                 | Branch_Covered =>
                  Result := Line_State'Min (Result, State);
               when Branch_Taken | Branch_Fallthrough =>
                  Result := Line_State'Min (Result, Covered);
            end case;
         end loop;

         Set_Color (Result);
         Put (Key.all);
         Put (": ");
         case Result is
            when Branch_Taken | Branch_Fallthrough =>
               raise Program_Error;
            when No_Code =>
               Put ("no code found");
            when Not_Covered =>
               Put ("some lines are not covered");
            when Partially_Covered =>
               Put ("all lines are covered");
            when Covered =>
               Put ("all instructions are covered - some decisions not taken");
            when Covered_No_Branch | Branch_Covered =>
               Put ("all instructions covered, all decisions taken");
         end case;
         Set_Color (Black);
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
