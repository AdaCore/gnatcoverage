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
with Qemu_Traces; use Qemu_Traces;
with Ada.Text_IO; use Ada.Text_IO;
with Hex_Images; use Hex_Images;

package body Traces is
   procedure Dump_Op (Op : Unsigned_8) is
   begin
      for I in reverse 0 .. 3 loop
         if (Shift_Right (Op, I) and 1) /= 0 then
            Put ('t');
         else
            Put ('-');
         end if;
      end loop;
      if (Op and Trace_Op_Block) /= 0 then
         Put (" block");
      end if;
      if (Op and Trace_Op_Fault) /= 0 then
         Put (" fault");
      end if;
   end Dump_Op;

   procedure Disp_State_Char (State : Trace_State) is
   begin
      case State is
         when Unknown => Put ('?');
         when Not_Covered => Put ('-');
         when Covered => Put ('+');
         when Branch_Taken => Put ('>');
         when Fallthrough_Taken => Put ('v');
         when Both_Taken => Put ('*');
      end case;
   end Disp_State_Char;

   procedure Dump_Entry (E : Trace_Entry) is
   begin
      Put (Hex_Image (E.First));
      Put ('-');
      Put (Hex_Image (E.Last));
      Put (' ');
      Disp_State_Char (E.State);
      Put (": ");
      Put (Hex_Image (E.Op));
      Put (' ');
      Dump_Op (E.Op);
      New_Line;
   end Dump_Entry;

   procedure Get_Pc (Res : out Pc_Type; Line : String; Pos : in out Natural)
   is
      Digit : Pc_Type;
      C : Character;
   begin
      Res := 0;
      while Pos <= Line'Last loop
         C := Line (Pos);
         case C is
         when '0' .. '9' =>
            Digit := Character'Pos (C) - Character'Pos ('0');
         when 'a' .. 'f' =>
            Digit := Character'Pos (C) - Character'Pos ('a') + 10;
         when 'A' .. 'F' =>
            Digit := Character'Pos (C) - Character'Pos ('A') + 10;
         when others =>
            return;
         end case;
         Res := Shift_Left (Res, 4) or Digit;
         Pos := Pos + 1;
      end loop;
   end Get_Pc;

end Traces;
