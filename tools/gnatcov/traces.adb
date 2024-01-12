------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2024, AdaCore                     --
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
with Qemu_Traces; use Qemu_Traces;
with Ada.Text_IO; use Ada.Text_IO;
with Hex_Images; use Hex_Images;

package body Traces is

   ---------------------
   -- Disp_State_Char --
   ---------------------

   procedure Disp_State_Char (State : Insn_State) is
   begin
      Put (Insn_State_Char (State));
   end Disp_State_Char;

   ----------------
   -- Dump_Entry --
   ----------------

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

   -------------
   -- Dump_Op --
   -------------

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

   ------------
   -- Get_Pc --
   ------------

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
