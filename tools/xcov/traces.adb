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
with Ada.Text_Io; use Ada.Text_IO;
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

--     procedure Annotate_Objdump
--     is
--        use Display;
--        Cur : Cursor := First (Entries);
--        E : Trace_Entry;
--        Sym : constant array (Unsigned_8 range 0 .. 3) of Character := "+>v*";
--     begin
--        if Cur = No_Element then
--           E := (First => 1, Last => 0, Op => 0, State => Unknown);
--        else
--           E := Element (Cur);
--           Next (Cur);
--        end if;

--        loop
--           declare
--              Line : constant String := Get_Line;
--              Pos : Natural := Line'First;
--              Pc : Pc_Type := 0;
--              Op : Unsigned_8;

--              Insn : array (0 .. 15) of Unsigned_8;
--              Insn_Len : Natural := 0;
--              Insn_Byte : Pc_Type;
--              Old_Pos : Natural;
--           begin
--              --  Try to extract a PC.
--              Get_Pc (Pc, Line, Pos);
--              if Pos = Line'First or else Line (Pos) /= ':' then
--                 Set_Color (Black);
--                 Put ("  ");
--              else
--                 --  Try to extract instruction bytes.
--                 if Line (Pos + 1) = Ascii.HT then
--                    Pos := Pos + 2;
--                    loop
--                       Old_Pos := Pos;
--                       Get_Pc (Insn_Byte, Line, Pos);
--                       exit when Pos /= Old_Pos + 2;
--                       exit when Insn_Len > Insn'Last;
--                       Insn (Insn_Len) := Unsigned_8 (Insn_Byte);
--                       Insn_Len := Insn_Len + 1;
--                       exit when Line (Pos) /= ' ';
--                       Pos := Pos + 1;
--                    end loop;
--                 end if;

--                 if Pc > E.Last then
--                    while Cur /= No_Element loop
--                       E := Element (Cur);
--                       --Dump_Entry (E);
--                       Next (Cur);
--                       exit when Pc <= E.Last;
--                    end loop;
--                 end if;

--                 if Pc + Pc_Type (Insn_Len) - 1 = E.Last then
--                    Op := E.Op and 3;
--                    if Machine = EM_PPC
--                      and Op = 1
--                      and Insn_Len = 4
--                    then
--                       if (Insn(0) and 16#Fc#) = 16#48# then
--                          --  b, ba, bl and bla
--                          Op := 3;
--                       elsif (Insn(0) and 16#Fd#) = 16#42#
--                         and then (Insn(1) and 16#80#) = 16#80#
--                       then
--                          --  bc always
--                          Op := 3;
--                       end if;
--                    end if;
--                    case Op is
--                       when 0 | 3 => Set_Color (Green);
--                       when 1 | 2 => Set_Color (Magenta);
--                       when others =>
--                          raise Program_Error;
--                    end case;
--                    Put (Sym (Op));
--                    Put (' ');
--                 elsif Pc >= E.First and Pc <= E.Last then
--                    Set_Color (Green);
--                    Put ("+ ");
--                 else
--                    Set_Color (Red);
--                    Put ("- ");
--                 end if;
--              end if;
--              Put_Line (Line);
--           end;
--        end loop;
--     exception
--        when End_Error =>
--           Set_Color (Black);
--     end Annotate_Objdump;

end Traces;
