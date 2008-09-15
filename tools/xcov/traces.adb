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
with Ada.Unchecked_Conversion;
with Qemu_Traces; use Qemu_Traces;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ada.Text_Io; use Ada.Text_IO;
with Hex_Images; use Hex_Images;
with Elf_Common; use Elf_Common;
with Display;
with Swaps;

package body Traces is

   function "=" (L, R : Trace_Entry) return Boolean is
   begin
      return L.First = R.First and L.Last = R.Last;
   end "=";

   function "<" (L, R : Trace_Entry) return Boolean is
   begin
      return L.First < R.First;
   end "<";

   Entries : Entry_Set.Set;

   use Entry_Set;

   --  Add a trace entry in the ordered_Set.  May discard useless entries
   --  or merge entries.
   procedure Add_Entry (First : Pc_Type; Last : Pc_Type; Op : Unsigned_8)
   is
      Cur : Cursor;
      Status : Boolean;
      E : Trace_Entry;
   begin
      --  Discard fault.
      if (Op and Trace_Op_Fault) /= 0 then
         return;
      end if;

      Insert (Entries,
              Trace_Entry'(First, Last, Op, Unknown),
              Cur, Status);
      if Status = False then
         --  Merge
         E := Element (Cur);
         E.Op := E.Op or Op;
         Replace_Element (Entries, Cur, E);
      end if;
   end Add_Entry;

   --  Walk the set and try to merge entries.
   procedure Merge_Entries
   is
      Cur : Cursor;
      N_Cur : Cursor;
      E : Trace_Entry;

      --  Merged entry.
      M : Trace_Entry;
      M_Pos : Cursor;
      Modified : Boolean;
   begin
      M_Pos := First (Entries);
      if M_Pos = No_Element then
         return;
      end if;
      M := Element (M_Pos);
      Modified := False;

      Cur := Next (M_Pos);

      while Cur /= No_Element loop
         E := Element (Cur);
         N_Cur := Next (Cur);

         --  If two traces are consecutive and the first one does not
         --  terminate with a jump, merge the traces.
         if E.First = M.Last + 1
           and then M.Op = Trace_Op_Block
           and then (E.Op and Trace_Op_Block) /= 0
         then
            M.Last := E.Last;
            M.Op := M.Op or E.Op;
            Modified := True;
            Delete (Entries, Cur);

         --  Merge two entries.
         elsif E.First >= M.First
           and then E.Last = M.Last
         then
            M.Op := M.Op or E.Op;
            Modified := True;
            Delete (Entries, Cur);

         else
            if Modified then
               Modified := False;
               Replace_Element (Entries, M_Pos, M);
            end if;
            M := E;
            M_Pos := Cur;
         end if;

         Cur := N_Cur;
      end loop;
   end Merge_Entries;

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

   procedure Set_Color (State : Trace_State)
   is
      use Display;
      C : Color;
   begin
      case State is
         when Unknown | Not_Covered => C := Red;
         when Covered | Both_Taken => C := Green;
         when Branch_Taken | Fallthrough_Taken => C := Cyan;
      end case;
      Set_Color (C);
   end Set_Color;

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

   procedure Dump_Traces
   is
      Cur : Cursor := First (Entries);
   begin
      while Cur /= No_Element loop
         Dump_Entry (Element (Cur));
         Next (Cur);
      end loop;
   end Dump_Traces;

   --  Test if the host is big endian.
   function Is_Big_Endian return Boolean
   is
      V32 : constant Unsigned_32 := 16#11_22_33_44#;
      type Four_Bytes is array (0 .. 3) of Unsigned_8;

      function To_Four_Bytes is new Ada.Unchecked_Conversion
        (Source => Unsigned_32, Target => Four_Bytes);

      Res : constant Four_Bytes := To_Four_Bytes (V32);
   begin
      return Res = (16#11#, 16#22#, 16#33#, 16#44#);
   end Is_Big_Endian;

   My_Big_Endian : constant Boolean := Is_Big_Endian;

   type Trace_File_Descriptor is record
      Fd : File_Descriptor;

      --  Parameter from header.
      Kind : Unsigned_8;
      sizeof_target_pc : unsigned_8;
      Big_Endian : Boolean;
      Machine : Unsigned_16;
   end record;

   Trace_Header_Size : constant Natural :=
     Trace_Header'Size / System.Storage_Unit;

   E32_Size : constant Natural := Trace_Entry32'Size / System.Storage_Unit;
   E64_Size : constant Natural := Trace_Entry64'Size / System.Storage_Unit;

   procedure Read_Trace_File_Header (Desc : in out Trace_File_Descriptor)
   is
      Hdr : Trace_Header;
   begin
      --  Read header.
      if Read (Desc.Fd, Hdr'Address, Trace_Header_Size) /= Trace_Header_Size
      then
         raise Bad_File_Format with "cannot read header";
      end if;

      --  Check header.
      if Hdr.Magic /= Qemu_Trace_Magic then
         raise Bad_File_Format with "invalid header (bad magic)";
      end if;

      if Hdr.Version /= Qemu_Trace_Version then
         raise Bad_File_Format with "invalid header (bad version)";
      end if;

      Desc.Kind := hdr.Kind;
      if Desc.Kind /= Qemu_Trace_Kind_Raw then
         raise Bad_File_Format with "invalid header (bad kind)";
      end if;

      Desc.Sizeof_Target_Pc := Hdr.Sizeof_Target_Pc;
      if Desc.Sizeof_Target_Pc /= 4 and Desc.Sizeof_Target_Pc /= 8 then
         raise Bad_File_Format with "invalid header (bad pc size)";
      end if;

      if Hdr.Big_Endian /= 0 and Hdr.Big_Endian /= 1 then
         raise Bad_File_Format with "invalid header (bad endianness)";
      end if;
      Desc.Big_Endian := Hdr.Big_Endian = 1;

      Desc.Machine := Unsigned_16 (Hdr.Machine_Hi) * 256
        + unsigned_16 (Hdr.Machine_Lo);

      if Machine = 0 or Machine = Desc.Machine then
         Machine := Desc.Machine;
      else
         raise Bad_File_Format with "target machine doesn't match previous one";
      end if;

   end Read_Trace_File_Header;

   procedure Read_Trace_File (Filename : String)
   is
      Desc : Trace_File_Descriptor;
      E32 : Trace_Entry32;
      E64 : Trace_Entry64;
      Addr : System.Address;
      Res : Integer;
      Res_Size : Integer;
   begin
      --  Open file.
      Desc.Fd := Open_Read (Filename, Binary);
      if Desc.Fd = Invalid_FD then
         raise Bad_File_Format with "cannot open file";
      end if;

      begin
         Read_Trace_File_Header (Desc);
      exception
         when others =>
            Close (Desc.Fd);
            raise;
      end;

      if Desc.Sizeof_Target_Pc = 4 then
         Addr := E32'Address;
         Res_Size := E32_Size;
      else
         Addr := E64'Address;
         Res_Size := E64_Size;
      end if;

      loop
         --  Read an entry.
         Res := Read (Desc.Fd, Addr, Res_Size);

         --  Check result.
         exit when Res = 0;
         if Res /= Res_Size then
            Close (Desc.Fd);
            raise Bad_File_Format with "file truncated";
         end if;

         --  Basic checks.
         if Desc.Sizeof_Target_Pc /= 4 then
            raise Bad_File_Format with "only 4 bytes pc are handled";
         end if;

         if Desc.Big_Endian /= My_Big_Endian then
            Swaps.Swap_32 (E32.Pc);
            Swaps.Swap_16 (E32.Size);
         end if;

         --  Disp the entry.
         if False then
            if Desc.Sizeof_Target_Pc = 4 then
               Put (Hex_Image (E32.Pc));
               Put ('-');
               Put (Hex_Image (E32.Pc + Unsigned_32 (E32.Size) - 1));
               Put (": ");
               Put (Hex_Image (E32.Op));
                  Put (' ');
                  Dump_Op (E32.Op);
               New_Line;
            end if;
         end if;

         --  Add the entry in the AVL.
         Add_Entry (First => E32.Pc,
                    Last => E32.Pc + Pc_Type (E32.Size) - 1,
                    Op => E32.Op);
      end loop;

      Close (Desc.Fd);

      Merge_Entries;

      --  Dump_Tree;
   end Read_Trace_File;

   procedure Write_Trace_File (Filename : String)
   is
      Fd : File_Descriptor;
      Hdr : Trace_Header;

      E : Trace_Entry;
      E32 : Trace_Entry32;
      E64 : Trace_Entry64;
      Addr : System.Address;
      Res_Size : Natural;
      Cur : Cursor;
   begin
      Fd := Create_File (Filename, Binary);
      if Fd = Invalid_FD then
         raise Write_Error with "failed to create the file";
      end if;

      Hdr := (Magic => Qemu_Trace_Magic,
              Version => Qemu_Trace_Version,
              Kind => Qemu_Trace_Kind_Raw,
              Sizeof_Target_Pc => Pc_Type_Size,
              Big_Endian => Boolean'Pos (Is_Big_Endian),
              Machine_Hi => Unsigned_8 (Shift_Right (Machine, 8)),
              Machine_Lo => Unsigned_8 (Machine and 16#Ff#),
             Padding => 0);

      if Write (Fd, Hdr'Address, Trace_Header_Size) /= Trace_Header_Size then
         Close (Fd);
         raise Write_Error with "failed to write header";
      end if;

      pragma Warnings (Off);
      if Pc_Type_Size = 4 then
         Addr := E32'Address;
         Res_Size := E32_Size;
      else
         Addr := E64'Address;
         Res_Size := E64_Size;
      end if;
      pragma Warnings (On);

      Cur := First (Entries);
      while Cur /= No_Element loop
         E := Element (Cur);

         pragma Warnings (Off);
         if Pc_Type_Size = 4 then
            E32 := (Pc => E.First,
                    Size => Unsigned_16 (E.Last - E.First + 1),
                    Op => E.Op,
                    Pad0 => 0);
         else
            raise Program_Error;
         end if;
         pragma Warnings (On);

         if Write (Fd, Addr, Res_Size) /= Res_Size then
            Close (Fd);
            raise Write_Error with "failed to write entry";
         end if;

         Cur := Next (Cur);
      end loop;

      Close (Fd);
   end Write_Trace_File;

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

   procedure Annotate_Objdump
   is
      use Display;
      Cur : Cursor := First (Entries);
      E : Trace_Entry;
      Sym : constant array (Unsigned_8 range 0 .. 3) of Character := "+>v*";
   begin
      if Cur = No_Element then
         E := (First => 1, Last => 0, Op => 0, State => Unknown);
      else
         E := Element (Cur);
         Next (Cur);
      end if;

      loop
         declare
            Line : constant String := Get_Line;
            Pos : Natural := Line'First;
            Pc : Pc_Type := 0;
            Op : Unsigned_8;

            Insn : array (0 .. 15) of Unsigned_8;
            Insn_Len : Natural := 0;
            Insn_Byte : Pc_Type;
            Old_Pos : Natural;
         begin
            --  Try to extract a PC.
            Get_Pc (Pc, Line, Pos);
            if Pos = Line'First or else Line (Pos) /= ':' then
               Set_Color (Black);
               Put ("  ");
            else
               --  Try to extract instruction bytes.
               if Line (Pos + 1) = Ascii.HT then
                  Pos := Pos + 2;
                  loop
                     Old_Pos := Pos;
                     Get_Pc (Insn_Byte, Line, Pos);
                     exit when Pos /= Old_Pos + 2;
                     exit when Insn_Len > Insn'Last;
                     Insn (Insn_Len) := Unsigned_8 (Insn_Byte);
                     Insn_Len := Insn_Len + 1;
                     exit when Line (Pos) /= ' ';
                     Pos := Pos + 1;
                  end loop;
               end if;

               if Pc > E.Last then
                  while Cur /= No_Element loop
                     E := Element (Cur);
                     --Dump_Entry (E);
                     Next (Cur);
                     exit when Pc <= E.Last;
                  end loop;
               end if;

               if Pc + Pc_Type (Insn_Len) - 1 = E.Last then
                  Op := E.Op and 3;
                  if Machine = EM_PPC
                    and Op = 1
                    and Insn_Len = 4
                  then
                     if (Insn(0) and 16#Fc#) = 16#48# then
                        --  b, ba, bl and bla
                        Op := 3;
                     elsif (Insn(0) and 16#Fd#) = 16#42#
                       and then (Insn(1) and 16#80#) = 16#80#
                     then
                        --  bc always
                        Op := 3;
                     end if;
                  end if;
                  case Op is
                     when 0 | 3 => Set_Color (Green);
                     when 1 | 2 => Set_Color (Magenta);
                     when others =>
                        raise Program_Error;
                  end case;
                  Put (Sym (Op));
                  Put (' ');
               elsif Pc >= E.First and Pc <= E.Last then
                  Set_Color (Green);
                  Put ("+ ");
               else
                  Set_Color (Red);
                  Put ("- ");
               end if;
            end if;
            Put_Line (Line);
         end;
      end loop;
   exception
      when End_Error =>
         Set_Color (Black);
   end Annotate_Objdump;

   procedure Init (Iterator : out Entry_Iterator; Pc : Pc_Type)
   is
      Key : constant Trace_Entry := (Pc, Pc, 0, Unknown);
   begin
      Iterator := (Cur => Floor (Entries, Key));
      if Iterator.Cur = No_Element then
         Iterator.Cur := First (Entries);
      end if;
   end Init;

   procedure Get_Next_Trace (Trace : out Trace_Entry;
                             Iterator : in out Entry_Iterator) is
   begin
      if Iterator.Cur = No_Element then
         Trace := Bad_Trace;
      else
         Trace := Element (Iterator.Cur);
         Next (Iterator.Cur);
      end if;
   end Get_Next_Trace;

   procedure Update_State (Iterator : Entry_Iterator; State : Trace_State)
   is
      Cur : Cursor;
      Trace : Trace_Entry;
   begin
      if Iterator.Cur = No_Element then
         Cur := Last (Entries);
      else
         Cur := Previous (Iterator.Cur);
      end if;
      Trace := Element (Cur);
      Trace.State := State;
      Replace_Element (Entries, Cur, Trace);
   end Update_State;

   procedure Split_Trace (Iterator : in out Entry_Iterator;
                          Pc : Pc_Type;
                          Cur_State, Next_State : Trace_State)
   is
      Cur : Cursor;
      Trace, Next_Trace : Trace_Entry;
   begin
      if Iterator.Cur = No_Element then
         Cur := Last (Entries);
      else
         Cur := Previous (Iterator.Cur);
      end if;
      Trace := Element (Cur);
      Next_Trace := Trace;
      Trace.State := Cur_State;
      Trace.Last := Pc;
      Replace_Element (Entries, Cur, Trace);
      Next_Trace.First := Pc + 1;
      Next_Trace.State := Next_State;
      Insert (Entries, Next_Trace);
   end Split_Trace;
end Traces;
