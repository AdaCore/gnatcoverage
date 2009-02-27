------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                    Copyright (C) 2008-2009, AdaCore                      --
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
with Ada.Unchecked_Deallocation;
with Ada.Characters.Handling;
with Ada.Text_IO; use Ada.Text_IO;
with Qemu_Traces; use Qemu_Traces;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Hex_Images; use Hex_Images;
with Swaps;
with Traces; use Traces;
with System;

package body Traces_Files is
   function Is_Big_Endian return Boolean;
   procedure Dump_Infos (Trace_File : Trace_File_Type);
   procedure Write_Trace_File_Info (Fd : File_Descriptor;
                                    Trace_File : Trace_File_Type);
   procedure Write_Trace_File_Traces (Fd : File_Descriptor;
                                      Trace_File : Trace_File_Type;
                                      Base : Traces_Base);

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
      Sizeof_Target_Pc : Unsigned_8;
      Big_Endian : Boolean;
   end record;

   Trace_Header_Size : constant Natural :=
     Trace_Header'Size / System.Storage_Unit;

   Trace_Info_Header_Size : constant Natural :=
     Trace_Info_Header'Size / System.Storage_Unit;

   E32_Size : constant Natural := Trace_Entry32'Size / System.Storage_Unit;
   E64_Size : constant Natural := Trace_Entry64'Size / System.Storage_Unit;

   procedure Check_Header (Desc : in out Trace_File_Descriptor;
                          Hdr : Trace_Header);
   procedure Read_Trace_File_Infos (Trace_File : out Trace_File_Type;
                                    Desc : Trace_File_Descriptor);

   procedure Decode_Trace_Header (Hdr : Trace_Header;
                                  Trace_File : in out Trace_File_Type;
                                  Desc : in out Trace_File_Descriptor);

   procedure Check_Header (Desc : in out Trace_File_Descriptor;
                          Hdr : Trace_Header) is
   begin
      --  Check header.
      if Hdr.Magic /= Qemu_Trace_Magic then
         raise Bad_File_Format with "invalid header (bad magic)";
      end if;

      if Hdr.Version /= Qemu_Trace_Version then
         raise Bad_File_Format with "invalid header (bad version)";
      end if;

      if Hdr.Big_Endian /= 0 and Hdr.Big_Endian /= 1 then
         raise Bad_File_Format with "invalid header (bad endianness)";
      end if;
      Desc.Big_Endian := Hdr.Big_Endian = 1;
   end Check_Header;

   procedure Decode_Trace_Header (Hdr : Trace_Header;
                                  Trace_File : in out Trace_File_Type;
                                  Desc : in out Trace_File_Descriptor)
   is
   begin
      Desc.Kind := Hdr.Kind;
      if Desc.Kind /= Qemu_Trace_Kind_Raw
        and then Desc.Kind /= Qemu_Trace_Kind_History
      then
         raise Bad_File_Format with "invalid header (bad kind)";
      end if;

      Desc.Sizeof_Target_Pc := Hdr.Sizeof_Target_Pc;
      if Desc.Sizeof_Target_Pc /= 4 and Desc.Sizeof_Target_Pc /= 8 then
         raise Bad_File_Format with "invalid header (bad pc size)";
      end if;

      Trace_File.Machine := Unsigned_16 (Hdr.Machine_Hi) * 256
        + Unsigned_16 (Hdr.Machine_Lo);

      if Machine = 0 or Machine = Trace_File.Machine then
         Machine := Trace_File.Machine;
      else
         raise Bad_File_Format
           with "target machine doesn't match previous one";
      end if;
   end Decode_Trace_Header;

   procedure Read_Trace_File_Infos (Trace_File : out Trace_File_Type;
                                    Desc : Trace_File_Descriptor)
   is
      Ihdr : Trace_Info_Header;
      Pad : String (1 .. Trace_Info_Alignment);
      Pad_Len : Natural;
   begin
      loop
         --  Read header.
         if Read (Desc.Fd, Ihdr'Address, Trace_Info_Header_Size)
           /= Trace_Info_Header_Size
         then
            raise Bad_File_Format with "cannot read info header";
         end if;

         if Desc.Big_Endian /= My_Big_Endian then
            Swaps.Swap_32 (Ihdr.Info_Kind);
            Swaps.Swap_32 (Ihdr.Info_Length);
         end if;

         if Ihdr.Info_Kind = Info_Kind_End then
            if Ihdr.Info_Length /= 0 then
               raise Bad_File_Format with "bad end info length";
            end if;
            exit;
         end if;

         --  Read data.
         declare
            Data : String (1 .. Natural (Ihdr.Info_Length));
         begin
            if Read (Desc.Fd, Data'Address, Data'Length) /= Data'Length then
               raise Bad_File_Format with "cannot read info data";
            end if;

            Append_Info (Trace_File, Ihdr.Info_Kind, Data);
         end;

         --  Read pad.
         Pad_Len := Natural (Ihdr.Info_Length) mod Trace_Info_Alignment;
         if Pad_Len /= 0 then
            Pad_Len := Trace_Info_Alignment - Pad_Len;
            if Read (Desc.Fd, Pad'Address, Pad_Len) /= Pad_Len then
               raise Bad_File_Format with "cannot read info pad";
            end if;
            if Pad (1 .. Pad_Len) /= (1 .. Pad_Len => Character'Val (0)) then
               raise Bad_File_Format with "bad padding content";
            end if;
         end if;

      end loop;
   end Read_Trace_File_Infos;

   procedure Read_Trace_File
     (Filename : String;
      Trace_File : out Trace_File_Type;
      Info_Cb : access procedure (File : Trace_File_Type);
      Trace_Cb : access procedure (E : Trace_Entry))
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
         raise Bad_File_Format with "cannot open file " & Filename;
      end if;

      declare
         Hdr : Trace_Header;
      begin
         --  Read header.
         if Read (Desc.Fd, Hdr'Address, Trace_Header_Size)
           /= Trace_Header_Size
         then
            raise Bad_File_Format with "cannot read header";
         end if;
         Check_Header (Desc, Hdr);

         if Hdr.Kind = Qemu_Trace_Kind_Info then
            Read_Trace_File_Infos (Trace_File, Desc);
            --  Read header.
            Res := Read (Desc.Fd, Hdr'Address, Trace_Header_Size);
            if Res = 0 then
               if Info_Cb /= null then
                  Info_Cb.all (Trace_File);
               end if;
               Close (Desc.Fd);
               return;
            end if;
            if Res /= Trace_Header_Size then
               raise Bad_File_Format with "cannot read header";
            end if;
            Check_Header (Desc, Hdr);
         end if;
         Decode_Trace_Header (Hdr, Trace_File, Desc);
      exception
         when others =>
            Close (Desc.Fd);
            raise;
      end;

      if Info_Cb /= null then
         Info_Cb.all (Trace_File);
      end if;

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
         Trace_Cb.all (Trace_Entry'(First => E32.Pc,
                                    Last => E32.Pc + Pc_Type (E32.Size) - 1,
                                    Op => E32.Op,
                                    State => Unknown));
      end loop;

      Close (Desc.Fd);
   end Read_Trace_File;

   procedure Read_Trace_File (Filename : String;
                              Trace_File : out Trace_File_Type;
                              Base : in out Traces_Base)
   is
      procedure Cb (E : Trace_Entry);

      procedure Cb (E : Trace_Entry) is
      begin
         Add_Entry (Base,
                    First => E.First,
                    Last => E.Last,
                    Op => E.Op);
      end Cb;
   begin
      Read_Trace_File (Filename, Trace_File, null, Cb'Access);
   end Read_Trace_File;

   procedure Dump_Infos (Trace_File : Trace_File_Type)
   is
      Info : Trace_File_Info_Acc;
      Is_Print : Boolean;
   begin
      Info := Trace_File.First_Infos;
      if Info = null then
         return;
      end if;
      loop
         Put ("Tag  : " & Hex_Image (Info.Kind));
         case Info.Kind is
            when Info_Kind_User_Tag =>
               Put (" (User_Tag)");
            when Info_Kind_Date =>
               Put (" (Date)");
            when others =>
               null;
         end case;
         New_Line;
         Put_Line ("Len  :" & Natural'Image (Info.Raw_Length));

         Put ("Data :");
         Is_Print := True;
         for I in Info.Data'Range loop
            if not Ada.Characters.Handling.Is_Graphic (Info.Data (I)) then
               Is_Print := False;
               exit;
            end if;
         end loop;
         if Is_Print then
            Put (' ');
            Put_Line (Info.Data);
         else
            for I in Info.Data'Range loop
               Put (' ');
               Put (Hex_Image (Unsigned_8 (Character'Pos (Info.Data (I)))));
            end loop;
            New_Line;
         end if;

         case Info.Kind is
            when Info_Kind_Date =>
               declare
                  Date_Info  : Trace_Info_Date;
                  subtype String_8 is String (1 .. 8);
                  function Str_To_Date_Info is new Ada.Unchecked_Conversion
                    (String_8, Trace_Info_Date);

                  procedure Put_Pad (Num : Natural; Width : Positive);

                  procedure Put_Pad (Num : Natural; Width : Positive)
                  is
                     Res : String (1 .. Width);
                     V : Natural := Num;
                  begin
                     for I in reverse Res'Range loop
                        Res (I) := Character'Val ((V rem 10)
                                                  + Character'Pos ('0'));
                        V := V / 10;
                     end loop;
                     Put (Res);
                  end Put_Pad;
               begin
                  Date_Info := Str_To_Date_Info (Info.Data);
                  Put ("       ");
                  Put_Pad (Natural (Date_Info.Year), 4);
                  Put ('-');
                  Put_Pad (Natural (Date_Info.Month), 2);
                  Put ('-');
                  Put_Pad (Natural (Date_Info.Day), 2);
                  Put (' ');
                  Put_Pad (Natural (Date_Info.Hour), 2);
                  Put (':');
                  Put_Pad (Natural (Date_Info.Min), 2);
                  Put (':');
                  Put_Pad (Natural (Date_Info.Sec), 2);
                  New_Line;
               end;
            when others =>
               null;
         end case;

         Info := Info.Next;
         exit when Info = null;
         New_Line;
      end loop;
   end Dump_Infos;

   procedure Dump_Trace_File (Filename : String)
   is
      Trace_File : Trace_File_Type;
   begin
      Read_Trace_File (Filename, Trace_File,
                       Dump_Infos'Access, Dump_Entry'Access);
      Free (Trace_File);
   end Dump_Trace_File;

   procedure Write_Trace_File_Info (Fd : File_Descriptor;
                                    Trace_File : Trace_File_Type)
   is
      Hdr : Trace_Header;
      Info : Trace_File_Info_Acc;
      Ihdr : Trace_Info_Header;
   begin
      Hdr := (Magic => Qemu_Trace_Magic,
              Version => Qemu_Trace_Version,
              Kind => Qemu_Trace_Kind_Info,
              Sizeof_Target_Pc => 0,
              Big_Endian => Boolean'Pos (Is_Big_Endian),
              Machine_Hi => 0,
              Machine_Lo => 0,
              Padding => 0);

      if Write (Fd, Hdr'Address, Trace_Header_Size)
        /= Trace_Header_Size
      then
         raise Write_Error with "failed to write first header";
      end if;

      Info := Trace_File.First_Infos;
      while Info /= null loop
         declare
            Pad : constant String (1 .. Trace_Info_Alignment) :=
              (others => Character'Val (0));
            Pad_Len : Natural;
         begin
            Ihdr.Info_Kind := Info.Kind;
            Ihdr.Info_Length := Unsigned_32 (Info.Raw_Length);

            if Write (Fd, Ihdr'Address, Trace_Info_Header_Size)
              /= Trace_Info_Header_Size
            then
               raise Write_Error with "failed to write info header";
            end if;

            if Write (Fd, Info.Data'Address, Info.Raw_Length)
              /= Info.Raw_Length
            then
               raise Write_Error with "failed to write info data";
            end if;

            Pad_Len := Info.Raw_Length mod Trace_Info_Alignment;
            if Pad_Len /= 0 then
               Pad_Len := Trace_Info_Alignment - Pad_Len;
               if Write (Fd, Pad'Address, Pad_Len) /= Pad_Len then
                  raise Write_Error with "failed to write info pad";
               end if;
            end if;
         end;

         Info := Info.Next;
      end loop;

      --  Write the terminator.
      Ihdr.Info_Kind := Info_Kind_End;
      Ihdr.Info_Length := 0;
      if Write (Fd, Ihdr'Address, Trace_Info_Header_Size)
        /= Trace_Info_Header_Size
      then
         raise Write_Error with "failed to write info header";
      end if;
   end Write_Trace_File_Info;

   procedure Write_Trace_File_Traces (Fd : File_Descriptor;
                                      Trace_File : Trace_File_Type;
                                      Base : Traces_Base)
   is
      pragma Unreferenced (Trace_File);
      Hdr : Trace_Header;

      E : Trace_Entry;
      E32 : Trace_Entry32;
      E64 : Trace_Entry64;
      Addr : System.Address;
      Res_Size : Natural;
      Cur : Entry_Iterator;
   begin
      Hdr := (Magic => Qemu_Trace_Magic,
              Version => Qemu_Trace_Version,
              Kind => Qemu_Trace_Kind_Raw,
              Sizeof_Target_Pc => Pc_Type_Size,
              Big_Endian => Boolean'Pos (Is_Big_Endian),
              Machine_Hi => Unsigned_8 (Shift_Right (Machine, 8)),
              Machine_Lo => Unsigned_8 (Machine and 16#Ff#),
             Padding => 0);

      if Write (Fd, Hdr'Address, Trace_Header_Size) /= Trace_Header_Size then
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

      Init (Base, Cur, 0);
      Get_Next_Trace (E, Cur);
      while E /= Bad_Trace loop

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
            raise Write_Error with "failed to write entry";
         end if;

         Get_Next_Trace (E, Cur);
      end loop;
   end Write_Trace_File_Traces;

   procedure Write_Trace_File (Filename : String;
                               Trace_File : Trace_File_Type;
                               Base : Traces_Base)
   is
      Fd : File_Descriptor;
   begin
      Fd := Create_File (Filename, Binary);
      if Fd = Invalid_FD then
         raise Write_Error with "failed to create the file";
      end if;

      if Trace_File.First_Infos /= null
        or else Trace_File.Kind = Qemu_Trace_Kind_Info
      then
         Write_Trace_File_Info (Fd, Trace_File);
      end if;

      --  Stop now if we only dump infos.
      if Trace_File.Kind = Qemu_Trace_Kind_Info then
         return;
      end if;

      Write_Trace_File_Traces (Fd, Trace_File, Base);

      Close (Fd);
   exception
      when others =>
         Close (Fd);
         raise;
   end Write_Trace_File;

   procedure Write_Trace_File (Filename : String;
                               Trace_File : Trace_File_Type)
   is
      Fd : File_Descriptor;
   begin
      Fd := Create_File (Filename, Binary);
      if Fd = Invalid_FD then
         raise Write_Error with "failed to create the file";
      end if;

      Write_Trace_File_Info (Fd, Trace_File);

      Close (Fd);
   exception
      when others =>
         Close (Fd);
         raise;
   end Write_Trace_File;

   procedure Append_Info (File : in out Trace_File_Type;
                          Kind : Unsigned_32;
                          Data : String)
   is
      Info : constant Trace_File_Info_Acc :=
        new Trace_File_Info'(Data'Length, null, Kind, Data);
   begin
      if File.Last_Infos = null then
         File.First_Infos := Info;
      else
         File.Last_Infos.Next := Info;
      end if;
      File.Last_Infos := Info;
   end Append_Info;

   function Get_Info (File : Trace_File_Type; Kind : Unsigned_32)
                     return String
   is
      Info : Trace_File_Info_Acc;
   begin
      Info := File.First_Infos;
      while Info /= null loop
         if Info.Kind = Kind then
            return Info.Data;
         end if;
         Info := Info.Next;
      end loop;
      return "";
   end Get_Info;

   procedure Free (Trace_File : in out Trace_File_Type)
   is
      procedure Unchecked_Deallocation is new
        Ada.Unchecked_Deallocation (Trace_File_Info, Trace_File_Info_Acc);
      Info, N_Info : Trace_File_Info_Acc;
   begin
      Info := Trace_File.First_Infos;
      while Info /= null loop
         N_Info := Info.Next;
         Unchecked_Deallocation (Info);
         Info := N_Info;
      end loop;
   end Free;

   procedure Create_Trace_File (Trace_File : out Trace_File_Type) is
   begin
      Trace_File := Trace_File_Type'(Kind => Qemu_Trace_Kind_Info,
                                     Sizeof_Target_Pc => 0,
                                     Big_Endian => Is_Big_Endian,
                                     Machine => 0,
                                     First_Infos => null,
                                     Last_Infos => null);
   end Create_Trace_File;

--     procedure Annotate_Objdump
--     is
--        use Display;
--        Cur : Cursor := First (Entries);
--        E : Trace_Entry;
--       Sym : constant array (Unsigned_8 range 0 .. 3) of Character := "+>v*";
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
--   end Annotate_Objdump;

end Traces_Files;
