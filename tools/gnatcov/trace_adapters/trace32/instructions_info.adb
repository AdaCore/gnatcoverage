------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2017, AdaCore                       --
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

with Interfaces;    use Interfaces;

with Binary_Files;  use Binary_Files;
with Disassemblers; use Disassemblers;
with Execs_Dbase;
with Outputs;       use Outputs;

package body Instructions_Info is

   --------------
   -- Load_Elf --
   --------------

   procedure Load_Elf (This      : in out Insn_Info;
                       Exec_Path : String)
   is
      Section_Iterator : Addresses_Iterator;
   begin
      begin
         Execs_Dbase.Open_Exec (Exec_Path, 0, This.Exec);
      exception
         when Binary_Files.Error =>
            Fatal_Error ("Could not open " & Exec_Path);
      end;

      Build_Symbols (This.Exec.all);

      Init_Iterator (This.Exec.all, Section_Addresses, Section_Iterator);
      loop
         Next_Iterator (Section_Iterator, This.Section);

         exit when This.Section = null;

         if This.Section.Section_Name.all = ".text" then
            Load_Section_Content (This.Exec.all, This.Section);
            This.I_Ranges := Get_Insn_Set_Ranges
              (This.Exec.all, This.Section.Section_Sec_Idx).all;
            return;
         end if;
      end loop;
      Fatal_Error ("Could not find .text section in " & Exec_Path);
   end Load_Elf;

   ------------
   -- Loaded --
   ------------

   function  Loaded (This : Insn_Info) return Boolean is
     (This.Exec /= null);

   ---------------------------
   -- Get_Next_Insn_Address --
   ---------------------------

   function Get_Next_Insn_Address (This : in out Insn_Info;
                                   PC : Pc_Type)
                                   return Pc_Type
   is
   begin
      return PC + This.Get_Insn_Length (PC);
   end Get_Next_Insn_Address;

   ---------------------
   -- Get_Insn_Length --
   ---------------------

   function Get_Insn_Length (This : in out Insn_Info;
                             PC   : Pc_Type)
                             return Pc_Type
   is
      Disas    : access Disassembler'Class;
      Code     : constant Binary_Content := This.Section.Section_Content;
   begin

      Disas := Disa_For_Machine (Machine, This.I_Ranges, This.Cache, PC);

      return Pc_Type (Disas.Get_Insn_Length (Slice (Code, PC, Code.Last)));
   end Get_Insn_Length;

   ---------------
   -- Is_Branch --
   ---------------

   function Is_Branch (This : in out Insn_Info;
                       PC   : Pc_Type)
                       return Boolean
   is
      Disas       : access Disassembler'Class;
      Code        : constant Binary_Content := This.Section.Section_Content;
      Branch      : Branch_Kind;
      Flag_Indir  : Boolean;
      Flag_Cond   : Boolean;
      Branch_Dest : Dest;
      FT_Dest     : Dest;
   begin
      Disas := Disa_For_Machine (Machine, This.I_Ranges, This.Cache, PC);
      Disas.Get_Insn_Properties (Slice (Code, PC, Code.Last),
                                 Pc          => PC,
                                 Branch      => Branch,
                                 Flag_Indir  => Flag_Indir,
                                 Flag_Cond   => Flag_Cond,
                                 Branch_Dest => Branch_Dest,
                                 FT_Dest     => FT_Dest);
      return Branch /= Br_None;
   end Is_Branch;

   -----------------
   -- Fallthrough --
   -----------------

   function Fallthrough_Address (This           : in out Insn_Info;
                         Caller, Target : Pc_Type)
                         return Boolean
   is
   begin
      return Get_Next_Insn_Address (This, Caller) = Target;
   end Fallthrough_Address;

end Instructions_Info;
