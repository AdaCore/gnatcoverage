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
with Ada.Unchecked_Deallocation;
with Ada.Containers.Ordered_Maps;
with Elf_Common; use Elf_Common;
with Elf_Arch; use Elf_Arch;
with Traces; use Traces;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Ada.Text_IO; use Ada.Text_IO;
with Hex_Images; use Hex_Images;
with Dwarf_Handling; use Dwarf_Handling;
with Traces_Sources; use Traces_Sources;
with Traces_Disa;
with Disa_Symbolize;

package body Traces_Names is

   type Subprogram_Name is record
      Filename : String_Acc;
      Insns : Binary_Content_Acc;
      Traces : Traces_Base_Acc;
   end record;

   function Equal (L, R : Subprogram_Name) return Boolean;
   function Compute_Routine_State (N : Subprogram_Name)
                                  return Line_State;

   function Equal (L, R : Subprogram_Name) return Boolean
   is
      pragma Unreferenced (L, R);
   begin
      return False;
   end Equal;

   package Names_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => String_Acc,
      Element_Type => Subprogram_Name,
      "<" => Less_Than,
      "=" => Equal);

   Names : Names_Maps.Map;

   procedure Read_Routines_Name
     (Efile : Elf_File; Filename : String_Acc; Exclude : Boolean)
   is
      use Addresses_Containers;
      use Names_Maps;

      Nbr_Shdr : constant Elf_Half := Get_Shdr_Num (Efile);
      type Set_Acc is access Addresses_Containers.Set;
      procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
        (Addresses_Containers.Set, Set_Acc);
      type Set_Acc_Array is array (0 .. Nbr_Shdr) of Set_Acc;
      Shdr_Sets : Set_Acc_Array := (others => null);

      procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
        (Addresses_Info, Addresses_Info_Acc);

      Shdr : Elf_Shdr_Acc;
      Last : Pc_Type;
      Addr : Pc_Type;

      Sym : Addresses_Info_Acc;
      Cur_Sym : Addresses_Containers.Cursor;

      Symtab_Idx : Elf_Half;
      Symtab_Shdr : Elf_Shdr_Acc;
      Symtab_Len : Elf_Size;
      Symtabs : Binary_Content_Acc;

      Strtab_Idx : Elf_Half;
      Strtab_Len : Elf_Size;
      Strtabs : Binary_Content_Acc;

      A_Sym : Elf_Sym;
      Sym_Name : String_Acc;

      Sym_Type : Unsigned_8;
      Cur_Name : Names_Maps.Cursor;
      Cur : Addresses_Containers.Cursor;
      Ok : Boolean;
   begin
      --  Search symtab and strtab.
      --  Exit in case of failure.
      Symtab_Idx := Get_Shdr_By_Name (Efile, ".symtab");
      if Symtab_Idx = SHN_UNDEF then
         Put_Line ("# No symbol table - file stripped ?");
         return;
      end if;
      Symtab_Shdr := Get_Shdr (Efile, Symtab_Idx);
      if Symtab_Shdr.Sh_Type /= SHT_SYMTAB
        or else Symtab_Shdr.Sh_Link = 0
        or else Natural (Symtab_Shdr.Sh_Entsize) /= Elf_Sym_Size
      then
         Put_Line ("# no strtab for .symtab - ill formed ELF file ?");
         return;
      end if;
      Strtab_Idx := Elf_Half (Symtab_Shdr.Sh_Link);

      --  Build sets for A+X sections.
      for Idx in 0 .. Nbr_Shdr - 1 loop
         Shdr := Get_Shdr (Efile, Idx);

         --  Only A+X sections are interesting.
         if (Shdr.Sh_Flags and (SHF_ALLOC or SHF_EXECINSTR))
           = (SHF_ALLOC or SHF_EXECINSTR)
           and then (Shdr.Sh_Type = SHT_PROGBITS)
         then
            Shdr_Sets (Idx) := new Addresses_Containers.Set;
         end if;
      end loop;

      --  Load symtab and strtab.
      Symtab_Len := Get_Section_Length (Efile, Symtab_Idx);
      Symtabs := new Binary_Content (0 .. Symtab_Len - 1);
      Load_Section (Efile, Symtab_Idx, Symtabs (0)'Address);

      Strtab_Len := Get_Section_Length (Efile, Strtab_Idx);
      Strtabs := new Binary_Content (0 .. Strtab_Len - 1);
      Load_Section (Efile, Strtab_Idx, Strtabs (0)'Address);

      --  Walk the symtab and put interesting symbols into the containers.
      for I in 1 .. Natural (Symtab_Len) / Elf_Sym_Size loop
         A_Sym := Get_Sym
           (Efile,
            Symtabs (0)'Address + Storage_Offset ((I - 1) * Elf_Sym_Size));
         Sym_Type := Elf_St_Type (A_Sym.St_Info);
         if  (Sym_Type = STT_FUNC or Sym_Type = STT_NOTYPE)
           and then A_Sym.St_Shndx in Shdr_Sets'Range
           and then Shdr_Sets (A_Sym.St_Shndx) /= null
         then
            Sym_Name := new String'
              (Read_String (Strtabs (A_Sym.St_Name)'Address));

            Addresses_Containers.Insert
              (Shdr_Sets (A_Sym.St_Shndx).all,
               new Addresses_Info'
               (Kind => Symbol_Addresses,
                First => Pc_Type (A_Sym.St_Value),
                Last => Pc_Type (A_Sym.St_Value + A_Sym.St_Size - 1),
                Parent => null,
                Symbol_Name => Sym_Name),
               Cur, Ok);
            if not Ok then
               Put_Line (Standard_Error,
                         "symbol " & Sym_Name.all
                           & " is an alias at " & Hex_Image (A_Sym.St_Value));
            end if;
         end if;
      end loop;
      Unchecked_Deallocation (Strtabs);
      Unchecked_Deallocation (Symtabs);

      --  Walk the sections and put the routines into the base.
      for I in Shdr_Sets'Range loop
         if Shdr_Sets (I) /= null then
            Shdr := Get_Shdr (Efile, I);

            Addr := Pc_Type (Shdr.Sh_Addr);
            Last := Pc_Type (Shdr.Sh_Addr + Shdr.Sh_Size - 1);

            --  Put_Line ("# " & Hex_Image (Addr) & "-" & Hex_Image (Last)
            --            & ": " & Get_Shdr_Name (Efile, I));

            Cur_Sym := First (Shdr_Sets (I).all);
            if Has_Element (Cur_Sym) then
               Sym := Element (Cur_Sym);
            else
               Sym := null;
            end if;

            --  Get the first symbol in the section.
            while Sym /= null and then Sym.First < Addr loop
               Unchecked_Deallocation (Sym.Symbol_Name);
               Unchecked_Deallocation (Sym);
               Next (Cur_Sym);
               if not Has_Element (Cur_Sym) then
                  Sym := null;
                  exit;
               end if;
               Sym := Element (Cur_Sym);
            end loop;

            while Sym /= null and then Sym.Last <= Last loop
               if Sym.First > Sym.Last then
                  if Sym.First <= Last then
                     Put_Line
                       (Standard_Error, "empty symbol " & Sym.Symbol_Name.all
                          & " at " & Hex_Image (Sym.First));
                  end if;
               else
                  if Sym.First > Addr then
                     Put_Line
                       (Standard_Error, "no symbols for "
                        & Hex_Image (Addr) & "-" & Hex_Image (Sym.First - 1));
                  end if;

                  Cur_Name := Names.Find (Sym.Symbol_Name);
                  if not Exclude then
                     if not Has_Element (Cur_Name) then
                        Names.Insert (Sym.Symbol_Name,
                                      Subprogram_Name'(Filename => Filename,
                                                       Insns => null,
                                                       Traces => null));
                        Sym.Symbol_Name := null;
                     else
                        if Element (Cur_Name).Filename = Filename then
                           Put_Line (Standard_Error,
                                     "symbol " & Sym.Symbol_Name.all
                                     & " is defined twice in " & Filename.all);
                        end if;
                     end if;
                  elsif Has_Element (Cur_Name) then
                     Names.Delete (Sym.Symbol_Name);
                  end if;
                  --  Put_Line (Sym.Symbol_Name.all);
                  Addr := Sym.Last;
                  exit when Addr = Pc_Type'Last;
                  Addr := Addr + 1;
               end if;

               Unchecked_Deallocation (Sym.Symbol_Name);
               Unchecked_Deallocation (Sym);
               Next (Cur_Sym);
               if not Has_Element (Cur_Sym) then
                  Sym := null;
                  exit;
               end if;
               Sym := Element (Cur_Sym);
            end loop;

            if Addr < Last then
               Put_Line
                 (Standard_Error, "no symbols for "
                    & Hex_Image (Addr) & "-" & Hex_Image (Last));
            end if;
            Unchecked_Deallocation (Shdr_Sets (I));
         end if;
      end loop;
   end Read_Routines_Name;

   procedure Read_Routines_Name (Filename : String; Exclude : Boolean)
   is
      Efile : Elf_File;
   begin
      Open_File (Efile, Filename);
      Load_Shdr (Efile);
      Read_Routines_Name (Efile, new String'(Filename), Exclude);
      Close_File (Efile);
   exception
      when Elf_Files.Error =>
         Put_Line (Standard_Output, "cannot open: " & Filename);
         raise;
   end Read_Routines_Name;

   procedure Read_Routines_Name_From_Text (Filename : String)
   is
      F : File_Type;
   begin
      Open (F, In_File, Filename);
      while not End_Of_File (F) loop
         declare
            L : constant String := Get_Line (F);
            Name : String_Acc;
            Cur : Names_Maps.Cursor;
         begin
            if L (L'First) = '#' then
               null;
            else
               Name := new String'(L);
               Cur := Names.Find (Name);
               if Names_Maps.Has_Element (Cur) then
                  Put_Line (Standard_Error,
                            "symbol " & Name.all & " is already defined");
               else
                  Names.Insert (Name,
                                Subprogram_Name'(Filename => null,
                                                 Insns => null,
                                                 Traces => null));
               end if;
            end if;
         end;
      end loop;
      Close (F);
   exception
      when Name_Error | Status_Error =>
         Put_Line (Standard_Output, "cannot open: " & Filename);
         raise;
   end Read_Routines_Name_From_Text;

   procedure Disp_All_Routines
   is
      use Names_Maps;
      Cur : Cursor;
   begin
      Cur := Names.First;
      while Has_Element (Cur) loop
         Put_Line (Key (Cur).all);
         Next (Cur);
      end loop;
   end Disp_All_Routines;

   function Add_Traces (Routine_Name : String_Acc;
                        Filename : String;
                        Content : Binary_Content) return Traces_Base_Acc
   is
      use Names_Maps;
      Cur : Cursor;

      procedure Update (Key : String_Acc; El : in out Subprogram_Name);

      procedure Update (Key : String_Acc; El : in out Subprogram_Name)
      is
         pragma Unreferenced (Key);
      begin
         if El.Insns = null and Content'Length > 0 then
            El.Insns := new Binary_Content'(Content);
            El.Filename := new String'(Filename);
         else
            --  FIXME: check the contents are similar
            --  FIXME: check size at first ?
            if Content'Length /= El.Insns.all'Length then
               Put_Line (Standard_Error,
                         "error: different function size for "
                           & Routine_Name.all);
               Put_Line (Standard_Error,
                         " (reference is " & El.Filename.all
                           & ", file is " & Filename & ")");
               raise Consolidation_Error;
            end if;
         end if;
         if El.Traces = null then
            El.Traces := new Traces_Base;
         end if;
      end Update;
   begin
      Cur := Names.Find (Routine_Name);
      if not Has_Element (Cur) then
         return null;
      end if;
      Names.Update_Element (Cur, Update'Access);
      return Element (Cur).Traces;
   end Add_Traces;

   function Compute_Routine_State (N : Subprogram_Name) return Line_State
   is
      State : Line_State := No_Code;
      Addr : Pc_Type := N.Insns'First;
      It : Entry_Iterator;
      T : Trace_Entry;
   begin
      Init (N.Traces.all, It, 0);
      loop
         Get_Next_Trace (T, It);
         exit when T = Bad_Trace;
         if T.First > Addr then
            State := Update_Table (State, Not_Covered);
            exit;
         end if;
         State := Update_Table (State, T.State);
         Addr := T.Last + 1;
      end loop;
      if Addr < N.Insns'Last then
         State := Update_Table (State, Not_Covered);
      end if;
      if State = No_Code then
         return Not_Covered;
      else
         return State_Map (DO178B_Level, State);
      end if;
   end Compute_Routine_State;

   procedure Dump_Routines_Traces (Exec : Exe_File_Type)
   is
      use Names_Maps;
      use Traces_Disa;
      Cur : Cursor;
      E : Subprogram_Name;
   begin
      Cur := Names.First;
      while Has_Element (Cur) loop
         E := Element (Cur);
         Put (Key (Cur).all);

         if E.Traces /= null then
            if E.Insns /= null then
               Set_Trace_State (E.Traces.all, E.Insns.all);
            end if;
            Put (' ');
            Put (State_Char (Compute_Routine_State (E)));
         end if;

         if E.Insns /= null then
            Put (": " & Hex_Image (E.Insns'First)
                   & '-' & Hex_Image (E.Insns'Last));
         end if;
         New_Line;
         if E.Traces /= null then
            --  Dump_Traces (E.Traces.all);
            if Flag_Show_Asm then
               Disp_Assembly_Lines
                 (E.Insns.all, E.Traces.all, Textio_Disassemble_Cb'Access,
                  Exec);
            end if;
         end if;
         Next (Cur);
      end loop;
   end Dump_Routines_Traces;

   procedure Dump_Routines_Traces
   is
      use Names_Maps;
      use Traces_Disa;
      Cur : Cursor;
      E : Subprogram_Name;
   begin
      Cur := Names.First;
      while Has_Element (Cur) loop
         E := Element (Cur);
         Put (Key (Cur).all);

         if E.Traces /= null then
            if E.Insns /= null then
               Set_Trace_State (E.Traces.all, E.Insns.all);
            end if;
            --  Dump_Traces (E.Traces.all);
            Put (' ');
            Put (State_Char (Compute_Routine_State (E)));
         end if;

         if E.Insns /= null then
            Put (": " & Hex_Image (E.Insns'First)
                   & '-' & Hex_Image (E.Insns'Last));
         end if;
         New_Line;

         if E.Traces /= null then
            --  Dump_Traces (E.Traces.all);
            if Flag_Show_Asm then
               Disp_Assembly_Lines
                 (E.Insns.all, E.Traces.all, Textio_Disassemble_Cb'Access,
                  Disa_Symbolize.Nul_Symbolizer);
            end if;
         end if;

         Next (Cur);
      end loop;
   end Dump_Routines_Traces;

end Traces_Names;
