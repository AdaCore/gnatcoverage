------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                        Copyright (C) 2009, AdaCore                       --
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
with Ada.Containers.Ordered_Maps;
with Interfaces;

with Elf_Disassemblers; use Elf_Disassemblers;
with Hex_Images;
with Traces; use Traces;
with Traces_Files;
with Traces_Disa;

package body Traces_History is
   procedure Dump_Traces_With_Asm (Exe : Exe_File_Type;
                                   Trace_Filename : String)
   is
      use Traces_Files;
      Addr : Addresses_Info_Acc := null;

      procedure Disp_Entry (E : Trace_Entry);

      procedure Disp_Entry (E : Trace_Entry)
      is
         use Traces_Disa;
         Sec : Addresses_Info_Acc;
         Line : String (1 .. 128);
         Line_Pos : Natural := Line'First;
      begin
         Dump_Entry (E);
         if Addr = null
           or else E.First not in Addr.First .. Addr.Last
         then
            Addr := Get_Symbol (Exe, E.First);
         end if;
         if Addr = null then
            Put_Line ("(not in the executable)");
         else
            Symbolize (Exe, E.First, Line, Line_Pos);
            Line (Natural'Min (Line'Last, Line_Pos)) := ':';
            Put_Line (Line (Line'First + 1 .. Line_Pos));
            Sec := Addr.Parent;
            while Sec.Kind /= Section_Addresses loop
               Sec := Sec.Parent;
            end loop;
            Load_Section_Content (Exe, Sec);
            For_Each_Insn (Sec.Section_Content (E.First .. E.Last), Covered,
                           Textio_Disassemble_Cb'Access, Exe);
         end if;
      end Disp_Entry;

      File : Trace_File_Type;
   begin
      Read_Trace_File (Trace_Filename, File, null, Disp_Entry'Access);
      Free (File);
   end Dump_Traces_With_Asm;

   type Graph_Node;
   type Graph_Node_Acc is access Graph_Node;

   type Graph_Node is record
      --  Corresponding statement.
      Stmt : Natural;
      Line : Addresses_Info_Acc;
      First, Last : Pc_Type;

      Branch : Branch_Kind;
      Flag_Indir, Flag_Cond : Boolean;
      Flag_Entry : Boolean;

      Prev : Graph_Node_Acc;
      Next_Ft, Next_Br : Graph_Node_Acc;
      Link_Ft, Link_Br : Graph_Node_Acc;
   end record;

   type Graph_Node_Vector is array (Natural range <>) of Graph_Node_Acc;
   type Graph_Node_Vec_Acc is access Graph_Node_Vector;

   package Node_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Pc_Type,
      Element_Type => Graph_Node_Acc,
      "<" => Interfaces."<");

   procedure Get_Entry (Nodes : in out Node_Maps.Map;
                        Pc : Pc_Type;
                        Res : out Graph_Node_Acc);

   procedure Get_Entry (Nodes : in out Node_Maps.Map;
                        Pc : Pc_Type;
                        Res : out Graph_Node_Acc)
   is
      use Node_Maps;
      Cur : Cursor;
   begin
      Cur := Nodes.Find (Pc);
      if Cur /= No_Element then
         Res := Element (Cur);
         Res.Flag_Entry := True;
      else
         Res := new Graph_Node'(Stmt => 0,
                                Line => null,
                                First => Pc,
                                Last => Pc,
                                Branch => Br_None,
                                Flag_Indir => False,
                                Flag_Cond => False,
                                Flag_Entry => False,
                                others => null);
         Nodes.Insert (Pc, Res);
      end if;
   end Get_Entry;

   procedure Generate_Graph (Exe : Exe_File_Type)
   is
      use Interfaces;

      It : Addresses_Iterator;
      Sym_It : Addresses_Iterator;
      Line_It : Addresses_Iterator;
      Sym : Addresses_Info_Acc;
      Sec : Addresses_Info_Acc;
      Line : Addresses_Info_Acc;
      Pc, Npc : Pc_Type;
      Machine : constant Unsigned_16 := Get_Machine (Exe);
      Nodes : Node_Maps.Map;

      procedure Print_Branch
        (Branch     : Branch_Kind;
         Flag_Cond  : Boolean;
         Flag_Indir : Boolean;
         Node       : Graph_Node_Acc);
      --  Display branching instruction information for the given
      --  control flow graph node.

      ------------------
      -- Print_Branch --
      ------------------

      procedure Print_Branch
        (Branch     : Branch_Kind;
         Flag_Cond  : Boolean;
         Flag_Indir : Boolean;
         Node       : Graph_Node_Acc)
      is
         procedure Print_Flag (Flag : Boolean; Name : String);
         --  Display information about a branch flag

         procedure Print_Dest
           (Dest_Char : Character;
            Dest      : Graph_Node_Acc);
         --  Display information about a branch destination

         ----------------
         -- Print_Flag --
         ----------------

         procedure Print_Flag (Flag : Boolean; Name : String) is
         begin
            if Flag then
               Put (" +" & Name);
            end if;
         end Print_Flag;

         ----------------
         -- Print_Dest --
         ----------------

         procedure Print_Dest
           (Dest_Char : Character;
            Dest      : Graph_Node_Acc)
         is
         begin
            if Dest = null then
               return;
            end if;

            Put (" " & Dest_Char & ":");
            Put (Hex_Images.Hex_Image (Dest.First));
            if Dest.Stmt = Node.Stmt then
               Put ("(in)");
            end if;
         end Print_Dest;

      --  Start of processing for Print_Branch

      begin
         case Branch is
            when Br_Jmp =>
               Put ("  jump");
            when Br_Call =>
               Put ("  call");
            when Br_Ret =>
               Put ("  ret");
            when Br_None =>
               return;
         end case;

         Print_Flag (Flag_Cond,  "conditional");
         Print_Flag (Flag_Indir, "indir");

         if Node /= null then
            Print_Dest ('v', Node.Next_Ft);

            if Node.Next_Br /= null then
               declare
                  Br_Char : Character;
               begin
                  if Node.First < Node.Next_Br.First then
                     Br_Char := '>';
                  else
                     Br_Char := '^';
                  end if;
                  Print_Dest (Br_Char, Node.Next_Br);
               end;
            end if;
         end if;
      end Print_Branch;

   --  Start of processing for Generate_Graph

   begin
      --  Load sections.
      Init_Iterator (Exe, Section_Addresses, It);
      loop
         Next_Iterator (It, Sec);
         exit when Sec = null;
         Load_Section_Content (Exe, Sec);
      end loop;

      Init_Iterator (Exe, Symbol_Addresses, Sym_It);
      Init_Iterator (Exe, Line_Addresses, Line_It);
      Next_Iterator (Line_It, Line);
      loop
         Next_Iterator (Sym_It, Sym);
         exit when Sym = null;

         New_Line;
         Disp_Address (Sym);

         Sec := Sym.Parent;
         pragma Assert (Sec.Section_Content /= null);

         Pc := Sym.First;
         Nodes.Clear;

         declare
            Branch : Branch_Kind;
            Flag_Indir, Flag_Cond : Boolean;
            Dest : Pc_Type;
            Verbose : constant Boolean := False;
            Node, Node_Ft, Node_Br : Graph_Node_Acc;
            Insn_Len : Pc_Type;
         begin
            Get_Entry (Nodes, Pc, Node_Br);
            while Pc < Sym.Last loop
               Insn_Len :=
                 Pc_Type (Disa_For_Machine (Machine).
                            Get_Insn_Length
                              (Sec.Section_Content
                                 (Pc .. Sec.Section_Content'Last)));

               declare
                  Insn : Binary_Content renames
                    Sec.Section_Content (Pc .. Pc + Insn_Len - 1);
               begin
                  Disa_For_Machine (Machine).Get_Insn_Properties
                    (Insn, Pc,
                     Branch, Flag_Indir, Flag_Cond,
                     Dest);

                  if Verbose then
                     Put (Hex_Images.Hex_Image (Pc));
                     Put (": ");
                     Put (Traces_Disa.Disassemble (Insn, Pc, Exe));
                     New_Line;

                     Print_Branch (Branch, Flag_Cond, Flag_Indir, null);
                     if Branch /= Br_None and then not Flag_Indir then
                        Put (' ');
                        Put (Hex_Images.Hex_Image (Dest));
                        if Dest in Sym.First .. Sym.Last then
                           Put (" (intra-routine)");
                        end if;
                     end if;
                     New_Line;
                  end if;
               end;

               if Branch /= Br_None then
                  if Flag_Indir then
                     Node_Br := null;
                  elsif Dest not in Sym.First .. Sym.Last then
                     Node_Br := null;
                  else
                     Get_Entry (Nodes, Dest, Node_Br);
                  end if;
                  if Flag_Cond then
                     Get_Entry (Nodes, Pc + Insn_Len, Node_Ft);
                  else
                     Node_Ft := null;
                  end if;

                  if not Nodes.Contains (Pc) then
                     Node := new Graph_Node'(Stmt => 0,
                                             Line => null,
                                             First => Pc,
                                             Last => Pc + Insn_Len - 1,
                                             Branch => Branch,
                                             Flag_Indir => Flag_Indir,
                                             Flag_Cond => Flag_Cond,
                                             Flag_Entry => False,
                                             others => null);
                     Nodes.Insert (Pc, Node);
                  else
                     Node := Nodes.Element (Pc);
                     if Node.Branch /= Br_None then
                        raise Program_Error;
                     end if;
                  end if;
                  Node.Branch := Branch;
                  Node.Flag_Indir := Flag_Indir;
                  Node.Flag_Cond := Flag_Cond;
                  Node.Next_Br := Node_Br;
                  Node.Next_Ft := Node_Ft;
               end if;
               Npc := Pc + Insn_Len;
               exit when Npc < Pc;
               Pc := Npc;
            end loop;
         end;

         --  Second pass: extend the graph blocks.
         if Nodes.Is_Empty then
            --  There should be at least the entry point.
            raise Program_Error;
         end if;

         --  Find the line for the PC.
         --  Inefficient, should have an index of lines sorted by PC range???

         Pc := Sym.First;
         while Line /= null and then Line.Last < Pc loop
            Next_Iterator (Line_It, Line);
         end loop;

         declare
            use Node_Maps;
            Cur : Cursor;
            C, N : Graph_Node_Acc;
            Stmt : Natural := 0;
         begin
            Cur := Nodes.First;
            C := Element (Cur);
            loop
               if False then
                  declare
                     use Hex_Images;
                  begin
                     Put (Hex_Image (C.First));
                     Put ('-');
                     Put (Hex_Image (C.Last));
                     Put (", key:");
                     Put (Hex_Image (Key (Cur)));
                     New_Line;
                  end;
               end if;

               --  Skip lines starting before the current block.
               while Line /= null and then Line.First < C.First loop
                  Next_Iterator (Line_It, Line);
               end loop;

               --  If the current block starts at a new line, increase the
               --  stmt counter.
               if Line /= null and then C.First = Line.First then
                  C.Line := Line;
                  Stmt := Stmt + 1;
                  C.Stmt := Stmt;
                  Next_Iterator (Line_It, Line);
               end if;

               Next (Cur);
               exit when Cur = No_Element;

               N := Element (Cur);
               if Line /= null and then Line.First < N.First - 1 then
                  C.Last := Line.First - 1;
                  if True then
                     Put_Line ("*add line at "
                                 & Hex_Images.Hex_Image (Line.First));
                  end if;
                  Stmt := Stmt + 1;
                  Nodes.Insert (Line.First,
                                new Graph_Node'(Stmt => Stmt,
                                                Line => Line,
                                                First => Line.First,
                                                Last => N.First - 1,
                                                Branch => Br_None,
                                                Flag_Indir => False,
                                                Flag_Cond => False,
                                                Flag_Entry => False,
                                                others => null));
               else
                  C.Last := N.First - 1;
               end if;
               N.Stmt := Stmt;

               C := N;
            end loop;
            C.Last := Sym.Last;
         end;

         --  Third pass: build the vector.
         declare
            use Node_Maps;
            use Hex_Images;
            use Traces_Disa;

            Cur : Cursor;
            Vec : Graph_Node_Vec_Acc;
            E : Graph_Node_Acc;
            Idx : Natural;
            Nbr_In_Jmp : Natural := 0;
            Prev_Stmt : Natural := 0;
         begin
            Vec := new Graph_Node_Vector (0 .. Integer (Nodes.Length) - 1);
            Idx := 0;
            Cur := Nodes.First;
            while Cur /= No_Element loop
               Vec (Idx) := Element (Cur);
               Idx := Idx + 1;
               Next (Cur);
            end loop;

            for I in Vec'Range loop
               E := Vec (I);

               New_Line;
               Put (Hex_Image (E.First));
               Put ('-');
               Put (Hex_Image (E.Last));
               Put (", stmt:");
               Put (Natural'Image (E.Stmt));

               if E.Stmt = Prev_Stmt then
                  Put (" (continued)");
               end if;
               Prev_Stmt := E.Stmt;

               if E.Line /= null then
                  Put (", ");
                  Disp_Address (E.Line);
                  --  Put (", line:");
                  --  Put (Natural'Image (E.Line.Line_Number));
               end if;
               New_Line;

               For_Each_Insn (Sec.Section_Content (E.First .. E.Last),
                              Covered,
                              Textio_Disassemble_Cb'Access,
                              Exe);

               Print_Branch
                 (Branch     => E.Branch,
                  Flag_Cond  => E.Flag_Cond,
                  Flag_Indir => E.Flag_Indir,
                  Node       => E);
               New_Line;

               if E.Branch /= Br_None and E.Flag_Cond then
                  if False
                    and then E.Next_Ft /= null
                    and then E.Next_Ft.Stmt = E.Stmt
                    and then E.Next_Ft.First > E.First
                  then
                     Nbr_In_Jmp := Nbr_In_Jmp + 1;
                  end if;
                  if E.Next_Br /= null
                    and then E.Next_Br.Stmt = E.Stmt
                    and then E.Next_Br.First > E.First
                  then
                     Nbr_In_Jmp := Nbr_In_Jmp + 1;
                  end if;
               end if;
            end loop;

            if Nbr_In_Jmp /= 0 then
               Put_Line (Natural'Image (Nbr_In_Jmp) & " intra-jumps for "
                           & Sym.Symbol_Name.all);
            end if;
         end;

      end loop;
   end Generate_Graph;

end Traces_History;
