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
with Ada.Containers.Ordered_Sets;
with Interfaces; use Interfaces;
with System;

package Traces is
   --  Define the size of the PC.
   subtype Pc_Type is Unsigned_32;
   Pc_Type_Size : constant Unsigned_8 := Pc_Type'Size / System.Storage_Unit;

   --  Target machine.
   --  Set to 0 when unknown.
   Machine : Unsigned_16 := 0;

   --  High level state of a trace entry.
   type Trace_State is
     (
      --  Not yet filled.
      Unknown,

      Not_Covered,

      --  The code is fully covered (and there is no conditionnal branches).
      Covered,

      --  The code is covered, the last instruction is a branch but the
      --  branch was always taken.
      Branch_Taken,

      --  The code is covered, the last instruction is a branch but the
      --  branch was never taken.
      Fallthrough_Taken,

      --  The code is covered, the last instruction is a branch and the
      --  branch was both taken and not taken.
      Both_Taken
      );

   --  Trace entry as put in the traces tree.
   type Trace_Entry is record
      First : Pc_Type;
      Last : Pc_Type;
      Op : Unsigned_8;
      State : Trace_State;
   end record;

   --  An invalid trace.
   Bad_Trace : constant Trace_Entry := (First => 1,
                                        Last => 0,
                                        Op => 0,
                                        State => Unknown);

   --  This exception is raised if the trace file is invalid or corrupted.
   Bad_File_Format : exception;

   --  This exception is raise in case of OS error during write.
   Write_Error : exception;

   --  Load in memory (and possibly merge) a trace file.
   procedure Read_Trace_File (Filename : String);

   --  Write traces to a file.
   --  Always generate a consolidated file.
   procedure Write_Trace_File (Filename : String);

   --  Dump (on standard output) a trace entry.
   procedure Dump_Entry (E : Trace_Entry);

   --  Dump (on standard output) the content of traces.
   procedure Dump_Traces;

   --  Add coverage annotations to the objdump disassembly output.
   --  Read objdump output from standard input.
   procedure Annotate_Objdump;

   --  Display a character representing the state.
   procedure Disp_State_Char (State : Trace_State);

   procedure Set_Color (State : Trace_State);

   --  Return a trace that contains or follows ADDR.
   type Entry_Iterator is limited private;
   procedure Init (Iterator : out Entry_Iterator; Pc : Pc_Type);
   procedure Get_Next_Trace (Trace : out Trace_Entry;
                             Iterator : in out Entry_Iterator);

   procedure Update_State (Iterator : Entry_Iterator; State : Trace_State);
   procedure Split_Trace (Iterator : in out Entry_Iterator;
                          Pc : Pc_Type;
                          Cur_State, Next_State : Trace_State);

   --  Convert hexa-decimal string contained in Line (Pos ..) to a Pc_Type.
   --  Put the result to RES, POS contains the index past the last character
   --  accepted.
   procedure Get_Pc (Res : out Pc_Type; Line : String; Pos : in out Natural);

   type Trace_State_Map is array (Trace_State) of Character;
   Trace_State_Char : constant Trace_State_Map;

private
   Trace_State_Char : constant Trace_State_Map :=
     (Unknown => '?',
      Not_Covered => '-',
      Covered => '+',
      Branch_Taken => '>',
      Fallthrough_Taken => 'v',
      Both_Taken => '*');

   --  Operations for ordered_sets.
   function "=" (L, R : Trace_Entry) return Boolean;
   function "<" (L, R : Trace_Entry) return Boolean;

   package Entry_Set is new Ada.Containers.Ordered_Sets
     (Element_Type => Trace_Entry,
      "<" => "<",
      "=" => "=");

   type Entry_Iterator is record
      Cur : Entry_Set.Cursor;
   end record;
end Traces;
