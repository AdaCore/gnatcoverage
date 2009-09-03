------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                       Copyright (C) 2009, AdaCore                        --
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

with Disa_Symbolize; use Disa_Symbolize;
with Traces; use Traces;

package Traces_Sources.Annotations is

   Flag_Show_Missing : Boolean := False;
   --  If True, Disp_Line_State displays info for files that are not found
   --  Why isn't this a parameter of Disp_Line_State???

   procedure Disp_File_Summary;
   --  Display per-file summary

private

   type Pretty_Printer is abstract tagged limited record
      Need_Sources : Boolean;
      Show_Asm     : Boolean;
   end record;

   procedure Pretty_Print_Start
     (Pp : in out Pretty_Printer) is null;
   --  Called once at the beginning of the process

   procedure Pretty_Print_Finish
     (Pp : in out Pretty_Printer) is null;
   --  Called once at the end of the process

   procedure Pretty_Print_File
     (Pp              : in out Pretty_Printer;
      Source_Filename : String;
      Stats           : Stat_Array;
      Has_Source      : Boolean;
      Skip            : out Boolean) is abstract;
   --  Called at the beginning of a source file display

   --  Subprograms below need comments???

   procedure Pretty_Print_Line
     (Pp       : in out Pretty_Printer;
      Line_Num : Natural;
      State    : Line_State;
      Line     : String) is abstract;

   procedure Pretty_Print_Label
     (Pp    : in out Pretty_Printer;
      Label : String) is null;

   procedure Pretty_Print_Insn
     (Pp    : in out Pretty_Printer;
      Pc    : Pc_Type;
      State : Insn_State;
      Insn  : Binary_Content;
      Sym   : Symbolizer'Class) is null;

   procedure Pretty_Print_End_File (Pp : in out Pretty_Printer) is abstract;

   procedure Disp_Line_State
     (Pp       : in out Pretty_Printer'Class;
      Show_Asm : Boolean);

end Traces_Sources.Annotations;
