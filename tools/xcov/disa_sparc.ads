------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                     Copyright (C) 2008-2009, AdaCore                     --
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

--  SPARC disassembler

with System;

with Disa_Symbolize; use Disa_Symbolize;
with Disassemblers;  use Disassemblers;
with Traces;         use Traces;

package Disa_Sparc is

   type SPARC_Disassembler is new Disassembler with private;

   overriding function Get_Insn_Length
     (Self : SPARC_Disassembler;
      Addr : System.Address) return Positive;
   --  Return the length of the instruction at Addr

   overriding procedure Disassemble_Insn
     (Self     : SPARC_Disassembler;
      Addr     : System.Address;
      Pc       : Pc_Type;
      Line     : out String;
      Line_Pos : out Natural;
      Insn_Len : out Natural;
      Sym      : Symbolizer'Class);
   --  Disassemble instruction at ADDR, and put the result in LINE/LINE_POS.
   --  LINE_POS is the index of the next character to be written (ie line
   --  length if Line'First = 1).

private
   type SPARC_Disassembler is new Disassembler with null record;
end Disa_Sparc;
