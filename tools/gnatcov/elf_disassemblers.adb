------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                    Copyright (C) 2006 Tristan Gingold                    --
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

with Disa_Ppc;
with Disa_Sparc;
with Disa_X86;

package body Elf_Disassemblers is

   Disa_For_Ppc   : aliased Disa_Ppc.PPC_Disassembler;
   Disa_For_Sparc : aliased Disa_Sparc.SPARC_Disassembler;
   Disa_For_X86   : aliased Disa_X86.X86_Disassembler;

   ----------------------
   -- Disa_For_Machine --
   ----------------------

   function Disa_For_Machine
     (Machine : Elf_Half) return access Disassembler'Class
   is
   begin
      case Machine is
         when EM_PPC =>
            return Disa_For_Ppc'Access;
         when EM_SPARC =>
            return Disa_For_Sparc'Access;
         when EM_386 =>
            return Disa_For_X86'Access;
         when others =>
            return null;
      end case;
   end Disa_For_Machine;

end Elf_Disassemblers;
