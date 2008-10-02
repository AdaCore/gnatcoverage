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
with Interfaces; use Interfaces;

package Ppc_Descs is

   --  Fields of PPC instructions.
   --  This is mostly the name documented in the PEM with a few additions.
   type Ppc_Fields is
     (
      F_AA, F_LK, F_Rc, F_OE, F_U, F_Br_Hint, F_L,

      F_A, F_B, F_C, F_D, F_S,
      F_FA, F_FB, F_FC, F_FD, F_FS,
      F_SIMM, F_UIMM,
      F_CrfD, F_CrfS, F_CrbD, F_CrbA, F_CrbB,
      F_BO, F_BI, F_BD,
      F_LI, F_SH, F_MB, F_ME, F_CRM, F_Sr, F_Tbr, F_Spr, F_NB,
      F_Disp, F_Imm, F_Fm, F_TO,

      F_OPC,
      F_XO,
      F_Eof);

   --  These fields add characters to the mnemonic.
   subtype Ppc_Mnemo_Fields is Ppc_Fields range F_AA .. F_L;

   --  Non fake fields.
   subtype Ppc_Valid_Fields is Ppc_Fields range Ppc_Fields'First .. F_XO;

   --  Generate a bit mask from a field.
   --  Use the PowerPC Big-endian convention, ie bit 0 is 2**31.
   function Get_Mask (Field : Ppc_Fields) return Unsigned_32;

   --  A bit number.  PPC convention (LSB) is assumed.
   subtype Bit_Number is Natural range 0 .. 31;

   --  An opcode field is described by the range of bit it spreads over.
   type Field_Type is record
      First, Last : Bit_Number;
   end record;

   type Fields_Mask_Type is array (Ppc_Valid_Fields) of Field_Type;

   --  PPC fields description.
   Fields_Mask : constant Fields_Mask_Type :=
     (F_TO | F_D | F_FD | F_BO | F_S | F_FS | F_CrbD => (6, 10),
      F_A | F_FA | F_BI | F_CrbA => (11, 15),
      F_B | F_FB | F_Crbb | F_Sh | F_Nb => (16, 20),
      F_C | F_FC | F_MB => (21, 25),
      F_ME => (26, 30),
      F_CrfD => (6, 8),
      F_Crfs => (11, 13),
      F_L => (10, 10),
      F_Bd => (16, 29),
      F_Aa => (30, 30),
      F_Lk => (31, 31),
      F_Fm => (7, 14),
      F_Imm => (16, 19),
      F_Crm => (12, 19),
      F_Li => (6, 29),
      F_Rc => (31, 31),
      F_Spr | F_Tbr => (11, 20),
      F_OE => (21, 21),
      F_Sr => (12, 15),
      F_Br_Hint => (10, 10),
      F_U => (5, 5),
      F_SIMM | F_UIMM | F_Disp => (16, 31),
      F_OPC => (0, 5),
      F_XO => (21, 30));

end Ppc_Descs;
