------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                      Copyright (C) 2008-2010, AdaCore                    --
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

package Sparc_Descs is
   --  Fields of Sparc instructions.
   --  Extracted from V8.pdf p44.
   type Sparc_Fields is
     (
      --  The format
      F_Op,

      --  Format 1 (op = 1)
      F_Disp30,

      --  Format 2 (op = 0)
      F_Rd,
      F_Op2,
      F_Imm22,
      F_A,
      F_Cond,
      F_Disp22,

      --  Format 3 (op = 2 or 3)
      F_Op3,
      F_Rs1,
      F_I,
      F_Asi,
      F_Rs2,
      F_Simm13,
      F_Opf,

      F_Others
     );

   --  Generate a bit mask from a field.
   --  function Get_Mask (Field : Sparc_Fields) return Unsigned_32;

   --  Extract field FIELD from V.
   --  function Get_Field (Field : Sparc_Fields; V : Unsigned_32)
   --  return Unsigned_32;

   --  A bit number.  Sparc convention (MSB) is assumed.
   subtype Bit_Number is Natural range 0 .. 31;

   --  An opcode field is described by the range of bit it spreads over.
   type Field_Type is record
      First, Last : Bit_Number;
   end record;

   type Fields_Mask_Type is array (Sparc_Fields) of Field_Type;

   --  PPC fields description.
   Fields_Mask : constant Fields_Mask_Type :=
     (
      F_Op => (31, 30),

      F_Disp30 => (29, 0),

      F_Rd => (29, 25),
      F_Op2 => (24, 22),
      F_Imm22 => (21, 0),
      F_A => (29, 29),
      F_Cond => (28, 25),
      F_Disp22 => (21, 0),

      F_Op3 => (24, 19),
      F_Rs1 => (18, 14),
      F_I => (13, 13),
      F_Asi => (12, 5),
      F_Rs2 => (4, 0),
      F_Simm13 => (12, 0),
      F_Opf => (12, 5),

      F_Others => (0, 31)
     );

end Sparc_Descs;
