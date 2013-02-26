------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2013, AdaCore                     --
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

with Interfaces; use Interfaces;
with Swaps;      use Swaps;

package Qemu_Traces_Entries is

   --  Trace entry for 32-bit programs. See qemu_traces.ads for details about
   --  qemu traces.

   type Trace_Entry is record
      Pc   : Unsigned_32;
      Size : Unsigned_16;
      Op   : Unsigned_8;

      --  Padding is here only to make the size of a Trace_Entry a multiple of
      --  4 bytes, for efficiency purposes.
      Pad0 : Unsigned_8 := 0;
   end record;

   procedure Swap_Pc (V : in out Unsigned_32) renames Swaps.Swap_32;

end Qemu_Traces_Entries;
