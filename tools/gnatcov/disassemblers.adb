------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with Elf_Arch;
with Interfaces; use Interfaces;
with Hex_Images; use Hex_Images;
with Version;

package body Disassemblers is

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Dest) return Boolean is
   begin
      if Left.Target < Right.Target then
         return True;
      elsif Left.Target > Right.Target then
         return False;
      else
         return Left.Delay_Slot < Right.Delay_Slot;
      end if;
   end "<";

   function Dump_Bin (Bin : Binary_Content; Size : Positive) return String;
   --  Return a hexadecimal dump for the first Size bytes of the given binary
   --  content. A shorter dump is returned if there aren't enough bytes.

   --------------
   -- Dump_Bin --
   --------------

   function Dump_Bin (Bin : Binary_Content; Size : Positive) return String
   is
      Dump : Unbounded_String;
      I    : Elf_Arch.Elf_Addr := Bin'First;
   begin
      while I <= Bin'Last and then Natural (I - Bin'First) < Size loop
         if I > Bin'First then
            Append (Dump, " ");
         end if;
         Append (Dump, Hex_Image (Bin (I)));
         I := I + 1;
      end loop;
      if Length (Dump) = 0 then
         Append (Dump, "<empty>");
      end if;
      return To_String (Dump);
   end Dump_Bin;

   ------------------------------
   -- Abort_Disassembler_Error --
   ------------------------------

   procedure Abort_Disassembler_Error
     (PC       : Pc_Type;
      Insn_Bin : Binary_Content;
      Exn      : Exception_Occurrence)
   is
   begin
      New_Line (Standard_Error);
      Put_Line (Standard_Error, "========================================");
      Put_Line
        (Standard_Error,
         "An error occurred while disassembling the instruction at "
         & Hex_Image (PC) & ":");
      Put (Standard_Error,
           Exception_Information (Exn));
      Put_Line
        (Standard_Error,
         "The involved bytes are: " & Dump_Bin (Insn_Bin, 20));
      Put_Line
        (Standard_Error,
         "This is GNATcoverage " & Version.Xcov_Version);
      raise Program_Error with "Aborting";
   end Abort_Disassembler_Error;

end Disassemblers;
