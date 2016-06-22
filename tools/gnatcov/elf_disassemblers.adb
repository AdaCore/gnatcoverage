------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2006-2013, AdaCore                     --
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

with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;

with Disa_ARM;
with Disa_Lmp;
with Disa_Ppc;
with Disa_Sparc;
with Disa_Thumb;
with Disa_X86;

package body Elf_Disassemblers is

   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Insn_Set_Ranges, Insn_Set_Ranges_Acc);

   use type Ada.Containers.Count_Type;

   Disa_For_ARM    : aliased Disa_ARM.ARM_Disassembler;
   Disa_For_E500   : aliased Disa_Ppc.E500_Disassembler;
   Disa_For_Ppc    : aliased Disa_Ppc.PPC_Disassembler;
   Disa_For_Sparc  : aliased Disa_Sparc.SPARC_Disassembler;
   Disa_For_Thumb  : aliased Disa_Thumb.Thumb_Disassembler;
   Disa_For_Visium : aliased Disa_Lmp.LMP_Disassembler;
   Disa_For_X86    : aliased Disa_X86.X86_Disassembler;

   ----------------------
   -- Disa_For_Machine --
   ----------------------

   function Disa_For_Machine
     (Machine  : Machine_Type;
      Insn_Set : Insn_Set_Type) return access Disassembler'Class
   is
   begin
      case Machine is
         when Unknown =>
            return null;
         when ARM =>
            case Insn_Set is
               when Default =>
                  raise Program_Error with
                    "Could not determine instruction set (ARM or Thumb)";
               when Data =>
                  raise Program_Error with
                    "Cannot disassemble data (ARM)";
               when ARM =>
                  return Disa_For_ARM'Access;
               when Thumb =>
                  return Disa_For_Thumb'Access;
            end case;
         when E500 =>
            return Disa_For_E500'Access;
         when PPC =>
            return Disa_For_Ppc'Access;
         when SPARC =>
            return Disa_For_Sparc'Access;
         when Visium =>
            return Disa_For_Visium'Access;
         when X86 | X86_64 =>
            return Disa_For_X86'Access;
      end case;
   end Disa_For_Machine;

   ----------
   -- Free --
   ----------

   procedure Free (I_Range : in out Insn_Set_Ranges_Acc) is
   begin
      Deallocate (I_Range);
   end Free;

   ---------------
   -- Add_Range --
   ---------------

   procedure Add_Range
     (Ranges      : in out Insn_Set_Ranges;
      First, Last : Pc_Type;
      Insn_Set    : Insn_Set_Type)
   is
   begin
      Ranges.Insert ((First, Last, Insn_Set));
   end Add_Range;

   ------------------
   -- Get_Insn_Set --
   ------------------

   function Get_Insn_Set
     (Ranges : Insn_Set_Ranges;
      Cache  : in out Insn_Set_Cache;
      PC     : Pc_Type) return Insn_Set_Type
   is
      use Ranges_Sets;

      Cur : Cursor renames Cache.Cur;
   begin
      --  If there is no specific instruction set information, as it is always
      --  the case on most architecture, loose no time in lookups.

      if Ranges.Length = 0 then
         return Default;
      end if;

      --  If we have a cache, try to use it

      if Cache /= Empty_Cache then
         declare
            I_Range : constant Insn_Set_Range := Element (Cur);
         begin
            if PC in I_Range.First .. I_Range.Last then
               return I_Range.Insn_Set;

            elsif PC > I_Range.Last then

               --  It looks like are moving forward in adresses: try to use the
               --  cache to avoid lookups.

               declare
                  Next_Cur     : constant Cursor := Next (Cur);
                  Next_I_Range : Insn_Set_Range;
               begin
                  if Next_Cur = No_Element then

                     --  There is no association anymore for higher PC: next
                     --  PCs are always default.

                     Cur := Next_Cur;
                     return Default;
                  end if;

                  Next_I_Range := Element (Next_Cur);
                  if PC < Next_I_Range.First then

                     --  This is the case in which PC is between two ranges,
                     --  but none covers it. This is not supposed to happen
                     --  in practice, but try to be correct anyway.

                     Cur := No_Element;
                     return Default;

                  elsif PC in Next_I_Range.First .. Next_I_Range.Last then

                     --  This is the case for which we hope this cache is the
                     --  more useful: the PC we were looking for is in the next
                     --  range. Just update the cache for the next lookup and
                     --  we are done!

                     Cur := Next_Cur;
                     return Next_I_Range.Insn_Set;
                  end if;

                  --  Execution reaches this point when we failed to get a
                  --  result just using the cache: it's time to perform a full
                  --  search...
               end;
            end if;

            --  If we reach this point, we know that PC < I_Range.First: the
            --  cache is not useful here.
         end;
      end if;

      Cur := Ranges.Floor ((PC, PC, Default));
      return (if Cur = No_Element
              then Default
              else Element (Cur).Insn_Set);
   end Get_Insn_Set;

   ---------------------
   -- Go_To_Next_Insn --
   ---------------------

   function Go_To_Next_Insn
     (Ranges   : Insn_Set_Ranges;
      Cache    : in out Insn_Set_Cache;
      PC       : in out Pc_Type;
      Insn_Set : out Insn_Set_Type) return Boolean
   is
      use Ranges_Sets;

      pragma Unreferenced (Ranges);

      pragma Assert (Cache.Cur /= No_Element);
      I_Range : constant Insn_Set_Range := Element (Cache.Cur);
      pragma Assert
        (PC in I_Range.First .. I_Range.Last
         and then I_Range.Insn_Set = Data);

   begin
      Cache.Cur := Next (Cache.Cur);
      if Cache.Cur /= No_Element then
         declare
            I_Range : constant Insn_Set_Range :=
              Element (Cache.Cur);
         begin
            PC := I_Range.First;
            Insn_Set := I_Range.Insn_Set;
            return True;
         end;
      end if;
      return False;
   end Go_To_Next_Insn;

   ------------------------
   -- Iterate_Over_Insns --
   ------------------------

   function Iterate_Over_Insns
     (Ranges   : Insn_Set_Ranges;
      Cache    : in out Insn_Set_Cache;
      Last_PC  : Pc_Type;
      PC       : in out Pc_Type;
      Insn_Set : out Insn_Set_Type) return Boolean
   is
      Insn_Set_Tmp : Insn_Set_Type := Get_Insn_Set (Ranges, Cache, PC);
      No_Code_Left : constant Boolean :=
        (Insn_Set_Tmp = Data
         and then not Go_To_Next_Insn (Ranges, Cache, PC, Insn_Set_Tmp));
   begin
      Insn_Set := Insn_Set_Tmp;
      return not No_Code_Left and then PC <= Last_PC;
   end Iterate_Over_Insns;

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Insn_Set_Range) return Boolean is
   begin
      --  Either L and R are the same, either they are not overlapping: assert
      --  this.
      --
      --  Empty ranges (First = Last) are used when looking for the range
      --  corrersponding to a single PC, so handle them as well. Make sure
      --  empty ranges are ordered right after ranges with the same First
      --  so that Ordered_Sets.Floor return the range.

      if L.First < R.First then
         pragma Assert (R.First = R.Last or else L.Last <= R.First);
         return True;

      elsif R.First < L.First then
         pragma Assert (L.First = L.First or else R.Last <= L.First);
         return False;

      --  Starting from here, we know that L.First = R.First: check for empty
      --  ranges first.

      elsif L.First = L.Last then
         return False;

      elsif R.First = R.Last then
         return True;

      --  Now, we have two non-empty ranges that have the same First: they
      --  *must* be equal.

      else
         pragma Assert (L = R);
         return False;
      end if;
   end "<";

end Elf_Disassemblers;
