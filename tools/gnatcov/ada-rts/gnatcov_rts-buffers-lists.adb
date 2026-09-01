------------------------------------------------------------------------------
--                                                                          --
--                   GNATcoverage Instrumentation Runtime                   --
--                                                                          --
--                        Copyright (C) 2024, AdaCore                       --
--                                                                          --
-- GNATcoverage is free software; you can redistribute it and/or modify it  --
-- under terms of the GNU General Public License as published by the  Free  --
-- Software  Foundation;  either version 3,  or (at your option) any later  --
-- version. This software is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with System; use System;

package body GNATcov_RTS.Buffers.Lists is

   procedure Reset_Buffer
     (Buffer_Address : System.Address; Last_Bit_Id : Any_Bit_Id);
   --  Reset a coverage buffer

   procedure Reset_Buffers (C_Buffers : Coverage_Buffers_Access);
   --  Reset statement, decision, and mcdc coverage buffers

   procedure Reset_Buffers_Group
     (C_Buffers_Group : Coverage_Buffers_Group_Access);
   --  Reset a buffers group, which consists of all the buffers for a unit,
   --  i.e. for its specification, its body, and its separates.

   ------------------
   -- Reset_Buffer --
   ------------------

   procedure Reset_Buffer
     (Buffer_Address : System.Address; Last_Bit_Id : Any_Bit_Id)
   is
      Buffer : Coverage_Buffer_Type (1 .. Last_Bit_Id);
      for Buffer'Address use Buffer_Address;
   begin
      for I in Buffer'Range loop
         Buffer (I) := False;
      end loop;
   end Reset_Buffer;

   -------------------
   -- Reset_Buffers --
   -------------------

   procedure Reset_Buffers (C_Buffers : Coverage_Buffers_Access) is
   begin
      if C_Buffers = null then
         return;
      end if;
      if C_Buffers.Statement /= System.Null_Address then
         Reset_Buffer (C_Buffers.Statement, C_Buffers.Statement_Last_Bit);
      end if;
      if C_Buffers.Decision /= System.Null_Address then
         Reset_Buffer (C_Buffers.Decision, C_Buffers.Decision_Last_Bit);
      end if;
      if C_Buffers.MCDC /= System.Null_Address then
         Reset_Buffer (C_Buffers.Statement, C_Buffers.MCDC_Last_Bit);
      end if;
   end Reset_Buffers;

   -------------------------
   -- Reset_Buffers_Group --
   -------------------------

   procedure Reset_Buffers_Group
     (C_Buffers_Group : Coverage_Buffers_Group_Access) is
   begin
      if C_Buffers_Group = null then
         return;
      end if;
      declare
         Buffers_Group :
           Coverage_Buffers_Group (1 .. Integer (C_Buffers_Group.Length));
         pragma Import (C, Buffers_Group);
         for Buffers_Group'Address use C_Buffers_Group.Buffers;
      begin
         for I in Buffers_Group'Range loop
            Reset_Buffers (Buffers_Group (I));
         end loop;
      end;
   end Reset_Buffers_Group;

   -------------------------------
   -- Reset_Group_Array_Buffers --
   -------------------------------

   procedure Reset_Group_Array_Buffers
     (Arr : GNATcov_RTS_Coverage_Buffers_Group_Array)
   is
      Buffers_Groups :
        Coverage_Buffers_Group_Array (1 .. Integer (Arr.Length));
      pragma Import (C, Buffers_Groups);
      for Buffers_Groups'Address use Arr.Groups;
   begin
      for I in Buffers_Groups'Range loop
         Reset_Buffers_Group (Buffers_Groups (I));
      end loop;
   end Reset_Group_Array_Buffers;

end GNATcov_RTS.Buffers.Lists;
