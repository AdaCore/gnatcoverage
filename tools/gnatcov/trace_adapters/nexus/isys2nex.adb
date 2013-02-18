------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2013, AdaCore                     --
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

with Ada.Sequential_IO;
with Interfaces; use Interfaces;

package body Isys2nex is

   --  The iSystem OCD data comprises a stream of bytes describing
   --  sequences of "items" in their terminology. An initial byte,
   --  provides a count of the number of items to follow. For each
   --  item, one byte gives the number of bits in the item, and subsequent
   --  bytes contain the unsigned integer data starting with the least
   --  significant bits and ending when enough bytes have been included
   --  to contain the number of bits. A new sequence of bytes starts
   --  with a count of items, or the end of the file is seen.
   --
   --  In the case of Nexus data, a sequence of items is a Nexus message,
   --  and each item is a packet in the Nexus message.  The fisrt packet
   --  contains the Message TCODE.

   type    Item_Unit_T is new Unsigned_8;
   package Item_IO is new Ada.Sequential_IO (Item_Unit_T);
   use Item_IO;
   OCD_File : File_Type;
   --  We define a type for the unit of data read from and OCD file,
   --  and create and instance of Sequential_IO with that type.

   procedure Fill_Packet
     (Min_Bits : Natural;
      Max_Bits : Natural;
      Packet   : out Nexus_Packet_T;
      Bits     : out Item_Unit_T);
   function Continue_Fill (Bits : Item_Unit_T) return Nexus_Packet_T;
   --  These 2 routines are used to fill in a component of a Nexus
   --  message -- a packet -- with an item read from the OCD file.
   --  'Fill_Packet' takes the minimum and maximum number of bits expected
   --  for the packet, fills in 'Packet' with the value, and 'Bits' with
   --  the actual number of bits stated in the item bit-size byte of the
   --  the file. This routine is called when the File_Ptr is positioned
   --  to read a bit-size value next. If the bit-size is out of the range
   --  'Min_Bits' .. 'Max_Bits', OCD_File will be closed, and Isys2Nex_Error
   --  exception will be raised. If the bit-size is too large to fit in
   --  'Packet' (but is valid), Packet will be filled with the first 32 bits,
   --  and the calling routine can use Continue_Fill to fill subsequent
   --  packets with the remaining bits.
   --
   --  Continue_Fill is called called by Fill_Packet after the bit-size
   --  byte is read and checked, and is also called directly by
   --  Ocdfile_To_Nexus_List in the case where an item is too long
   --  to fit in a single packet.

   --  Note: the minimum and maximum nuber of bits for a packet is
   --  hard coded into the calls below, with values taken from the
   --  NZ63C Nexus module used on the Freescale 5554 microcontroller.
   --  A better way to list these values should be found. Also, the
   --  values are not checked for now, since iSytem's representation
   --  sometimes uses more bits than needed, though the values seem
   --  to be in the range allowed for by the NZ63C document (i.e.,
   --  iSytems data appears to sometimes add some extra leading 0s).
   --  The iSystem behavior needs to be better understood in order
   --  to see if Min_Bits/Max_Bits can actually be used. ???

   function Ocdfile_To_Nexus_List (OCD_Filename : String)
                                   return Nexus_Message_List_T is

      Nex_List      : Nexus_Message_List_T;
      Num_Items     : Item_Unit_T;
      Actual_N_Bits : Item_Unit_T;

      Packet    : Nexus_Packet_T;
      TCODE     : Nexus_Tcode_T;
      Nexus_Msg : Nexus_Message_Rec_Ptr_T;

   begin

      Open (OCD_File, In_File, OCD_Filename);
      loop
         if End_Of_File (OCD_File) then
            Close (OCD_File);
            return Nex_List;
         end if;
         --  Normal completion is any time the end-of-file is reached where the
         --  iSytem <Num Items>  element would be next.

         Read (OCD_File, Num_Items);
         if Num_Items = 0 then
            Close (OCD_File);
            raise Isys2nex_Error with "Error in OCD file: Number Items = 0";
         end if;

         Fill_Packet (6, 6, Packet, Actual_N_Bits);
         TCODE := Nexus_Tcode_T (Packet);

         if Natural (Num_Items) /= Nexus_N_Packets_Array (TCODE) then
            Close (OCD_File);
            raise Isys2nex_Error with "Number of Items wrong for TCODE";
         end if;

         Nexus_Msg := new Nexus_Message_Rec_T (TCODE);
         Fill_Packet (4, 4, Nexus_Msg.SRC, Actual_N_Bits);
         case TCODE is

            --  Only message types expected for Program Trace output are
            --  handled below. A Data Trace message is an error, e.g.

            when Debug_Status =>
               Fill_Packet
                 (8, 8, Nexus_Msg.Debug_Status_V.STATUS, Actual_N_Bits);

            when Ownership_Trace_Message =>
               raise Isys2nex_Error with "Unhandled TCODE: Ownership Trace";

            when Prog_Trace_Direct_Branch_Message =>
               Fill_Packet
                 (1, 8, Nexus_Msg.Prog_Trace_Direct_Branch_Message_V.I_CNT,
                  Actual_N_Bits);

            when Prog_Trace_Indirect_Branch_Message =>
               Fill_Packet
                 (1, 8, Nexus_Msg.Prog_Trace_Indirect_Branch_Message_V.I_CNT,
                  Actual_N_Bits);
               Fill_Packet
                 (1, 32, Nexus_Msg.Prog_Trace_Indirect_Branch_Message_V.U_ADDR,
                  Actual_N_Bits);

            when Data_Trace_Data_Write_Message =>
               raise Isys2nex_Error with "Unhandled TCODE: Data Trace Write";
            when Data_Trace_Data_Read_Message =>
               raise Isys2nex_Error with "Unhandled TCODE: Data Trace Read";

            when Error_Message =>
               Fill_Packet (8, 8, Nexus_Msg.Error_Message_V.ECODE,
                            Actual_N_Bits);

            when Prog_Trace_Direct_Branch_Message_Sync =>
               Fill_Packet
                 (1, 8,
                  Nexus_Msg.Prog_Trace_Direct_Branch_Message_Sync_V.I_CNT,
                  Actual_N_Bits);
               Fill_Packet
                 (1, 32,
                  Nexus_Msg.Prog_Trace_Direct_Branch_Message_Sync_V.F_ADDR,
                  Actual_N_Bits);

            when Prog_Trace_Indirect_Branch_Message_Sync =>
               Fill_Packet
                 (1, 8,
                  Nexus_Msg.Prog_Trace_Indirect_Branch_Message_Sync_V.I_CNT,
                  Actual_N_Bits);
               Fill_Packet
                 (1, 32,
                  Nexus_Msg.Prog_Trace_Indirect_Branch_Message_Sync_V.F_ADDR,
                  Actual_N_Bits);

            when Data_Trace_Data_Write_Message_Sync =>
               raise Isys2nex_Error with "Unhandled TCODE: Data Write - Sync";
            when Data_Trace_Data_Read_Message_Sync =>
               raise Isys2nex_Error with "Unhandled TCODE: Data Read - Sync";

            when Watchpoint_Message =>
               Fill_Packet
                 (4, 4, Nexus_Msg.Watchpoint_Message_V.WPHIT, Actual_N_Bits);

            when Resource_Full_Message =>
               raise Isys2nex_Error with "Unhandled TCODE: Resource Full";
            when Prog_Trace_Indirect_Branch_Hist_Message =>
               raise Isys2nex_Error with
                 "Unhandled TCODE: Indirect Branch Hist";
            when Prog_Trace_Indirect_Branch_Hist_Message_Sync =>
               raise Isys2nex_Error with
                 "Unhandled TCODE: Indirect Branch Hist - Sync";
            when Prog_Trace_Program_Correlation_Message =>
               Fill_Packet
                 (1, 4,
                  Nexus_Msg.Prog_Trace_Program_Correlation_Message_V.EVCODE,
                  Actual_N_Bits);
               Fill_Packet
                 (1, 8,
                  Nexus_Msg.Prog_Trace_Program_Correlation_Message_V.I_CNT,
                  Actual_N_Bits);
               Fill_Packet
                 (1, 32,
                  Nexus_Msg.Prog_Trace_Program_Correlation_Message_V.HIST,
                  Actual_N_Bits);

            when  1 | 7 | 9 | 10 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23
               |  24 | 25 | 26 | 30 | 31 | 32 =>
               raise Isys2nex_Error with "Ivalid TCODE";
         end case;
         Nexus_Append_To_List (Nex_List, Nexus_Msg);
      end loop;
   exception
      when Name_Error =>
         raise Isys2nex_Error with "Error opening file: " & OCD_Filename;
      when End_Error =>
         Close (OCD_File);
         raise Isys2nex_Error with "Premature end-of-file in" & OCD_Filename;
   end Ocdfile_To_Nexus_List;

   procedure Fill_Packet
     (Min_Bits : Natural;
      Max_Bits : Natural;
      Packet   : out Nexus_Packet_T;
      Bits     : out Item_Unit_T) is

      pragma Unreferenced (Min_Bits);  --  see below
      N_Bits       : Item_Unit_T;
      Bits_To_Fill : Item_Unit_T;
   begin
      Read (OCD_File, N_Bits);
      --  if Natural (N_Bits) < Min_Bits or else Natural (N_Bits) > Max_Bits
      --     Close (OCD_File);
      --     raise Isys2nex_Error with "Bits for packet out of range";
      --  end if;
      if Natural (N_Bits) > 32 and then Max_Bits <= 32 then
         Close (OCD_File);
         raise Isys2nex_Error with "Bit size too big for packet";
      end if;
      --  The OCD file from winIdea is using more bits in some cases
      --  than come from the Nexus module. For now, we just check that
      --  we don't cross a 32 bit boundary so we can fit the values
      --  (which are correct) into the packet type. ???
      Bits := N_Bits;
      if Bits > 32 then
         Bits_To_Fill := 32;
      else
         Bits_To_Fill := Bits;
      end if;
      Packet := Continue_Fill (Bits_To_Fill);
   exception
      when End_Error =>
         Close (OCD_File);
         raise Isys2nex_Error with "Premature end-of-file in OCD_File";
   end Fill_Packet;

   function Continue_Fill (Bits : Item_Unit_T) return Nexus_Packet_T is
      Packet : Nexus_Packet_T;
      Byte : Item_Unit_T;
      N_Bytes : Positive;
      Mult    : Nexus_Packet_T;
   begin
      Packet := 0;
      Mult := 1;
      N_Bytes := (Integer (Bits) + 7) / 8;
      loop
         Read (OCD_File, Byte);
         Packet := Packet + Nexus_Packet_T (Byte) * Mult;
         exit when N_Bytes = 1;
         N_Bytes := N_Bytes - 1;
         Mult := Mult * 16#100#;
      end loop;
      return Packet;
   exception
      when End_Error =>
         Close (OCD_File);
         raise Isys2nex_Error with "Premature end-of-file in OCD_File";
   end Continue_Fill;

end Isys2nex;
