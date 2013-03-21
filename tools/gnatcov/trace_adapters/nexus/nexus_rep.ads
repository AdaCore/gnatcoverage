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

--  This package provides types and routines for representing and
--  manipulating data specified by the Nexus Forum standard (IEEE-ISTO 5001),
--  describing a debug interface for embedded processors.
--
--  This is a very early version of this package and is subject to
--  significant changes as it is adapted to be used with more
--  HW implementations of the Nexus standard.
--
--  E.g., it is currently strongly influenced by its application to program
--  trace capabilites provided by the Freescale NZ63C module on the MPC5554.
--  TCODE values are those implemented by the NZ63C as well as the packets
--  included in messages, and the ranges of bit sizes for each packet.

package Nexus_Rep is

   --  Nexus trace data comprises "messages" containing a variable
   --  number of "packets" of data, some of which are fixed in size, and some
   --  of variable sizes. Each message contains a TCODE value indicating
   --  the type of message, and determining which packets are included in
   --  the message.
   --
   --  A Nexus interface streams out these messages, and here we represent
   --  such a stream of messages as a list of record values. The Nexus record
   --  is a discrminated type, with the TCODE as the discriminant. All
   --  packets are represented by a 32 bit modular type. There are some
   --  Nexus packets which can be up to 64 bits in length -- these are
   --  represented by a pair of 32 bit packet components.

   type    Nexus_Packet_T is mod 2 ** 32;
   subtype Nexus_Tcode_T  is Nexus_Packet_T range 0 .. 33;

   type Nexus_Message_Rec_T;
   type Nexus_Message_Rec_Ptr_T is access Nexus_Message_Rec_T;
   type Nexus_Message_List_Elem_T;
   type Nexus_Message_List_Elem_Ptr_T is access Nexus_Message_List_Elem_T;
   type Nexus_Message_List_Elem_T is record
      Message : Nexus_Message_Rec_Ptr_T;
      Next    : Nexus_Message_List_Elem_Ptr_T;
   end record;
   type Nexus_Message_List_T is record
      First : Nexus_Message_List_Elem_Ptr_T;
      Last  : Nexus_Message_List_Elem_Ptr_T;
   end record;

   procedure Nexus_Append_To_List
     (L : in out Nexus_Message_List_T; Msg : Nexus_Message_Rec_Ptr_T);

   Debug_Status                                 : constant Nexus_Tcode_T :=  0;
   Ownership_Trace_Message                      : constant Nexus_Tcode_T :=  2;
   Prog_Trace_Direct_Branch_Message             : constant Nexus_Tcode_T :=  3;
   Prog_Trace_Indirect_Branch_Message           : constant Nexus_Tcode_T :=  4;
   Data_Trace_Data_Write_Message                : constant Nexus_Tcode_T :=  5;
   Data_Trace_Data_Read_Message                 : constant Nexus_Tcode_T :=  6;
   Error_Message                                : constant Nexus_Tcode_T :=  8;
   Prog_Trace_Direct_Branch_Message_Sync        : constant Nexus_Tcode_T := 11;
   Prog_Trace_Indirect_Branch_Message_Sync      : constant Nexus_Tcode_T := 12;
   Data_Trace_Data_Write_Message_Sync           : constant Nexus_Tcode_T := 13;
   Data_Trace_Data_Read_Message_Sync            : constant Nexus_Tcode_T := 14;
   Watchpoint_Message                           : constant Nexus_Tcode_T := 15;
   Resource_Full_Message                        : constant Nexus_Tcode_T := 27;
   Prog_Trace_Indirect_Branch_Hist_Message      : constant Nexus_Tcode_T := 28;
   Prog_Trace_Indirect_Branch_Hist_Message_Sync : constant Nexus_Tcode_T := 29;
   Prog_Trace_Program_Correlation_Message       : constant Nexus_Tcode_T := 33;

   type Debug_Status_Rec is record
      STATUS : Nexus_Packet_T;
   end record;
   type Ownership_Trace_Message_Rec is record
      PROCESS : Nexus_Packet_T;
   end record;
   type Prog_Trace_Direct_Branch_Message_Rec is record
      I_CNT : Nexus_Packet_T;
   end record;
   type Prog_Trace_Indirect_Branch_Message_Rec is record
      I_CNT  : Nexus_Packet_T;
      U_ADDR : Nexus_Packet_T;
   end record;
   type Data_Trace_Data_Write_Message_Rec is record
      DSIZ    : Nexus_Packet_T;
      U_ADDR  : Nexus_Packet_T;
      DATA_Lo : Nexus_Packet_T;
      DATA_Hi : Nexus_Packet_T;
   end record;
   type Data_Trace_Data_Read_Message_Rec is record
      DSIZ    : Nexus_Packet_T;
      U_ADDR  : Nexus_Packet_T;
      DATA_Lo : Nexus_Packet_T;
      DATA_Hi : Nexus_Packet_T;
   end record;
   type Error_Message_Rec is record
      ECODE : Nexus_Packet_T;
   end record;
   type Prog_Trace_Direct_Branch_Message_Sync_Rec is record
      I_CNT  : Nexus_Packet_T;
      F_ADDR : Nexus_Packet_T;
   end record;
   type Prog_Trace_Indirect_Branch_Message_Sync_Rec is record
      I_CNT  : Nexus_Packet_T;
      F_ADDR : Nexus_Packet_T;
   end record;
   type Data_Trace_Data_Write_Message_Sync_Rec is record
      DSZ     : Nexus_Packet_T;
      F_ADDR  : Nexus_Packet_T;
      DATA_Lo : Nexus_Packet_T;
      DATA_Hi : Nexus_Packet_T;
   end record;
   type Data_Trace_Data_Read_Message_Sync_Rec is record
      DSZ     : Nexus_Packet_T;
      F_ADDR  : Nexus_Packet_T;
      DATA_Lo : Nexus_Packet_T;
      DATA_Hi : Nexus_Packet_T;
   end record;
   type Watchpoint_Message_Rec is record
      WPHIT : Nexus_Packet_T;
   end record;
   type Resource_Full_Message_Rec is record
      RCODE : Nexus_Packet_T;
      HIST  : Nexus_Packet_T;
   end record;
   type Prog_Trace_Indirect_Branch_Hist_Message_Rec is record
      I_CNT  : Nexus_Packet_T;
      U_ADDR : Nexus_Packet_T;
      HIST   : Nexus_Packet_T;
   end record;
   type Prog_Trace_Indirect_Branch_Hist_Message_Sync_Rec is record
      I_CNT  : Nexus_Packet_T;
      F_ADDR : Nexus_Packet_T;
      HIST   : Nexus_Packet_T;
   end record;
   type Prog_Trace_Program_Correlation_Message_Rec is record
      EVCODE : Nexus_Packet_T;
      I_CNT  : Nexus_Packet_T;
      HIST   : Nexus_Packet_T;
   end record;

   type Nexus_Message_Rec_T (Tcode : Nexus_Tcode_T) is record
      SRC : Nexus_Packet_T;
      case Tcode is
         when Debug_Status =>
            Debug_Status_V
              : Debug_Status_Rec;
         when Ownership_Trace_Message =>
            Ownership_Trace_Message_V
              : Ownership_Trace_Message_Rec;
         when Prog_Trace_Direct_Branch_Message =>
            Prog_Trace_Direct_Branch_Message_V
              : Prog_Trace_Direct_Branch_Message_Rec;
         when Prog_Trace_Indirect_Branch_Message =>
            Prog_Trace_Indirect_Branch_Message_V
              : Prog_Trace_Indirect_Branch_Message_Rec;
         when Data_Trace_Data_Write_Message =>
            Data_Trace_Data_Write_Message_V
              : Data_Trace_Data_Write_Message_Rec;
         when Data_Trace_Data_Read_Message  =>
            Data_Trace_Data_Read_Message_V
              : Data_Trace_Data_Read_Message_Rec;
         when Error_Message =>
            Error_Message_V
              : Error_Message_Rec;
         when Prog_Trace_Direct_Branch_Message_Sync =>
            Prog_Trace_Direct_Branch_Message_Sync_V
              : Prog_Trace_Direct_Branch_Message_Sync_Rec;
         when Prog_Trace_Indirect_Branch_Message_Sync =>
            Prog_Trace_Indirect_Branch_Message_Sync_V
              : Prog_Trace_Indirect_Branch_Message_Sync_Rec;
         when Data_Trace_Data_Write_Message_Sync =>
            Data_Trace_Data_Write_Message_Sync_V
              : Data_Trace_Data_Write_Message_Sync_Rec;
         when Data_Trace_Data_Read_Message_Sync =>
            Data_Trace_Data_Read_Message_Sync_V
              : Data_Trace_Data_Read_Message_Sync_Rec;
         when Watchpoint_Message =>
            Watchpoint_Message_V
              : Watchpoint_Message_Rec;
         when Resource_Full_Message =>
            Resource_Full_Message_V
              : Resource_Full_Message_Rec;
         when Prog_Trace_Indirect_Branch_Hist_Message =>
            Prog_Trace_Indirect_Branch_Hist_Message_V
              : Prog_Trace_Indirect_Branch_Hist_Message_Rec;
         when Prog_Trace_Indirect_Branch_Hist_Message_Sync =>
            Prog_Trace_Indirect_Branch_Hist_Message_Sync_V
              : Prog_Trace_Indirect_Branch_Hist_Message_Sync_Rec;
         when Prog_Trace_Program_Correlation_Message =>
            Prog_Trace_Program_Correlation_Message_V
              : Prog_Trace_Program_Correlation_Message_Rec;
         when  1 | 7 | 9 | 10 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23
           |  24 | 25 | 26 | 30 | 31 | 32 =>
            null;
      end case;
   end record;

   subtype Nexus_N_Packets_T is Natural;
   type    Nexus_N_Packets_Arr_T is array (Nexus_Tcode_T) of Nexus_N_Packets_T;
   Nexus_N_Packets_Array : constant Nexus_N_Packets_Arr_T :=
     (Debug_Status                                 => 3,
      Ownership_Trace_Message                      => 3,
      Prog_Trace_Direct_Branch_Message             => 3,
      Prog_Trace_Indirect_Branch_Message           => 4,
      Data_Trace_Data_Write_Message                => 5,
      Data_Trace_Data_Read_Message                 => 5,
      Error_Message                                => 3,
      Prog_Trace_Direct_Branch_Message_Sync        => 4,
      Prog_Trace_Indirect_Branch_Message_Sync      => 4,
      Data_Trace_Data_Write_Message_Sync           => 5,
      Data_Trace_Data_Read_Message_Sync            => 5,
      Watchpoint_Message                           => 3,
      Resource_Full_Message                        => 4,
      Prog_Trace_Indirect_Branch_Hist_Message      => 5,
      Prog_Trace_Indirect_Branch_Hist_Message_Sync => 5,
      Prog_Trace_Program_Correlation_Message       => 5,
      others => 0);
   --  May not fit here perfectly, but these values are for the convenience
   --  of routines which read a file representation of Nexus data that
   --  includes a count of packets before each message (iSystem OCD data
   --  in particular).

end Nexus_Rep;
