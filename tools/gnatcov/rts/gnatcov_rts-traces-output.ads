------------------------------------------------------------------------------
--                                                                          --
--                   GNATcoverage Instrumentation Runtime                   --
--                                                                          --
--                     Copyright (C) 2019-2021, AdaCore                     --
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

--  This unit needs to be compilable with Ada 95 compilers

with System;

with GNATcov_RTS.Buffers.Lists; use GNATcov_RTS.Buffers.Lists;

package GNATcov_RTS.Traces.Output is

   subtype Serialized_Timestamp is String (1 .. 8);
   --  Little-endian 64-bit unsigned integer. Represents a date/time as the
   --  number of seconds that passed since the Unix Epoch. In other words, a
   --  64-bit Unix timestamp.

   generic
      type Output_Stream (<>) is limited private;
      --  Entity where the trace file is written

      with procedure Write_Bytes
        (Stream : in out Output_Stream;
         Bytes  : System.Address;
         Count  : Natural) is <>;
      --  Write Count bytes located at Bytes to the given Stream

   procedure Generic_Write_Trace_File
     (Output       : in out Output_Stream;
      Buffers      : Unit_Coverage_Buffers_Array;
      Program_Name : String;
      Exec_Date    : Serialized_Timestamp;
      User_Data    : String := "");
   --  Write a trace file to Output to contain the coverage data in Buffers.
   --
   --  Program_Name, Exec_Date, and User_Data are used to fill the
   --  corresponding metadata in the written trace file.

end GNATcov_RTS.Traces.Output;
