------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2021, AdaCore                     --
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

with GNAT.Strings; use GNAT.Strings;

with Inputs;

package Rundrv is

   type SO_Set_Kind is (None, Some_SO, All_SO);
   type SO_Set_Type (Kind : SO_Set_Kind := Some_SO) is record
      case Kind is
         when None    => null;
         when Some_SO => Set : Inputs.Inputs_Type;
         when All_SO  => null;
      end case;
   end record;
   --  Holder for a set of selected shared objects to include in trace files

   procedure Driver
     (Exe_File      : String;
      Target_Family : String_Access;
      Target_Board  : String_Access;
      Tag           : String_Access;
      Output        : String_Access;
      Histmap       : String_Access;
      Kernel        : String_Access;
      Eargs         : String_List_Access;
      SO_Set        : SO_Set_Type);
   --  Run Exe_File on an instrumented execution environment (depending on
   --  Target: GNATemulator, Valgrind, DynamoRIO, etc). Pass Eargs as
   --  command-line arguments for Exe_File. Write traces in the Output trace
   --  file. If Tag is not null, append it to the trace header.

end Rundrv;
