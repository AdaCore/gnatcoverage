------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2023-2024, AdaCore                     --
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

with Instrument.Common; use Instrument.Common;

--  Implementation of the gnatcov instrument-main, which inserts a call to
--  dump coverage buffers according to the various dump options passed
--  on the command line.

procedure Instrument.Main
  (Instrumenter  : in out Language_Instrumenter'Class;
   Dump_Config   : Any_Dump_Config;
   Main_Filename : String;
   Prj           : Prj_Desc) is
begin
   --  If the dump-trigger is manual, there is nothing to do

   if Dump_Config.Trigger = Manual then
      return;
   end if;

   Instrumenter.Auto_Dump_Buffers_In_Main
     (Filename => Main_Filename, Dump_Config => Dump_Config, Prj => Prj);
end Instrument.Main;
