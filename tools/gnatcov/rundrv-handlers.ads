------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2016-2022, AdaCore                     --
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

with GNAT.Regpat; use GNAT.Regpat;

with Subprocesses;  use Subprocesses;
with Rundrv.Config; use Rundrv.Config;

package Rundrv.Handlers is

   procedure Get_Gnatemu_Driver
     (Context : Context_Type;
      Found   : out Boolean;
      Cmd     : out Command_Type;
      Native  : out Boolean);
   --  Helper for Lookup_Driver. If there is a GNATemulator available for this
   --  target, create a command to run it. See Rundrv.Config.Lookup_Driver for
   --  the semantics of arguments.

   --  The following procedures create commands to run programs on a given
   --  target (so we have one function per family of targets). See
   --  Rundrv.Config.Driver_Creator_Type for the semantics of arguments.

   procedure Native_Linux
     (Context : Context_Type;
      Matches : Match_Array;
      Cmd     : out Command_Type;
      Native  : out Boolean);
   procedure Native_Windows
     (Context : Context_Type;
      Matches : Match_Array;
      Cmd     : out Command_Type;
      Native  : out Boolean);
   procedure ISystem
     (Context : Context_Type;
      Matches : Match_Array;
      Cmd     : out Command_Type;
      Native  : out Boolean);
   procedure Prepare
     (Context : Context_Type;
      Matches : Match_Array;
      Cmd     : out Command_Type;
      Native  : out Boolean);
   procedure Visium_ELF
     (Context : Context_Type;
      Matches : Match_Array;
      Cmd     : out Command_Type;
      Native  : out Boolean);

end Rundrv.Handlers;
