------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                    Copyright (C) 2009-2011, AdaCore                      --
--                                                                          --
-- Couverture is free software; you can redistribute it  and/or modify it   --
-- under terms of the GNU General Public License as published by the Free   --
-- Software Foundation; either version 2, or (at your option) any later     --
-- version.  Couverture is distributed in the hope that it will be useful,  --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details. You  should  have  received a copy of the GNU --
-- General Public License  distributed with GNAT; see file COPYING. If not, --
-- write  to  the Free  Software  Foundation,  59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Containers.Vectors;

with GNAT.Strings; use GNAT.Strings;

with SC_Obligations; use SC_Obligations;
with Slocs;          use Slocs;
with Traces;         use Traces;
with Traces_Elf;     use Traces_Elf;

package Diagnostics is

   type Report_Kind is (Notice, Warning, Error);

   type Message is record
      Kind : Report_Kind;
      PC   : Pc_Type;
      Sloc : Source_Location;
      SCO  : SCO_Id;
      Msg  : String_Access;
   end record;

   package Message_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Message);

   Detached_Messages : Message_Vectors.Vector;
   --  Messages without an associated source line

   procedure Report
     (Exe  : Exe_File_Acc;
      PC   : Pc_Type;
      Msg  : String;
      Kind : Report_Kind := Error);

   procedure Report
     (Sloc : Source_Location;
      Msg  : String;
      Kind : Report_Kind := Error);

   procedure Report
     (SCO  : SCO_Id;
      Msg  : String;
      Kind : Report_Kind := Error);
   --  Report a violation of a source coverage obligation. Note: the SCO kind
   --  will be prepended to Msg in reports, unless Msg starts with the ^
   --  character.

   procedure Report
     (Msg  : String;
      PC   : Pc_Type         := No_PC;
      Sloc : Source_Location := No_Location;
      SCO  : SCO_Id          := No_SCO_Id;
      Kind : Report_Kind     := Error);
   --  Output diagnostic message during coverage analysis. Messages with Notice
   --  kind are omitted unless global flag Verbose is set. A prefix is
   --  prepended depending on message kind:
   --     --- notice
   --     *** warning
   --     !!! error
   --  The message is also recorded in the source line information for its sloc
   --  or in the Detached_Messages vector, if there is no such source line
   --  information. If SCO is not No_SCO_Id, the message denotes a violation
   --  of the denoted Source Coverage Obligation.

   function Image (M : Message) return String;

end Diagnostics;
