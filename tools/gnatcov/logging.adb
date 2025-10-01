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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO;             use Ada.Text_IO;

package body Logging is

   Verbose : Boolean := False;
   --  Whether the "--verbose" command line flag is active

   Enabled_Traces : String_Sets.Set;
   --  Set of names for all GNATCOLL traces requested to be enabled on the
   --  command line ("--log").

   Explicitly_Enabled : String_Vectors.Vector;
   --  Copy of the To_Enable argument passed to Initialize

   function Unprefixed_Name (Handle : GNATCOLL_Trace) return String;
   --  If Handle's name starts with GNATCOLL_Trace_Prefix (i.e. if it is a
   --  GNATcov trace), return its normalized unprefixed name. Return an empty
   --  string otherwise.

   procedure Try_Enabling (Handle : GNATCOLL_Trace);
   --  If the verbose mode is enabled, enable Handle if its name starts with
   --  GNATCOLL_Trace_Prefix. Otherwise, enable it if its name starts with
   --  GNATCOLL_Trace_Prefix *and* is in Enabled_Traces.

   GNATcov_Traces : String_Sets.Set;
   --  Temporary for Print_List: set of names for GNATcov traces

   procedure Add_Trace (Handle : GNATCOLL_Trace);
   --  If Handle's name starts with GNATCOLL_Trace_Prefix, add the name to
   --  GNATcov_Traces.

   ------------------
   -- Create_Trace --
   ------------------

   function Create_Trace (Unit_Name : String) return GNATCOLL_Trace is
   begin
      return GNATCOLL.Traces.Create (GNATCOLL_Trace_Prefix & Unit_Name);
   end Create_Trace;

   ---------------------
   -- Unprefixed_Name --
   ---------------------

   function Unprefixed_Name (Handle : GNATCOLL_Trace) return String is
      Name : constant String := To_Lower (Handle.Unit_Name);
   begin
      if Has_Prefix (Name, GNATCOLL_Trace_Prefix) then
         return Name (Name'First + GNATCOLL_Trace_Prefix'Length .. Name'Last);
      else
         return "";
      end if;
   end Unprefixed_Name;

   ------------------
   -- Try_Enabling --
   ------------------

   procedure Try_Enabling (Handle : GNATCOLL_Trace) is
      Name : constant String := Unprefixed_Name (Handle);
   begin
      if Name /= "" and then (Verbose or else Enabled_Traces.Contains (+Name))
      then
         Handle.Set_Active (Active => True);
      end if;
   end Try_Enabling;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Verbose : Boolean; To_Enable : String_Vectors.Vector)
   is
   begin
      Logging.Verbose := Verbose;
      Explicitly_Enabled := To_Enable;
      for Name of To_Enable loop
         Enabled_Traces.Include (+To_Lower (+Name));
      end loop;

      GNATCOLL.Traces.Parse_Config_File;
      GNATCOLL.Traces.For_Each_Handle (Try_Enabling'Access);
   end Initialize;

   -----------------------
   -- Get_Configuration --
   -----------------------

   procedure Get_Configuration
     (Verbose : out Boolean; To_Enable : out String_Vectors.Vector) is
   begin
      Verbose := Logging.Verbose;
      To_Enable := Explicitly_Enabled;
   end Get_Configuration;

   ---------------
   -- Add_Trace --
   ---------------

   procedure Add_Trace (Handle : GNATCOLL_Trace) is
      Name : constant String := Unprefixed_Name (Handle);
   begin
      if Name /= "" then
         GNATcov_Traces.Include (+Name);
      end if;
   end Add_Trace;

   ----------------
   -- Print_List --
   ----------------

   procedure Print_List is
   begin
      --  To get deterministic (and convenient) output, first build an ordered
      --  set of names, then print it in order.

      GNATcov_Traces.Clear;
      GNATCOLL.Traces.For_Each_Handle (Add_Trace'Access);
      for Name of GNATcov_Traces loop
         Put_Line (+Name);
      end loop;
   end Print_List;

end Logging;
