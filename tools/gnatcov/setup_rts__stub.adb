------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
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

--  Stub for the body of Setup_RTS: see the note about Stubs in gnatcov.gpr

package body Setup_RTS is

   -----------
   -- Image --
   -----------

   function Image (Profile : Any_RTS_Profile) return String is
      pragma Unreferenced (Profile);
   begin
      return (raise Program_Error);
   end Image;

   -----------
   -- Value --
   -----------

   function Value (Profile : String) return Any_RTS_Profile is
      pragma Unreferenced (Profile);
   begin
      return (raise Program_Error);
   end Value;

   -------------------------
   -- Default_Dump_Config --
   -------------------------

   function Default_Dump_Config
     (RTS_Profile : Resolved_RTS_Profile; RTS : String) return Any_Dump_Config
   is
      pragma Unreferenced (RTS_Profile, RTS);
   begin
      return (raise Program_Error);
   end Default_Dump_Config;

   ---------------------------
   --- Default_Project_File --
   ---------------------------

   function Default_Project_File return String is
   begin
      return (raise Program_Error);
   end Default_Project_File;

   ------------------
   -- Project_Name --
   ------------------

   function Project_Name (Project_File : String) return String is
      pragma Unreferenced (Project_File);
   begin
      return (raise Program_Error);
   end Project_Name;

   -----------
   -- Setup --
   -----------

   procedure Setup
     (Project_File : String;
      Target       : String;
      RTS          : String;
      Config_File  : String;
      Db_Dir       : String;
      Prefix       : String;
      RTS_Profile  : Any_RTS_Profile;
      Install_Name : String;
      Gargs        : String_Vectors.Vector)
   is
      pragma Unreferenced (Project_File);
      pragma Unreferenced (Target);
      pragma Unreferenced (RTS);
      pragma Unreferenced (Config_File);
      pragma Unreferenced (Prefix);
      pragma Unreferenced (RTS_Profile);
      pragma Unreferenced (Install_Name);
      pragma Unreferenced (Gargs);
   begin
      raise Program_Error;
   end Setup;

   ----------
   -- Load --
   ----------

   function Load
     (Target          : String;
      RTS             : String;
      Config_File     : String;
      Runtime_Project : String) return Setup_Config
   is
      pragma Unreferenced (Target, RTS, Config_File, Runtime_Project);
   begin
      return (raise Program_Error);
   end Load;

   -----------------------
   -- Check_RTS_Profile --
   -----------------------

   function Check_RTS_Profile
     (Profile : Resolved_RTS_Profile; Dump_Config : Any_Dump_Config)
      return Boolean
   is
      pragma Unreferenced (Profile, Dump_Config);
   begin
      return (raise Program_Error);
   end Check_RTS_Profile;

end Setup_RTS;
