------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2022-2024, AdaCore                     --
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

--  This package implements the "setup" gnatcov command.

with Logging;
with Strings;  use Strings;
with Switches; use Switches;

package Setup_RTS is

   Setup_RTS_Trace : constant Logging.GNATCOLL_Trace :=
     Logging.Create_Trace ("SETUP_RTS");

   type Any_RTS_Profile is (Auto, Full, Embedded, Minimal);
   --  See documentation for the --rts-profile option

   subtype Resolved_RTS_Profile is Any_RTS_Profile range Full .. Minimal;

   --  Serialization/deserialization functions for Any_RTS_Profile. The
   --  deserialization one raises Constraint_Error exceptions for invalid input
   --  strings.

   function Image (Profile : Any_RTS_Profile) return String;
   function Value (Profile : String) return Any_RTS_Profile;

   function Default_Dump_Config
     (RTS_Profile : Resolved_RTS_Profile; RTS : String) return Any_Dump_Config;
   --  Return the default dump configuration to use for the given runtime

   function Default_Project_File return String;
   --  Return the name of the instrumentation runtime project file that is
   --  shipped with gnatcov. Raise a fatal error if we cannot find it.

   function Project_Name (Project_File : String) return String;
   --  Given a project file, return the name of the project it is supposed to
   --  contain.

   procedure Setup
     (Project_File : String;
      Target       : String;
      RTS          : String;
      Config_File  : String;
      Db_Dir       : String;
      Prefix       : String;
      RTS_Profile  : Any_RTS_Profile;
      Install_Name : String;
      Gargs        : String_Vectors.Vector);
   --  Build and install the given Project_File instrumentation runtime
   --  project.
   --
   --  Target/RTS/Config_File are the standard GPR loading parameters to use
   --  for the build. DB_Dir is the path to the additional knowledge base
   --  directory, if any.
   --
   --  Prefix is the installation prefix to pass to gprinstall (--prefix
   --  argument). If the empty string is passed, do not pass --prefix to
   --  gprinstall, letting it install the project in the toolchain prefix.
   --
   --  RTS_Profile is used to determine the features to enable in the
   --  instrumentation runtime. If it is Auto, try to infer what features are
   --  available from the name of the RTS (the actual runtime used in the GPR
   --  world, i.e. not necessarily the value of the RTS argument passed here).
   --
   --  If non-empty, Install_Name is used as an alternative name for the
   --  installed project.
   --
   --  All items in Gargs are passed as additional command-line arguments to
   --  gprbuild.

   ------------------
   -- Setup config --
   ------------------

   --  The "gnatcov setup" command may store in the installed instrumentation
   --  project some information about what features it provides, and default
   --  values for instrumentation parameters (trace dump trigger, dump channel,
   --  etc.).
   --
   --  The Setup_Config type below is used to represent this information. As
   --  this information storage mechanism is optional, each information may be
   --  absent, hence the "..._Present" boolean components.

   type Setup_Config is record
      Project_File : Unbounded_String := Null_Unbounded_String;
      --  Project file associated with this setup config (when that config was
      --  loaded from a file).

      RTS_Profile         : Resolved_RTS_Profile;
      RTS_Profile_Present : Boolean;
      --  RTS profile used when setting up the instrumentation runtime, and
      --  whether this information is available.

      Default_Dump_Config : Any_Dump_Config;
      --  Default dump config to use in "gnatcov instrument". No need for a
      --  "..._Present" flag for this one as when we do not have access to the
      --  setup config, we just assume defaults for the Any_Dump_Config type.
   end record;

   Default_Setup_Config : constant Setup_Config :=
     (Project_File        => <>,
      RTS_Profile         => Full,
      RTS_Profile_Present => False,
      Default_Dump_Config => (others => <>));

   function Load
     (Target          : String;
      RTS             : String;
      Config_File     : String;
      Runtime_Project : String) return Setup_Config;
   --  Load the setup config associated to the project file called
   --  Runtime_Project (that project is loaded using Target, RTS and
   --  Config_File). If we cannot load the runtime project file or if the setup
   --  config is missing from this project, abort with a fatal error.

   function Check_RTS_Profile
     (Profile : Resolved_RTS_Profile; Dump_Config : Any_Dump_Config)
      return Boolean;
   --  Check that Dump_Config is compatible with Profile. If there is an
   --  incompatibility, emit warnings and return True. Return False otherwise.
   --
   --  The RTS profile information is a heuristic: we believe it will be useful
   --  to use it generally, but there is a chance that it leads us to wrong
   --  incompatibility predictions. We thus prefer to emit a warning rather
   --  than fail with an error, so that users are not blocked when that
   --  heuristic is wrong.

end Setup_RTS;
