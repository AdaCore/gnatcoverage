------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2022, AdaCore                       --
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

with Strings;    use Strings;

package Setup_RTS is

   type Any_RTS_Profile is (Auto, Full, Embedded);
   --  See documentation for the --rts-profile option

   subtype Resolved_RTS_Profile is Any_RTS_Profile range Full .. Embedded;

   procedure Setup
     (Target             : String;
      RTS                : String;
      Config_File        : String;
      Prefix             : String;
      RTS_Profile        : Any_RTS_Profile;
      Runtime_Project    : String;
      Runtime_Source_Dir : String;
      Gargs              : String_Vectors.Vector);
   --  Build and install the "gnatcov_rts" project shipped in gnatcov's "share"
   --  directory.
   --
   --  Target/RTS/Config_File are the standard GPR loading parameters to use
   --  for the build.
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
   --  Runtime_Project is the name to use for the instrumentation runtime
   --  project (renamed from "GNATcov_RTS").
   --
   --  All items in Gargs are passed as additional command-line arguments to
   --  gprbuild.

end Setup_RTS;
