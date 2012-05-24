------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2012, AdaCore                       --
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

--  GNAT projects support

package Project is

   procedure Load_Project (Prj_Name : String);
   --  Load the named project

   procedure Add_Scenario_Var (Key, Value : String);
   --  Set the indicated scenario variable to the given value

   procedure Compute_Project_View;
   --  Recompute the view of the loaded project within the current scenario

   --------------------------------------
   -- Accessors for project properties --
   --------------------------------------

   function Get_Level return String;
   --  Return Prj_Name'Level

   procedure Enumerate_Mains (Main_Cb : access procedure (Main_Name : String));
   --  Call Main_Cb once for every main of the root project

   procedure Enumerate_LIs (LI_Cb : access procedure (LI_Name : String));
   --  Call LI_Cb once for every relevant library information (ALI/GLI) file

end Project;
