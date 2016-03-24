------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2012, AdaCore                     --
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

package body Coverage.Tags is

   -------------------
   -- Enter_Routine --
   -------------------

   procedure Enter_Routine
     (TP        : access Tag_Provider_Type;
      Subp_Info : Traces_Names.Subprogram_Info)
   is
      Subprogram_Info : constant Address_Info_Acc :=
                          Get_Address_Info
                            (Exec => Subp_Info.Exec.all,
                             Kind => Subprogram_Addresses,
                             PC   => Subp_Info.Insns.First);
   begin
      TP.Current_Routine := Subp_Info;
      TP.Current_Subp    := Subprogram_Info;
   end Enter_Routine;

   ------------------------
   -- Get_Slocs_And_Tags --
   ------------------------

   overriding function Get_Slocs_And_Tags
     (TP : access Default_Tag_Provider_Type;
      PC : Pc_Type) return Tagged_Slocs
   is
   begin
      return Get_Slocs_With_Tag (TP.Current_Subp.Lines, PC, No_SC_Tag);
   end Get_Slocs_And_Tags;

   ------------------------
   -- Get_Slocs_With_Tag --
   ------------------------

   function Get_Slocs_With_Tag
     (Set : Address_Info_Sets.Set;
      PC  : Pc_Type;
      Tag : SC_Tag) return Tagged_Slocs
   is
      Slocs : constant Source_Locations := Get_Slocs (Set, PC);
   begin
      return Tslocs : Tagged_Slocs (Slocs'Range) do
         for J in Slocs'Range loop
            Tslocs (J) := (Sloc => Slocs (J), Tag => Tag);
         end loop;
      end return;
   end Get_Slocs_With_Tag;

   -------------
   -- Map_Tag --
   -------------

   overriding function Map_Tag
     (TP     : access Default_Tag_Provider_Type;
      CS     : Checkpoints.Checkpoint_State;
      CP_Tag : SC_Tag) return SC_Tag
   is
      pragma Unreferenced (TP, CS, CP_Tag);
   begin
      return No_SC_Tag;
   end Map_Tag;

   --------------
   -- Tag_Name --
   --------------

   overriding function Tag_Name
     (TP  : access Default_Tag_Provider_Type;
      Tag : SC_Tag) return String
   is
      pragma Unreferenced (TP);
      pragma Assert (Tag = No_SC_Tag);
   begin
      return "";
   end Tag_Name;

end Coverage.Tags;
