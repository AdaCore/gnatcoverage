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

   -------------
   -- Get_Tag --
   -------------

   overriding function Get_Tag
     (TR  : access Default_Tag_Repository_Type;
      Exe : Exe_File_Acc;
      PC  : Pc_Type) return SC_Tag
   is
      pragma Unreferenced (TR, Exe, PC);
   begin
      return No_SC_Tag;
   end Get_Tag;

   --------------
   -- Tag_Name --
   --------------

   overriding function Tag_Name
     (TR  : access Default_Tag_Repository_Type;
      Tag : SC_Tag) return String
   is
      pragma Unreferenced (TR);
      pragma Assert (Tag = No_SC_Tag);
   begin
      return "";
   end Tag_Name;

begin
   Tag_Repository := Default_Tag_Repository'Access;
end Coverage.Tags;
