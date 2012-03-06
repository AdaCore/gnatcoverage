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

package body Nexus_Rep is

   procedure Nexus_Append_To_List
     (L : in out Nexus_Message_List_T; Msg : Nexus_Message_Rec_Ptr_T) is
      Elem : constant Nexus_Message_List_Elem_Ptr_T
        := new Nexus_Message_List_Elem_T;
   begin
      Elem.Message := Msg;
      if L.First = null then
         L.First := Elem;
         L.Last  := Elem;
      else
         L.Last.Next := Elem;
         L.Last := Elem;
      end if;
   end Nexus_Append_To_List;
end Nexus_Rep;
