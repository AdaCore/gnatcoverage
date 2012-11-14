------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

with Ada.Containers.Indefinite_Ordered_Maps;

package body Factory_Registry is

   package Registry_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type     => String,
      Element_Type => Create_Function);

   Registry_Map : Registry_Maps.Map;

   ----------------------
   -- Register_Factory --
   ----------------------

   package body Register_Factory is

      --------------
      -- Create_T --
      --------------

      function Create_T return RT_Access is
      begin
         return new T;
      end Create_T;

   --  Start of processing for Register_Factory

   begin
      Registry_Map.Insert (Name, Create_T_Acc);
   end Register_Factory;

   ------------
   -- Create --
   ------------

   function Create (Name : String) return RT_Access is
   begin
      return Registry_Map.Element (Name).all;
   end Create;

end Factory_Registry;
