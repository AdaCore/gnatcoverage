------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2016, AdaCore                     --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Ordered_Maps;

package body Factory_Registry is

   type Factory_Record is record
      Factory : Create_Function;
      Tag     : Ada.Tags.Tag;
   end record;

   package Registry_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type     => String,
      Element_Type => Factory_Record);

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
      Registry_Map.Insert (Name, (Factory => Create_T_Acc, Tag => T'Tag));
   end Register_Factory;

   ------------
   -- Create --
   ------------

   function Create (Name : String) return RT_Access is
      use Registry_Maps;

      Cur : constant Cursor := Registry_Map.Find (Name);
   begin
      if Cur = No_Element then
         raise Constraint_Error with "invalid name: " & Name;
      else
         return Element (Cur).Factory.all;
      end if;
   end Create;

   ----------
   -- Name --
   ----------

   function Name (Tag : Ada.Tags.Tag) return String is
      use type Ada.Tags.Tag;
      use Registry_Maps;

   begin
      for Cur in Registry_Map.Iterate loop
         if Element (Cur).Tag = Tag then
            return Key (Cur);
         end if;
      end loop;

      raise Constraint_Error with
        "no factory for " & Ada.Tags.External_Tag (Tag);
   end Name;

   ----------------------
   -- Registered_Names --
   ----------------------

   function Registered_Names (Separator : String) return String is
      use Registry_Maps;

      Result : Unbounded_String;
      First  : Boolean := True;
   begin
      for Cur in Registry_Map.Iterate loop
         if not First then
            Append (Result, Separator);
         end if;
         First := False;

         Append (Result, Key (Cur));
      end loop;
      return To_String (Result);
   end Registered_Names;

end Factory_Registry;
