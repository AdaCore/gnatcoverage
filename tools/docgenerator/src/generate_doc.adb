------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                        Copyright (C) 2008, AdaCore                       --
--                                                                          --
-- Couverture is free software; you can redistribute it  and/or modify it   --
-- under terms of the GNU General Public License as published by the Free   --
-- Software Foundation; either version 2, or (at your option) any later     --
-- version.  Couverture is distributed in the hope that it will be useful,  --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details. You  should  have  received a copy of the GNU --
-- General Public License  distributed with GNAT; see file COPYING. If not, --
-- write  to  the Free  Software  Foundation,  59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Doc_Generator.Requirements;

procedure Generate_Doc is
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Put_Line ("Error");
   else
      declare
         File_Name : String := Ada.Command_Line.Argument (1);
      begin
         Put_Line (File_Name);
         --  pick the first file (the requirement file)
         Doc_Generator.Requirements.Parse_File (File_Name);
         Doc_Generator.Requirements.Print;
      end;
   end if;
   exception
      when others => Put_Line ("Excpetion Raised");
end Generate_Doc;
