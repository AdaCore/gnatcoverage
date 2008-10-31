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
with Doc_Generator.Target_Tests;

procedure Generate_Doc is
begin
   if Ada.Command_Line.Argument_Count /= 2 then
      Put_Line ("Error");
   else
      declare
         Req_File_Name : String := Ada.Command_Line.Argument (1);
         Target_File_Name : String := Ada.Command_Line.Argument (2);
      begin
         Put_Line ("<html>");
         Put_Line ("<head>");
         Put_Line ("<script src=""jshelper.js"">");
         Put_Line ("</script>");

         Put_Line ("<title>XCOV Qualification Evidence</title>");
         Put_Line ("<link rel=""stylesheet"" type=""text/css""" &
                   "href=""xcov.css"">");
         Put_Line ("</head>");

         Put_Line ("<body>");
         Put_Line ("<b>Contents</b><br/>");
         Put_Line ("<ol>");
         Put_Line ("<li><a href=""#TORs"">" &
                   "Tool Operation Requirements</a></li>");
         Put_Line ("<li><a href=""#Tests"">" &
                   "Target Test Cases</a></li>");
         Put_Line ("</ol>");


         Put_Line ("<h1><a name=""TORs"" id=""TORs""/>" &
                     "Tool Operational Requirements</a></h1>");
         Put_Line ("<h2>Requirement analysis strategy</h2>");
         Put_Line ("<h2>Requirements</h2>");


         --  Put_Line (Req_File_Name);
         --  pick the first file (the requirement file)
         Doc_Generator.Requirements.Parse_File (Req_File_Name);
         Doc_Generator.Requirements.Print;

         Put_Line ("<br/><br/>");

         Put_Line ("<h1><a name""Tests"" id=""Tests"">" &
                   "Target test cases</a></h1>");
         Put_Line ("<h2>Test cases definition strategy</h2>");
         Put_Line ("<h2>Test cases</h2>");

         Doc_Generator.Target_Tests.Parse_File (Target_File_Name);
         Doc_Generator.Target_Tests.Print;
         Put_Line ("</body>");
         Put_Line ("</html>");
      end;
   end if;
   --  exception
   --   when others => Put_Line ("Excpetion Raised");
end Generate_Doc;
