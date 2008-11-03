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

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded.Text_IO;

with Doc_Generator.Utils;

package body Doc_Generator.Requirements is
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   use Ada.Strings.Fixed;
   use Ada.Strings;

   procedure Add_Code (F : File_Type);

   procedure Create_Driver (R : in Requirement_Ref;
                            Line : String; F : File_Type);

   procedure Create_Requirement (R : in out Requirement_Ref;
                                 Line : String; F : File_Type);


   procedure Add_Code (F : File_Type) is
      use Doc_Generator.Utils;

      procedure Analyze
        (P_Name : String;
         Code : Unbounded_String) is
         Tmp : Driver_Ref := null;
      begin
         Tmp := T_Map.Element (P_Name);
         Tmp.Code := Code;
      exception
         when Constraint_Error =>
            null;
      end Analyze;

   begin

      Get_And_Analyse_Procedure_Code
        (F, Analyze'Access);

   end Add_Code;


   procedure Create_Driver (R : in Requirement_Ref;
                            Line : String; F : File_Type) is
      D : Driver_Ref := new Driver;
   begin
      --  parse the description
      D.ID :=
        Remove_Std_prefix
          (To_Unbounded_String (Trim (Get_Line (F), Both)));
      D.Subprogram := Remove_Std_prefix
        (To_Unbounded_String (Trim (Get_Line (F), Both)));
      R.Drivers.Append (D);

      --  insert the couple (procedure_name, pointer_to_obj) in the map
      --  as a key, we use the relative procedure name
      declare
         Last_Pos_Of_Point : Natural :=
           Ada.Strings.Unbounded.Index
             (D.Subprogram, ".", Ada.Strings.Backward) + 1;
         Relative_Proc_Name : String :=
           Ada.Strings.Unbounded.Slice
             (D.Subprogram, Last_Pos_Of_Point,
              Ada.Strings.Unbounded.Length (D.Subprogram));
      begin
         T_Map.Insert
           (Relative_Proc_Name, D);
         --  Ada.Text_IO.Put_Line (Relative_Proc_Name);
      end;

      --  skip the lines not containing the invocation of targets
      while Trim (Get_Line (F), Both) /= Driver_Expected_Tag loop
         null;
      end loop;

      --  now start looking for targets
      loop
         declare
            T : String := Trim (Get_Line (F), Both);

            Exp_Cov : Function_Coverage := NOT_COVERED;
            --  Pos : Positive;
         begin
            exit when T = End_Tag;
            declare
               Sub : String := Get_Interesting_Substring (T, Std_Prefix, " ");
               Exp : Function_Coverage := Get_Coverage (T);
            begin
               To_Lower (Sub);
               D.Targets.Append
                 (new Target '
                    (ID => Null_Unbounded_String,
                     Description => Null_Unbounded_String,
                     Subprogram => To_Unbounded_String (Sub),
                     Expected_Coverage => Exp));
               Test_Cases_N := Test_Cases_N + 1;
            end;
         end;
      end loop;
   end Create_Driver;

   procedure Create_Requirement (R : in out Requirement_Ref;
                                 Line : String; F : File_Type) is
      Pos : Natural := 0;
   begin

      --  create a new requirement
      R := new Requirement;
      R.In_File := To_Unbounded_String (Name (F));
      R.ID := To_Unbounded_String
        (Get_Interesting_Substring (Line, Requirement_Start_Tag,
         Requirement_End_Tag));

      --  and now fill its description
      loop
         declare
            L : String := Trim (Get_Line (F), Both);
         begin
            --  continue to fill the description until the END_TAG is found
            Starts_With (L, End_Tag, Pos);
            exit when Pos /= 0;
            R.Description := R.Description &
            Replace_Slice (L, L'First, L'First + 1, " ");
         end;
      end loop;

      Req_List.Append (R);

   end Create_Requirement;


   procedure Parse_Requirements (F : File_Type) is
      Current_Requirement : Requirement_Ref := null;
   begin
      while not End_Of_File (F) loop

         declare
            Line : String := Get_Line (F);
            Pos : Natural := 0;
         begin
            --  If the line contains the description of a driver
            Starts_With (Line, Driver_Tag, Pos);
            if Pos /= 0 then
               --  attach a new driver to the current requirement
               Create_Driver (Current_Requirement, Line, F);
            else
               --  if the line starts with the description of a requirement
               Starts_With (Line, Requirement_Start_Tag, Pos);
               if Pos /= 0 then
                  --  create a new requirement
                  Create_Requirement (Current_Requirement, Line, F);
               end if;
            end if;
         end;
      end loop;

      declare
         Body_File : File_Type;
         File_Name : String := Overwrite
           (Name (F), Name (F)'Length, "b");
      begin
         Open (Body_File, IN_FILE, File_Name);
         Add_Code (Body_File);
      end;

   end Parse_Requirements;

   --  at the moment this is just for debug purpose --

   procedure Parse_File (Path : String) is

   begin
      Doc_Generator.Utils.Parse_File_List (Path, Parse_Requirements'Access);
   end Parse_File;

   -----------
   -- Print --
   -----------

   procedure Print is

      procedure Print_Driver (It : Driver_List.Cursor) is
         Dr : Driver_Ref := Driver_List.Element (It);
         procedure Print_Target (It : Target_List.Cursor) is
            T : Target_Ref := Target_List.Element (It);
         begin
            Ada.Strings.Unbounded.Text_IO.Put_Line
              ("<tr><td><a href=""#" & T.Subprogram & """/>" & T.Subprogram &
               "</a></td><td><i>" &
               Function_Coverage'Image (T.Expected_Coverage) &
               "</i></td></tr>");
         end Print_Target;

      begin
         Ada.Strings.Unbounded.Text_IO.Put_Line
           ("<tr bgcolor=""LemonChiffon""><td colspan=""2""><b>" &
            Dr.Subprogram &
            " exercising test cases:</b></td></tr>");
         Ada.Strings.Unbounded.Text_IO.Put_Line
           ("<tr class=""code""><td colspan=""2"">"
            & Dr.Code & "</td></tr>");
         Dr.Targets.Iterate (Print_Target'Access);
      end Print_Driver;

      procedure Print_Req (It : Requirement_List.Cursor) is
         Req : Requirement_Ref := Requirement_List.Element (It);
      begin
         Ada.Strings.Unbounded.Text_IO.Put_Line
           ("<H3>Requirement <a name=""" & Req.ID & """ id=""" & Req.ID &
            """>" & Req.ID & "</a></H3>");
         Put_Line ("<table class=""summary"">");
         Ada.Strings.Unbounded.Text_IO.Put_Line
           ("<tr><td><b>ID: </b></td><td>" & Req.ID & "</td></tr>");
         Ada.Strings.Unbounded.Text_IO.Put_Line
           ("<tr><td><b>Specification: </b></td><td>" &
            Req.Description & "</td></tr>");
         Ada.Text_IO.Put_Line
           ("<tr><td><b>checked by:</b></td><td>" & Natural'Image
              (Integer (Req.Drivers.Length)) &
            " test suites in <a href=""file:///" &
            To_String (Req.In_File) & """/>file</a></i></td></tr>");
         Put_Line ("</table>");
         Ada.Strings.Unbounded.Text_IO.Put_Line
           ("<a href=""#" & Req.ID & """ onclick=""showhide('" &
            Req.ID & "_TC');"">" & "Toogle detailed test cases information" &
            "</a>");
         Ada.Strings.Unbounded.Text_IO.Put_Line
           ("<div id=""" & Req.ID & "_TC"" style=""display: none;"">");
         Ada.Text_IO.Put_Line
            ("<table border=""1"" cellspacing=""1" &
            " align=""center"">");
         Req.Drivers.Iterate (Print_Driver'Access);
         Ada.Text_IO.Put_Line ("</table>");
          Ada.Text_IO.Put_Line ("</div><br/><br/>");
      end Print_Req;

   begin
      Req_List.Iterate (Print_Req'Access);
      Ada.Text_IO.Put_Line ("<h2>Statistics</h2>");
      Ada.Text_IO.Put_Line ("<b>" & Natural'Image (Integer (Req_List.Length))
                            & " requirements </b>");
      Ada.Text_IO.Put_Line ("tested by <b>" & Natural'Image (Test_Cases_N) &
                           " tests</b> for each target language");
   end Print;

end Doc_Generator.Requirements;
