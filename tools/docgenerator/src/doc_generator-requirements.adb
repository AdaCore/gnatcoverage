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

   procedure Create_Driver (R : in Requirement_Ref;
                            Line : String; F : File_Type);

   procedure Create_Requirement (R : in out Requirement_Ref;
                                 Line : String; F : File_Type);


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
               D.Targets.Append
                 (new Target '
                    (ID => Null_Unbounded_String,
                     Description => Null_Unbounded_String,
                     Subprogram => To_Unbounded_String (Sub),
                     Expected_Coverage => Exp));
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
              ("        " & T.Subprogram & " with expected result: " &
               Function_Coverage'Image (T.Expected_Coverage));
         end Print_Target;

      begin
         Ada.Strings.Unbounded.Text_IO.Put_Line
           ("    " & Dr.Subprogram & " exercising:");
         Dr.Targets.Iterate (Print_Target'Access);
      end Print_Driver;

      procedure Print_Req (It : Requirement_List.Cursor) is
         Req : Requirement_Ref := Requirement_List.Element (It);
      begin
         Ada.Strings.Unbounded.Text_IO.Put_Line ("Requirement " & Req.ID);
         Ada.Strings.Unbounded.Text_IO.Put_Line ("  " & Req.Description);
         Ada.Text_IO.Put_Line
           ("   checked by " & Natural'Image
              (Integer (Req.Drivers.Length)) & " drivers:");
         Req.Drivers.Iterate (Print_Driver'Access);
      end Print_Req;

   begin
      Req_List.Iterate (Print_Req'Access);
   end Print;

end Doc_Generator.Requirements;
