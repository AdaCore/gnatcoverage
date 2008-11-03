with Ada.Text_IO;
with Ada.Strings.Unbounded.Text_IO;

with Doc_Generator.Utils;
with Ada.Strings.Fixed;

package body Doc_Generator.Target_Tests is
   use Ada.Text_IO;

   procedure Add_Code (F : File_Type);

   procedure Add_Code (F : File_Type) is
      use Doc_Generator.Utils;
      use Ada.Strings.Unbounded;

      procedure Analyze
        (P_Name : String;
         Code : Unbounded_String) is
         Tmp : Target_Ref := null;
         N : String := P_Name;
      begin
         To_Lower (N);
         Tmp := T_Map.Element (N);
         Tmp.Code := Code;
      exception
         when Constraint_Error =>
            Put_Line ("Problem: looking for " & P_Name);
      end Analyze;

   begin

      Get_And_Analyse_Procedure_Code
        (F, Analyze'Access);

   end Add_Code;

   procedure Create_Target (F : Ada.Text_IO.File_Type) is
      use Ada.Text_IO;
      use Ada.Strings.Unbounded;
      T : Target_Ref := new Target;
      Desc : Ada.Strings.Unbounded.Unbounded_String;
      Pos : Natural := 0;

      --  this function extract the name of the procedure
      --  from an Ada procedure declaration
      function Peek_Procedure_Name (S : String) return String is
         use Ada.Strings.Fixed;
         use Ada.Strings;
         Pos : Natural;
         S1 : String := Remove (S, "procedure");
         S2 : String := Trim (S1, Both);
         Div_Pos_1 : Natural := Index (Name (F), "\", Backward) + 1;
         Div_Pos_2 : Natural := Index (Name (F), "/", Backward) + 1;
         Div_Pos : Natural;
         Ext_Pos : Natural := Index (Name (F), ".ad", Backward) - 1;

      begin

         --  cope with multiple OS
         --  probably there should be something similar to Java
         --  getFileSeparator() ?
         if Div_Pos_1 > 0 then
            Div_Pos := Div_Pos_1;
         else
            Div_Pos := Div_Pos_2;
         end if;

         declare
            --  add the name of the file to get the absolute procedure name
            Pr : String :=
              Ada.Strings.Unbounded.Slice
                (Ada.Strings.Unbounded.To_Unbounded_String (Name (F)),
                 Div_Pos, Ext_Pos);
            --  change "-" in the file name with Ada counterpart
            Prefix : String := Replace_All (Pr, "-", ".");
         begin
            To_Lower (Prefix);
            To_Lower (S2);

            Pos := Index (S2, " ");
            if Pos > 0 then
               return Prefix & "." & Delete (S2, Pos, S2'Length);
            end if;

            return Prefix & "." & S2;

         end;

      end Peek_Procedure_Name;

   begin

      T.In_File := To_Unbounded_String (Name (F));

      while not End_Of_File (F) loop
         declare
            Line : String := Get_Line (F);
         begin
            --  look for the Id_Tag
            Starts_With (Line, Id_Tag, Pos);
            if Pos > 0 then
               T.ID := To_Unbounded_String
               (Ada.Strings.Fixed.Trim
                  (Remove (Remove (Line, Id_Tag), "}"), Ada.Strings.Both));
               exit;
            end if;
            Desc := Desc & " " & Ada.Strings.Fixed.Trim
              (Remove (Line, "--"), Ada.Strings.Both);
         end;
      end loop;
      T.Description := Desc;

      --  look for procedure declaration
      while not End_Of_File (F) loop
         declare
            Line : String := Get_Line (F);
         begin
            Starts_With (Line, "procedure", Pos);
            if Pos > 0 then
               T.Subprogram := To_Unbounded_String
                 (Peek_Procedure_Name (Line));
               exit;
            end if;
         end;
      end loop;

      All_Targets.Append (T);

      declare
         Last_Pos_Of_Point : Natural :=
           Ada.Strings.Unbounded.Index
             (T.Subprogram, ".", Ada.Strings.Backward) + 1;
         Relative_Proc_Name : String :=
           Ada.Strings.Unbounded.Slice
             (T.Subprogram, Last_Pos_Of_Point,
              Ada.Strings.Unbounded.Length (T.Subprogram));
      begin
         --  Ada.Text_IO.Put_Line ("Insert " & Relative_Proc_Name);
         T_Map.Insert (Relative_Proc_Name, T);
      end;

   end Create_Target;

   procedure Parse_Targets (F : Ada.Text_IO.File_Type) is
      use Ada.Text_IO;
      use Ada.Strings.Fixed;
   begin
      while not End_Of_File (F) loop
         declare
            Line : String := Get_Line (F);
            Pos : Natural := 0;
         begin
            Starts_With (Line, Target_Tag, Pos);
            if Pos > 0 then
               Create_Target (F);
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
   end Parse_Targets;

   procedure Parse_File (Path : String) is
   begin
      Doc_Generator.Utils.Parse_File_List
        (Path, Parse_Targets'Access);
   end Parse_File;

   procedure Print is

      procedure Print_Target (It : Target_List.Cursor) is
         use Ada.Strings.Unbounded.Text_IO;
         use Ada.Strings.Unbounded;
         T : Target_Ref := Target_List.Element (It);
      begin
         Put_Line ("<h3>Test case <a name=""" & T.Subprogram & """ id=""" &
                   T.Subprogram & """>" & T.ID & "</a></h3>");
         Put_Line ("<table class=""summary"">");
         Put_Line ("<tr><td><b>ID: </b></td><td>" & T.ID & "</td></tr>");
         Put_Line ("<tr><td><b>subprogram:</b></td><td>" & T.Subprogram
                   & " in " &
                   "<a href=""file:///" & To_String (T.In_File)
                   & """/>file</a></td></tr>");
         Put_Line ("<tr><td><b>description:</b></td><td>" & T.Description
                   & "</td></tr>");
         Put_Line ("</table>");

         Ada.Strings.Unbounded.Text_IO.Put_Line
           ("<a href=""#" & T.ID & """ onclick=""showhide('" &
            T.ID & "_Code');"">" & "Show code" &
            "</a>");
         Ada.Strings.Unbounded.Text_IO.Put_Line
           ("<div id=""" & T.ID & "_Code"" style=""display: none;"">");
         Ada.Text_IO.Put_Line
            ("<table border=""1"" cellspacing=""1""" &
            ">");
         Ada.Text_IO.Put_Line ("<tr class=""code"">" &
                               "<td>");
         Ada.Strings.Unbounded.Text_IO.Put_Line (T.Code);
         Ada.Text_IO.Put_Line ("</td></tr>");
         Ada.Text_IO.Put_Line ("</table>");
         Ada.Text_IO.Put_Line ("</div>");

      end Print_Target;

   begin
      Target_List.Iterate (All_Targets, Print_Target'Access);
   end Print;

end Doc_Generator.Target_Tests;

