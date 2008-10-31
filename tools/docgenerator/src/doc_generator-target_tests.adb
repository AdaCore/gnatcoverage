with Ada.Text_IO;
with Ada.Strings.Unbounded.Text_IO;

with Doc_Generator.Utils;
with Ada.Strings.Fixed;

package body Doc_Generator.Target_Tests is

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

   end Create_Target;

   procedure Parse_Targets (F : Ada.Text_IO.File_Type) is
      use Ada.Text_IO;
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
         Put_Line ("<b>ID: </b>" & T.ID & "<br/>");
         Put_Line ("<b>subprogram:</b> " & T.Subprogram
                   & " in " &
                   "<a href=""file:///" & To_String (T.In_File)
                   & """/>file</a><br/>");
         Put_Line ("<b>description:</b> " & T.Description);
      end Print_Target;

   begin
      Target_List.Iterate (All_Targets, Print_Target'Access);
   end Print;

end Doc_Generator.Target_Tests;

