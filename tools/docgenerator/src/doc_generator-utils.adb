with Ada.Strings.Fixed;
with Ada.Strings.Unbounded.Text_IO;

package body Doc_Generator.Utils is
   use Ada.Text_IO;

   procedure Color_Key_Word
     (Code : in out Ada.Strings.Unbounded.Unbounded_String);

   procedure Parse_File_List
     (Path : String;
      Parsing_Procedure : access procedure (F : File_Type))
   is
      use Ada.Strings;
      F : File_Type;
   begin
      Open (F, IN_FILE, Path);
      while not End_Of_File (F) loop
         declare
            F_Name : String := Get_Line (F);
            Data_File : File_Type;
         begin
            Open (Data_File, IN_FILE, F_Name);
            Parsing_Procedure.all (Data_File);
            Close (Data_File);
         end;
      end loop;
      Close (F);
   end Parse_File_List;

   function Get_Procedure_Name (S : String) return String is
      use Ada.Strings.Fixed;
      use Ada.Strings;
      S1 : String := Remove (S, "procedure");
      S1b : String := Remove (S1, "end");
      S2 : String := Trim (S1b, Both);
      Pos1 : Natural := Index (S2, " ");
      Pos2 : Natural := Index (S2, "(");
      Pos3 : Natural := Index (S2, ";", Backward);
      Pos : Natural := 0;
   begin
      if Pos1 >= Pos2 then
         if Pos3 >= Pos1 then
            Pos := Pos3;
         else
            Pos := Pos1;
         end if;
      else
         Pos := Pos2;
      end if;

      if Pos = 0 then
         return Trim (S2, Both);
      end if;

      return Trim (Delete (S2, Pos, S2'Length), Both);

   end Get_Procedure_Name;

   procedure Get_And_Analyse_Procedure_Code
     (F : File_Type;
      Analyse : access procedure
        (P_Name : String;
         Code : Unbounded_String)) is
      use Ada.Strings.Unbounded;
      Pos : Natural := 0;
      Pos_Old : Positive := 1;
      Procedure_Found : Boolean := False;
      Current_Procedure_Name : Ada.Strings.Unbounded.Unbounded_String;
      Code : Unbounded_String;
      Line_Number : Positive := 1;
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      while not End_Of_File (F) loop
         declare
            Line : String := Get_Line (F);
         begin
            if Procedure_Found then

               Code := Code & " <br/> " & Positive'Image (Line_Number)
                 & ".    " & Line;
               Line_Number := Line_Number + 1;

               Pos := Ada.Strings.Fixed.Index (Line, "end");

               if Pos > 0 then

                  --  Ada.Strings.Unbounded.Text_IO.Put_Line
                  --  (Current_Procedure_Name & " : " & Line & " : " &
                  --  Get_Procedure_Name (Line) & "***<br/>");

                  if Get_Procedure_Name (Line) = Current_Procedure_Name then

                     --  Ada.Strings.Unbounded.Text_IO.Put_Line
                     --  ("QUI!!! : " & Current_Procedure_Name & "<br/>");

                     Procedure_Found := False;
                     Color_Key_Word (Code);
                     Analyse.all (To_String (Current_Procedure_Name), Code);

                  end if;

               end if;

            else

               Pos := Ada.Strings.Fixed.Index (Line, "procedure");
               if Pos > 0 then
                  Line_Number := 2;
                  Procedure_Found := True;
                  Current_Procedure_Name :=
                    To_Unbounded_String (Get_Procedure_Name (Line));
                  Code := "1.    " & To_Unbounded_String (Line);
               end if;
            end if;
         end;
      end loop;
   end Get_And_Analyse_Procedure_Code;

   procedure Color_Key_Word
     (Code : in out Ada.Strings.Unbounded.Unbounded_String) is
      Pos1 : Natural := Index (Code, " ") + 1;
      Pos2 : Natural := Index (Code, " ", Pos1);

   begin

      while Pos1 /= 0 and then Pos2 /= 0 loop
         Pos2 := Pos2 - 1;
         declare
            Slice : String := Ada.Strings.Unbounded.Slice (Code, Pos1, Pos2);
         begin
            if Key_Word_Set.Contains (Slice) then
               Replace_Slice (Code, Pos1, Pos2, "<b>" & Slice & "</b>");
            end if;
         end;
         Pos1 := Index (Code, " ", Pos2 + 1) + 1;
         Pos2 := Index (Code, " ", Pos1);
      end loop;
   exception
         when Constraint_Error => Put_Line ("EXCEPTION!!!");
   end Color_Key_Word;

begin

   Key_Word_Set.Insert ("procedure");
   Key_Word_Set.Insert ("in");
   Key_Word_Set.Insert ("in out");
   Key_Word_Set.Insert ("out");
   Key_Word_Set.Insert ("is");
   Key_Word_Set.Insert ("end");
   Key_Word_Set.Insert ("for");
   Key_Word_Set.Insert ("if");
   Key_Word_Set.Insert ("if;");
   Key_Word_Set.Insert ("else");
   Key_Word_Set.Insert ("then");
   Key_Word_Set.Insert ("while");
   Key_Word_Set.Insert ("loop");
   Key_Word_Set.Insert ("loop;");
   Key_Word_Set.Insert ("exit");
   Key_Word_Set.Insert ("when");
   Key_Word_Set.Insert ("case");
   Key_Word_Set.Insert ("and");
   Key_Word_Set.Insert ("or");
   Key_Word_Set.Insert ("begin");
   Key_Word_Set.Insert ("not");
   Key_Word_Set.Insert ("null;");

end Doc_Generator.Utils;
