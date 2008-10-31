with Ada.Strings.Fixed;

package body Doc_Generator.Utils is
   use Ada.Text_IO;

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
      S2 : String := Trim (S1, Both);
      Pos1 : Natural := Index (S2, " ");
      Pos2 : Natural := Index (S2, "(");
      Pos : Natural := 0;
   begin
      if Pos1 >= Pos2 then
         Pos := Pos1;
      else
         Pos := Pos2;
      end if;

      if Pos = 0 then
         return S2;
      end if;

      return Delete (S2, Pos, S2'Length);

   end Get_Procedure_Name;

end Doc_Generator.Utils;
