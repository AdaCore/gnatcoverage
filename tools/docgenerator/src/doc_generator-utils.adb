package body Doc_Generator.Utils is
   use Ada.Text_IO;

   procedure Parse_File_List
     (Path : String;
      Parsing_Procedure : access procedure (F : File_Type))
   is
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

end Doc_Generator.Utils;
