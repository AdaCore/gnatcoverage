with Ada.Text_IO;

package Doc_Generator.Utils is
   use Ada.Text_IO;

   procedure Parse_File_List
     (Path : String;
      Parsing_Procedure : access procedure (F : File_Type));

   function Get_Procedure_Name (S : String) return String;

end Doc_Generator.Utils;
