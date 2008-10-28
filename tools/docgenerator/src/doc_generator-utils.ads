with Ada.Text_IO;

package Doc_Generator.Utils is
   use Ada.Text_IO;

   procedure Parse_File_List
     (Path : String;
      Parsing_Procedure : access procedure (F : File_Type));

end Doc_Generator.Utils;
