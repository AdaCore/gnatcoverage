with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Strings.Hash;

package Doc_Generator.Utils is
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   procedure Parse_File_List
     (Path : String;
      Parsing_Procedure : access procedure (F : File_Type));

   function Get_Procedure_Name (S : String) return String;

   procedure Get_And_Analyse_Procedure_Code
     (F : File_Type;
      Analyse : access procedure
        (P_Name : String; Code : Unbounded_String));

private

   package Key_Word_Set_Pkg is new Ada.Containers.Indefinite_Hashed_Sets
     (String,
      Ada.Strings.Hash,
      "=");

   Key_Word_Set : Key_Word_Set_Pkg.Set;

end Doc_Generator.Utils;
