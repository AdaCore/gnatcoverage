with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is

   procedure Not_Executed_Branch (B1, B2 : Boolean := False) is
   begin
      if B1 then
         if B2 then
            Put_Line ("Not_Executed_Branch");
         end if;
      end if;
   end Not_Executed_Branch;

   procedure Partially_Covered_Branch (B : Boolean := False) is
   begin
      if B then
         Put_Line ("Partially_Covered_Branch");
      end if;
   end Partially_Covered_Branch;

   procedure Fully_Covered_Branch (B : Boolean) is
   begin
      if B then
         Put_Line ("Fully_Covered_Branch: True");
      else
         Put_Line ("Fully_Covered_Branch: False");
      end if;
   end Fully_Covered_Branch;

end Pkg;
