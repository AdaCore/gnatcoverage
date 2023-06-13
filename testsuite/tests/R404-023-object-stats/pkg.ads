package Pkg is
   procedure Not_Executed_Branch (B1, B2 : Boolean := False);
   procedure Partially_Covered_Branch (B : Boolean := False);
   procedure Fully_Covered_Branch (B : Boolean);
end Pkg;
