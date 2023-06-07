with Pkg; use Pkg;

procedure Main is
begin
   Not_Executed_Branch;
   Partially_Covered_Branch;
   Fully_Covered_Branch (False);
   Fully_Covered_Branch (True);
end Main;
