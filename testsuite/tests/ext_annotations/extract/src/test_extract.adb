with Pkg;

procedure Test_Extract is
   function Classify (X : Integer) return Integer;
   pragma Import (C, Classify, "classify");

   Ok    : Boolean;
   Dummy : Integer;
begin
   Pkg.Check (20, Ok);
   Pkg.Skipped (1, Dummy);
   Dummy := Classify (20);
end Test_Extract;
