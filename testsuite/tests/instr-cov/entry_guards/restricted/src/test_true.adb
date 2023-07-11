with Pkg;

procedure Test_True is
   T : Pkg.PT;
begin
   T.Init (A => True, B => True);
   T.Wait_Cond;
end Test_True;
