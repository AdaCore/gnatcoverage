with Pkg;

--  Fully cover both the contract and the decision in the body.

procedure Test_Pkg is
   Dummy : Boolean;
begin
   Dummy := Pkg.F (True, True);
   Dummy := Pkg.F (True, False);
   Dummy := Pkg.F (False, False);
end Test_Pkg;

--# pkg.adb
-- /pre/    l+ ## 0
-- /assert/ l+ ## 0
-- /ret/    l+ ## 0
