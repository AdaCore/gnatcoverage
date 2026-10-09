pragma Flare_0_1;
with Pkg;

procedure Test_No_Early_Return is
begin
   Pkg.Proc_Finally (False);
   Pkg.Decl_Finally (False, False);
   Pkg.Begin_Finally (False, False);
end Test_No_Early_Return;

--#  pkg.adb
--   /finally/ l+ ## 0
--
--   /ret-.*/  l- ## s-
