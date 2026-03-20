with Support; use Support;
with Pkg;

procedure Test_F is
   All_F : Pkg.My_Arr := (False, False, False);
begin
   Assert (not Pkg.All_True (All_F));
   Assert (not Pkg.Some_True (All_F));
end Test_F;

--# pkg.adb
--
-- /decl/ l! ## dT-
-- /ret/  l+ ## 0
--
-- Decision coverage with bin traces is imprecise on simple expressions
--
-- %opts: --trace-mode=bin
-- =/decl/ l! ## d!
