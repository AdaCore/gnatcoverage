with Support; use Support;
with Pkg;

procedure Test_T is
   All_T : Pkg.My_Arr := (True, True, True);
begin
   Assert (Pkg.All_True (All_T));
   Assert (Pkg.Some_True (All_T));
end Test_T;

--# pkg.adb
--
-- /decl/ l! ## dF-
-- /ret/  l+ ## 0
--
-- Decision coverage with bin traces is imprecise on simple expressions
--
-- %opts: --trace-mode=bin
-- =/decl/ l! ## d!
