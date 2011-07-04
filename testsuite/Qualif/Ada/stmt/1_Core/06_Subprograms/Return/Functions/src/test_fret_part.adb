--  Test driver for return statements. It executes a part of the functional
--  code, so some return statements are expected to be reported as uncovered
--  and some other as uncovered.

with Fret; use Fret;
with Support;           use Support;
procedure Test_Fret_Part is
   Int : Integer := 0;
begin
   Int := Fun1 (1);
   Assert (Int = 1);

   Int := Fun2 (0);
   Assert (Int = 0);

end Test_Fret_Part;

--# fret.adb
-- /fun1_start/         l+ 0
-- /fun1_first_return/  l+ 0
-- /fun_1_fin/          l- s-

-- /fun2_start/         l+ 0
-- /fun2_1_return/      l- s-
-- /fun2_1_elsif/       l+ 0
-- /fun2_2_return/      l- s-
-- /fun2_2_elsif/       l+ 0
-- /fun2_case/          l- s-
-- /fun2_3_return/      l- s-
-- /fun2_4_return/      l- s-
-- /fun2_others/        l- s-
-- /fun_2_fin/          l+ 0
