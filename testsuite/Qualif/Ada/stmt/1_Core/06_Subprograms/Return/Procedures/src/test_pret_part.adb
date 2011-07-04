--  Test driver for return statements. It executes a part of the functional
--  code, so some return statements are expected to be reported as uncovered
--  and some other as uncovered.

with Pret; use Pret;
with Support;           use Support;
procedure Test_Pret_Part is
   Int : Integer := 0;
begin
   Proc1 (1, Int);
   Assert (Int = 2);

   Proc2 (1, Int);
   Assert (Int = 2);

   Int := Fun1 (1);
   Assert (Int = 1);

   Int := Fun2 (0);
   Assert (Int = 0);

end Test_Pret_Part;

--# pret.adb
-- /proc1_1/            l+ 0
-- /proc1_return/       l- s-
-- /proc1_after_return/ l+ 0

-- /proc2_start/        l+ 0
-- /proc2_1/            l+ 0
-- /proc2_2/            l- s-
-- /proc2_3/            l- s-
-- /proc2_others/       l- s-
-- /proc2_fin/          l- s-

