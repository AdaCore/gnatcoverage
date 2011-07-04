--  Test driver for return statements. It executes all the functional code, so
--  everything in the functional code is expected to be reported as covered.

with Pret; use Pret;
with Support;           use Support;
procedure Test_Pret_Full is
   Int : Integer := 0;
begin
   Proc1 (1, Int);
   Assert (Int = 2);

   Proc1 (0, Int);
   Assert (Int = 0);

   Proc2 (1, Int);
   Assert (Int = 2);

   Proc2 (2, Int);
   Assert (Int = 4);

   Proc2 (3, Int);
   Assert (Int =7 );

   Proc2 (4, Int);
   Assert (Int = 0);

end Test_Pret_Full;

--# pret.adb
-- /proc1_1/            l+ 0
-- /proc1_return/       l+ 0
-- /proc1_after_return/ l+ 0

-- /proc2_start/        l+ 0
-- /proc2_1/            l+ 0
-- /proc2_2/            l+ 0
-- /proc2_3/            l+ 0
-- /proc2_others/       l+ 0
-- /proc2_fin/          l+ 0

