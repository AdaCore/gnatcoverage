--  Test driver for return statements. It only "with's" the functional code,
--  but does not execute anything from it, so everything is expected to be
--  reported as uncovered.

with Pret; use Pret;
with Support;           use Support;
procedure Test_Pret_No is
begin
   Assert (True);
end Test_Pret_No;

--# pret.adb
-- /proc1_1/            l- ## s-
-- /proc1_return/       l- ## s-
-- /proc1_after_return/ l- ## s-
-- /proc2_start/        l- ## s-
-- /proc2_1/            l- ## s-
-- /proc2_2/            l- ## s-
-- /proc2_3/            l- ## s-
-- /proc2_others/       l- ## s-
-- /proc2_fin/          l- ## s-
