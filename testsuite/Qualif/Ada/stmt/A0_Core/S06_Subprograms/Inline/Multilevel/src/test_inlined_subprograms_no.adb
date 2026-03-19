--  Test driver for inlined subprograms. It only "with's" the functional code,
--  but does not execute anything from it, so everything is expected to be
--  reported as uncovered.

with Inlined_Subprograms; use Inlined_Subprograms;
with Support;             use Support;
procedure Test_Inlined_Subprograms_No is
begin
   Assert (True);
end Test_Inlined_Subprograms_No;

--# inlined_subprograms.adb
-- /proc_no_inline/ l- ## s-
-- /proc1/          l- ## s-
-- /proc2/          l- ## s-
-- /fun_no_inline/  l- ## s-
-- /fun1/           l- ## s-
-- /fun2/           l- ## s-
