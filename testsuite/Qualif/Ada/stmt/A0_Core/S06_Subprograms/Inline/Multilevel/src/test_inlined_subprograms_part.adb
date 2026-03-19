--  Test driver for inlined subprograms. It executes a part of the functional
--  code, so some code from subprogram bodies is expected to be reported as
--  uncovered and some - as covered.

with Inlined_Subprograms; use Inlined_Subprograms;
with Support;             use Support;
procedure Test_Inlined_Subprograms_Part is
   Int : Integer := 1;
begin
   Proc_No_Inline (Int);
   Assert (Int = 101);
end Test_Inlined_Subprograms_Part;

--# inlined_subprograms.adb
-- /proc_no_inline/ l+ ## 0
-- /proc1/          l+ ## 0
-- /proc2/          l- ## s-
-- /fun_no_inline/  l- ## s-
-- /fun1/           l+ ## 0
-- /fun2/           l- ## s-
