--  Test driver for inlined subprograms. It executes all the functional code,
--  so everything in the functional code is expected to be reported as covered.

with Inlined_Subprograms; use Inlined_Subprograms;
with Support;             use Support;
procedure Test_Inlined_Subprograms_Full is
   Int : Integer := 1;
begin
   Proc_No_Inline (Int);
   Assert (Int = 101);

   Int := Fun_No_Inline (0);
   Assert (Int = 1);
end Test_Inlined_Subprograms_Full;

--# inlined_subprograms.adb
-- /proc_no_inline/ l+ 0
-- /proc1/          l+ 0
-- /proc2/          l+ 0
-- /fun_no_inline/  l+ 0
-- /fun1/           l+ 0
-- /fun2/           l+ 0
