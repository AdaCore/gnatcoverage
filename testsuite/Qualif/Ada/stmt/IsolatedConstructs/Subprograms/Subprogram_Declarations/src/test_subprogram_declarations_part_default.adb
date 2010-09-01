--  Test driver for subprogram declarations and subprogram body declarations.
--  It "with's" the functional code and  executes only a part of it. The goal
--  of this test is to check that implicit calls resulted from evaluating
--  default initialization expressions makes the code of the called subprograms
--  to be reported as covered. This test does not include one indirect
--  subprogram call by means of access-to-procedure value

with Subprogram_Pack; use Subprogram_Pack;
with Support;         use Support;
procedure Test_Subprogram_Declarations_Part_Default is
   R : Rec;    --  implicit call to Fun1
   I : Integer;
begin
   Assert (R.I = 0);

   Proc2 (I);  --  implicit call to Fun2
   Assert (I = 7);

end Test_Subprogram_Declarations_Part_Default;

--# subprogram_pack.adb
-- /fun1/       l+ 0
-- /fun2/       l+ 0
-- /proc1/      l- s-
-- /proc2/      l+ 0
-- /proc3/      l- s-
-- /proc4/      l- s-
-- /local_proc/ l+ 0
-- /local_fun/  l+ 0
