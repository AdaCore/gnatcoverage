--  Test driver for subprogram declarations and subprogram body declarations.
--  It "with's" the functional code and  executes only a part of it. There is
--  no subprogram calls resulted from evaluating default initialization
--  expressions in this test. This test includes one indirect subprogram call
--  by means of access-to-procedure value.

with Library_Level_Fun;
with Library_Level_Proc;
with Subprogram_Pack; use Subprogram_Pack;
with Support;         use Support;
procedure Test_Subprogram_Declarations_Part is
   I           : Integer;
   Ref_To_Proc : Access_To_Proc;
begin
   I := 0;
   Library_Level_Proc (I);
   Assert (I = 1);

   I := Fun1 (10);
   Assert (I = 9);

   Proc1 (I);
   Assert (I = 6);

   Ref_To_Proc := Proc3'Access;
   Ref_To_Proc (I);
   Assert (I = 12);

end Test_Subprogram_Declarations_Part;

--# library_level_fun.adb
-- /fun/        l- s-

--# library_level_proc.adb
-- /proc/       l+ 0

--# subprogram_pack.adb
-- /fun1/       l+ 0
-- /fun2/       l- s-
-- /proc1/      l+ 0
-- /proc2/      l- s-
-- /proc3/      l+ 0
-- /proc4/      l- s-
-- /local_proc/ l- s-
-- /local_fun/  l+ 0
