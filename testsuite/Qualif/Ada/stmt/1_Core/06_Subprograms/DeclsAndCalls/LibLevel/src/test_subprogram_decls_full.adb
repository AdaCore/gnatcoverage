--  Test driver for subprogram declarations and subprogram body declarations.
--  It executes all the functional code, using explicit subprogram calls, calls
--  resulted from evaluating default initialization expressions and indirect
--  calls through access-to-subprogram value. All the functional code is
--  expected to be reported as covered.

with Library_Level_Fun;
with Library_Level_Proc;
with Subprogram_Pack; use Subprogram_Pack;
with Support;         use Support;
procedure Test_Subprogram_Decls_Full is
   R           : Rec;    --  implicit call to Fun1
   I           : Integer;
   Ref_To_Proc : Access_To_Proc;
begin
   Assert (R.I = 0);

   Proc2 (I);  --  implicit call to Fun2
   Assert (I = 7);

   I := 0;
   Library_Level_Proc (I);
   Assert (I = 1);

   I := Library_Level_Fun (I);
   Assert (I = 2);

   Proc1 (I);
   Assert (I = -1);

   Ref_To_Proc := Proc3'Access;
   Ref_To_Proc (I);
   Assert (I = -2);

   Ref_To_Proc := Proc4'Access;
   Ref_To_Proc (I);
   Assert (I = -6);

end Test_Subprogram_Decls_Full;

--# library_level_fun.adb
-- /fun/        l+ ## 0

--# library_level_proc.adb
-- /proc/       l+ ## 0

--# subprogram_pack.adb
-- /fun1/       l+ ## 0
-- /fun2/       l+ ## 0
-- /proc1/      l+ ## 0
-- /proc2/      l+ ## 0
-- /proc3/      l+ ## 0
-- /proc4/      l+ ## 0
-- /local_proc/ l+ ## 0
-- /local_fun/  l+ ## 0
