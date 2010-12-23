--  Test driver for subprogram declarations and subprogram body declarations.
--  It executes a part of the functional code. It calls subprograms from the
--  functional code using indirect calls by means access-to-subprogram values.
--  So for some subprograms the code in their bodies is expected to be reported
--  as covered, and for other subprograms - as uncovered.

with Library_Level_Fun;
with Library_Level_Proc;
with Subprogram_Pack; use Subprogram_Pack;
with Support;         use Support;
procedure Test_Subprogram_Decls_Indirect_Calls is
   I           : Integer;
   Ref_To_Proc : Access_To_Proc;
begin
   I := 0;
   Ref_To_Proc := Library_Level_Proc'Access;
   Ref_To_Proc (I);
   Assert (I = 1);

   I := 9;
   Ref_To_Proc := Proc1'Access;
   Ref_To_Proc (I);
   Assert (I = 6);

   Ref_To_Proc := Proc3'Access;
   Ref_To_Proc (I);
   Assert (I = 12);

   Ref_To_Proc := Proc4'Access;
   Ref_To_Proc (I);
   Assert (I = 36);

end Test_Subprogram_Decls_Indirect_Calls;

--# library_level_fun.adb
-- /fun/        l- s-

--# library_level_proc.adb
-- /proc/       l+ 0

--# subprogram_pack.adb
-- /fun1/       l- s-
-- /fun2/       l- s-
-- /proc1/      l+ 0
-- /proc2/      l- s-
-- /proc3/      l+ 0
-- /proc4/      l+ 0
-- /local_proc/ l- s-
-- /local_fun/  l- s-
