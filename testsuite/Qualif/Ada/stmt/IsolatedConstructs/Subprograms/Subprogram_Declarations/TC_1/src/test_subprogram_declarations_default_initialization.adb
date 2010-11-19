--  Test driver for subprogram declarations and subprogram body declarations.
--  It creates a record object, and the elaboration of this declaration issues
--  a function call as a part of the elaboration of the object declaration. It
--  also calls a subprogram using the default value for one of the parameters,
--  evaluation of the corresponding default expression issues another function
--  call. So the code of the functions is expected to be reported as covered,
--  and the code of procedures - as uncovered.

with Subprogram_Pack; use Subprogram_Pack;
with Support;         use Support;
procedure Test_Subprogram_Declarations_Default_Initialization is
   R : Rec;    --  implicit call to Fun1
   I : Integer;
begin
   Assert (R.I = 0);

   Proc2 (I);  --  implicit call to Fun2
   Assert (I = 7);

end Test_Subprogram_Declarations_Default_Initialization;

--# subprogram_pack.adb
-- /fun1/       l+ 0
-- /fun2/       l+ 0
-- /proc1/      l- s-
-- /proc2/      l+ 0
-- /proc3/      l- s-
-- /proc4/      l- s-
-- /local_proc/ l+ 0
-- /local_fun/  l+ 0
