pragma Ada_2012;

with Support; use Support;

with Foo; use Foo;

procedure Test_Foo is
begin
   Obj.Set (3);
   Assert (Obj.Get = 3);
   Assert (Obj.Cond);
   Obj.Do_Nothing (0);
   Obj.Set (-3);
   Assert (not Obj.Cond);
end Test_Foo;

--# foo.adb

--  /null_proc/ l+ ## 0
--  /int_expr/  l+ ## 0
--  /bool_expr/ l! ## c!:"Val = -5"

--  Declarations in protected type private parts only started getting
--  SCOS from the compiler after the 22 release branch, and the gnatcov
--  instrumenter was adjusted accordingly just before the branch.
--
--  So, for binary traces only, we can't count on a SCO until GNAT Pro 23.
--
--  We don't have a way to check for versions in a flexible manner and
--  this is very minor so just stick a weak expectation at this stage.

--# foo.ads

--  /decl/      ~l+ ## 0
