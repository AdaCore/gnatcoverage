pragma Ada_2012;

with Foo;

procedure Test_Foo is

   procedure Null_Proc (Param : Integer) is null -- # null_proc
   with Inline;

begin

   Null_Proc (Foo.Fact (0));

end Test_Foo;

--# foo.ads
--
--%opts: --trace-mode=src
-- /expr_func/ l! ## dT-
--%opts: --trace-mode=bin
-- /expr_func/ l! ## d!