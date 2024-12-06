with Foo;

--  Check that when instrumenting an expression function with aspects, these
--  aspects are also correctly attached to both the new intermediary function
--  and to the augmented expression function.

procedure Test_Aspect is
begin
   Foo;
end Test_Aspect;

--# aspect.ads
-- /expr/  l- ## s-
--# foo.adb
-- /d/ l+ ## 0
