with Aspect;

--  Check that when instrumenting an expression function with aspects, these
--  aspects are also correctly attached to both the new intermediary function
--  and to the augmented expression function.

procedure Test_Aspect is
begin
   Aspect;
end Test_Aspect;

--# aspect.adb
-- /expr/  l- ## s-
-- /type/  l+ ## 0
-- /dummy/ l+ ## 0
