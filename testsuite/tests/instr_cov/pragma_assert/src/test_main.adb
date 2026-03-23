pragma Ada_2012;
with Pkg;

procedure Test_Main is

   --  We expect gnatcov to detect two decisions below: one for the whole
   --  IF expression, and one for its condition (Pkg.Foo).

   pragma Assert                        -- # assert_stmt
     ((if                               -- # inner_decision
      Pkg.Foo                           -- # inner_cond_1
      then Pkg.Bar) and then Pkg.Foo);  -- # inner_cond_2

begin
   for J in 1 .. 3 loop

      --  Regression test: gnatcov used to instrument Loop_Invariant as
      --  statements, by inserting a witness statement right before. This
      --  does not work, as Loop_Invariant pragmas must be groupped
      --  together.

      pragma Loop_Invariant (Pkg.Foo);  --# loop_invariant_1
      pragma Loop_Invariant (Pkg.Bar);  --# loop_invariant_2
   end loop;
end Test_Main;

--# test_main.adb
--
-- /assert_stmt/ l. ## 0
-- /inner_decision/ l. ## 0
-- /inner_cond_1/ l. ## 0
-- /inner_cond_2/ l. ## 0
-- /loop_invariant_1/ l. ## 0
-- /loop_invariant_2/ l. ## 0
