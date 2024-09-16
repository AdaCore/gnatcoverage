procedure Test_Main
is
   A, B : Boolean := True;

   procedure Foo (B: Boolean) is null;  -- # cov_on

   procedure Uncovered;

   procedure Uncovered is
   begin
      Foo (A and then B);               -- # cov_off
   end Uncovered;

begin
   Foo (A and then B);                  -- # cov_off

   Foo (A and then B);                  -- # cov_off

   Foo (A and then B);                  -- # partial_cov_on
end Test_Main;

--# test_main.adb
--
-- /cov_off/        lD ## 0
-- /cov_on/         l+ ## 0
-- /partial_cov_on/ l! ## eF-
