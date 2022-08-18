with Forall, Expr, Support; use Expr, Support;

-- Outer "for all" decision False only, Predicate False only

procedure Test_Forall_F_F is
begin
   -- for-all short-circuited by False on the first item in the sequence
   Assert (not Forall ((1, 5, 6), GT_3'Access));
end;

--# forall.adb
--  /eval/ l! ## dT-:"for all", dT-:"P ("
--  /true/ l- ## s-
--  /false/ l+ ## 0

-- Decision coverage with bin traces is imprecise on simple expressions

-- %opts: --trace-mode=bin
-- =/eval/ l! ## dT-:"for all", d!:"P ("
