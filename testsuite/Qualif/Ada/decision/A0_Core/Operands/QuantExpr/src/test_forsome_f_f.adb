with Forsome, Expr, Support; use Expr, Support;

-- Outer "for some" decision False only, Predicate False only

procedure Test_Forsome_F_F is
begin
   Assert (not Forsome ((1, 2), GT_3'access));
end;

--# forsome.adb
--  /eval/ l! ## dT-:"for some", dT-:"P ("
--  /true/ l- ## s-
--  /false/ l+ ## 0

-- Decision coverage with bin traces is imprecise on simple expressions

-- %opts: --trace-mode=bin
-- =/eval/ l! ## dT-:"for some", d!:"P ("
