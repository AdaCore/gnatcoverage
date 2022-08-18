with Forsome, Expr, Support; use Expr, Support;

-- Outer "for some" True only, from predicate True on first item

procedure Test_Forsome_T_TX is
   X : Integer := 0;
begin
   Assert (Forsome ((5, X), GT_3'access));
end;

--# forsome.adb
--  /eval/ l! ## dF-:"for some", dF-:"P ("
--  /true/ l+ ## 0
--  /false/ l- ## s-

-- Decision coverage with bin traces is imprecise on simple expressions

-- %opts: --trace-mode=bin
-- =/eval/ l! ## dF-:"for some", d!:"P ("
