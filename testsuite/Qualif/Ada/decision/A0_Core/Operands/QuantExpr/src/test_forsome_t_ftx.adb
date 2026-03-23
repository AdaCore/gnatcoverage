with Forsome, Expr, Support; use Expr, Support;

-- Outer "for some" True only, from predicate True on middle item

procedure Test_Forsome_T_FTX is
   X : Integer := 0;
begin
   Assert (Forsome ((2, 5, X), GT_3'access));
end;

--# forsome.adb
--  /eval/ l! ## dF-:"for some"
--  /true/ l+ ## 0
--  /false/ l- ## s-
