with Support; use Support;
with Values; use Values;

procedure Test_Range is
   R : Range_T := (Lb => 0, Ub => 5);
begin
   Assert (Locate (X => 3, I => R) = In_Range);
end;

--# values.adb
--  /range_decl/    l+ ## 0
--  /range_assign/  l+ ## 0
--  /range_expr/    l+ ## 0c
--  /range_return/  l+ ## 0

--  /point_decl/    l- ## s-
--  /point_assign/  l- ## s-
--  /point_return/  l- ## s-

--  /ikind_return/  l+ ## 0
--  /ikind_expr/    l+ ## 0c

--  /loc_return/    l+ ## 0
--  /loc_expr/      l+ ## 0c
