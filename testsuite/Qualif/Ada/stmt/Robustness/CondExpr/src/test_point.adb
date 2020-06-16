with Support; use Support;
with Values; use Values;

procedure Test_Point is
   P : Point_T := (V => 5);
begin
   Assert (Locate (X => 2, I => P) = Out_Range);
end;

--# values.adb
--  /range_decl/    l- ## s-
--  /range_assign/  l- ## s-
--  /range_expr/    l- ## 0c
--  /range_return/  l- ## s-

--  /point_decl/    l+ ## 0
--  /point_assign/  l+ ## 0
--  /point_return/  l+ ## 0

--  /ikind_return/  l+ ## 0
--  /ikind_expr/    l+ ## 0c

--  /loc_return/    l+ ## 0
--  /loc_expr/      l+ ## 0c
