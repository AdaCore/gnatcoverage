with Support; use Support;
with Values; use Values;

procedure Test_0 is
begin
   null;
end;

--# values.adb
--  /range_decl/    l- ## s-
--  /range_assign/  l- ## s-
--  /range_expr/    l- ## 0c
--  /range_return/  l- ## s-

--  /point_decl/    l- ## s-
--  /point_assign/  l- ## s-
--  /point_return/  l- ## s-

--  /loc_return/    l- ## s-
--  /loc_expr/      l- ## 0c

--  /ikind_return/    l- ## s-
--  /ikind_expr/      l- ## 0c

-- Old compilers are weak on mere declarations, possibly
-- keyed on optimization.

-- %tags:7.1.2
-- =/range_decl/    ~l- ## ~s-
-- =/point_decl/    ~l- ## ~s-
