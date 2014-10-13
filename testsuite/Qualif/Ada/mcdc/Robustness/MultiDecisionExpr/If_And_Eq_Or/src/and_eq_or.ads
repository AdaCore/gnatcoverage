function And_Eq_Or (A, B, C, D : Boolean) return Boolean;

--    a b  a && b
-- 1) T T  T
-- 2) F X  F
-- 3) T F  F

--    c d  c || d
-- 1) F F  F
-- 2) T X  T
-- 3) F T  T


