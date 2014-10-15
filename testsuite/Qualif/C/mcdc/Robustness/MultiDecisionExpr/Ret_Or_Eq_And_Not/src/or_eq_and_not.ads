function Or_Eq_And_Not (A, B, C, D : Boolean) return Boolean;
pragma Import (C, Or_Eq_And_Not, "oreqandnot");

--    A  B   (A || B)
-- 1) F  F   F         A  B
-- 2) F  T   T            B
-- 3) T  X   T         A

--    C  D   (C && !D)
-- 1) T  F   T         C  D
-- 2) F  X   F         C
-- 3) T  T   F            D
