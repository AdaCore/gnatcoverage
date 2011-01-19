function Porpand (A, B, C : Boolean) return Boolean;

-- (A or else B) and then C

-- Vector # assignments, grouping cases that differ only on
-- unevaluated conditions:

--   A   B   C    F()   Vec#
--   ----------  ----
--   F   F  (X)   F      1
--   F   T   F    F      2
--   F   T   T    T      3
--   T  (X)  F    F      4
--   T  (X)  T    T      5


-- Independance pairs (uc + masking):

-- ip(A) = 1,5
-- ip(B) = 1,3
-- ip(C) = 2,3 4,5 + 2,5 3,4
