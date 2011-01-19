function C3 (A, B, C : Boolean) return Boolean;

-- (A and then B) or else C

-- Vector # assignments, grouping cases that differ only on
-- unevaluated conditions:

--   A   B   C    F()   Vec#
--   ----------  ----
--   T   T  (X)   T      1
--   T   F   T    T      2
--   T   F   F    F      3
--   F  (X)  T    T      4
--   F  (X)  F    F      5


-- Independance pairs (uc + masking):

-- ip(A) = 1,5
-- ip(B) = 1,3
-- ip(C) = 2,3 4,5 + 2,5 3,4

