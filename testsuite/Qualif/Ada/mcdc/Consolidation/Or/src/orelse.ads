function Orelse (A, B : Boolean) return Boolean;

-- Vector # assignments, followin two-bit values:

--   A   B   Or   Vec#
--   -----------   ----
--   F   F   F      0
--   F   T   T      1
--   T  (F)  T      2
--   T  (T)  T      3

-- Independance pairs:

-- ip(A) = 0,2 0,3
-- ip(B) = 0,1


