with Ranges, Support; use Ranges, Support;

-- Exercise various situations where two ranges overlap.
-- Exempted precondition evaluated True only.

procedure Test_Ranges_Overlap is
   Ra, Rb : XYrange;
begin
   -- |---- Ra ----|
   --     |---- Rb ----|

   Set (Ra, 1, 4);
   Set (Rb, 2, 6);
   Assert (Overlap (Ra, Rb));

   --     |---- Ra ----|
   -- |--- Rb ---|

   Set (Ra, 1, 4);
   Set (Rb, 0, 3);
   Assert (Overlap (Ra, Rb));

   -- |---- Ra ----|
   --   |-- Rb --|

   Set (Ra, 1, 4);
   Set (Rb, 2, 3);
   Assert (Overlap (Ra, Rb));

   --   |-- Ra --|
   -- |---- Rb ----|

   Set (Ra, 2, 3);
   Set (Rb, 1, 4);
   Assert (Overlap (Ra, Rb));
end;

--# ranges.adb
--  /checkValid/    l! d!
--  /assignValid/   l+ 0
--  /assignInvalid/ l- s-
--  /preValid/      l! dF-
--  /evalOverlap/   l! dF-

