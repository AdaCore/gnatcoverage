with Ranges, Support; use Ranges, Support;

-- Exercise various situations where two ranges don't overlap.
-- Exempted precondition evaluated True only.

procedure Test_Ranges_No_Overlap is
   Ra, Rb : XYrange;
begin
   -- |-- Ra --|
   --            |-- Rb --|
   Set (Ra, 1, 4);
   Set (Rb, 5, 8);
   Assert (not Overlap (Ra, Rb));

   --            |-- Ra --|
   -- |-- Rb --|
   Set (Ra, 5, 8);
   Set (Rb, 1, 3);
   Assert (not Overlap (Ra, Rb));
end;

--# ranges.adb
--  /checkValid/    l! ## dT-
--  /assignValid/   l+ ## 0
--  /assignInvalid/ l- ## s-
--  /preValid/      l* ## x+
--  /checkOverlap/  l! ## eT-

