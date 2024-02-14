with Ranges, Support; use Ranges, Support;

-- Exercise various situations where two ranges overlap.
-- Exempted precondition evaluated True only. raise uncovered.

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
--  /checkValid/    l! ## dT-
--  /assignValid/   l+ ## 0
--  /assignInvalid/ l- ## s-
--  /preValid/      l* ## x+
--  /preValid_if/   l= ## XoT-
--  /preValid_inc/  l= ## Xs-
--  /preValid_ret/  l= ## Xs-
--  /checkOverlap/  l! ## dF-
--  /overlapTrue/   l+ ## 0
--  /overlapFalse/  l- ## s-
