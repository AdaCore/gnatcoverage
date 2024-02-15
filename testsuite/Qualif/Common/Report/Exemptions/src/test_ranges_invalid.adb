with Ranges, Support; use Ranges, Support;

-- Exercise a single overlap check, involving an invalid range.
-- Exempted precondition evaluated False only.

procedure Test_Ranges_Invalid is
   Ra, Rb : XYrange;
begin
   Set (Ra, 4, 1); -- invalid
   Set (Rb, 1, 2);

   Assert (Overlap (Ra, Rb));
end;

--# ranges.adb
--  /checkValid/    l+ ## 0
--  /assignValid/   l+ ## 0
--  /assignInvalid/ l+ ## 0
--  /preValid/      s=>l#, dum=>l* ## s=>x0, dum=>x+
--  /preValid_if/   s=>l=, dum=>l= ## s=>X0, dum=>XoF-
--  /preValid_inc/  s=>l=, dum=>l= ## s=>X0, dum=>X0
--  /preValid_ret/  s=>l=, dum=>l= ## s=>X0, dum=>X0
--  /checkOverlap/  l- ## s-
--  /overlapTrue/   l- ## s-
--  /overlapFalse/  l- ## s-
