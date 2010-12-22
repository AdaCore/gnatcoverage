with Ranges, Support, Silent_Last_Chance; use Ranges, Support;

-- Exercise a single overlap check, involving an invalid range.
-- Exempted precondition evaluated False only.

procedure Test_Ranges_Invalid is
   Ra, Rb : XYrange;
   Ov : Boolean;
begin
   Set (Ra, 4, 1); -- invalid
   Set (Rb, 1, 2);

   --  One call to Overlap, with invalid range Ra.
   --  Invoke silent last chance handler.

   Ov := Overlap (Ra, Rb);
end;

--# ranges.adb
--  /checkValid/    l+ 0
--  /assignValid/   l+ 0
--  /assignInvalid/ l+ 0
--  /preValid/      l! d!
--  /evalOverlap/   l- s-


