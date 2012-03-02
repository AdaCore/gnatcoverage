with Ranges, Support, Silent_Last_Chance; use Ranges, Support;

-- Exercise a single overlap check, involving an invalid range B.
-- Exempted precondition evaluated False only.

procedure Test_Ranges_Invalid_B is
   Ra, Rb : XYrange;
   Ov : Boolean;
begin
   Set (Ra, 1, 2);
   Set (Rb, 4, 1); -- invalid

   --  One call to Overlap, with invalid range Rb.
   --  Invoke silent last chance handler.

   Ov := Overlap (Ra, Rb);
exception
    when others => null;
end;

--# ranges.adb
--  /checkValid/    l+ 0
--  /assignValid/   l+ 0
--  /assignInvalid/ l+ 0
--  /preValid/      l* x+
--  /checkOverlap/  l- s-

