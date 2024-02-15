with Values, Support; use Values, Support;

procedure Test_Values is
   V : Integer;
begin
   Assert (One = 1.0);

   V := Value_Of (X => 15);
   Assert (V = 15);

   Latch (V => 12);
   V := Value_Of_X;
   Assert (V = 12);
end;

--# values.adb
--  /stmt/ l+ ## 0
--  /out/  l0 ## s0
