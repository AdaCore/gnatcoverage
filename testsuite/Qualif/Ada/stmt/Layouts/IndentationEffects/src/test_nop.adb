with Support, Nop; use Support;

--P  Invoke Nop and verify that the unreached statements are reported
--P  uncovered.

procedure Test_Nop is
   K : constant := 12;
   X : Integer := K;
begin
   Nop (X);
   Assert (X = K);
end;

--# nop.adb
--  /XnoCov/  l- s-
