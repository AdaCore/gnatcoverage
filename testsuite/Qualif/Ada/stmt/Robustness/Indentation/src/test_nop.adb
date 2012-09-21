with Support, Nop; use Support;

procedure Test_Nop is
   K : constant := 12;
   X : Integer := K;
begin
   Nop (X);
   Assert (X = K);
end;

--# nop.adb
--  /incX/  l- ## s-
