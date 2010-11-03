with Support, Nop; use Support;

--  Invoke a subprogram which features statements nested in conditional
--  constructs that won't transfer control to them, but idented in a
--  misleading fashion so that the conditioning might seem not to apply.
--
--  Verify that the unreached statements are reported uncovered despite the
--  misleading code layout.

procedure Test_Nop is
   K : constant := 12;
   X : Integer := K;
begin
   Nop (X);
   Assert (X = K);
end;

--# nop.adb
--  /incX/  l- s-
