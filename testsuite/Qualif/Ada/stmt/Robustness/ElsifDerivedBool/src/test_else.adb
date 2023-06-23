with Support;

with Pkg; use Pkg;

procedure Test_Else is
begin
   Support.Assert (not Or_Else (False, False));
end Test_Else;

--# pkg.adb
--
-- /if-stmt/    l+ ## 0
-- /ret-if/     l- ## s-
-- /elsif-stmt/ l+ ## 0
-- /ret-elsif/  l- ## s-
-- /ret-else/   l+ ## 0
