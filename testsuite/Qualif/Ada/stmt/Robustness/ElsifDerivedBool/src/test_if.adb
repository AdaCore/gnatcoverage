with Support;

with Pkg; use Pkg;

procedure Test_If is
begin
   Support.Assert (Or_Else (True, False));
end Test_If;

--# pkg.adb
--
-- /if-stmt/    l+ ## 0
-- /ret-if/     l+ ## 0
-- /elsif-stmt/ l- ## s-
-- /ret-elsif/  l- ## s-
-- /ret-else/   l- ## s-
