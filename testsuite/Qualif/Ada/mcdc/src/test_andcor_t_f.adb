with Support, AndCor; use Support, AndCor;

procedure Test_AndCor_T_F is
begin
   Assert (F (True, False, False) = False);
end;

--# andcor.adb
-- /andthen/     l! dT-
-- /orelse/      l! dT-
-- /returnOr/    l+ 0
-- /orTrue/      l- s-
-- /orFalse/     l+ 0
-- /returnTrue/  l- s-
-- /returnFalse/ l+ 0
-- /returnValue/ l+ 0
