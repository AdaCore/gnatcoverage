with Support, AndCor; use Support, AndCor;

procedure Test_AndCor_F is
begin
   Assert (F (False, False, False) = False);
   Assert (F (False, False, True)  = False);
   Assert (F (False, True , False) = False);
   Assert (F (False, True , True)  = False);
end;

--# andcor.adb
-- /andthen/     l! dT-
-- /orelse/      l- s-
-- /returnOr/    l- s-
-- /orTrue/      l- s-
-- /orFalse/     l- s-
-- /returnTrue/  l- s-
-- /returnFalse/ l+ 0
-- /returnValue/ l+ 0
-- /decl/ l+ 0
