with Support, AndCor; use Support, AndCor;

procedure Test_AndCor_T_T is
begin
   Assert (F (True, True, True)  = True);
   Assert (F (True, False, True) = True);
   Assert (F (True, True, False) = True);
end;

--# andcor.adb
-- /andthen/     l! oF-
-- /orelse/      l! oF-
-- /returnOr/    l+ 0
-- /orTrue/      l+ 0
-- /orFalse/     l- s-
-- /returnTrue/  l+ 0
-- /returnFalse/ l- s-
-- /returnValue/ l+ 0
-- /decl/ l+ 0
