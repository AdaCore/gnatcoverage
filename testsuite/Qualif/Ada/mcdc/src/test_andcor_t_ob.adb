with Support, AndCor; use Support, AndCor;

procedure Test_AndCor_T_OB is
begin
   Assert (F (True, True, False) = True);
   Assert (F (True, False, False) = False);
end;

--# andcor.adb
-- /andthen/     l! m!:"A"
-- /orelse/      l! m!:"C"
-- /returnOr/    l+ 0
-- /orTrue/      l+ 0
-- /orFalse/     l+ 0
-- /returnTrue/  l+ 0
-- /returnFalse/ l+ 0
-- /returnValue/ l+ 0
-- /decl/ l+ 0
