with Support, AndCor; use Support, AndCor;

procedure Test_AndCor_T_B_SC is
begin
   Assert (F (True, False, False) = False);
   Assert (F (True, True , True)  = True);
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
