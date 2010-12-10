with Support, AndCor; use Support, AndCor;

procedure Test_AndCor_T_C is
begin
   Assert (F (True, False, False) = False);
   Assert (F (True, False, True)  = True);
end;

--# andcor.adb
-- /andthen/     l! m!:"A"
-- /orelse/      l! m!:"B"
-- /returnOr/    l+ 0
-- /returnTrue/  l+ 0
-- /returnFalse/ l+ 0
-- /returnValue/ l+ 0
-- /decl/ l+ 0
