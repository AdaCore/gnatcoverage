with Support, AndCor; use Support, AndCor;

procedure Test_AndCor_T_B is
begin
   Assert (F (True, False, False) = False);
   Assert (F (True, True , False) = True);
end;

--# andcor.adb
-- /andthen/     l! c!:"A"
-- /orelse/      l! c!:"C"
-- /returnOr/    l+ 0
-- /orTrue/      l+ 0
-- /orFalse/     l+ 0
-- /returnTrue/  l+ 0
-- /returnFalse/ l+ 0
-- /returnValue/ l+ 0
