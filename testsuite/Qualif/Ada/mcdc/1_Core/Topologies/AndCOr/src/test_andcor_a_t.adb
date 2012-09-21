with Support, AndCor; use Support, AndCor;

procedure Test_AndCor_A_T is
begin
   Assert (F (True , True, True) = True);
   Assert (F (False, True, True) = False);
end;

--# andcor.adb
-- /andthen/     l! ## c!:"Orelse"
-- /orelse/      l! ## oF-
-- /returnOr/    l+ ## 0
-- /orTrue/      l+ ## 0
-- /orFalse/     l- ## s-
-- /returnTrue/  l+ ## 0
-- /returnFalse/ l+ ## 0
-- /returnValue/ l+ ## 0
-- /decl/ l+ ## 0
