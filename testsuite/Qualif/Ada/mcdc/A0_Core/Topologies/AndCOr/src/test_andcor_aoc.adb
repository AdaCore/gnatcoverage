with Support, AndCor; use Support, AndCor;

procedure Test_AndCor_AOC is
begin
   Assert (F (True , False, False) = False);
   Assert (F (True , False, True)  = True);
   Assert (F (False, True , False) = False);
end;

--# andcor.adb
-- /andthen/     l+ ## 0
-- /orelse/      l! ## c!:"B"
-- /returnOr/    l+ ## 0
-- /orTrue/      l+ ## 0
-- /orFalse/     l+ ## 0
-- /returnTrue/  l+ ## 0
-- /returnFalse/ l+ ## 0
-- /returnValue/ l+ ## 0
-- /decl/ ~l+ ## 0
