with Support, AndCor; use Support, AndCor;

procedure Test_AndCOr_AOBC is
begin
   Assert (F (True , False, False) = False);
   Assert (F (True , False, True)  = True);
   Assert (F (True , True , False) = True);
   Assert (F (False, True , False) = False);
end;

--# andcor.adb
-- /andthen/     l+ ## 0
-- /orelse/      l+ ## 0
-- /returnOr/    l+ ## 0
-- /orTrue/      l+ ## 0
-- /orFalse/     l+ ## 0
-- /returnTrue/  l+ ## 0
-- /returnFalse/ l+ ## 0
-- /returnValue/ l+ ## 0
-- /decl/ ~l+ ## 0
