with Support, Andthen; use Support, Andthen;

procedure Test_Andthen_A is
begin
   Assert (And_Then (True, True) = True);
   Assert (And_Then (False, True) = False);
end;

--# andthen.adb
--  /andthen/  l+ ## 0
--  /retTrue/  l+ ## 0
--  /retFalse/ l+ ## 0
--  /retVal/   l+ ## 0
