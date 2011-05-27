with Support, Andthen; use Support, Andthen;

procedure Test_Andthen_F is
begin
   Assert (And_Then (False, True) = False);
   Assert (And_Then (True, False) = False);
end;

--# andthen.adb
--  /andthen/  l+;mu=>l! mu=>dT-
--  /retTrue/  l- s-
--  /retFalse/ l+ 0
--  /retVal/   l+ 0

