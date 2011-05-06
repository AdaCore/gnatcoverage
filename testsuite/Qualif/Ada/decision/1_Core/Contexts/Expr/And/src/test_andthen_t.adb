with Support, Andthen; use Support, Andthen;

procedure Test_Andthen_T is
begin
   Assert (And_Then (True, True) = True);
end;

--# andthen.adb
--  /andthen/  l! dF-
--  /retTrue/  l+ 0
--  /retFalse/ l- s-
--  /retVal/   l+ 0
