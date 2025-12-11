with Support, Andthen; use Support, Andthen;

procedure Test_Andthen_0 is
begin
   Assert (True);
end;

--# andthen.adb
--  /retTrue/  l- ## s-
--  /retFalse/ l- ## s-
--  /retVal/   l- ## s-
--
--%opts: --trace-mode=bin
--  /andthen/  l. ## 0
--
--%opts: --trace-mode=src
--  /andthen/  l! ## e-
