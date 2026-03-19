with Falseonly;

procedure Test_Falseonly is
begin
   Falseonly;
end;

--# falseonly.adb
--  /test/ l! ## dT-
--  /then/ l. ## 0
--  /else/ l+ ## 0

-- %cov: --non-coverable
--  =/then/ l0 ## s0
