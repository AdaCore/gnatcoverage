with Inc, Support; use Support;

-- Arrange to cover all statements but not an inner decision

procedure Test_Inc is
   X : Integer := 1;
begin
   Inc (X, Amount => 1);
   Assert (X = 2);
end;

--# inc.adb
--  /inc/ l+ ## 0
