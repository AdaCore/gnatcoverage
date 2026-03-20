with Support, Inc; use Support;

-- Call, and state bogus s- expectation

procedure Test_Inc is
   X : Integer := 1;
begin
   Inc (X);
   Assert (X = 2);
end;

--# inc.adb
--  /inc/ l- ## s-
