with Support, Andidor; use Support;

--  Don't even call the evaluator. Expect the single statement
--  is reported uncovered.

procedure Test_Andidor_0 is
begin
   Assert (True);
end;

--# andidor.adb
--  /eval/ l- s-
