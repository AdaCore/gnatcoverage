with Ops, Support; use Ops, Support;

procedure Test_Ops_Up is
   X : Integer := 12;
begin
   Up (X);
   Assert (X = 13);
end;

--# ops.adb
--  /eval/ ~l+ ## 0
--  /up/   l+ ## 0
--  /down/ l- ## s-
