with Ops, Support; use Ops, Support;

procedure Test_Ops_Fu is
   X : Integer := 12;
begin
   Up (X);
   Down (X);
   Assert (X = 12);
end;

--# ops.adb
--  /eval/ ~l+ 0
--  /up/   l+ 0
--  /down/ l+ 0
