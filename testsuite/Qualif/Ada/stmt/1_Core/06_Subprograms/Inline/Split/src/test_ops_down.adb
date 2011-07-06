with Ops, Support; use Ops, Support;

procedure Test_Ops_Down is
   X : Integer := 12;
begin
   Down (X);
   Assert (X = 11);
end;

--# ops.adb
--  /eval/ ~l+ 0
--  /up/   l- s-
--  /down/ l+ 0
