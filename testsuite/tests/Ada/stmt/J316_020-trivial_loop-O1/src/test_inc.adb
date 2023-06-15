with Inc_By, Support; use Support;

procedure Test_Inc is
   X : Integer := 12;
begin
   Inc_By (15, X);
   Assert (X = 27);
end;

--# inc_by.adb
-- /decl/ ~l+ ## 0
-- /incLoop|incX/ l+ ## 0
