with Support, Mystreams; use Support, Mystreams;

procedure Test_Read_Double is
   P : aliased Port_T;
   X : Sint;
begin
   Mystreams.Sint_Mode := Double;
   Sint'Read (P'Access, X);
   Assert (X.Value = 0);
end;

--# mystreams.adb
--  /read_test/    l+ ## 0
--  /read_copy/    l- ## s-
--  /read_double/  l+ ## 0

--  /write_test/    l- ## s-
--  /write_copy/    l- ## s-
--  /write_double/  l- ## s-

--  /pread/  l- ## s-
--  /pwrite/ l- ## s-

