with Support, Mystreams; use Support, Mystreams;

procedure Test_Read_Copy is
   P : aliased Port_T;
   X : Sint;
begin
   Mystreams.Sint_Mode := Copy;
   Sint'Read (P'Access, X);
   Assert (X.Value = 0);
end;

--# mystreams.adb
--  /read_test/    l+ ## 0
--  /read_copy/    l+ ## 0
--  /read_double/  l- ## s-

--  /write_test/    l- ## s-
--  /write_copy/    l- ## s-
--  /write_double/  l- ## s-

--  /pread/  l- ## s-
--  /pwrite/ l- ## s-

