with Support, Mystreams; use Support, Mystreams;

procedure Test_Write_Copy is
   P : aliased Port_T;
   X : Sint := (Value => 1);
begin
   Sint_Mode := Copy;
   Sint'Write (P'Access, X);
   Assert (Latch = 1);
end;

--# mystreams.adb
--  /write_test/   l+ ## 0
--  /write_copy/   l+ ## 0
--  /write_double/ l- ## s-

--  /read_test/   l- ## s-
--  /read_copy/   l- ## s-
--  /read_double/ l- ## s-

--  /pread/  l- ## s-
--  /pwrite/ l- ## s-

