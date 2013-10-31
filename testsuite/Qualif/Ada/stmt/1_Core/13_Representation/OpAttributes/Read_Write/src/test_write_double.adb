with Support, Mystreams; use Support, Mystreams;

procedure Test_Write_Double is
   P : aliased Port_T;
   X : Sint := (Value => 1);
begin
   Sint_Mode := Double;
   Sint'Write (P'Access, X);
   Assert (Latch = 2);
end;

--# mystreams.adb
--  /write_test/   l+ ## 0
--  /write_copy/   l- ## s-
--  /write_double/ l+ ## 0

--  /read_test/   l- ## s-
--  /read_copy/   l- ## s-
--  /read_double/ l- ## s-

--  /pread/  l- ## s-
--  /pwrite/ l- ## s-

