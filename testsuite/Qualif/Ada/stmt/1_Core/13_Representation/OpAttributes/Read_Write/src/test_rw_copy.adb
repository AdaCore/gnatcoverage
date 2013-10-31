with Support, Mystreams; use Support, Mystreams;

procedure Test_RW_Copy is
   P : aliased Port_T;
   X : Sint := (Value => 1);
begin
   Mystreams.Sint_Mode := Copy;
   Sint'Write (P'Access, X);
   Assert (Latch = 1);
   
   Sint'Read (P'Access, X);
   Assert (X.Value = 1);
end;

--# mystreams.adb
--  /read_test/    l+ ## 0
--  /read_copy/    l+ ## 0
--  /read_double/  l- ## s-

--  /write_test/    l+ ## 0
--  /write_copy/    l+ ## 0
--  /write_double/  l- ## s-

--  /pread/  l- ## s-
--  /pwrite/ l- ## s-

