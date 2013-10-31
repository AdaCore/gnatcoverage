with Support, Mystreams; use Support, Mystreams;

procedure Test_WDRC is
   P : aliased Port_T;
   X : Sint := (Value => 1);
begin
   Mystreams.Sint_Mode := Double;
   Sint'Write (P'Access, X);
   Assert (Latch = 2);
   
   Mystreams.Sint_Mode := Copy;
   Sint'Read (P'Access, X);
   Assert (X.Value = 2);
end;

--# mystreams.adb
--  /read_test/    l+ ## 0
--  /read_copy/    l+ ## 0
--  /read_double/  l- ## s-

--  /write_test/    l+ ## 0
--  /write_copy/    l- ## s-
--  /write_double/  l+ ## 0

--  /pread/  l- ## s-
--  /pwrite/ l- ## s-

