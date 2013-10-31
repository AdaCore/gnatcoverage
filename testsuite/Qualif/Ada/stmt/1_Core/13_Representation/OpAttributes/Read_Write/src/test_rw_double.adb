with Support, Mystreams; use Support, Mystreams;

procedure Test_RW_Double is
   P : aliased Port_T;
   X : Sint := (Value => 1);
begin
   Mystreams.Sint_Mode := Double;
   Sint'Write (P'Access, X);
   Assert (Latch = 2);
   
   Sint'Read (P'Access, X);
   Assert (X.Value = 4);
end;

--# mystreams.adb
--  /read_test/    l+ ## 0
--  /read_copy/    l- ## s-
--  /read_double/  l+ ## 0

--  /write_test/    l+ ## 0
--  /write_copy/    l- ## s-
--  /write_double/  l+ ## 0

--  /pread/  l- ## s-
--  /pwrite/ l- ## s-

