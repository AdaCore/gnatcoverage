with Support, Mystreams; use Support, Mystreams;

procedure Test_WCRD is
   P : aliased Port_T;
   X : Sint := (Value => 1);
begin
   Mystreams.Sint_Mode := Copy;
   Sint'Write (P'Access, X);
   Assert (Latch = 1);
   
   Mystreams.Sint_Mode := Double;
   Sint'Read (P'Access, X);
   Assert (X.Value = 2);
end;

--# mystreams.adb
--  /read_test/    l+ ## 0
--  /read_copy/    l- ## s-
--  /read_double/  l+ ## 0

--  /write_test/    l+ ## 0
--  /write_copy/    l+ ## 0
--  /write_double/  l- ## s-

--  /pread/  l- ## s-
--  /pwrite/ l- ## s-

