with Synchronization;

procedure Test_Driver is
begin
  null;
end;


--# synchronization.adb
--  /prod/          l+ ## 0
--  /cons/          l+ ## 0
--  /test/          l+ ## 0
--  /exception/     l- ## s-
--  /put_element/   l+ ## 0
--  /open_barrier/  l+ ## 0
--  /get_element/   l+ ## 0
--  /close_barrier/ l+ ## 0
