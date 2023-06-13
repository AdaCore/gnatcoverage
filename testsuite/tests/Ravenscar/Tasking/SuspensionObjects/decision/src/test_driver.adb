with Synchronization;

procedure Test_Driver is
begin
  null;
end;


--# synchronization.adb
--  /prod/              l+ ## 0
--  /signal_send/       l+ ## 0
--  /signal_ack_wait/   l+ ## 0
--  /test_2/            l! ## dT-
--  /exception_2/       l- ## s-
--  /signal_wait/       l+ ## 0
--  /test_1/            l! ## dF-
--  /change/            l+ ## 0
--  /signal_ack_send/   l+ ## 0
--  /exception_1/       l- ## s-
