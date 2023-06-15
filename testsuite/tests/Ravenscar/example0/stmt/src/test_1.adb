with Sync; use Sync;
procedure Test_1 is
begin
  Termination.Wait;
end;


--# sync.adb
--  /pro_do/    l+ ## 0
--  /con_do/    l+ ## 0
--  /psh_do/    l+ ## 0
--  /psh_ttell/ l+ ## 0
--  /psh_tell/  l- ## s-
--  /pop_do/    l+ ## 0
--  /pop_ttell/ l+ ## 0
--  /pop_tell/  l+ ## 0
--  /rch_do/    l+ ## 0
--  /wat_do/    l+ ## 0
