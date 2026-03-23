with Make_Call;

--  Make a call to a procedure and check its coverage status

procedure Test_Call is
begin
   Make_Call;
end Test_Call;

--# make_call.adb
-- /proc/ l+ ## 0
-- /stmt/ l+ ## 0
-- /decl/ l+ ## 0
-- /call/ l+ ## 0
