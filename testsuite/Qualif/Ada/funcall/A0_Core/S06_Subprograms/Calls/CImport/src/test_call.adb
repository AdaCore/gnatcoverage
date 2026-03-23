with Call_Add;

--  Test the coverage analysis of different calls to a function imported from
--  C.

procedure Test_Call is
begin
    Call_Add;
end Test_Call;

--# call_add.adb
-- /fun/    l+ ## 0
-- /stmt/   l+ ## 0
-- /if_1/   l! ## dT-
-- /call_1/ l- ## s=>s-, f=>s-,c-
-- /if_2/   l! ## dT-
-- /if_3/   l- ## s=>s-, f=>s-,c-
-- /call_2/ l- ## s=>s-, f=>s-,c-
-- /if_4/   l! ## dF-
-- /call_3/ l+ ## 0
