with Call_Add;

procedure Test_Import_Not_Called is
begin
    null;
end Test_Import_Not_Called;

--# call_add.adb
-- /fun/    l- ## f-
-- /stmt/   l- ## s-
-- /if_1/   l- ## s=>s-, f=>s-,c-
-- /call_1/ l- ## s=>s-, f=>s-,c-
-- /if_2/   l- ## s-
-- /if_3/   l- ## s=>s-, f=>s-,c-
-- /call_2/ l- ## s=>s-, f=>s-,c-
-- /if_4/   l- ## s=>s-, f=>s-,c-
-- /call_3/ l- ## s=>s-, f=>s-,c-
