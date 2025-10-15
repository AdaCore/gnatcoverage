with Make_Call;

--  Make no call to procedure Increment and check the coverage status of the
--  no-executed call.

procedure Test_No_Call is
begin
   null;
end Test_No_Call;

--# make_call.adb
-- /proc/ l- ## f-
-- /stmt/ l- ## s-
-- /decl/ l- ## s-
-- /call/ l- ## s-,c-
