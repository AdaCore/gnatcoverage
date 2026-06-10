with Postcond;

--  Test that the only coverage information that is reported for assertions
--  is that concerning the postcondition.

procedure Test_Postcond is
begin
   Postcond;
end Test_Postcond;

--# postcond.adb
-- /post/   l- ## a-
-- /assert/ l+ ## 0
-- /ko/     l- ## s-
-- /stmt/   l+ ## 0
