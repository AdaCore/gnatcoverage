--  Special driver for this test, as we have an additional nested statement in
--  the eval1 line, and two statements on the line for eval0

with FUAND_Helper;

procedure Test_FUAND_0 is
begin
   null;
end;

--# fuand.adb
--  /eval0/   l- ## s-,s-
--  /eval1/   l- ## s-
--  /true/    l- ## s-
--  /false/   l- ## s-
--  /decl/    l- ## s-
