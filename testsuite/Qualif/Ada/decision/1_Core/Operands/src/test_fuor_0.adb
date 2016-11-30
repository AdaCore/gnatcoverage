with FUOR_Helper;

procedure Test_FUOR_0 is
begin
   null;
end;

--# fuor.adb
--  /eval0/ l- ## s-
--  /eval1/ l- ## 0c
--  /true/  l- ## s-
--  /false/ l- ## s-
--  /decl/  l- ## s-

-- Old compiler are weaker on mere declarations, not
-- the point of any of the tests in this this family.

-- %tags:7.0.2
-- =/decl/  ~l- ## ~s-
