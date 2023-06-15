--  Test driver for checking DC in case when a class-wide operation is
--  involved. The class-wide operation to check is not a condition in any
--  decision, it is just called on its own.
--
--  The driver does not call anything - the check is made that no code of
--  interest is reported as covered.

with Check_Class_Wide;

with Support; use Support;
procedure Test_No is
   Result : Integer;
begin
   Assert (True);
end Test_No;

--# parent.adb
-- /compute_c_eval/    l- ## s-
-- /compute_c_true/    l- ## s-
-- /compute_c_false/   l- ## s-
-- /valid_c/           l- ## s-

--# parent-child1.adb
-- /compute_eval/      l- ## s-
-- /compute_true/      l- ## s-
-- /compute_false/     l- ## s-
-- /valid_if_eval/     l- ## s-
-- /valid_if_true/     l- ## s-
-- /valid_elsif_eval/  l- ## s-
-- /valid_elsif_ true/ l- ## s-
-- /valid_false/       l- ## s-

--# parent-child2.adb
-- /compute_eval/      l- ## s-
-- /compute_true/      l- ## s-
-- /compute_false/     l- ## s-
-- /valid_if_eval/     l- ## s-
-- /valid_if_true/     l- ## s-
-- /valid_elsif_eval/  l- ## s-
-- /valid_elsif_ true/ l- ## s-
-- /valid_false/       l- ## s-
