with Actions; use Actions;

--  Basic test for the %tags support in expectations, allowing
--  conditional sections of expectation statements depending on
--  the testsuite discriminants.

procedure Test_Tags is   
begin   
   Process (5, Process_Positive'Access);
   Process (-5, Process_Negative'Access);
   Process (0, Process_Zero'Access);
end;

-- ALL is expected to always be part of the suite discriminants
-- NEVER is expected never to be there

--# actions.adb

-- %tags: ALL
--  /unreach_pos/  l- ## s-

-- %tags: !NEVER
--  /unreach_neg/  l- ## s-

-- %tags: ALL, !NEVER
--  /unreach_zero/ l- ## s-

-- %tags: NEVER
--  /check/ l- ## s-
