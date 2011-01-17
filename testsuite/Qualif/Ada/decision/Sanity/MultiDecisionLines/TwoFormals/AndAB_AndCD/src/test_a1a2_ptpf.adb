with Support, A1A2; use A1A2, support;

procedure Test_A1A2_PTPF is
begin
   Assert (F (A => True, B => True, C => False, D => True) = False);
end;

--# a1a2.adb
--  /valueF/ l! dT-:"A and then B"
--  /evals/  l! dF-:"A and then B," # dT-:"C and then D"
