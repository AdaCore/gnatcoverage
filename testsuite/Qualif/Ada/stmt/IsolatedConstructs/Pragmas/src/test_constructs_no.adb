with Support;    use Support;
with Constructs;

--  Check behavior when no functional code is called.

procedure Test_Constructs_No is
begin
   null;
end Test_Constructs_No;

--# constructs.adb
-- /mainstream/ l- s-
-- /nonemptyrange/ l- s-
-- /morethenoneinrange/ l- s-
-- /emptyrange/ l- s-
-- /oneelement/ l- s-
-- /XgtR/ l- s-
-- /XltL/ l- s-
-- /neverexecuted/ l- s-
