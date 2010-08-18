with Support;    use Support;
with Constructs;

--  Check behavior when a small part of the constructs of interest in the
--  functional code is called.

procedure Test_Constructs_Part_Small is
begin
   Assert (not Constructs.In_Range (1, 3, 2));
end Test_Constructs_Part_Small;

--# constructs.adb
-- /mainstream/ l+ 0
-- /nonemptyrange/ l- s-
-- /morethenoneinrange/ l- s-
-- /emptyrange/ l+ 0
-- /oneelement/ l- s-
-- /XgtR/ l- s-
-- /XltL/ l- s-
-- /neverexecuted/ l- s-
