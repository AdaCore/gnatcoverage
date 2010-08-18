with Support;    use Support;
with Constructs;

--  Check behavior when a big part of the constructs of interest in the
--  functional code is called.

procedure Test_Constructs_Part_Big is
begin
   Assert (not Constructs.In_Range (1, 2, 3));
end Test_Constructs_Part_Big;

--# constructs.adb
-- /mainstream/ l+ 0
-- /nonemptyrange/ l+ 0
-- /morethenoneinrange/ l+ 0
-- /emptyrange/ l- s-
-- /oneelement/ l- s-
-- /XgtR/ l- s-
-- /XltL/ l+ 0
-- /neverexecuted/ l- s-
