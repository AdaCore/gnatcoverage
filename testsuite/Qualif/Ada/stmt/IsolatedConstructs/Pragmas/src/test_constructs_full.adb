with Support;    use Support;
with Constructs;

--  Check behavior when all the constructs of interest in the functional code
--  are called

procedure Test_Constructs_Full is
begin
   Assert (not Constructs.In_Range (1, 3, 2));
   Assert (not Constructs.In_Range (1, 2, 2));
   Assert (not Constructs.In_Range (1, 2, 3));
   Assert (not Constructs.In_Range (7, 2, 3));
end Test_Constructs_Full;

--# constructs.adb
-- /mainstream/ l+ 0
-- /nonemptyrange/ l+ 0
-- /morethenoneinrange/ l+ 0
-- /emptyrange/ l+ 0
-- /oneelement/ l+ 0
-- /XgtR/ l+ 0
-- /XltL/ l+ 0
-- /neverexecuted/ l- s-
