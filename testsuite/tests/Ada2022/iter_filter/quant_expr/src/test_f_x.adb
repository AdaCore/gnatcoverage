with Support; use Support;
with Foo;     use Foo;

procedure Test_F_X is
   Arr : constant Arr_T := (-1, -2, -3);
begin
   --  A for all with no element passing the filter is True

   Assert (All_Naturals_Are_Non_Zero (Arr));
end Test_F_X;

--# foo.ads
--
-- /stmt/   l+ ## 0
-- /filter/ l! ## dT-
-- /pred/   l! ## d-
