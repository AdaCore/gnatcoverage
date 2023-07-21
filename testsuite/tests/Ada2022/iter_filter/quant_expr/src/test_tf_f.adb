with Support; use Support;
with Foo;     use Foo;

procedure Test_TF_F is
   Arr : constant Arr_T := (-1, -2, 0);
begin
   Assert (not All_Naturals_Are_Non_Zero (Arr));
end Test_TF_F;

--# foo.ads
--
-- /stmt/   l+ ## 0
-- /filter/ l+ ## 0
-- /pred/   l! ## dT-
