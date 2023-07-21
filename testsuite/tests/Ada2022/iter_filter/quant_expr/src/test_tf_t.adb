with Support; use Support;
with Foo;     use Foo;

procedure Test_TF_T is
   Arr : constant Arr_T := (1, -2, 3);
begin
   Assert (All_Naturals_Are_Non_Zero (Arr));
end Test_TF_T;

--# foo.ads
--
-- /stmt/   l+ ## 0
-- /filter/ l+ ## 0
-- /pred/   l! ## dF-
