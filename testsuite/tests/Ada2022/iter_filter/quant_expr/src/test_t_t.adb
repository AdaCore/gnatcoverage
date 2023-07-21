with Support; use Support;
with Foo;     use Foo;

procedure Test_T_T is
   Arr : constant Arr_T := (1, 2, 3);
begin
   Assert (All_Naturals_Are_Non_Zero (Arr));
end Test_T_T;

--# foo.ads
--
-- /stmt/   l+ ## 0
-- /filter/ l! ## dF-
-- /pred/   l! ## dF-
