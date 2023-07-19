with Support; use Support;
with Foo;     use Foo;

procedure Test_Full is
   Arr : constant Arr_T := (1, -2, 3, 0, 9);
begin
   Assert (not All_Naturals_Are_Non_Zero (Arr));
end Test_Full;

--# foo.ads
--
-- /stmt/   l+ ## 0
-- /filter/ l+ ## 0
-- /pred/   l+ ## 0
