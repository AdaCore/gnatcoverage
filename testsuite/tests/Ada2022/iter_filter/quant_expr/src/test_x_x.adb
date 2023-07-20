with Support; use Support;
with Foo;     use Foo;

procedure Test_X_X is
   Arr : constant Arr_T := (1 .. 0 => 0);
begin
   --  A for all with not elements is True

   Assert (All_Naturals_Are_Non_Zero (Arr));
end Test_X_X;

--# foo.ads
--
-- /stmt/   l+ ## 0
-- /filter/ l! ## d-
-- /pred/   l! ## d-
