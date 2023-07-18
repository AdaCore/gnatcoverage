with Support; use Support;

with Pkg; use Pkg;

procedure Test_F_F is
   Arr_Main : My_Arr := (1 .. 10 => -18);
   Arr_Backup : My_Arr := (1 .. 4 => 4);
begin
   Assert (not All_Positive (Arr_Main, Arr_Backup, False));
end Test_F_F;

--# pkg.ads
--
-- /stmt/      l+ ## 0
-- /if_expr/   l! ## dT-
-- /predicate/ l! ## dT-
