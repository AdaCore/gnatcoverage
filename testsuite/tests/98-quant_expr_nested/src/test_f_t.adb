with Support; use Support;

with Pkg; use Pkg;

procedure Test_F_T is
   Arr_Main : My_Arr := (1 .. 10 => 18);
   Arr_Backup : My_Arr := (1 .. 4 => -4);
begin
   Assert (All_Positive (Arr_Main, Arr_Backup, False));
end Test_F_T;

--# pkg.ads
--
-- /stmt/      l+ ## 0
-- /if_expr/   l! ## dT-
-- /predicate/ l! ## dF-
