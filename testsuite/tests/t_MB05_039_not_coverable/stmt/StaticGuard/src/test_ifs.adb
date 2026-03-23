with Ifs, State; use Ifs, State;
with Support; use Support;

procedure Test_Ifs is
begin
   If_F_Var_Last;
   Assert (Ticks = 1);

   If_F_Cmp_Last;
   Assert (Ticks = 2);

   If_F_Var_Not_Last;
   Assert (Ticks = 4);

   If_F_Cmp_Not_Last;
   Assert (Ticks = 6);

   Ifelse_F_Var_Last;
   Assert (Ticks = 8);

   Ifelse_F_Cmp_Last;
   Assert (Ticks = 10);

   Ifelse_F_Var_Not_Last;
   Assert (Ticks = 13);

   Ifelse_F_Cmp_Not_Last;
   Assert (Ticks = 16);

   Ifelse_T_Var_Last;
   Assert (Ticks = 18);

   Ifelse_T_Cmp_Last;
   Assert (Ticks = 20);

   Ifelse_T_Var_Not_Last;
   Assert (Ticks = 23);

   Ifelse_T_Cmp_Not_Last;
   Assert (Ticks = 26);
end;

--# ifs.adb
--  /reach/  l+ ## 0
--  /out/    l0 ## s0
--  /test/   l+ ## 0
