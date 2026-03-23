with Assert, Engines; use Engines;

procedure Test_Engines_PP is
   E : Engine;
begin
   --  PP case: Both values set to stability threshold Plus one

   E.P := Stable_P + 1;
   E.T := Stable_T + 1;
   Assert (not Stable (E));
end;
