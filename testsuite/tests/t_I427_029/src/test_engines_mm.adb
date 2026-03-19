with Assert, Engines; use Engines;

procedure Test_Engines_MM is
   E : Engine;
begin
   --  MM case: Both values set to stability threshold Minus one

   E.P := Stable_P - 1;
   E.T := Stable_T - 1;
   Assert (Stable (E));
end;
