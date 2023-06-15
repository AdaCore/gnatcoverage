with Assert, Engines; use Engines;

procedure Test_Engines_MP is
   E : Engine;
begin
   E.P := Stable_P - 1;
   E.T := Stable_T + 1;
   Assert (not Stable (E));
end;
