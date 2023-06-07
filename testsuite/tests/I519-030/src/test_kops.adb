with Ops4, Ops8, Vars;

procedure Test_Kops is
   Vx : Integer := Vars.X;
begin
   Ops4.Inc (Vx);
   Vars.Assert_Eq (Vx, 16);

   Ops8.Inc (Vx);
   Vars.Assert_Eq (Vx, 24);
end;
