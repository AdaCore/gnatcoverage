with Int_Control; use Int_Control;

procedure TS1 is
   T : Integer;
begin
   T := TC.Current_Temperature;
   pragma Assert (T >= 0);
   T := PC.Current_Pressure;
   pragma Assert (T >= 0);
end;
