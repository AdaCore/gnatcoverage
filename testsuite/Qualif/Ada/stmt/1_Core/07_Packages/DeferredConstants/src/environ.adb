package body Environ is
   
   X : Value := 0;
   
   function Probe return State is
   begin
      X := X + 1; -- # probe
      if X = 1 then -- # probe
         Did_Initial_Probe := True; -- # init_probe
      else
         Did_Other_Probe := True; -- # other_probe
      end if;
      return (X => X); -- # probe
   end;
   
   function Match (A, B : State) return Boolean is
   begin
      return A.X = B.X; -- # compare
   end;
end;

