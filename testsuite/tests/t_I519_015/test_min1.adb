with Min;

procedure Test_Min1 is
   V : Float;
begin
   V := 1.5;
   V := Min (V, 0.5);
   if V /= 0.5 then
      raise Program_Error;
   end if;
end Test_Min1;
