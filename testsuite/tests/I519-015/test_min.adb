with Min;

procedure Test_Min is
   V : Float;
begin
   V := 1.5;
   V := Min (V, 0.5);
   V := Min (V, 2.0);
   if V /= 0.5 then
      raise Program_Error;
   end if;
end Test_Min;
