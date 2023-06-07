with Min;

procedure Test_Min2 is
   V : Float;
begin
   V := 1.5;
   V := Min (V, 2.5);
   if V /= 1.5 then
      raise Program_Error;
   end if;
end Test_Min2;
