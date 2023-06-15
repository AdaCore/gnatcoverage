with Min;

procedure Test_Min is
   V : Integer;
begin
   V := 3;
   V := Min (V, 4);
   V := Min (V, 1);
   if V /= 1 then
      raise Program_Error;
   end if;
end Test_Min;
