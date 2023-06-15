with Robots; use Robots;

procedure Test_Robots is
   R : Robot;
begin
   Run (R, Nop);
   Run (R, Rotate);

   if R.Exec /= 2 then
      raise Program_Error;
   end if;
end;
