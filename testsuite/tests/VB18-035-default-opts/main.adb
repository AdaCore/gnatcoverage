procedure Main is
   Val : Boolean := True;
   pragma Volatile (Val);
begin
   if Val then
      Val := False;
   else
      Val := True;
   end if;
end Main;
