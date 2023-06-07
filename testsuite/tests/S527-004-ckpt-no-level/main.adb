procedure Main is
   Val : Boolean := True;
   pragma Volatile (Val);
begin
   if Val then
      Val := False;
   end if;
end Main;
