with Lib; use Lib;

procedure Driver is
begin
   if Get (11) then
      raise Program_Error;
   end if;
end Driver;
