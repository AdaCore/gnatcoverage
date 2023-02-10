with Pkg;

procedure Test_F is
begin
   if Pkg.Store_AT (True, False).X /= False then
      raise Program_Error;
   end if;
end Test_F;
