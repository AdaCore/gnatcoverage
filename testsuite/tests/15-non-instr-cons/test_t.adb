with Pkg;

procedure Test_T is
begin
   if Pkg.Store_AT (True, True).X /= True then
      raise Program_Error;
   end if;
end Test_T;
