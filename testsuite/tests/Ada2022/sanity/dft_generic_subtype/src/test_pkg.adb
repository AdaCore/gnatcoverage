with Pkg;

procedure Test_Pkg is
begin
   if Pkg.Default_Ident (False) then
      raise Program_Error;
   end if;
end Test_Pkg;

--# gen_ident.adb
--
-- /ret/ l+ ## 0
