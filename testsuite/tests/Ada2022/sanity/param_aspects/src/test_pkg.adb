with Pkg;

procedure Test_Pkg is
begin
   if Pkg.Foo (1, True) /= 1 then
      raise Program_Error;
   end if;
end Test_Pkg;

--# pkg.ads
--
-- /expr/ l+ ## 0
