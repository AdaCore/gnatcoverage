with Pkg; use Pkg;

procedure Test_TT_T is
begin
   S := (others => (0, 0));
   Set_X (True, True, 10, True);
   if S (False) /= (0, 0) then
      raise Program_Error;
   elsif S (True) /= (-10, 0) then
      raise Program_Error;
   end if;
end Test_TT_T;

--# pkg.adb
--
-- /stmt/  l+ ## 0
-- /src/   l+ ## 0
-- /index/ l! ## eF-
-- /value/ l! ## dF-
