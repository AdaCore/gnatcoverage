with Pkg; use Pkg;

procedure Test_Part_Full is
begin
   S := (others => (0, 0));

   Set_X (True, False, 5, False);
   if S /= (False => (5, 0), True => (0, 0)) then
      raise Program_Error;
   end if;

   Set_X (True, False, 10, True);
   if S /= (False => (-10, 0), True => (0, 0)) then
      raise Program_Error;
   end if;
end Test_Part_Full;

--# pkg.adb
--
-- /stmt/  l+ ## 0
-- /src/   l+ ## 0
-- /index/ l! ## c!:"A"
-- /value/ l+ ## 0
