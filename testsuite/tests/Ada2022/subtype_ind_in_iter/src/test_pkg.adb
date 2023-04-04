with Pkg;

procedure Test_Pkg is
   Res : Positive;
   pragma Volatile (Res);
begin
   Res := Pkg.Do_Sum (10);
   if Res /= 55 then
      raise Program_Error;
   end if;
end Test_Pkg;

--# pkg.adb
--  /Constrained/ l+ ## 0
