with Pkg;

procedure Test_Full is
   package Pkg_Inst is new Pkg;
begin
   if Pkg_Inst.Eval_F (False) then
      raise Program_Error;
   end if;
end Test_Full;

--# pkg.ads
--  /f/      l+ ## 0
--  /eval_f/ l+ ## 0
