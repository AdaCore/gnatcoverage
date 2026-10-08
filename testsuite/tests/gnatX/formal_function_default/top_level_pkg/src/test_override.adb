pragma Ada_2012;

with Pkg;

procedure Test_Override is
   function F (B : Boolean) return Boolean is (B);
   package Pkg_Inst is new Pkg (F);
begin
   if Pkg_Inst.Eval_F (False) then
      raise Program_Error;
   end if;
end Test_Override;

--# pkg.ads
--  /f/      l- ## s-
--  /eval_f/ l+ ## 0
