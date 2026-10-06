pragma Ada_2012;

with Eval_F;

procedure Test_Override is
   function F (B : Boolean) return Boolean is (B);
   function Eval_F_Inst is new Eval_F (F);
begin
   if Eval_F_Inst (False) then
      raise Program_Error;
   end if;
end Test_Override;

--# eval_f.ads
--  /f/ l- ## s-
