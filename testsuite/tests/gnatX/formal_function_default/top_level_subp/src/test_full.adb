with Eval_F;

procedure Test_Full is
   function Eval_F_Inst is new Eval_F;
begin
   if Eval_F_Inst (False) then
      raise Program_Error;
   end if;
end Test_Full;

--# eval_f.ads
--  /f/ l+ ## 0
