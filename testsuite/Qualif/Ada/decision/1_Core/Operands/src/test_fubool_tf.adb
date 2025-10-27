with FUBOOL_Helper;

procedure Test_FUBOOL_TF is
begin
   FUBOOL_Helper.Eval_T;
   FUBOOL_Helper.Eval_F;
end;

--# fubool.adb
--  /decl/   l+ ## 0
--  /eval0/  l+ ## 0
--  /true/   l+ ## 0
--  /false/  l+ ## 0
