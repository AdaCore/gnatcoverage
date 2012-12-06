with Support, PandPor; use Support, PandPor;

--  C covered for Masking MC/DC only

procedure Test_PandPor_C_Mask is
begin
   Assert (F (True, False, True) = True);
   Assert (F (False, True, False) = False);
end;

--# pandpor.adb
-- /eval(Stmt|Other)/      l! ## c!:"A",c!:"B",u=>c!:"C"
-- /decisionTrue/  l+ ## 0
-- /decisionFalse/ l+ ## 0
-- /returnValue/   l+ ## 0
-- /decl/   ~l+ ## 0

