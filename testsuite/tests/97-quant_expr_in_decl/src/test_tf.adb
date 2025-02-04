with Support; use Support;
with Pkg;

procedure Test_TF is
   All_T : Pkg.My_Arr := (True, True, True);
   All_F : Pkg.My_Arr := (False, False, False);
begin
   Assert (Pkg.All_True (All_T));
   Assert (Pkg.Some_True (All_T));
   Assert (not Pkg.All_True (All_F));
   Assert (not Pkg.Some_True (All_F));
end Test_TF;

--# pkg.adb
--
-- /decl/ l+ ## 0
-- /ret/  l+ ## 0
