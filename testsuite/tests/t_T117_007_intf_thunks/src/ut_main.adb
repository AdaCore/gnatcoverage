pragma Ada_2012;

with Package_A;

procedure Ut_Main is

   Instance : aliased Package_A.T_A;

   A_Instance : Package_A.T_NA_A := Package_A.NA_A;

   returnValue : Integer;

begin

   Instance.Init;
   Instance.Run;
   returnValue := Instance.Get_Variable;

end Ut_Main;
