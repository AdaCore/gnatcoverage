with Pkg_1;
with Pkg_2;

procedure Main is
   T1 : Pkg_1.PT;
   T2 : Pkg_2.PT;
begin
   T1.Say_Hello;
   T1.Set_Cond (False);
   T1.Set_Cond (True);
   T1.Wait_Cond;

   T2.Set_Cond (False);
   T2.Set_Cond (True);
   T2.Wait_Cond;
end Main;
