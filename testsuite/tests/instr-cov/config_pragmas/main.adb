with Pkg;

procedure Main is
   T : Pkg.PT;
begin
   T.Init (False);
   select
      T.Wait_Cond;
   else
      null;
   end select;
end Main;
