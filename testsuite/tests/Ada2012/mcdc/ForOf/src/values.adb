pragma Ada_2012;

package body Values is

   procedure Process (A : in out Array_Type) is
   begin
      for E of A loop -- # stmt
         E.Expr := E.A and then E.B; -- # eval
      end loop;
   end;

end;
