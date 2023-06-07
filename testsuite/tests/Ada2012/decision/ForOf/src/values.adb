pragma Ada_2012;

package body Values is
   
   procedure Process (A : in out Array_Type; Bump : Boolean) is
   begin
      for E of A loop -- # stmt
         E.X := E.X * 2; -- # stmt
         if Bump then -- # eval
            E.Count := E.Count + 1; -- # true
         end if;
      end loop;
   end;
   
end;
