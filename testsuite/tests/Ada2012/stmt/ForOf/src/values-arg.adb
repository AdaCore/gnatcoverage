pragma Ada_2012;

package body Values.ARG is

   procedure Do_Loop_Over (A : in out Array_Type) is
   begin
      for E of A loop -- # stmt
         Touch (E);  -- # loop_op
      end loop;
   end;

end;
