pragma Ada_2012;

package body Values.Global is
   
   procedure Do_Loop is
   begin
      for E of GSA loop -- # stmt
         Touch (E);     -- # loop_op
      end loop;
   end;
   
end;
