pragma Ada_2012;

with Support; use Support;

package body Values.ARG is
     
   procedure Do_Loop_Over
     (A : in out Array_Type;
      Early_Return : Boolean;
      Max_Iterations : Integer)
   is
      N_Iterations : Integer := 0; -- # decl
   begin
      if Early_Return then -- # test-return
         return;           -- # return
      end if;
      for E of A loop -- # for-stmt
         if N_Iterations = Max_Iterations then -- # test-exit
            exit; -- # exit
         end if;
         Touch (E);  -- # loop_op
         N_Iterations := N_Iterations + 1; -- # loop_op
      end loop;
      Assert (N_Iterations <= Max_Iterations); -- # post-loop
   end;
   
end;
