with Lib_Init;

package body Lib_Loop_Statements is
   
   N : Integer;
   
   function N_Ones return Integer is
   begin
      N := 0; -- # count
      for I in Values'Range loop -- # iter
         if Values (I) = 1 then  -- # test-inc
            N := N + 1;          -- # inc
         end if;
      end loop;
      return N; -- # count
   end;
   
begin
   N := 0; -- # elab
   while N < Lib_Init.N1 loop -- # test-init
      Values (Values'First + N + 1) := 1; -- # init
      N := N + 1;                        -- # init
   end loop;
end;
