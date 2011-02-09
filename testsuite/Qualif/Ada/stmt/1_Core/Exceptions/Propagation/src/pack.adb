package body Pack is
   pragma Unsuppress (All_Checks);

   procedure Proc_With_Blocks (I, J, K : in out My_Int) is
      pragma Unsuppress (All_Checks);
   begin
      begin
         begin
            begin
               begin
                  if K = My_Int'Last then      -- # if1
                     raise Exception_1;         -- # raise1
                  end if;

                  if J = My_Int'Last then      -- # if2
                     raise Exception_2;         -- # raise2
                  end if;

                  J := J + 10;                 -- # after_raise2
                  K := K + 10;                 -- # after_ce_raise
                  I := I + 10;                 -- # after_ce_raise
               exception
                  when Constraint_Error =>
                     J := 0;                   -- # handler_CE
               end;

               K := K / (I - J);               -- # after_block_1
            exception
               when Exception_1 =>
                  K := 0;                      -- # handler_E1
            end;

            I := I + K;                        -- # after_block_2

         exception
           when Exception_2 =>
              J := 0;                          -- # handler_E2
         end;

         I := J + 50;                          -- # after_block_3
         J := K + 50;                          -- # after_block_3

      exception
         when others =>
            I := 1;                            -- # handler_others
            J := 1;                            -- # handler_others
            K := 1;                            -- # handler_others
      end;
   end Proc_With_Blocks;
end Pack;
