package body Pkg is
   task body T is
      A, B : Boolean;                         -- # init
   begin
      accept Init (A, B : Boolean) do         -- # init
         T.A := A;                            -- # init
         T.B := B;                            -- # init
      end;

      loop
         select                               -- # init
            accept Set_A (Value : Boolean) do -- # set-a-stmts
               A := Value;                    -- # set-a-stmts
            end;
         or
            accept Set_B (Value : Boolean) do -- # set-b-stmts
               B := Value;                    -- # set-b-stmts
            end;
         or
            when A and then B =>              -- # wait-guard
               accept Wait_Cond;              -- # wait-stmts
         or
            terminate;
         end select;
      end loop;
   end T;
end Pkg;
