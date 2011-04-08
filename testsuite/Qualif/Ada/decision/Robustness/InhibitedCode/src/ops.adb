with Ctl; use Ctl;

package body Ops is
   procedure Inc (X : in out Integer) is
   begin
      X := X + 1;                    -- # incOp
      if Count_Ops then              -- # incCheck
         Opcount := Opcount + 1;     -- # incCount
         if Opcount > 50 then        -- # incCount
            raise Program_Error;     -- # incCount
         end if;
      end if;
   end;
begin
   V := V + 2;                 -- # elabOp
   if Count_Ops then           -- # elabCheck
      Opcount := Opcount + 1;  -- # elabCount
      if Opcount > 50 then     -- # elabCount
         raise Program_Error;  -- # elabCount
      end if;
   end if;
end;
