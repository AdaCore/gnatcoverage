with Ctl; use Ctl;

package body Ops is
   procedure Inc (X : in out Integer) is
   begin
      X := X + 1;                    -- # incOp
      if Count_Ops then              -- # incCheck
         Opcount := Opcount + 1;     -- # incCount
      end if;
   end;
begin
   V := V + 2;                 -- # elabOp
   if Count_Ops then           -- # elabCheck
      Opcount := Opcount + 1;  -- # elabCount
   end if;
end;
