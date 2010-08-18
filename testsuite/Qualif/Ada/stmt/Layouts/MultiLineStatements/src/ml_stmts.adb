package body Ml_Stmts is
   procedure Eval_And_Not (A, B : Boolean; E : out Boolean) is
      Not_B : Boolean;
   begin
      Not_B := not B;  -- # Statementmark
      E :=             -- # Statementmark
        A              -- # Linemark
        and            -- # Linemark
        Not_B;         -- # Linemark
   end;
end;
