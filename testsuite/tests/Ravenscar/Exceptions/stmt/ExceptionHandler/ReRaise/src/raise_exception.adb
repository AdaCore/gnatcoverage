with P; use P;

procedure Raise_Exception is
begin
   raise Constraint_Error;      -- # force_exception
exception
   when Constraint_Error =>
      raise;                    -- # raise
   when others =>
      Wrong_Exception_Propagated := True;   -- # wrong_exception
end;
