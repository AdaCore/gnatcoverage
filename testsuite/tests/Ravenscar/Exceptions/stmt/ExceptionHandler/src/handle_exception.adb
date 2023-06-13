with P; use P;
with Raise_Exception;

procedure Handle_Exception is
begin
   Raise_Exception;   -- # raise
exception
   when Constraint_Error =>
      Correct_Exception_Propagated := True;  -- # handled
   when others =>
      Wrong_Exception_Propagated := True;    -- # mis_handled
end Handle_Exception;
