package body Goto_Next is
   function Identity (X : Integer) return Integer is
      X_Value : Integer;
   begin
      X_Value := X;   -- # Call
      goto Done;      -- # Call

      <<Done>>
      return X_Value; -- # Call
   end;
end;
