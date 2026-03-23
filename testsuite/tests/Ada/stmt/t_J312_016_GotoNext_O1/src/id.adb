function Id (X : Integer) return Integer is
   X_Value : Integer; -- # decl
begin
   X_Value := X;   -- # Call
   goto Done;      -- # Call
<<Done>>
    return X_Value; -- # Call
end;
