package body Value is
   function F (X : Boolean) return Boolean is
      Temp : Boolean := X;  -- # eval :o/0:
   begin
      return X; -- # returnVal
   end;
end;
