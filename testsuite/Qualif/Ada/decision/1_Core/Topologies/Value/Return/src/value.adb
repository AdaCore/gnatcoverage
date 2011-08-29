package body Value is
   function F (X : Boolean) return Boolean is
   begin
      return X;  -- # eval :o/0:
   end;
end;
