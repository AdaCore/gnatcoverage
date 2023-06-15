package body Types_A is
   
   function Valid (X : Int) return Boolean is
   begin
      return X.Value <= X.UB;
   end;

end;
