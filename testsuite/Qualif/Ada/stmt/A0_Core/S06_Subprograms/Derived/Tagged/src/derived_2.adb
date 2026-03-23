package body Derived_2 is
   function Fun2 (X : Derived_T_2) return Integer is
   begin
      return X.I - 1;        -- # fun2
   end Fun2;

   procedure Proc2 (X : in out Derived_T_2) is
   begin
      X.I := X.I + 10;        -- # proc2
   end Proc2;
end Derived_2;
