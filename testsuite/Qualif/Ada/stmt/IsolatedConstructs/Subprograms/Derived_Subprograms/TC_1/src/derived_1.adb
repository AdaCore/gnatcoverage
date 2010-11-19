package body Derived_1 is
   procedure Proc2 (X : in out Derived_T_1) is
   begin
      X.I := X.I + 2;        -- # proc2
   end Proc2;
end Derived_1;
