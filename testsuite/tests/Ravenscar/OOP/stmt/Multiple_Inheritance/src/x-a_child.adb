package body X.A_Child is

   procedure P_IA_1 (X : in out T_X_A) is
   begin
      X.I1 := 0;    -- # xa_p_ia_1
      X.I2 := 0;    -- # xa_p_ia_1
   end P_IA_1;

   procedure P_IA_2 (X : in out T_X_A) is
   begin
      X.I1 := X.I1 + 10; -- # xa_p_ia_2
      X.I2 := X.I2 + 20; -- # xa_p_ia_2
   end P_IA_2;

   function Test_IA (X : T_X_A) return Boolean is
   begin
      return X.I1 > X.I2;  -- # xa_test_ia
   end Test_IA;

end X.A_Child;
