package body X.B_Child is

   procedure P_IB_1 (X : in out T_X_B) is
   begin
      X.I1 := 0;   -- # xb_p_ib_1
      X.I2 := 0;   -- # xb_p_ib_1
   end P_IB_1;

   procedure P_IB_2 (X : in out T_X_B) is
   begin
      X.I1 := X.I1 + 10;  -- # xb_p_ib_2
      X.I2 := X.I2 + 20;  -- # xb_p_ib_2
   end P_IB_2;

   function Test_IB (X : T_X_B) return Boolean is
   begin
      return X.I1 > X.I2; -- # xb_test_ib
   end Test_IB;

end X.B_Child;
