package body Y.B_Child is

   procedure P_IB_1 (Y : in out T_Y_B) is
   begin
      Y.I1 := 100;  -- # yb_p_ib_1
      Y.I2 := 100;  -- # yb_p_ib_1
   end P_IB_1;

   procedure P_IB_2 (Y : in out T_Y_B) is
   begin
      Y.I1 := Y.I1 - 10;  -- # yb_p_ib_2
      Y.I2 := Y.I2 - 20;  -- # yb_p_ib_2
   end P_IB_2;

   function Test_IB (Y : T_Y_B) return Boolean is
   begin
      return Y.I1 < Y.I2;  -- # yb_test_ib
   end Test_IB;

end Y.B_Child;
