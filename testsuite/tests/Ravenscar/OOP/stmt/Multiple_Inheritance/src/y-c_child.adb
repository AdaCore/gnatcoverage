package body Y.C_Child is

   function Test_IA (Y : T_Y_C) return Boolean is
   begin
      return Y.I1 > Y.I2;      -- # yc_test_ia
   end Test_IA;

   function Test_IB (Y : T_Y_C) return Boolean is
   begin
      return Y.I1 > Y.I2;      -- # yc_test_ib
   end Test_IB;

   function Test_IC (Y : T_Y_C) return Boolean is
   begin
      return Y.I1 > Y.I2;      -- # yc_test_ic
   end Test_IC;

end Y.C_Child;
