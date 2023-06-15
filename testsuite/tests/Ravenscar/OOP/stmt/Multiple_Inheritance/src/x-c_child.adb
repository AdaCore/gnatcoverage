package body X.C_Child is

   function Test_IA (X : T_X_C) return Boolean is
   begin
      return X.I1 > X.I2;     -- # xc_test_ia
   end Test_IA;

   function Test_IB (X : T_X_C) return Boolean is
   begin
      return X.I1 > X.I2;     -- # xc_test_ib
   end Test_IB;

   function Test_IC (X : T_X_C) return Boolean is
   begin
      return X.I1 > X.I2;     -- # xc_test_ic
   end Test_IC;

end X.C_Child;
