package body Y.A_Child is

   procedure P_IA_1 (Y : in out T_Y_A) is
   begin
      Y.I1 := 100; -- # ya_p_ia_1
      Y.I2 := 100; -- # ya_p_ia_1
   end P_IA_1;

   procedure P_IA_2 (Y : in out T_Y_A) is
   begin
      Y.I1 := Y.I1 - 10;  -- # ya_p_ia_2
      Y.I2 := Y.I2 - 20;  -- # ya_p_ia_2
   end P_IA_2;

   function Test_IA (Y : T_Y_A) return Boolean is
   begin
      return Y.I1 < Y.I2;  -- # ya_test_ia
   end Test_IA;

end Y.A_Child;
