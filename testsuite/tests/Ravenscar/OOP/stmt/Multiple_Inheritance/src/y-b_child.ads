with B; use B;
package Y.B_Child is
   type T_Y_B is new T_Y and IB with record
      I2 : Integer;
   end record;

   procedure P_IB_1 (Y : in out T_Y_B);
   procedure P_IB_2 (Y : in out T_Y_B);

   function Test_IB (Y : T_Y_B) return Boolean;

end Y.B_Child;
