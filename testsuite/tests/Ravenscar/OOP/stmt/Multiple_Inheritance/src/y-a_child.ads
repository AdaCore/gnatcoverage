with A; use A;
package Y.A_Child is
   type T_Y_A is new T_Y and IA with record
      I2 : Integer;
   end record;

   procedure P_IA_1 (Y : in out T_Y_A);
   procedure P_IA_2 (Y : in out T_Y_A);

   function Test_IA (Y : T_Y_A) return Boolean;

end Y.A_Child;
