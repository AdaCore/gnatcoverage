with B; use B;
package X.B_Child is
   type T_X_B is new T_X and IB with record
      I2 : Integer;
   end record;

   procedure P_IB_1 (X : in out T_X_B);
   procedure P_IB_2 (X : in out T_X_B);

   function Test_IB (X : T_X_B) return Boolean;

end X.B_Child;
