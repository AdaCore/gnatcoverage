with A; use A;
package X.A_Child is
   type T_X_A is new T_X and IA with record
      I2 : Integer;
   end record;

   procedure P_IA_1 (X : in out T_X_A);
   procedure P_IA_2 (X : in out T_X_A);

   function Test_IA (X : T_X_A) return Boolean;

end X.A_Child;
