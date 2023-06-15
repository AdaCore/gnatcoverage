with C; use C;
package X.C_Child is
   type T_X_C is new T_X and IC with record
      I2 : Integer;
   end record;

   function Test_IA (X : T_X_C) return Boolean;
   function Test_IB (X : T_X_C) return Boolean;
   function Test_IC (X : T_X_C) return Boolean;

end X.C_Child;
