with C; use C;
package Y.C_Child is
   type T_Y_C is new T_Y and IC with record
      I2 : Integer;
   end record;

   function Test_IA (Y : T_Y_C) return Boolean;
   function Test_IB (Y : T_Y_C) return Boolean;
   function Test_IC (Y : T_Y_C) return Boolean;

end Y.C_Child;
