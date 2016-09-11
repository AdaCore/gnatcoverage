with System;

package Objects is
   type T_Object is null record;
   
   procedure Probe (O : T_Object) is
      null; -- # test
      
   A_Probe : System.Address;
   procedure Process (O : T_Object);
end;
