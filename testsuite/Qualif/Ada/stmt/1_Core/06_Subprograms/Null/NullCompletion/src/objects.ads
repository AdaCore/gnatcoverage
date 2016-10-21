with System;

package Objects is
   type T_Object is tagged null record;
   
   procedure Probe (O : T_Object);
   
   procedure Process (O : T_Object);
   
   A_Probe : System.Address;
end;
