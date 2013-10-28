with System;

package Data is
   Data_Array : aliased array (1 .. 1) of Integer;
   function Data_Address (Index : Integer) return System.Address;
end;
