
package Regions is
   type Region (Size : Natural) is record
      Data : String (1 .. Size);
   end record;
   pragma Suppress_Initialization (Region);

   procedure Check (R : Region);
end;
