package Counters is
   type Counter is record
      Value : Integer;
   end record;
   type Counter_Access is access all Counter;

   procedure Dec (C : Counter_Access);
   function Zerop (C : Counter_Access) return Boolean;
end;
