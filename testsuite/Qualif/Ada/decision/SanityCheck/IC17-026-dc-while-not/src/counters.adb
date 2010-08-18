package body Counters is

   procedure Dec (C : Counter_Access) is
   begin
      C.Value := C.Value - 1;
   end;

   function Zerop (C : Counter_Access) return Boolean is
   begin
      return C.Value = 0;
   end;
end;
