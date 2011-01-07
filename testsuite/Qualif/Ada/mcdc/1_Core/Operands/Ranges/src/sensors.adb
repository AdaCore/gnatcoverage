package body Sensors is

   function Both_OK (A, B : Sensor) return Boolean is
   begin
      return A.V in A.Lo .. A.Hi and then B.V in B.Lo .. B.Hi;  -- # evaluate
   end;
end;
