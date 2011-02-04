package body Sensors.Predicates is
   function Nopass (S : Sensor) return Boolean is
   begin
      return False;
   end;

   function Pass (S : Sensor) return Boolean is
   begin
      return True;
   end;

   function Inrange (S : Sensor) return Boolean is
   begin
      return S.V >= S.ALB and then S.V <= S.AHB;
   end;
end;
