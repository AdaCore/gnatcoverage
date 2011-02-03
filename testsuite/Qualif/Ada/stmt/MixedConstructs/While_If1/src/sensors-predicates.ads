package Sensors.Predicates is
   function Nopass (S : Sensor) return Boolean;
   function Pass (S : Sensor) return Boolean;
   function Inrange (S : Sensor) return Boolean;

   type SP_Access is access function (S : Sensor) return Boolean;

end;
