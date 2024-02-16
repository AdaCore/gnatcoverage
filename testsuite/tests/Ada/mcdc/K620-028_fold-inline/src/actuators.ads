package Actuators is

   -- A very basic Actuator abstraction

   type Actuator is limited record

      Value : Integer;
      -- Current output level

      Safety : Integer;
      -- Value threshold beyond which output might
      -- cause hardware damage

   end record;
   pragma Volatile (Actuator);

   procedure Fast_Increment
     (A : in out Actuator; Value : Integer);
end;
