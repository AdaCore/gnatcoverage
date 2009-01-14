
--  This package exposes a very basic and incomplete Engine abstraction

package Engines is
   
   --  The Engine abstraction per se
   
   type Engine is record
      P, T : Integer; --  Internal state: Pressure & Temperature
   end record;
   
   --  Pressure and Temperature thresholds wrt engine stability
   
   Stable_P : constant := 10;
   Stable_T : constant := 50;
   
   --  The engine synthetic state: Critical if both parameters are beyond
   --  their respective stability threshold, Alarming if one parameter is,
   --  Stable otherwise.
   
   type State is (Stable, Alarming, Critical);
   
   function State_Of (E : Engine) return State;
   
end;


