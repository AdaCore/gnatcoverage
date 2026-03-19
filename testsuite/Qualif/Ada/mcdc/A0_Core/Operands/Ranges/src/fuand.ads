--  Provider of a functional and-then decision evaluator where
--  conditions involve explicit range checks

with Sensors; use Sensors;

package FUAND is
   function Both_In_Range (Ops : Sensor_Pair) return Boolean;
   -- Whether both Ops.A and Ops.B are such that ".v in .lo .. .hi"
end;
