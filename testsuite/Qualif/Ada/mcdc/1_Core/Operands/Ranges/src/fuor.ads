--  Provider of a functional or-else decision evaluator where
--  conditions involve explicit range checks

with Sensors; use Sensors;

package FUOR is
   function One_In_Range (Ops : Sensor_Pair) return Boolean;
   -- Whether both Ops.A and Ops.B are such that ".v in .lo .. .hi"
end;
