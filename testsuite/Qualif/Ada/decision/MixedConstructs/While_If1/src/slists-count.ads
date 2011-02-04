with Sensors.Predicates; use Sensors.Predicates;

package Slists.Count is
   procedure Count_In
     (SL : Sensor_List;
      SP : SP_Access := Pass'Access;
      NT, NF : out Natural);
end;

