package Sensors.Status is

   --  Evaluate a sensor status from its history of values

   type Sensor_Status is
     (Undecidable,  -- No hist value available
      Ok,           -- All hist values within bounds
      Check,        -- 1   hist value not within bounds
      Broken);      -- >1  hist values not within bounds

   function Status_Of (S : Sensor) return Sensor_Status;

end;
