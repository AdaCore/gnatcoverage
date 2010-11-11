package MX is

   procedure Trigger_0XR;
   --  Call into the functional code, arranging to get into none of the
   --  exempted regions

   procedure Trigger_XR1;
   procedure Trigger_XR2;
   procedure Trigger_XR3;
   --  Call into the functional code, arranging to get into the exempted
   --  region 1, 2 or 3 only

end;
