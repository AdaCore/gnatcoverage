package MX is

   type Xflags is record
      X1, X2, X3, Xh : Boolean;
   end record;

   procedure Trigger_0XR;
   --  Call into the functional code, arranging to get into none of the
   --  exempted regions

   procedure Trigger_XR1;
   procedure Trigger_XR2;
   procedure Trigger_XR3;
   --  Call into the functional code, arranging to get into the exempted
   --  region 1, 2 or 3 only

   --  This unit is a helper to trigger some pieces of the functional code by
   --  calling into it with well chosen set of arguments. It is not subject to
   --  coverage expectations.
end;
