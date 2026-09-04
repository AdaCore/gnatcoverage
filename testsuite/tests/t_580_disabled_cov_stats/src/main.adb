with Interfaces.C;
with System;

with Dis;
with Pkg;

procedure Main is
   I : Interfaces.C.int := 0;

   procedure C_Disabled (I : System.Address);
   pragma Import (C, C_Disabled, "c_disabled");
begin
   Pkg.Half (0);
   Dis (0);
   C_Disabled (I'Address);
end Main;
