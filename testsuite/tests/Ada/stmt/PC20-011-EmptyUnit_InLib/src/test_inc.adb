with Support, Ops.Values; use Support, Ops.Values;

procedure Test_Inc is
   
   --  The library initialization procedure isn't called automatically in some
   --  configurations so we call it explicitly from here. Calling it multiple
   --  times is not a problem, so we're fine in configurations where it is
   --  called automatically before reaching here.
   
   procedure Mylib_Init;
   pragma Import (C, Mylib_Init, "mylibinit");

   K : Integer := 12;
   pragma Volatile (K);
begin
   Mylib_Init;
   Inc (K);
end;

--# ../mylib/ops.ads
-- /decl/ l. ## 0

--# ../mylib/ops-values.adb
-- /stmt/ l+ ## 0

