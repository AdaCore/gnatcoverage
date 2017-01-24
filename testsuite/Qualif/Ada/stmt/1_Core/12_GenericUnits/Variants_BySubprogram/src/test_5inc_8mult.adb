with V5, V8; use V5, V8;
with Support; use Support;

-- Call inc only on the v5 instance and mult only on the v8 instance.

procedure Test_5inc_8mult is
   V5o : V5.Vector_Type := (others => 1);
   V8o : V8.Vector_Type := (others => 2);
begin
   V5.Inc (V5o, 3);
   Assert (V5o(1) = 4);
   
   V8.Mult (V8o, 2);
   Assert (V5o(2) = 4);
end;

--# vops.adb
-- defaults, for instances conslidated:
--  /mult/   l+ ## 0
--  /inc/    l+ ## 0

-- %cov: -S instance
--  =/mult/  l! ## s-@(i:V5)
--  =/inc/   l! ## s-@(i:V8)

-- -S instance needs to see the instantiation ALIs as well:

--# v5.ads

--# v8.ads


