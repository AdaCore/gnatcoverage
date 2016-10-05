with Support; use Support;
with Objects; use Objects;

procedure Test_Objects_0 is
begin
   Assert (N_Positives = 0);
end;

--# objects.adb
--  /test/ l- ## s-
--  /pos/  l- ## s-
--  /pragma/ l- ## s-

-- Old compilers are less precise in tracking
-- dominance 

-- %tags:(7.0.2|7.2.2)
-- =/pragma/ ~l- ## ~s-
