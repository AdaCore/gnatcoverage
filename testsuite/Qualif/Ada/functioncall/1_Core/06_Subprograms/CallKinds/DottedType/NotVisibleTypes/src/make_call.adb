pragma Ada_2012;

--  LIMITATION : invisible return types
--  Import Pkg2 to make function F visible. F returns type T defined in Pkg1,
--  which is not imported here making T invisible.
--  With the current function and call coverage instrumentation solution, we
--  would need T to be visible. Because of this, gnatcov cannot instrument
--  calls to such functions.

with Pkg2;

procedure Make_Call             -- # test
is
   Dummy : Boolean := Pkg2.F.B; -- # dummy
begin
   null;                        -- # null
end Make_Call;
