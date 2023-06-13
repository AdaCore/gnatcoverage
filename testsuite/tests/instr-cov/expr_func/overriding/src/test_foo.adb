pragma Ada_2012;

with Pak; use Pak;

procedure Test_Foo is
   Dummy_1 : constant T := Make (False);
   Dummy_2 : constant TT := Make (True);
begin
   null;
end Test_Foo;

--# pak.ads
--
--%opts: --trace-mode=src
-- /dc/ l! ## dF-
--%opts: --trace-mode=bin
-- /dc/ l! ## d!
