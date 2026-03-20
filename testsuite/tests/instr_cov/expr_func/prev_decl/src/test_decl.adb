pragma  Ada_2012;

with Foo;
with Bar;
procedure Test_Decl is
   Obj : Foo.T;
   Res : Bar.T_Bar;
   pragma Volatile (Res);
begin
   Res := Obj.Prim;
end Test_Decl;

--# foo.ads
--
-- /decl/ l. ## 0
-- /EF/   l+ ## 0
