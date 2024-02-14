pragma Ada_2012;

procedure Test_Foo is
   Bool1 : Boolean := True;
   pragma Volatile (Bool1);
   Bool2 : Boolean := True;
   pragma Volatile (Bool2);

   --  This test would fail to build if the intrumentation inverted the order
   --  of the declaration of Int1 and of the function Foo.

   Int1 : Integer := (if Bool1 and then Bool2 then 0 else 1);  -- # Integer
   pragma Volatile (Int1);

   function Foo return Integer is (Int1);                      -- # Expr_Fun
begin
   Int1 := Foo;
end Test_Foo;

--# test_foo.adb
--
-- /Integer/  l! ## dF-
-- /Expr_Fun/ l+ ## 0
