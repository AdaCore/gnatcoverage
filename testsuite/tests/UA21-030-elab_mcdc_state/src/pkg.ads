package Pkg is

   A : Boolean := False;  -- # decl
   pragma Volatile (A);
   B : Boolean := True;   -- # decl
   pragma Volatile (B);

   function Foo return Boolean;

   C : Boolean := A and then B;  -- # elab_spec
   pragma Volatile (C);

end Pkg;