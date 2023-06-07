package body Pkg is

   Ret_Val : Boolean;  -- # decl
   pragma Volatile (Ret_Val);

   function Foo return Boolean is
   begin
      return Ret_Val; -- # fun
   end Foo;

begin
   Ret_Val := A or else C; -- # elab_body
end Pkg;