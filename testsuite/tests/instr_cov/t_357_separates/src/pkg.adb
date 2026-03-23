package body Pkg is

   package Sep is
      Foo : Boolean := True;  -- # decl
      function Bar return Boolean;
   end Sep;

   package body Sep is separate;

   function Qux return Boolean is
   begin
      return Sep.Bar; -- # eval
   end Qux;

end Pkg;
