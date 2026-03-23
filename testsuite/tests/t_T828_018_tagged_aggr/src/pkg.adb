package body Pkg is
   function Create (I : Integer) return T is
   begin
      return (I => I); -- # create
   end Create;

   procedure Foo is
   begin
      null; -- # foo
   end Foo;
end Pkg;
