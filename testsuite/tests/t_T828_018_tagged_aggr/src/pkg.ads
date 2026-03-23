package Pkg is
   type T is tagged record
      I : Integer;
   end record;

   function Create (I : Integer) return T;
   procedure Foo; -- # foo
end Pkg;
