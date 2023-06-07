package body Pkg is

   -----------
   -- Clone --
   -----------

   function Clone (Self : Object'Class) return Object'Class
   is
   begin
      return Self; -- # clone
   end Clone;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Self : Model'Class) return Boolean is
   begin
      return not Self.Not_Null; -- # is-null
   end Is_Null;

end Pkg;
