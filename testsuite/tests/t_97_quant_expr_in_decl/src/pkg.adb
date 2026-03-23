pragma Ada_2012;

package body Pkg is

   --------------
   -- All_True --
   --------------

   function All_True (Arr : My_Arr) return Boolean is
      Res : constant Boolean := (for all Elt of Arr => Elt); -- # decl
   begin
      return Res; -- # ret
   end All_True;

   ---------------
   -- Some_True --
   ---------------

   function Some_True (Arr : My_Arr) return Boolean is
      Res : constant Boolean := (for some Elt of Arr => Elt); -- # decl
   begin
      return Res; -- # ret
   end Some_True;

end Pkg;
