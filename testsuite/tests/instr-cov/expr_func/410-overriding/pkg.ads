pragma Ada_2012;

package Pkg is

   type T is tagged null record;
   function T_Prim (Obj : T; A, B : Boolean) return Boolean is (A or else B);

   type T_Child is new T with record
      X : Integer;
   end record;
   overriding function T_Prim (Obj : T_Child; A, B : Boolean) return Boolean;

end Pkg;
