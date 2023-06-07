package Parent is
   type T is abstract tagged null record;

   function Compute (X : T) return Integer is abstract;
   function Valid (X : T) return Boolean is abstract;


   function Compute_C (X : T'Class) return Integer;
   function Valid_C (X : T'Class) return Boolean;
end Parent;
