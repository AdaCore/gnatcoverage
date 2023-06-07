package Parent.Child2 is
   type T2_T is new T with record
      C1 : Integer;
   end record;

   function Compute (X : T2_T) return Integer;
   function Valid (X : T2_T) return Boolean;
end Parent.Child2;
