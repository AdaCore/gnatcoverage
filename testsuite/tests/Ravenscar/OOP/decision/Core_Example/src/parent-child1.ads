package Parent.Child1 is
   type T1_T is new T with record
      C1 : Integer;
   end record;

   function Compute (X : T1_T) return Integer;
   function Valid (X : T1_T) return Boolean;
end Parent.Child1;
