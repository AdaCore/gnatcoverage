package Values is

   type Interval_T is abstract tagged null record;
   type Position_T is (In_Range, Out_Range);

   type Range_T is new Interval_T with record
      Lb, Ub : Integer;
   end record;

   type Point_T is new Interval_T with record
      V : Integer;
   end record;

   function Locate (X : Integer; I : Interval_T'Class) return Position_T;
end;
