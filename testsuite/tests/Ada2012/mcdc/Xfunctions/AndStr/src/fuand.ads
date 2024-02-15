pragma Ada_2012;

package FUAND is

   type Coord is range 0 .. 9;
   type Object is abstract tagged record
      X, Y : Coord;
   end record;

   function Shape (O : Object) return String is abstract;
   function Color (O : Object) return String is abstract;

   function Line (O : Object) return String;
   function Col  (O : Object) return String;

   function Fight (A, B : Object'Class) return Boolean is
      ((Color(A) & "-" & Shape(A)) /= (Color(B) & "-" & Shape(B)) and then (Line(A) & Col(A)) = (Line(B) & Col(B))); -- # eval

   procedure Test_A;
   procedure Test_B;
   procedure Test_T;

end;
