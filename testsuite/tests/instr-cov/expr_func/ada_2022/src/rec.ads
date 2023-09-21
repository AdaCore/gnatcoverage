pragma Ada_2022;

package Rec is

   type T is tagged record
      X : Positive;
   end record;

   function Fact (Input : T) return Positive is
     (if Input.X = 1 then 1 else Input.X * Fact ((X => Input.X - 1))); -- # expr_dc :o/d:

end Rec;
