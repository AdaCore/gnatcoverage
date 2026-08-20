package Pkg is
   type T is tagged record
     I : Integer;
   end record;

   function Make_T (I : Integer) return T is (T'(I => I));
end Pkg;
