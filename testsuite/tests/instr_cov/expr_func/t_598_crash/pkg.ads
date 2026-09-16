package Pkg is
   subtype Count_Type is No_Such_Type;
   type Vector is tagged null record;
   function Length (Self : Vector) return Count_Type;
   function Empty_Vector return Vector is (0);
end Pkg;
