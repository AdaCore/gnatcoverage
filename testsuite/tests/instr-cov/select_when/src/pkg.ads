package Pkg is
   task type T is
      entry Init (A, B : Boolean);
      entry Set_A (Value : Boolean);
      entry Set_B (Value : Boolean);
      entry Wait_Cond;
   end T;
end Pkg;
