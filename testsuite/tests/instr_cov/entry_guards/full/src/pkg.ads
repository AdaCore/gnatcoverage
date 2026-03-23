package Pkg is
   protected type PT is
      procedure Init (A, B : Boolean);
      entry Wait_Cond;
   private
      A, B : Boolean := False; -- # init
   end PT;
end Pkg;
