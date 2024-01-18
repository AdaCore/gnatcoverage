package Pkg is
   protected type PT is
      procedure Init (Cond : Boolean);
      entry Wait_Cond;
   private
      Cond : Boolean := False;
   end PT;
end Pkg;
