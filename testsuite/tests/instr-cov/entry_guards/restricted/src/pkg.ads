pragma Restrictions (Pure_Barriers);

package Pkg is
   protected type PT is
      procedure Init (A, B : Boolean);
      entry Wait_Cond;
   private
      A, B : Boolean := False;
   end PT;
end Pkg;
