pragma Restrictions (Simple_Barriers);

package Pkg_1 is
   protected type PT is        --  SCO 1 (stmt)
      procedure Say_Hello;
      entry Wait_Cond;
      procedure Set_Cond (Value : Boolean);
   private
      Cond : Boolean := False; --  SCO 2 (stmt)
   end PT;
end Pkg_1;
