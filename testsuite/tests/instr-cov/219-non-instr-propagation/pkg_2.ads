pragma Restrictions (Simple_Barriers);

package Pkg_2 is
   protected type PT is        --  SCO 1 (stmt)
      procedure Set_Cond (Value : Boolean);
      entry Wait_Cond;
   private
      Cond : Boolean := False; --  SCO 2 (stmt)
   end PT;
end Pkg_2;
