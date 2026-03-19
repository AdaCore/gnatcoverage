pragma Debug_Policy (Check);

with Support; use Support;

package body Pval is
   function F (X : Boolean) return Boolean is
      Res : Boolean := False;           -- # true

      procedure Set_Res_True is
      begin
         Res := True;                 -- # false
      end Set_Res_True;

   begin
      pragma Debug (Identity (not X), Set_Res_True); -- # eval
      return Res;                      -- # ret
   end F;
end Pval;
