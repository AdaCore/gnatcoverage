
package body Pack is

   procedure Not_Fully_Covered_Stmt_Wise is
   begin
      if Where then
         N_True := N_True + 1;
      else
         N_False := N_False + 1;
      end if;
   end Not_Fully_Covered_Stmt_Wise;

end Pack;
