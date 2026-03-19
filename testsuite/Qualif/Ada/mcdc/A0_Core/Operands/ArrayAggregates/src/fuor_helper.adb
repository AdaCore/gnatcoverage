with FUOR, Pools, Support; use FUOR, Pools, Support;

package body FUOR_Helper is

   procedure Eval_FF_F is
   begin
      Assert (Invalid_Val_Or_Loc
                ((Val => Valid_Val, Loc => Valid_Loc)) = False);
   end;

   procedure Eval_FT_T is
   begin
      Assert (Invalid_Val_Or_Loc
                ((Val => Valid_Val, Loc => Invalid_Loc)) = True);
   end;

   procedure Eval_TF_T is
   begin
      Assert (Invalid_Val_Or_Loc
                ((Val => Invalid_Val, Loc => Valid_Loc)) = True);
   end;

   procedure Eval_TT_T is
   begin
      Assert (Invalid_Val_Or_Loc
                ((Val => Invalid_Val, Loc => Invalid_Loc)) = True);
   end;

end;
