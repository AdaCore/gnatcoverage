with FUAND, Pools, Support; use FUAND, Pools, Support;

package body FUAND_Helper is

   procedure Eval_FF_F is
   begin
      Assert (Invalid_Val_And_Loc
                ((Val => Valid_Val, Loc => Valid_Loc)) = False);
   end;

   procedure Eval_FT_F is
   begin
      Assert (Invalid_Val_And_Loc
                ((Val => Valid_Val, Loc => Invalid_Loc)) = False);
   end;

   procedure Eval_TF_F is
   begin
      Assert (Invalid_Val_And_Loc
                ((Val => Invalid_Val, Loc => Valid_Loc)) = False );
   end;

   procedure Eval_TT_T is
   begin
      Assert (Invalid_Val_And_Loc
                ((Val => Invalid_Val, Loc => Invalid_Loc)) = True);
   end;

end;
