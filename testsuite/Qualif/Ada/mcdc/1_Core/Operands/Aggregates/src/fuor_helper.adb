with FUOR, Support; use FUOR, Support;

package body FUOR_Helper is

   --  The decision is like A or else B, each operand being a check
   --  for a possible form of terminator.

   Invalid_Val : Value := "null";
   Invalid_Loc : Pool  := (others => Noloc_Id);

   Valid_Val : Value := "abcd";
   Valid_Loc : Pool  := (others => 'C');

   procedure Eval_FF_F is
   begin
      Assert (not Invalid ((Val => Valid_Val, Loc => Valid_Loc)));
   end;

   procedure Eval_FT_T is
   begin
      Assert (Invalid ((Val => Valid_Val, Loc => Invalid_Loc)));
   end;

   procedure Eval_TF_T is
   begin
      Assert (Invalid ((Val => Invalid_Val, Loc => Valid_Loc)));
   end;

   procedure Eval_TT_T is
   begin
      Assert (Invalid ((Val => Invalid_Val, Loc => Invalid_Loc)));
   end;

end;
