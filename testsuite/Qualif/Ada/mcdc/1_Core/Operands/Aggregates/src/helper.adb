with Blocks, Support; use Blocks, Support;

package body Helper is

   --  The decision is like A or else B, each operand being a check
   --  for a possible form of terminator.

   Term_A : Block := "null";
   Term_B : Block := (5 .. 8 => 'x');

   Noterm : Block := "abcdef";

   procedure Eval_FF_F is
   begin
      Assert (not Terminator (Noterm));
   end;

   procedure Eval_FT_T is
   begin
      Assert (Terminator (Term_B));
   end;

   procedure Eval_TF_T is
   begin
      Assert (Terminator (Term_A));
   end;

   procedure Eval_TT_T is
   begin
      Assert (Terminator (Term_A));
   end;

end;
