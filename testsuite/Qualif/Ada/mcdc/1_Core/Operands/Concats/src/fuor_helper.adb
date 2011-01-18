with FUOR, Support; use FUOR, Support;

package body FUOR_Helper is

   procedure Eval_FF_F is
   begin
      Assert (Postfits_or (S     => "a",
                           Post1 => ".txt",
                           Post2 => ".obj",
                           Max   => 3) = False);
   end;

   procedure Eval_FT_T is
   begin
      Assert (Postfits_or (S     => "a",
                           Post1 => ".txt",
                           Post2 => ".c",
                           Max   => 3) = True);
   end;

   procedure Eval_TF_T is
   begin
      Assert (Postfits_or (S     => "a",
                           Post1 => ".c",
                           Post2 => ".obj",
                           Max   => 3) = True);
   end;

   procedure Eval_TT_T is
   begin
      Assert (Postfits_or (S     => "a",
                           Post1 => ".c",
                           Post2 => ".obj",
                           Max   => 10) = True);
   end;

end;

