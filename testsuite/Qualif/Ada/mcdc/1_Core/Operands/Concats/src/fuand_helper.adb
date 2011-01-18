with FUAND, Support; use FUAND, Support;

package body FUAND_Helper is

   procedure Eval_FF_F is
   begin
      Assert (Postfits_And (S     => "a",
                            Post1 => ".c",
                            Post2 => ".o",
                            Max   => 2) = False);
   end;

   procedure Eval_FT_F is
   begin
      Assert (Postfits_And (S     => "a",
                            Post1 => ".txt",
                            Post2 => ".o",
                            Max   => 3) = False);
   end;

   procedure Eval_TF_F is
   begin
      Assert (Postfits_And (S     => "a",
                            Post1 => ".c",
                            Post2 => ".obj",
                            Max   => 3) = False);
   end;

   procedure Eval_TT_T is
   begin
      Assert (Postfits_And (S     => "a",
                            Post1 => ".c",
                            Post2 => ".obj",
                            Max   => 10) = True);
   end;

end;

