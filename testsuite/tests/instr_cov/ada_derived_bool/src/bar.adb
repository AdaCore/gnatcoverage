with Ada.Text_IO;

package body Bar is

   procedure Msg (M : String) is
   begin
      Ada.Text_IO.Put_Line (M); -- # msg
   end Msg;

   procedure Proc (V : Rec; B : My_Boolean) is
   begin
      if B              -- # cond1
         and not V.Val  -- # cond2
      then
         Msg("Uh oh");  -- # then
      else
         Msg("Hooray"); -- # else
      end if;
   end Proc;

end Bar;
