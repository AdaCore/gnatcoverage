with FUAND, Support; use FUAND, Support;

package body FUAND_Helper is

   --  Strings of length 0, 1 and 2

   S0 : aliased String := "";
   S1 : aliased String := (5 .. 5 => '1');
   S2 : aliased String := (1 .. 2 => '2');


   procedure Eval_FF_F is
   begin
      -- A'Length not > 0, A'Length not = B'Length
      Assert (Poslen_And_Eql ((A => S0'Access, B => S1'Access)) = False);
   end;

   procedure Eval_FT_F is
   begin
      -- A'Length not > 0, A'Length = B'Length
      Assert (Poslen_And_Eql ((A => S0'Access, B => S0'Access)) = False);
   end;

   procedure Eval_TF_F is
   begin
      -- A'length > 0, A'Length /= B'Length
      Assert (Poslen_And_Eql ((A => S1'Access, B => S0'Access)) = False);
   end;

   procedure Eval_TT_T is
   begin
      -- A'length > 0, A'Length = B'Length
      Assert (Poslen_And_Eql ((A => S2'Access, B => S2'Access)) = True);
   end;

end;

