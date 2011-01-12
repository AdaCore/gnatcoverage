with FUAND, Support; use FUAND, Support;

package body FUAND_Helper is

   --  Strings of length 0, 1 and 2

   S0 : String_Access := new String (1 .. 0);
   S1 : String_Access := new String (5 .. 5);
   S2 : String_Access := new String (1 .. 2);

   procedure Eval_FF_F is
   begin
      -- A'Length not > 0, A'Length not = B'Length
      Assert (Poslen_And_Eql ((A => S0, B => S1)) = False);
   end;

   procedure Eval_FT_F is
   begin
      -- A'Length not > 0, A'Length = B'Length
      Assert (Poslen_And_Eql ((A => S0, B => S0)) = False);
   end;

   procedure Eval_TF_F is
   begin
      -- A'length > 0, A'Length /= B'Length
      Assert (Poslen_And_Eql ((A => S1, B => S0)) = False);
   end;

   procedure Eval_TT_T is
   begin
      -- A'length > 0, A'Length = B'Length
      Assert (Poslen_And_Eql ((A => S2, B => S2)) = True);
   end;

end;

