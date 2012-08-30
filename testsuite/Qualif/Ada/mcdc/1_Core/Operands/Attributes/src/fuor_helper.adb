with FUOR, Support; use FUOR, Support;

package body FUOR_Helper is

   --  Strings of length 0, 1 and 2

   S0 : aliased String := "";
   S1 : aliased String := (5 .. 5 => '1');
   S2 : aliased String := (1 .. 2 => '2');

   procedure Eval_FF_F is
   begin
      -- A not Empty, A'Length not = B'Length
      Assert (not Empty_Or_Eql ((A => S2'Access, B => S1'Access)));
   end;

   procedure Eval_FT_T is
   begin
      -- A not Empty, A'Length = B'Length
      Assert (Empty_Or_Eql ((A => S1'Access, B => S1'Access)));
   end;

   procedure Eval_TF_T is
   begin
      -- A Empty, A'Length not = B'Length
      Assert (Empty_Or_Eql ((A => S0'Access, B => S1'Access)));
   end;

   procedure Eval_TT_T is
   begin
      -- A Empty, A'Length = B'Length
      Assert (Empty_Or_Eql ((A => S0'Access, B => S0'Access)));
   end;

end;

