with FUOR, Support; use FUOR, Support;

package body FUOR_Helper is

   S0 : String_Access := new String (1 .. 0);
   S1 : String_Access := new String (5 .. 5);
   S2 : String_Access := new String (1 .. 2);

   procedure Eval_FF_F is
   begin
      Assert (not Empty_Or_Eql ((A => S2, B => S1)));
   end;

   procedure Eval_FT_T is
   begin
      Assert (Empty_Or_Eql ((A => S1, B => S1)));
   end;

   procedure Eval_TF_T is
   begin
      Assert (Empty_Or_Eql ((A => S0, B => S1)));
   end;

   procedure Eval_TT_T is
   begin
      Assert (Empty_Or_Eql ((A => S0, B => S0)));
   end;

end;

