package body Local_Instantiations is

   procedure Proc_With_Instantiations
     (I, J   : in out Integer;
      B1, B2 : in out Boolean)
   is
      package Local_Pack_Instance is new Pack.Pack_G (Boolean, True);
      package Local_Stack is new Stacks_G (Integer, 0);

      S : Local_Stack.Stack := Local_Stack.Default_Stack;
   begin
      Local_Pack_Instance.Swap (B1, B2);

      Local_Stack.Push (J, S);
      Local_Stack.Pop (I, S);
      J := Local_Stack.N_Values (Local_Stack.Default_Stack);
   end Proc_With_Instantiations;

   function Fun_With_Instantiations (I : Integer) return Integer is
      procedure Update is new Update_G (Integer);
      Res : Integer := I;
   begin
      Update (Res);
      return Res;
   end Fun_With_Instantiations;
end  Local_Instantiations;
