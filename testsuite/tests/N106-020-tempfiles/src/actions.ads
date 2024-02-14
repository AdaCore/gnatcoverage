package Actions is

   type Action_Access is access procedure (X : Integer);

   procedure Process_Positive (X : Integer);
   procedure Process_Negative (X : Integer);
   procedure Process_Zero (X : Integer);

   procedure Process (X : Integer; Action : Action_Access);

end;
