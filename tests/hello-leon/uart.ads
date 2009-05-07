package Uart is
   procedure Put (C : Character);

   procedure New_Line;
   procedure Put_Line (Str : String);
   procedure Put (Str : String);

   procedure Get (C : out Character);
end Uart;
