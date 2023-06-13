package Buffer is

   protected type Buffer is

      --  Synchronisation buffer for two threads.

      entry Push (V : Integer; Tell : Boolean);
      --  Hang until the buffer is free and fill it with V

      entry Pop (V : out Integer; Tell : Boolean);
      --  Hang until the buffer is filled, set V and make the buffer empty

   private
      Value : Integer;               -- # component_decl
      Has_Value : Boolean := False;  -- # component_decl
   end;

end Buffer;
