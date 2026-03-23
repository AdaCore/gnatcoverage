with Ada.Text_IO; use Ada.Text_IO;

procedure Main is

   task type My_Task is
      entry Pass_Boolean (B : Boolean);
      entry Pass_Integer (I : Integer);
   end My_Task;

   task body My_Task is
      Stored_B : Boolean;
   begin
      select
         accept Pass_Boolean (B : Boolean) do
            Stored_B := B;
         end Pass_Boolean;

         Put_Line ("Hello, world!");
         if Stored_B then
            Put_Line ("B was True");
         end if;
      or
         accept Pass_Integer (I : Integer) do
            pragma Unreferenced (I);
            null;
         end Pass_Integer;
      end select;
   end My_Task;

   T : My_Task;
begin
   T.Pass_Boolean (False);
end Main;
