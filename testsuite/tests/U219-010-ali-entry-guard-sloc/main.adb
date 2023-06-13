procedure Main is

   task type My_Task is
      entry P1;
      entry P2;
      entry P3;
   end My_Task;

   task body My_Task is
   begin
      select
         accept P1 do
            null;
         end P1;
      or when (not True) =>
         accept P2 do
            null;
         end P2;
      or
         accept P3 do
            null;
         end P3;
      end select;
   end My_Task;

   T : My_Task;
begin
   T.P1;
end Main;
