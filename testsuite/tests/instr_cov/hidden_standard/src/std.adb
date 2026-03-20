package body Std is
   Top_Category : Boolean := (Cat = Premium);

   type My_Bool is new Boolean;

   procedure Process (X : in out Integer) is
   begin
      if My_Bool (Top_Category) then -- # eval
         X := X + 15;
      end if;
   end Process;
end Std;
