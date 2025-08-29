package body Commands is

   procedure Stat (Safe : Boolean) is
   begin
      if Safe then
         N_Safe := N_Safe + 1;
      else
         N_Unsafe := N_Unsafe + 1;
      end if;
   end Stat;

   function Safe (Cmd : Command; Front : Perceived) return Boolean is

      --  Standing straight is always safe, and any other action is
      --  safe as soon as there is just solid ground ahead:
      Result : constant Boolean := Cmd = Hold or else Front = Ground;
   begin
      Stat (Result);
      return Result;
   end Safe;
end Commands;
