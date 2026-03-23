package body Robots is

   function Current_Mode (R : Robot) return Opmode is
   begin
      return R.Mode;
   end;

   function Unsafe (C : Command) return Boolean is
   begin
      return C = Step;
   end;

   procedure Run (R : in out Robot; C : Command) is
      Mode : Opmode;
   begin
      Mode := Current_Mode (R);
      if Mode = Cautious
        and then Unsafe (C)
      then
         return;
      end if;
      R.Exec := R.Exec + 1;
   end;

end;
