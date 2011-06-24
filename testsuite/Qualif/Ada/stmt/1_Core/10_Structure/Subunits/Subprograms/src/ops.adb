package body Ops is

   procedure Vsub (Opd: in out Opdata) is separate;
   procedure Psub (Opd: in out Opdata) is separate;

   procedure Touch (Opd: in out Opdata) is
   begin
      Opd.Nops_Done := Opd.Nops_Done + 1; -- # touch
   end;

   -- Internal subprogram here --
   procedure Isub (Opd: in out Opdata) is separate;

   procedure Do_Ops (V, P, I : Boolean; Opd: in out Opdata) is
   begin
      if V then     -- # doops
         Vsub (Opd); -- # vsub
      end if;
      if P then     -- # doops
         Psub (Opd); -- # psub
      end if;
      if I then     -- # doops
         Isub (Opd); -- # isub
      end if;
   end;

end;
