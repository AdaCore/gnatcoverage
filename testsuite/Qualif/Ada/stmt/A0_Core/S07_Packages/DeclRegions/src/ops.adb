package body Ops is

   --  Body of private op, declared in private part

   procedure Op_Private (Opd : in out Opdata) is
   begin
      Opd.Np := Opd.Np + 1; -- # private
   end;

   --  Body of entirely internal op

   procedure Op_Internal (Opd : in out Opdata) is
   begin
      Opd.Ni := Opd.Ni + 1; -- # internal
   end;

   --  Body of public interface

   procedure Call_Ops (Pop, Iop : Boolean; Opd : in out Opdata) is
   begin
      if Pop then     -- # ops
         Op_Private (Opd);  -- # private
      end if;
      if Iop then     -- # ops
         Op_Internal (Opd); -- # internal
      end if;
   end;
end;
