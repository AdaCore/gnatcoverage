package body Gstacks is

   procedure Push (V : Value; S : in out Stack) is
   begin
      S.Store (S.N_Values + 1) := V;  -- # Xpush
      S.N_Values := S.N_Values + 1;   -- # Xpush
   end;

   procedure Pop (V : out Value; S : in out Stack) is
   begin
      V := S.Store (S.N_Values);      -- # Xpop
      S.N_Values := S.N_Values - 1;   -- # Xpop
   end;

   function N_Values (S : in Stack) return Natural is
   begin
      return S.N_Values;              -- # Xnval
   end;
end;
