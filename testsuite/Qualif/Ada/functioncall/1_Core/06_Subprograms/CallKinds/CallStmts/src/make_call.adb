procedure Make_Call                            -- # proc
is
   procedure Increment (I : in out Integer) is -- # proc
   begin
      I := I + 1;                              -- # stmt
   end Increment;

   Dummy : Integer := 0;                       -- # decl
begin
   Increment (Dummy);                          -- # call
end Make_Call;
