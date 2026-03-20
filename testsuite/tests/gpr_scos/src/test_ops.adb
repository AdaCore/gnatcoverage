with Inc, Dec;

procedure Test_Ops is

   procedure Assert (P : Boolean) is
   begin
      if not P then
         raise Program_Error;
      end if;
   end;

   X : Integer := 12;
begin
   Inc (X);
   Assert (X = 13);
   Dec (X);
   Assert (X = 12);
end;
