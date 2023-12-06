pragma Ada_2012;
pragma Assertion_Policy (Check);

procedure Main
is
   function ATCC_Violation (X : Boolean) return Boolean
   with Pre => (X or else not X)
   is
      function Id (X : Boolean) return Boolean is
      begin
         if X or else X then
            return X;
         end if;
         return X;
      end Id;

      Dummy : Boolean := Id (True);
   begin
      return Id (False);
   end ATCC_Violation;
begin
   declare
      Dummy : Boolean := ATCC_Violation (True);
   begin
      null;
   end;
   return;
end Main;
