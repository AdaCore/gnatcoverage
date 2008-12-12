package body Actors is

   function Live (A : Actor'Class) return Boolean is
   begin
      return A.Live;
   end;

   procedure Kill (A : in out Actor'Class) is
   begin
      A.Live := False;
   end;
end;
