package body BB_Pack is
begin
   Sum := X + Y; -- # elab

   declare
      Prod : Integer := X * Y; -- # elab
   begin
      Sum_Plus_Prod := Sum + Prod;
   end;
end;
