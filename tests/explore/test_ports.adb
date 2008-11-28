with Links;

procedure Test_Ports is
   package Integer_Links is new Links (Data_Type => Integer);
   use Integer_Links;

   X : Integer;
   IP, OP : aliased Integer_Links.IOport (Capacity => 1);
   L : aliased Integer_Links.IOlink;
begin
   Connect (Outp => OP'access, Inp => IP'Access, Link => L);

   Push (12, OP);
   Process (L);
   Pop (X, IP);
end;
