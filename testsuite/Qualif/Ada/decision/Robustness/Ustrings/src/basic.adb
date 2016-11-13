package body Basic is

   procedure Try1 (A : String) is
      Seq : Unbounded_String := To_Unbounded_String (A); -- # decl
   begin
      if Length (Seq) > 20 then -- # test
         Last_Long := Seq;   -- # long
      else
         Last_Short := Seq; -- # short
      end if;
   end;

   procedure Try2 (A : String) is
   begin
      declare
         Seq : Unbounded_String := To_Unbounded_String (A); -- # decl
      begin
         if Length (Seq) > 20 then -- # test
            Last_Long := Seq;   -- # long
         else
            Last_Short := Seq; -- # short
         end if;
      end;
   end;

   procedure Try3 (A : String) is
   begin
      declare
         Seq : Unbounded_String := To_Unbounded_String (A); -- # decl
         L : Integer := Length (Seq); -- # decl
      begin
         if L > 20 then -- # test
            Last_Long := Seq;   -- # long
         else
            Last_Short := Seq; -- # short
         end if;
      end;
   end;

end;

