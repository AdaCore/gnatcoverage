with Gen;
with Non_Ghost_Inst;

procedure Main is

   package Ghost_Pak is
      pragma Ghost;
      package Ghost_Gen is new Gen (T => Boolean, Init => True);
   end Ghost_Pak;

   Res : Boolean;
   pragma Volatile (Res);
begin
   pragma Assert (Ghost_Pak.Ghost_Gen.Eq (True));
   Res := Non_Ghost_Inst.Non_Eq (False);
end Main;
