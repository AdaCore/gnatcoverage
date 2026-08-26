pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main
is
   type Animal is (Dog, Cat, Cow);

   function Foo (A : Animal; B, C : Boolean) return String;

   function Foo (A : Animal; B, C : Boolean) return String is -- # fun
   begin
      return                  -- # gexpr
        (case A is            -- # gexpr
         when Dog => "",      -- # gexpr
         when Cat => "Meow",  -- # gexpr
         when Cow =>          -- # gexpr
           (if                -- # gexpr
               B and then C   -- # gexpr
            then
               "Moooh"        -- # gepxr
            else
               "No Moo :(")); -- # gexpr
   end Foo;

   T : Boolean := True;
   F : Boolean := False;
begin
   pragma Assert (T and then not F);

   Put_Line (Foo (Dog, True, True));   -- # call
   Put_Line (Foo (Cat, True, True));   -- # call
   Put_Line (Foo (Cow, True, True));   -- # call
   Put_Line (Foo (Cow, True, False));  -- # call
   Put_Line (Foo (Cow, False, False)); -- # call
end Main;
