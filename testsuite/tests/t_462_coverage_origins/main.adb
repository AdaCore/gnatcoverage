pragma Ada_2022;
pragma Assertion_Policy (Check);

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;

--  The folling SCOs are present in this test. We expect to have origins for
--  each of them:
--  Statement
--  Decision
--  MCDC
--  ATCC
--  Function
--  Call
--  Guarded expression

procedure Main
is
   X : constant Integer := Integer'Value (Argument (1));
   Dummy : Integer := 0;

begin

   -- DECISION     covered for X = 1
   -- DECISION not covered for X = 1
   if X = 1 then
      -- STATEMENT     covered for X = 1
      -- STATEMENT not covered for X = 2
      Dummy := X;

      declare
         type Animal is (Dog, Cat, Cow);

         function Gexpr (A : Animal; B, C : Boolean) return String;

         --  FUNCTION covered for X >= 0
         function Gexpr (A : Animal; B, C : Boolean) return String is
         begin
            -- GUARDED EXPR covered for X >= 0
            return
              (case A is
               when Dog => "",
               when Cat => "Meow",
               when Cow =>
                 (if
                     B and then C
                  then
                     "Moooh"
                  else
                     "No Moo :("));
         end Gexpr;

      begin
         --  CALL covered for X >= 0
         Put_Line (Gexpr (Dog, True, True));
         Put_Line (Gexpr (Cat, True, True));
         Put_Line (Gexpr (Cow, True, True));
         Put_Line (Gexpr (Cow, True, False));
         Put_Line (Gexpr (Cow, False, False));
         null;
      end;

      declare
         Dummy_T : Boolean := True;
         Dummy_F : Boolean := False;
      begin
         --  ATCC covered for X = 1
         pragma Assert (Dummy_T and then not Dummy_F);
      end;
   end if;

   -- DECISION partially covered to True for X > 0
   if X >= 0 then
      Dummy := X;
   end if;

   -- DECISION not covered for X < 0
   if X < 0 then
      Dummy := X;
   end if;

   if X = 2 then
      declare
         procedure Foo (L,R : Boolean);
         procedure Foo (L,R : Boolean) is
         begin
            -- MCDC     covered for X = 2
            -- MCDC not covered for X = 0 and X = 1
            if L and then R then
               Dummy := X;
            end if;
         end Foo;
      begin
         --  STATEMENT BLOCK
         Foo (False, False);
         Foo (True,  False);
         Foo (False, True);
         Foo (True,  True);
      end;
   end if;
end Main;
