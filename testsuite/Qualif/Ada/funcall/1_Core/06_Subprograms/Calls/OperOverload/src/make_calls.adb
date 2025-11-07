pragma Ada_2012;

with Operators; use Operators;

procedure Make_Calls                                 -- # test
is
   T     : Boolean := True;                          -- # decl
   F     : Boolean := False;                         -- # decl
   Dummy : Boolean := True;                          -- # decl
begin
   Dummy := F + T;                                   -- # op_v
   Dummy := "+" (T, F);                              -- # op_v
   Dummy := F + T + F + T;                           -- # op_A
   Dummy := "+" (T, "+" (T, F));                     -- # op_B

   Dummy := One < Two;                               -- # op_v
   Dummy := Reversed."<" (One, Two);                 -- # op_v

   Dummy := +One = -1;                               -- # op_v
   Dummy := Reversed.Inner."+" (One) = -1;           -- # op_v

   Dummy := (40 & "Two") = 42;                       -- # op_v
   Dummy := (1.3 & 3.2 & 2.1) = 42.0;                -- # op_C

   Dummy := F + ("&" ("A", 8) = 42);                 -- # op_D
   Dummy := "+" (F, ("&" ("A", 8) = 42));            -- # op_E

   Dummy := ("A" & "B" & 42 & "D" & "E") = 42;       -- # op_F

   --  Short circuit the execution of the second call to "+"
   Dummy := (F + T) and then (F + T);                -- # short

end Make_Calls;
