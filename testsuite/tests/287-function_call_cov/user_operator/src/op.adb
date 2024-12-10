pragma Ada_2012;

with Reversed; use Reversed;

procedure Op
is
   T : Boolean := True;                                  -- # decl
   F : Boolean := False;                                 -- # decl
   Dummy : Boolean := True;                              -- # decl

   function "+" (Left, Right : Boolean) return Boolean   -- # fun
   is (Left and Right);                                  -- # fun

   function "+" (Arg : Reversed.T) return Reversed.T     -- # fun
   is (0 - Arg);                                         -- # fun

   function "&" (L,R : Float) return Float               -- # fun
   is (42.0);                                            -- # fun

   function "&" (L : Integer; R : String) return Integer -- # fun
   is (42);                                              -- # fun

   function "&" (L : String; R : Integer) return Integer -- # fun
   is (42);                                              -- # fun

begin
   Dummy := F + T;                                       -- # ok
   Dummy := "+" (T, F);                                  -- # ok
   Dummy := F + T + F + T;                               -- # ok
   Dummy := "+" (T, "+" (T, F));                         -- # ok

   Dummy := One < Two;                                   -- # ok
   Dummy := Reversed."<" (One, Two);                     -- # ok

   Dummy := +One = -1;                                   -- # ok
   Dummy := Reversed.Inner."+" (One) = -1;               -- # ok

   Dummy := (40 & "Two") = 42;                           -- # ok
   Dummy := (1.3 & 3.2 & 2.1) = 42.0;                    -- # ok

   Dummy := F + ("&" ("A", 8) = 42);                     -- # ok
   Dummy := "+" (F, ("&" ("A", 8) = 42));                -- # ok

   Dummy := ("A" & "B" & 42 & "D" & "E") = 42;           -- # ok

   if F then                                             -- # if
       Dummy := F + T;                                   -- # ko
       Dummy := "+" (T, F);                              -- # ko
       Dummy := F + T + F + T;                           -- # A_ko
       Dummy := "+" (T, "+" (T, F));                     -- # B_ko

       Dummy := One < Two;                               -- # ko
       Dummy := Reversed."<" (One, Two);                 -- # ko

       Dummy := +One = -1;                               -- # ko
       Dummy := Reversed.Inner."+" (One) = -1;           -- # ko

       Dummy := (40 & "Two") = 42;                       -- # ko
       Dummy := (1.3 & 3.2 & 2.1) = 42.0;                -- # C_ko

       Dummy := F + ("&" ("A", 8) = 42);                 -- # D_ko
       Dummy := "+" (F, ("&" ("A", 8) = 42));            -- # E_ko

       Dummy := ("A" & "B" & 42 & "D" & "E") = 42;       -- # F_ko
   end if;
end Op;
