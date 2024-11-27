pragma Ada_2012;

procedure Enum is
   type Color is (Red, Green, Blue, Not_On);       -- # decl

   Pixel_1 : Color := Red;                         -- # decl
   Pixel_2 : Color := Green;                       -- # decl
   Pixel_3 : Color := Blue;                        -- # decl

   Nb_Red : Integer := 0;                          -- # decl
begin

   --  In an if statement
   if Pixel_1 not in Red then                      -- # if
     Nb_Red := Nb_Red + 1;                         -- # stmt_no
   end if;

   --  In a case expression
   Nb_Red := Nb_Red +                              -- # stmt_yes
      (case Pixel_2 is                             -- # stmt_yes
          when Red            => 1,                -- # stmt_yes
          when Green .. Green => 0,                -- # stmt_yes
          when Blue | Not_On  => 0);               -- # stmt_yes

   --  In a case statement
   case Pixel_3 is                                 -- # stmt_yes
      when Red            => Nb_Red := Nb_Red + 1; -- # stmt_no
      when Green .. Green => null;                 -- # stmt_no
      when Blue | Not_On  => null;                 -- # stmt_yes
   end case;
end Enum;
