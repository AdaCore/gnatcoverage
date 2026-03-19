pragma Ada_2012;

procedure Make_Call                                     -- # fun
is
   package Pkg1 is

     subtype T1 is Integer;                             -- # decl
     type New_T1 is new Integer;                        -- # decl

     function F1 (X : T1) return T1 is (X);             -- # fun
     function New_F1 (X : New_T1) return New_T1 is (X); -- # fun

      package Pkg2 is

         subtype T2 is Boolean;                         -- # decl

         function F2 (X : T2) return T2 is (X);         -- # fun

         package Pkg3 is

            subtype T3 is Float;                        -- # decl

            function F3 (X : T3) return T3 is (X);      -- # fun

         end Pkg3;
      end Pkg2;
   end Pkg1;

   Dummy1 : Integer := Pkg1.F1 (8);                     -- # call
   Dummy2 : Boolean := Pkg1.Pkg2.F2 (True);             -- # call
   Dummy3 : Float   := Pkg1.Pkg2.Pkg3.F3 (0.0);         -- # call
begin
   if                                                   -- # if
     Pkg1.Pkg2.F2                                       -- # d_f2
       (Pkg1.F1 (42)                                    -- # d_f1
       = 42                                             -- # deci
       and then                                         -- # deci
       Pkg1.Pkg2.Pkg3.F3 (5.0)                          -- # d_f3
       = 5.0                                            -- # deci
       and then                                         -- # deci
       Integer(Pkg1.New_F1 (0))                         -- # d_nf1
       /= 0)                                            -- # deci
       and then False                                   -- # false
   then
      null;                                             -- # null
   end if;
end Make_Call;
