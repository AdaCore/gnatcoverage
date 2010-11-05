package body Services is

   function Services (E : Event; O : Object) return Boolean is

      --  Object O can service even E if they are of identical categories.
      --  Resort to local subprograms homonyms with the outer one to compare.

      procedure Services (E : Event; Cat : Category; R : out Boolean) is
      begin
         R := E.Cat = Cat; -- # checkEcat
      end;
      procedure Services (O : Object; Cat : Category; R : out Boolean) is
      begin
         R := O.Cat = Cat; -- # checkOcat
      end;

      E_Services_Ocat, O_Services_Ecat : Boolean;
   begin
      Services (E, O.Cat, E_Services_Ocat); -- # checkEcat
      if not E_Services_Ocat then           -- # checkEcat
         return False;  -- # outEservOcat
      end if;

      Services (O, E.Cat, O_Services_Ecat); -- # checkOcat
      if not O_Services_Ecat then           -- # checkOcat
         return False;  -- # outOservEcat
      end if;

      return True; -- # outOk
   end;

end;
