package Services is
   type Category is (A, B, C);
   type Object (Cat : Category) is null record;
   type Event (Cat : Category) is null record;

   function Services (E : Event; O : Object) return Boolean;
end;
