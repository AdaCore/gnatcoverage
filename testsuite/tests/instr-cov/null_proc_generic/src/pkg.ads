package Pkg is
   generic
      type T is private;
   procedure P (Item : T);

   procedure P (Item : T) is null;
end Pkg;
