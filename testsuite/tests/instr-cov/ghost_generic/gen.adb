package body Gen is
   function Eq (X : T) return Boolean is
   begin
      return X = Val;
   end Eq;

   function Non_Eq (X : T) return Boolean is
   begin
      return X /= Val;
   end Non_Eq;
end Gen;
