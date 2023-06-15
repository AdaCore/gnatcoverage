generic
   type T is (<>);
   Init : T;
package Gen is
   Val : T := Init;
   function Eq (X : T) return Boolean;
   function Non_Eq (X : T) return Boolean;
end Gen;
